#------------------------------------------------------------------------------#
# TO DO:                                                                       #
#
# 1. Function should prevent comparison of unequal tMP.
# 2. Function should prevent comparison of unequal nT.
# 3. Function should optionally prevent comparison of unequal replicates (check).
# 4. Dimension header to 100 simulations, showing 10.
# 5. Need to add procedure label to simulation list.
# 6. Use standard Layout and Options controls, with axis limits vectors.
# 7. Can lose the Constants, tMP, nT, nReps reporting boxes.
# 8. Can use variable width fields in the header.
# 9. Need to report number of simulations - Selected/Total ?
# 10. Add Select All, Clear buttons.
# 11. When coming from guiView or guiOpmod should pass iSim and check it?
# 12. Determine whether droplist can be used to add Time Periods, or maybe a
#     sliding object of size 10 with 3 displayed? Period minYr, maxYr?
# 13. Choice of zone multipliers needs to be bundled with "Objectives".
# 14. The objectives need to have descriptive names Pr(SSB > USR) or
#     Pr(SSB > LRP).  Could we make it so any objective can be displayed?
#     Yes - use an object with say 10 rows and 3 shown.  objName, target, prob,
#     time, and then another one with objName, target, time, prob.
#     Defaults are Canadian policy - Pr(SSB > LRP), where LRP set by zone mult.
#     Pr(SSB > TARG), where TARG set by something new on zone control. Use the
#     reference points.  Pull down Fmsy, F0.1, etc, etc, User (default Bmsy)...
#     Could have a field with Prob, Time, Dep, SSB, Catch, etc targName, targValue..
# 15. Estimation creates additional plots, e.g., quantile plots of SSB by simulation.
# 16. Estimation allows bias plots, and probabilities of false pos, false neg.
# 17. Need to add in COSEWIC criteria.
# 18. Prob( increase ), Mag( Change ).
#
# (c) mseR: Management Strategy Evaluation in R, Version 3.x                   #
#                                                                              #
#     Copyright 2008, 2009 by A.R. Kronlund and S.P. Cox.                      #
#                                                                              #
#     This software comes with ABSOLUTELY NO WARRANTY, expressed or implied.   #
#     You have been provided with a copy of mseR for educational purposes.     #
#     You are requested not to redistribute this software without seeking      #
#     permission from the authors.                                             #
#                                                                              #
#     Of course, comments and suggestions for improvement greedily accepted.   #
#                                                                              #
#      "Pretty good management will do."  Bill de la Mare, Dec. 19, 2006.      #
#                                                                              #
#          "Success consists of going from failure to failure without          #
#                  loss of enthusiasm."  Winston Churchill.                    #
#                                                                              #
#------------------ mseRPerfFuns.r: mseR Perf GUI Functions -------------------#
#--                                                                          --#
#-- mseRperfr: An mseR module that provides performance statistics from      --#
#              feedback simulations.                                         --#
#--                                                                          --#
#-- Logic Flow: guiPerf  -> .guiPerfSetup -> .wkDirSetup -> createWin        --#
#--                      -> .subGuiPerf                                      --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          J.S. Cleary (Pacific Biological Station, Nanaimo, B.C.)         --#
#--          K.   Holt (Pacific Biological Station, Nanaimo, B.C.)           --#
#--                                                                          --#
#-- Contributors:                                                            --#
#--                                                                          --#
#--          A.F. Sinclair (contributions from PBSref)                       --#
#--          T.K. Deering  (contributions from PBSref)                       --#
#--                                                                          --#
#-- First Implementation: 17-Feb-10 from mseROpMod and mseR v2.0.             -#
#--                                                                          --#
#                                                                              #
# NOTES:                                                                       #
#                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
# 2. Unique file names can be created by command:                              #
#                                                                              #
#    paste( "sim", format( Sys.time(), format="%d%m%Y%H%M%S" ), sep="" )       #
#                                                                              #
# 3. Any information read from a text box has a linefeed \n the end of the     #
#    input that needs to be removed, in most cases, before use.                #
# 4. Very inelegant crashes can occur if the browser is called when GUI open.  #
#    This is a PBSmodelling/TkTcl issue and not something we can fix.          #
# 5. PBSModelling has the very undesirable of changing character vectors to    #
#    factor variables within the GUI parameter list.  Not good.  Watch out.    #
#                                                                              #
# REQUIRES: PBSmodelling, RODBC                                                #
#                                                                              #
# References:                                                                  #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. 2008. PBS          #
#   Modelling 2.00: user�s guide revised from Canadian Technical Report of     #
#   Fisheries and Aquatic Science 2674: v+146 p. Last updated October 23, 2008 #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
# guiPerf       : Run performance statistics GUI.                              #
#                                                                              #
# GUI Error Checking/Validation:                                               #
#                                                                              #
# .validGuiPerfPars: Check if the Perf GUI parameters are valid.               #
#                                                                              #
# GUI Submit Control Functions:                                                #
#                                                                              #
# .subGuiPerf   : Processes guiPerf submit actions, e.g., buttons.             #
#                                                                              #
# GUI Helper Functions:                                                        #
#                                                                              #
# GUI Plot Functions:                                                          #
#                                                                              #
# NOTE: Indented funcition names are nested within .doPerfPlots.               #
#                                                                              #
# GUI Hidden Functions (to show hidden function ls(all.names=TRUE):            #
#                                                                              #
# .errorMessage : Writes a generic error message to the console.               #
# .getWinName   : Get the current GUI window name.                             #
# .PerfHdrStatus: Updates the status of the header table in guiPerf.           #
# .wkDirSetup   : Working directory set-up.                                    #
#------------------------------------------------------------------------------#

source( "mseRglobals.r" )
source( "mseRrefPoints.r" )
source( "mseRtools.r" )

#-----------------------------------------------------------------------------##
#-- mseR Perf Functions                                                     --##
#-----------------------------------------------------------------------------##

# guiPerf     (Run mseR feedback loop performance statistics )
# Purpose:    Set up and run the Perf GUI to inspect individual replicates.
# Parameters: None
# Returns:    NULL (invisibly)
guiPerf <- function()
{
  return( .guiPerfSetup("mseRguiPerf"))
}

#------------------------------------------------------------------------------#
#-- Gui Error Checking                                                       --#
#------------------------------------------------------------------------------#

# .validGuiPerfPars (valid parameters for simulation GUI):
# Purpose:       Check whether the parameters supplied in Perf GUI are valid.
#                If invalid, display an error message in the R console and
#                clear the invalid field.
#                If it is a correctable field, corrects it in the GUI and does
#                not flag it as invalid
# Parameters:    None
# GUI inputs:    Parameters taken directly from active guiSim. 
# Returns:       TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:   Clears invalid fields
#                Corrects correctable fields (if any)
.validGuiPerfPars <- function( nReps=.MAXREP )
{
  # Get the GUI  values and make them local to this function.
  getWinVal( scope="L" )
  
  changes <- list()                  
  isValid <- TRUE

  # Simulation index.  
  #if ( is.na(iSim) || (iSim < 1) || (iSim > nrow(viewHeader)) )
  #{
  #  cat( "\nSimulation ID must be 0 < iSim <= nSim.\n" )
  #  changes$iSim <- 1
  #  isValid <- FALSE
  #}
  
  # Replicate index.
  if ( is.na(iRep) || (iRep < 1) || (iRep > nReps) )
  {
    cat( "\nReplicate ID must be 0 < iRep <= ",nReps,".\n" )
    changes$iRep <- 1
    isValid <- FALSE
  }
  
  # Fix X-axis: Year range.
  if ( is.na(minYear) || (minYear < 0) || (minYear > minYear) )
  {
    cat( "\nMinimum year must be > 0 and <= maximum year.\n" )
    changes$minYear <- NA
    isValid <- FALSE
  }
  
  if ( is.na(maxYear) || (maxYear < 0) || (maxYear <= minYear) )
  {
    cat( "\nMaximum year must be > 0 and > xMinYr.\n" )
    changes$minYear <- NA
    isValid <- FALSE
  }
  
  # Fix Y-axis: Total Biomass.
  if ( is.na(minBio) || (minBio < 0) || (minBio > maxBio) )
  {
    cat( "\nMinimum total biomass must be > 0 and <= maximum total biomass.\n" )
    changes$minBio <- NA
    isValid <- FALSE
  }
  
  if ( is.na(maxBio) || (maxBio < 0) || (maxBio <= minBio) )
  {
    cat( "\nMaximum total biomass must be > 0 and >= minimum total biomass.\n" )
    changes$maxBio <- NA
    isValid <- FALSE
  }
  
  # Set the changes into the GUI.  
  setWinVal( changes )
  return( isValid )
}

#-----------------------------------------------------------------------------##
#-- GUI Submit Functions                                                    --##
#-----------------------------------------------------------------------------##

# .subGuiPerf    (Processes guiPerf submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiPerf program flow:
#             - Attempts to check validity of the GUI parameters;
#             - Buttons to move between simulations;
#             - Buttons to move between replicates;
#             - Determines what plot is displayed;
#             - ANIMATE the plot or EXIT the GUI.
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
#
#             The plot layout parameters are checked in .doPerfPlots rather
#             than here.
# Source:     A.R. Kronlund
.subGuiPerf <- function()
{
  win     <- .getWinName()                       # Get the current window name
  gvar    <- paste( ".", win, sep="" )           # Global variable name
  
  guiInfo <- getWinVal( scope="L", winName=win ) # GUI information local scope
  act     <- getWinAct()[1]                      # Get last menu window action
  guiChanges <- guiInfo                          # Make a copy for changes.

  status <- .setGuiSync( simulation[[iSim]], winName=win )
  
  if ( status$sim==TRUE )
  {
    # .guiPerfSetup has already loaded the first blob into the working directory.
    nReps   <- blob$pars$nReps                     # Total number of replicates.
    # Validity check.
    valid   <- .validGuiPerfPars(nReps)            # Are guiPerf parameters valid?
  }
  else
  {
    valid <- FALSE                                 # No feedback results to Perf.
    #bringToTop(-1)
  }

  #-- Check which button was pressed, if any.
  #--- But you have to allow access to simulation widget...
  
  # EXIT the Simulation GUI (leave graphics on).
  if ( act=="vwExit" )
  {
    .closeActWin()
  }
    
  # Clear the graphics windows.
  if ( act=="vwGfxOff" )
  {
    graphics.off()
  }    
  
  # NEXT simulation.
  if ( act=="vwNextSim" )
  {
    # Increment the simulation counter, but do not permit iSim > nSim.
    iSim <- min( (iSim+1), nSim )
    
    # Update the reference points on the GUI from the next simulation.
    guiChanges <- .updateGuiRefPoints( guiChanges, simulation[[iSim]]$om,
                  pass=TRUE, digits=4 )   
    
    # Update the parameter object widget.
    guiChanges$opModPars <- simulation[[iSim]]$om$opModPars
    
    # Update the simulation information.
    guiChanges$iSim    <- iSim
    guiChanges$simName <- names(simulation)[iSim]
    
    # Save the global GUI parameters.
    gui <- list( iSim=guiChanges$iSim, nSim=nSim, iRep=iRep )
    assign( "gui", gui, pos=1 )
      
    status <- .setGuiSync( simulation[[guiChanges$iSim]], winName=win )
            
    # Update the GUI.
    setWinVal( guiChanges,winName=win )
  }
  
  # PREVIOUS simulation.
  if ( act=="vwPrevSim" )
  {
    # Decrement the iSim counter, but do not permit iSim < 1.
    iSim <- max( 1, (iSim-1) )
    
    # Update the reference points GUI from the previous simulation.
    guiChanges <- .updateGuiRefPoints( guiChanges, simulation[[iSim]]$om,
                  pass=TRUE, digits=4 )   

    # Update the parameter object widget.
    guiChanges$opModPars <- simulation[[iSim]]$om$opModPars
    
    # Update the simulation tracking information.
    guiChanges$iSim    <- iSim
    guiChanges$simName <- names(simulation)[iSim]
        
    # Save the global GUI parameters.
    gui <- list( iSim=guiChanges$iSim, nSim=nSim, iRep=iRep )
    assign( "gui", gui, pos=1 )
      
    status <- .setGuiSync( simulation[[guiChanges$iSim]], winName=win )    
      
    # Update the GUI.
    setWinVal( guiChanges, winName=win )
  }
  
  # This resets the iSim and iRep counters in the GUI, then updates locally.
  #setWinVal( guiChanges, winName=win )               # Set changed GUI parameters.
  #guiChanges <- getWinVal( scope="L", winName=win )  # Get GUI parameters, local. 
    
  if ( (act=="vwPrevSim") || (act=="vwNextSim") || (act=="iSim") )
  {
    # Remove whitespace from field read from GUI text box.
    #fileName <- gsub( " ", "", perfHeader$Rdata[iSim] )

    if ( status$sim==TRUE )
    {      
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simulation[[iSim]]$om$stamp,".Rdata",sep="" )
      cat( "\n(.subGuiPerf) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
      
      valid <- TRUE
      setWidgetState( varname="perfPlotType", state="normal" )

    }
    else
    {
      valid <- FALSE
      setWidgetState( varname="perfPlotType", state="disabled" )
    }
  }
    
  if ( act=="vwExpSim" )
  {
   if ( exists( "simulation" ) )
      .saveSimulationToRdata()
    else
      cat( "(.subGuiPerf) No simulation tracking list available to export.\n" )
  }    
    
  if ( act=="vwImpSim" )
  {
    # *** Need to put a check in to ensure that a valid simulation tracking
    #     list was loaded...
    
    .loadSimulationFromRdata()
    
    # At this time I will assume that the old iSim position should not be
    # restored, rather we will start fresh.
    
    # Update the reference points from first scenario.
    guiChanges <- .updateRefPoints( guiChanges, pass=TRUE, simulation[[1]]$om,
                  digits=4 )
    
    # Update the pars object.
    guiChanges$opModPars <- simulation[[1]]$opModPars
      
    # Update the simulation information.
    guiChanges$iSim    <- 1
    guiChanges$nSim    <- length(simulation)
    guiChanges$simName <- names(simulation)[1]
      
    status <- .setGuiSync( simulation[[guiChanges$iSim]], winName=win )      
      
    # Update the GUI.
    setWinVal( guiChanges,winName=win )
      
    if ( status$sim==TRUE )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simulation[[1]]$om$stamp,".Rdata",sep="" )
      cat( "\n(.subGuiPerf) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
      
      valid <- TRUE
    }
    else
      valid <- FALSE
      
    # Save the global GUI parameters.
    gui <- list( iSim=1, nSim=nSim, iRep=1 )
    assign( "gui", gui, pos=1 )           
    
    # Update the plots.
    #.doPerfPlots( obj=blob, iSim=1, iRep=1 )       
  }
    
  if ( act=="vwTables" )
  {
    .saveSimulationToExcel( .FXLS )
    openFile( .FXLS )    
  }
  
  if ( valid )
  {
    # NEXT replicate.
    if ( act=="vwNextRep" )
    {
      # Do not permit iRep > nReps.
      guiChanges$iRep <- min( (iRep + 1), nReps )
      setWinVal( guiChanges, win )
    }
    
    # PREVIOUS replicate.
    if ( act=="vwPrevRep" )
    {
      # Do not permit iReps < 1.
      guiChanges$iRep <- max( 1, (iRep-1) )
      setWinVal( guiChanges, win )
    }  
  
    # Save the Perf GUI parameters to a global in the working environment.
    if ( !act=="vwExit" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      .doGuiPerfPlots( obj=blob, iSim=iSim, iRep=iRep )
    }  
  }
  else
  {
    if ( status$sim==TRUE )
      setWidgetState( varname="perfPlotType", state="normal" )
    else
      setWidgetState( varname="perfPlotType", state="disabled" )
  }
  
  #else
  #{
  #  bringToTop(-1)
  #  # EXIT the Simulation GUI (leave graphics on).
  #  if ( act=="vwExit" )
  #  {
  #    .closeActWin()
  #  }
  #}
  
  return( invisible() )  
}

#------------------------------------------------------------------------------#
#-- GUI Helper Functions                                                     --#
#------------------------------------------------------------------------------#

# .guiPerfSetup  (perf setup for GUI creation)
# Purpose:    Set up and run the specified GUI
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.guiPerfSetup <- function( win )
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL

  # Copy the GUI window description files and any .exe files to temp directory.
  dir <- .wkDirSetup()
  cat( "(.guiPerfSetup) Working directory setup in ",dir,"\n" )
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  graphics.off()
  
  # Can a menu be created?
  goMenu <- FALSE
  
  # Does a simulation list exist in the global workspace (NOT working directory)?
  # *** Need to re-think this - remove first If block?
  
  if ( exists( "simulation" ) )
  {
    cat( "(.guiPerfSetup) Found simulation list.\n" )
    
    # Create objects required for PBSmodelling menus in the working directory.
    assign ( "opModPars", simulation[[1]]$om$opModPars, pos=1 )
    goMenu <- TRUE
  }
  # Does a simulation .Rdata file exist in the working directory?
  else if ( file.exists( .FSIM ) )
  {
    # Load a default simulation list.
    load( .FSIM )
    cat( "(.guiPerfSetup) Found simulation file: ",.FSIM,"\n" )
    
    # Create objects required for PBSmodelling menus in the working directory.
    assign ( "opModPars", simulation[[1]]$om$opModPars, pos=1 )
    goMenu <- TRUE  
  }

  # Valid conditions exist for menu creation, values can only come from
  # a saved simulation list so all should be correct.
  
  # *** Validity checking for header information here - function needs to hit
  #     the simulation tracking list.
  
  # *** Hack to give a temporary header.
  # *** Rules apply here, including excluding those simulations without feedback
  #     results.
  pfSimList <- data.frame( simID=character(100), simLabel=character(100),
     scenario=character(100), tMP=numeric(100), nT=numeric(100), nReps=numeric(100),
     Select=rep(FALSE,100), Rdata=character(100) )
  assign( "pfSimList", pfSimList, pos=1 )
  
  periodList <- data.frame( periodName=c("Short","Medium","Long","Interim"),
                  Year1=c(1,1,1,10), Year2=c(10,20,40,20) )
  assign( "periodList",periodList,pos=1 )
  
  if ( goMenu )
  {
    # Initialize the GUI from the PBSmodelling windows description file.
    createWin( paste( dir, "/", win, "Win.txt", sep="" ) )
    
    # Get the GUI parameters and make scope local to this function.
    guiInfo    <- getWinVal( winName=win )
    guiChanges <- list()
    
    #setWidgetState( varname="viewPlotType", state="disabled", radiovalue="vwHCR",       win )              
           
    # Initialize guiPerf action.
    if ( win=="mseRguiPerf" )
    {
      # Display the last viewed simulation in the last session, if it existed.
      if ( exists("gui") )
        guiChanges$iSim <- gui$iSim
      else
        guiChanges$iSim <- 1
        
      # Display the last viewed replicate in the last session, if it existed.
      if ( exists("gui") )
        guiChanges$iRep <- gui$iRep
      else
        guiChanges$iRep <- 1

      # Round the reference points for display.
      guiChanges <- .updateGuiRefPoints( guiChanges,
                       simulation[[guiChanges$iSim]]$om, pass=TRUE, digits=4 )
      guiChanges$opModPars <- simulation[[guiChanges$iSim]]$om$opModPars
            
      # Update simulation tracking variables, position display at iSim=1.
      guiChanges$simName <- names(simulation)[guiChanges$iSim]
      guiChanges$nSim    <- length(simulation)
      
      # Update the GUI, and re-extract the GUI parameters.
      setWinVal( guiChanges, win )
      guiInfo <- getWinVal( winName=win, scope="L" )
      
      # Update synchronization status.
      status <- .setGuiSync( simulation[[guiChanges$iSim]], winName=win )
     
      # Is there a sync'ed feeback simulation?
      if ( status$sim==TRUE )
      {
        setWidgetState( varname="perfPlotType", state="normal" )
        # Load an Rdata working directory containing a list called blob.
        simFile <- paste( "sim",simulation[[iSim]]$om$stamp,".Rdata",sep="" )
        cat( "\n(.guiPerfSetup) Loading",simFile,"...\n" )    
        load( file=simFile )
        assign( "blob", blob, pos=1 )
      
        # *** Change to "gui" when done, make nReps and nRep consistent.
        guiChanges$nRep <- blob$simGuiPars$pars$nReps
        
        # Create initial plots.    
        .doGuiPerfPlots( simulation[[iSim]]$om )
      }
      else
      {
        setWidgetState( varname="perfPlotType", state="disabled" )
      }
    
      # Update the GUI, and re-extract the GUI parameters.
      setWinVal( guiChanges, win )
      guiInfo <- getWinVal( winName=win, scope="L" )
     
       # Save the global GUI parameters.
      if ( exists("gui") )
        gui <- list( iSim=guiChanges$iSim, nSim=guiChanges$nSim, iRep=gui$iRep )
      else
        gui <- list( iSim=guiChanges$iSim, nSim=guiChanges$nSim, iRep=1 )
      assign( "gui", gui, pos=1 )

      # Save all the GUI parameters, including ref points to working directory.
      # *** This will be used later to restore the last user settings.      
      assign( ".guiPerfPars",guiInfo,pos=1 )     
     
      # Create initial plots.    
      #.doGuiPerfPlots( simulation[[iSim]]$om )
    }     # if goMenu
  }
  else
    cat( "\nERROR (.guiPerfSetup): GUI creation not possible.\n ")  

  return( invisible() )
}     # .guiPerfSetup

#-----------------------------------------------------------------------------##
#-- Helper Functions (some HIDDEN, e.g., .foo)                              --##
#-----------------------------------------------------------------------------##

# simLegend   (Adds a legend to a plot that identifies the simulation)
# Purpose:    Adds a plot legend that identifies the simID, scenario, procedure,
#             current replicate (if applicable), and number of replicates in sim.
# Parameters: iSim, iRep, nReps, simFile, simID, scenario, procedure, showFile.
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
simLegend <- function( iSim,iRep,nReps, simFile="",simID="",scenario="",
                       procedure="", showFile=FALSE, ... )
{
  # Sim string. 
  simTxt <- paste( "[",iSim,"] ",simID, sep="" )

  # Rep string: if a replicate view, then indicate replicate ID and nReps.
  #             else just indicate the number of replicates in the simulation.
  if ( iRep > 0 )
    repTxt <- paste( " Rep ",iRep,"/",nReps, sep="" )
  else
    repTxt <- paste( " Reps: ",nReps, sep="" )

  # Scenario and procedure ID string: if procedure string supplied, then add it
  # to the idTxt string, else just add the scenario label.
  if ( procedure!="" )
    idTxt  <- paste( " Scenario: ",scenario,",  Proc: ",procedure,sep="" )
  else
    idTxt  <- paste( " Scenario: ",scenario,sep="" )
  
  # if showFile=T, then show the Rdata file name that holds the simulation.
  if ( showFile )
    txt <- paste( simTxt,repTxt,idTxt,simFile, collapse="" )
  else
    txt <- paste( simTxt,repTxt,idTxt, collapse="" )
 
  # If a replicate view, then add the legend to the outer plot margin,
  # else add it to the plot area as in the case of guiPerf graphics.
  if ( iRep > 0 )
    mtext( cex=.VIEWLEGCEX, outer=TRUE, txt,... )
  else
    panLab( 0.05,0.95, adj=0, cex=.VIEWLEGCEX, txt )
     
  return( invisible() )
}

# .wkDirSetup (Working directory set-up):
# Purpose:    Creates three (3) sub-directories if they do not exist:
#             (a) .FTEMP - contains R code and PBSModelling GUI control files
#                 copied from working directory.
#             (b) .FHELP - contains help files copied from working directory.
#             (c) .FDOCS - contains code and model documentation.
# Notes:      In "shell", translate=TRUE turns forward slash / to backslash \.
# Parameters: None
# Returns:    The full path to the new directory
# Source:     mseR V2.0, modified from PBSref
.wkDirSetup <- function()
{
  pckg  <- .PACKAGE                      # Current package name.
  fdocs <- .FDOCS                        # Directory for documentation files.
  fhelp <- .FHELP                        # Directory for help files.
  ftemp <- .FTEMP                        # Directory for temporary files.
  wkDir <- getwd()                       # Current R working directory.
  
  # These two will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)          # Path to the R directory
  #fnam <- paste(rdir, dname, fils, sep = "/")  # the files you want to copy
  	
	# Create .FDOC directory if needed, then copy any required files.
	docDir <- paste( wkDir, .FDOCS, sep="/" )
	if ( !file.exists(docDir) )
	  shell( paste( "mkdir", docDir ), translate=TRUE )
	
	# Create. FHELP directory if needed, then copy any required files.
	helpDir <- paste( wkDir, .FHELP, sep="/" )
	if ( !file.exists(helpDir) )
	  shell( paste( "mkdir", helpDir ), translate=TRUE )
	
	helpFiles <- c( "mseRguiRefAbout.txt" )
	
	srcFiles <- paste( wkDir,   helpFiles, sep="/" )    # Source files.
	tarFiles <- paste( helpDir, helpFiles, sep="/" )    # Target files.
	file.copy( srcFiles, tarFiles, overwrite = TRUE )
	
	# Create .FTEMP directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .FTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  tempFiles <- c(
                   "mseRperf.r",
                   "mseRguiPerfWin.txt",
                   "mseRabout.txt"
                )

  srcFiles <- paste( wkDir,   tempFiles, sep="/" )    # Source files.
  tarFiles <- paste( tempDir, tempFiles, sep="/" )    # Target files.
  file.copy( srcFiles, tarFiles, overwrite = TRUE)
  
  return(wkDir)
}

# *** Move this to mseRguiPerf.r when done.
# .doGuiPerfPlots  (Wrapper to get guiPerf parameters and call the required plot)
# Purpose:      Calls the plot specified by perfPlotType that is returned from
#               perf GUI (guiPerf).
# Parameters:   NONE.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.doGuiPerfPlots <- function( obj, iSim=1, iRep=1, gfx=NULL )
{
  guiInfo <- getWinVal(scope="L", winName="mseRguiPerf") # GUI info local scope.
  act     <- getWinAct()[1]                              # Last menu window action
  if ( is.null(act) )
    act <- ""

  # Some plots can be animated.  If animate is TRUE then a plot function that
  # mirrors the static plot is called.
  if ( !is.null(act) && act=="vwAnimate" )
    animate=TRUE
  else
    animate=FALSE

  nReps <- blob$pars$nReps   # Determine the number of replicates.

  #----------------------------------------------------------------------------#
  # Graphics layout setup                                                      #
  #----------------------------------------------------------------------------#

  # Save par settings that can be restored (some cannot, hence no.readonly=TRUE).
  #oldpar <- par( no.readonly=TRUE )

  # If vwAuto changes to TRUE, then reset graphics window.

  # Either the number of rows or columns changed, or byrow changed so restart
  # the graphics page with the new settings.

  # If vwAuto is TRUE, then do nothing, else change par settings.
  if ( act=="vwAuto" || act=="vwNrows" || act=="vwNcols" || act=="vwPlotByRow" )
    dev.off()

  if ( vwAuto )
    par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(1,1) )
  else
  {
    if ( act=="vwNrows" || act=="vwNcols" || act=="vwPlotByRow" )
    {
      if ( vwPlotByRow )
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(nrows,ncols) )
      else
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfcol=c(nrows,ncols) )
    }
  }

  #----------------------------------------------------------------------------#
  # Selectivity by gear, discard rate by gear, discard mortality rate by gear. #
  #----------------------------------------------------------------------------#

  # Selectivity by length and gear.
  if ( perfPlotType=="vwSlg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minLen, maxLen )

    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minSel,maxSel )
    .plotSlg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
              bygear=vwGears, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Selectivity by age, length and gear.
  if ( perfPlotType=="vwSalg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minSel,maxSel )
    .plotSalg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
               doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Proportion discarded at length and age.
  if ( perfPlotType=="vwPlg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c(minLen,maxLen)
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minDis,maxDis )

    .plotPlg( blob$pars, gfx=list( annotate=vwAnnotate, autolayout=vwAuto,
              bygear=vwGears, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Proportion discarded at age, length and gear.
  if ( perfPlotType=="vwPalg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minDis,maxDis )
    .plotPalg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
               doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }
  
  #----------------------------------------------------------------------------#
  # Age Structure Plots                                                        #
  #----------------------------------------------------------------------------#
  
  if ( perfPlotType=="vwCatgProp" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minAge,maxAge )
      
    # Pass the nGear catch-at-age proportions for the replicate.
    .plotCatAgeBubbles( blob$om$uCatg[iRep,,,], gearNames=blob$pars$gNames,
      gfx=list( annotate=vwAnnotate, bygears=vwGears, doLegend=vwLegend,
      xLim=xLim, yLim=yLim ) )
  }
  
  if ( perfPlotType=="vwCatgFreq" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minYear,maxYear )
    
    .plotCatAgeFreq( blob$om$uCatg[iRep,,,], gearNames=blob$pars$gNames,
      gfx=list( annotate=vwAnnotate, bygears=vwGears, doLegend=vwLegend,
      showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  #----------------------------------------------------------------------------#
  # Combination Plots                                                          #
  #----------------------------------------------------------------------------#

  if ( (perfPlotType == "vwBioCatF") && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
      
    yLim1 <- NULL
    yLim2 <- NULL
    yLim3 <- NULL      
    if ( vwSetYaxis )
    {
      yLim1 <- c( minSSB,maxSSB )
      yLim2 <- c( minCat,maxCat )
      yLim3 <- c( minF,  maxF   )
    }
    .plotBioCatF( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim1=yLim1, yLim2=yLim2,
      yLim3=yLim3 ) )
  }
  else if ( (perfPlotType == "vwBioCatF") && (animate==TRUE) )
  {
    .doAnimate( blob, perfPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRperfGui")
    .plotBioCatF( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
    animate <- FALSE
  }

  if ( (perfPlotType == "vwBioCatR") && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
      
    yLim1 <- NULL
    yLim2 <- NULL
    yLim3 <- NULL      
    if ( vwSetYaxis )
    {
      yLim1 <- c( minSSB,maxSSB )
      yLim2 <- c( minCat,maxCat )
      yLim3 <- c( minRec,maxRec )
    }
    .plotBioCatR( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim1=yLim1, yLim2=yLim2,
      yLim3=yLim3 ) )
  }
  else if ( (perfPlotType == "vwBioCatR") && (animate==TRUE) )
  {
    .doAnimate( blob, perfPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRperfGui")
    plotBioCatR( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
    animate <- FALSE
  }

  #----------------------------------------------------------------------------#
  # Time-series plots                                                          #
  #----------------------------------------------------------------------------#

  if ( perfPlotType == "vwBiomass" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear, maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minSSB, maxSSB )
    .plotBiomass( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                  doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwBioSurvey" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minSSB,maxSSB )
    .plotBioSurvey( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      bygears=vwGears, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwCatch" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minCat,maxCat )
    .plotCatch( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwCtg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minCat,maxCat )
    .plotCtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwDtg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minCat,maxCat )
    .plotDtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwCatDis" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minCat, maxCat )
    .plotCtgDtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      bygears=vwGears, doLegend=vwLegend, showProj=vwProj,xLim=xLim,yLim=yLim ))
  }

  if ( perfPlotType == "vwFmort" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minF,maxF )
    .plotF( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( ( perfPlotType == "vwHCR" ) && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minSSB,maxSSB )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minF,maxF )
      
    .plotPerfHCR( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, xLim=xLim, yLim=yLim, colorZones=TRUE ) )
  }
  else if ( (perfPlotType == "vwHCR") && (animate==TRUE) )
  {
    .doAnimate( blob, perfPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRperfGui")
    .plotPerfHCR( blob, iSim, iRep, vwAnnotate, vwSet, vwSetYaxis )
    animate <- FALSE
  }
  
  if ( perfPlotType == "vwItg" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minIdx,maxIdx )
    .plotItg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwNumbers" )
  {
    xLim <- NULL
    yLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    if ( vwSetYaxis )
      yLim <- c( minNum,maxNum )
    .plotNumbers( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                  doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwObsSurvey" )
  {
    plotObsSurvey( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
  }

  if ( perfPlotType == "vwRecruits" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minYear,maxYear )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minRec,maxRec )
    .plotRecruits( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                   doLegend=vwProj, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType =="vwRecSpawn" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( minSSB,maxSSB )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( minRec,maxRec )
    .plotRecSpawners( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                      doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  if ( perfPlotType == "vwRefPoints" )
  {
    # This function must be located in mseRopMod.r.
    plotRefPts( blob$pars )
  }

  # Add a legend for the simulation for specified plots.
  if ( vwAnnotate && !animate )
  {
#    if ( (viewPlotType=="vwBioCatF") || (viewPlotType=="vwBioCatR") )
#      simLegend( iSim, iRep, nReps,
#                 simID=guiInfo$viewHeader$simID[iSim],
#                 scenario=guiInfo$viewHeader$scenario[iSim],
#                 procedure=guiInfo$viewHeader$procedure[iSim],
#                 side=3, line=0 )
#    else if ( viewPlotType=="vwRefPoints" )
#      simLegend( iSim, iRep, nReps,
#                 simID=guiInfo$viewHeader$simID[iSim],
#                 scenario=guiInfo$viewHeader$scenario[iSim],
#                 side=1, line=0 )
#    else
#      simLegend( iSim, iRep, nReps,
#                 simID=guiInfo$viewHeader$simID[iSim],
#                 scenario=guiInfo$viewHeader$scenario[iSim],
#                 procedure=guiInfo$viewHeader$procedure[iSim],
#                 side=1, line=2, col="gray50" )
  }
  
  # Save plot(s) as an enhanced Windows metafile.
  if ( vwSaveEMF )
    savePlot( filename=paste( simName,perfPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
  # Save plot(s) as a PDF file.
  if ( vwSavePDF )
    savePlot( filename=paste( simName,perfPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                

  # Restore graphics settings.
  #par( oldpar )

  return( invisible() )
}