#------------------------------------------------------------------------------#
# TO DO:                                                                       #
#                                                                              #
# 1. Translate "About" into a pop-up dialog box with a Close button.           #
# 2. Review globals, decide which should go into mseRglobals.r                 #
# 3. In .calcWa, where is bias correction?  Should c1, c2 be hard-wired?       #
# 4. Add annotate like capability to show gear types.                          #
# 5. Add check boxes to reference points to show on plots.                     #
# 6. Parameter description.                                                    #
#                                                                              #
# 7. The initial simulation loaded from the GUI - do we need "New".            #
# 8. Need to prevent deletion of simulations when there is only 1.             #
# 9. No need to update if at simulation 1 and you back up, or at simulation n  #
#    and you want to go forward.                                               #
#                                                                              #
#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 3.x                   #
#                                                                              #
#     Copyright 2008, 2009 by A.R. Kronlund, S.P. Cox                          #
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
#--------------------- mseRopMod.r: Operating Model Module ---------- ---------#
#--                                                                          --#
#-- mseRopMod.r: A mseR module for specifying the operating model.           --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 12-Nov-09 from mseRrefpt_funs.r and mseR 2.0       --#
#--                                                                          --#
#-- 12-Nov-09: mseRrefpt_funs developed by Sean Cox from mseR 2.0.           --#
#-- 12-Nov-09: mseRrefPoints.r implementation by A.R. Kronlund               --#
#-- 26-Nov-09: mseRopMod.r by combining mseRrefPoints.r and mseRsim.r        --#
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
# 6. The GUI control file must be named xxxWin.txt, where xxx is an arbitrary  #
#    prefix referenced specified in guiRefSetup.                               
#                                                                              #
# ADDING PLOTS:                                                                #
#                                                                              #
# Instructions on how to add a plot here.                                      #
#                                                                              #
# REQUIRES: PBSmodelling, RODBC, mseRtools.r                                   #
#                                                                              #
# References:                                                                  #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. 2009.              #
#   PBSmodelling 2.20: user’s guide revised from Canadian Technical Report of  #
#   Fisheries and Aquatic Science 2674: vi + 173 p. Last updated July 28, 2009 #
#                                                                              #
# GUI Functions (listed in order found in code):                               #
#                                                                              #
#   Flow: guiOpMod -> .guiOpModSetup -> .wkDirSetup ->                         #
#                     createWin -> .subGuiOpMod                                #
#                                                                              #
# guiOpMod           : Run the mseR Operating Model GUI.                       #
#                                                                              #
# .guiOpmodSetup   : Set up and run the GUI.                                   #
# .wkDirSetup      : Working directory set-up.                                 #
# .validGuiOpModPars : Check if the GUI parameters are valid.                  #
# .subGuiOpMod     : Processes guiOpMod submit actions, e.g. buttons, entries. #
#                                                                              #
# Helper Functions (Hidden, to show hidden function ls(all.names=TRUE)):       #
#                                                                              #
# Life History and Reference Point Calculations:                               #
#                                                                              #
# calcRefPoints    :                                                           #
#                                                                              #
# Reference Point Helper Functions (HIDDEN):                                   #
#                                                                              #
# .calcEquil       : Equilibrium yield calculations.                           #
# .calcPerRecruit  :                                                           #
# .calcRefCurves   :                                                           #
# .calcSchedules   :                                                           #
# .getF01          :                                                           #
# .getF40          :                                                           #
# .getFmax         :                                                           #
# .getFmsy         :                                                           #
# .getFcra         :                                                           #
#------------------------------------------------------------------------------#

source( "mseRglobals.r" )
source( "mseRrefPoints.r" )
source( "mseRtools.r" )

# General global variables (this will eventually be set to mseR).
.PACKAGE <- ""

# Following directory names usually prefixed by .PACKAGE so that, for example,
# the directory name for .FDOCS becomes "mseRdocs".
.FDOCS    <- "docs"              # Directory containing documents.
.FHELP    <- "help"              # Directory containing help files.
.FTEMP    <- "temp"              # Directory containing temporary files.

.FOPM     <- "mseROpMod.par"        # File containing operating model parameters.
.FSIM     <- "mseRsimulation.Rdata" # Simulation file - an R binary file.
.FXLS     <- "mseRsimulation.xls"   # Excel output file for simulations.

# General mseR globals.
.MAXF <- 4.0

#------------------------------------------------------------------------------#
#-- GUI Functions                                                            --#
#------------------------------------------------------------------------------#

# guiOpMod    (Run GUI)
# Purpose:    Run the GUI.
# Parameters: None
# Returns:    NULL (invisibly)
# Author:     A.R. Kronlund
guiOpMod <- function()
{
  return( .guiOpModSetup("mseRguiOpMod") )
}

# .guiOpModSetup (Setup for guiOpMod creation)
# Purpose:       Set up and run guiOpMod
# Parameters:    win is a character containing the name of the window to setup
# Returns:       NULL (invisibly)
# Source:        PBSref (modified), guiRef (December 2009).
# Author:        A.R. Kronlund
.guiOpModSetup <- function(win)
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  dir <- .wkDirSetup()
  cat( "\nWorking directory setup in ",dir,"\n" )
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  graphics.off()
  
  # Can a menu be created?
  goMenu <- FALSE
  
  # Does a default simulation Rdata file exist?
  simulationFileExists <- file.exists( .FSIM )
  if ( simulationFileExists )
  {
    # Load a default simulation list.
    load( .FSIM )
    cat( win," (.guiOpModSetup) Found reference point parameter file: ",.FSIM,"\n\n" )
    
    # Create objects required for PBSmodelling menus in the working directory.
    # Need to add checks to see if things exist...
    assign ( "opModPars", simulation$om$pars, pos=1 )
    goMenu <- TRUE
  }
  # There is no default simulation Rdata file, set up a null list.
  else
  {
    cat( "\nNeed to prompt for a parameter file for the operating model...\n" )

    # Read a parameter file and save copy to working directory for GUI to use.    
    tmp <- .readParFile( .FOPM )
    assign( "opModPars", tmp, pos=1 )

    # Create a new simluation list and save to the working directory.
    
    tmpSim <- list( fit=NULL, om=NULL, mp=NULL, perf=NULL, opt=NULL )
    simulation <- list()
    simulation[[1]] <- tmpSim
    names( simulation ) <- "Simulation1"
    assign( "simulation", simulation, pos=1 )
    
    goMenu <- TRUE
  }

  # Valid conditions exist for menu creation.
  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( paste( dir, "/", win, "Win.txt", sep="" ) )

    # Disable any widgets as required.
    setWidgetState( varname="opModPlotType", state="disabled", radiovalue="Fvsh", win )
    
    # Get the GUI parameters and make scope local to this function.
    guiInfo    <- getWinVal( winName=win )
    guiChanges <- list()
      
    # Initialize guiOpMod action.
    if ( win=="mseRguiOpMod" )
    {
      # Update the reference points from current GUI settings.
      opModParsList <- .convertToList( guiInfo$opModPars )
      tmp           <- calcRefPoints( opModParsList )
      
      # Make a list that will update the GUI.
      tmp$opModPars <- guiInfo$opModPars
      guiChanges    <- .updateGuiOpMod( tmp )

      # Save all the GUI parameters, including ref points to working directory.      
      assign( "guiOpModPars",tmp,pos=1 )
      cat( "Reference points initialized:\n" )
      
      # Update the simulation list.
      simulation[[1]]$om <- tmp
      assign( "simulation",simulation,pos=1 )
      
      # Update the GUI.
      guiChanges$simName <- names(simulation)[1]
      guiChanges$iSim    <- 1
      guiChanges$nSim    <- 1
      setWinVal( guiChanges, win )

      # Create initial plots.    
      .doGuiOpModPlots( simulation[[1]]$om )
    }
    
  }
  else
    cat( "\nERROR (.guiOpModSetup): GUI creation not possible.\n ")

  return( invisible() )
}     # .guiOpModSetup

# *** Update to selectFile.
# readOpModPars (reads guiOpMod GUI parameters from file written by saveOpModPars)
# Purpose:      Reads an ASCII file with reference points parameters.
# Parameters:   GUI parameters for guiRef. Output GUI parameter list.
# Returns:      NULL (invisibly)
# Side effects: Reads an ASCII control file written by saveRefPars and resets
#               the guiRef GUI parameters.
# Source:       A.R. Kronlund
readOpModPars <- function( parFile=.FOPM )
{
  tmpFile <- promptOpenFile( initialfile=parFile,
               filetype=list( c(".par","par files") ) )
  if ( tmpFile=="" )
    return( invisible() )      # No such file exists, bail.
  else
    parFile <- tmpFile
    
  # Read file, return a data frame with headers "parameter" and "value".
  val <- .readParFile( parFile )
  
  # Construct the list for the GUI.
  #guiList <- list()
  
  # Loop thru the rows, plucking out the parameter and value columns.
  # Find the name after the last "$" and assign it to guiList.
  #for ( i in 1:nrow(val) )
  #{
  #   parName <- val[ i,"parameter" ]
  #   guiVal  <- val[ i,"value" ]
     
  #   tokenPos <- max(which(strsplit(parName,'')[[1]]=='$')) + 1
     
  #   guiName <- substring( parName, tokenPos,nchar(parName) )
  #   listText <- paste( "guiList$",guiName,"=",guiVal,sep="" )
  #   eval( parse( text=listText ) )
  #}
 
  #browser()
  #setWinVal( guiList, winName="mseRguiRef" ) # GUI information local scope.
  return( val )
}

.updateGuiOpMod <- function( refPointObj, digits=4 )
{
  result <- list()
       
  # Round the reference points for display in the GUI.
  result$ssbFmsy   <- round( refPointObj$ssbFmsy,   digits=digits )
  result$yieldFmsy <- round( refPointObj$yieldFmsy, digits=digits )
  result$F0        <- round( refPointObj$F0,        digits=digits )
  result$F01       <- round( refPointObj$F01,       digits=digits )
  result$F40       <- round( refPointObj$F40,       digits=digits )
  result$Fmsy      <- round( refPointObj$Fmsy,      digits=digits )
  result$Fcra      <- round( refPointObj$Fcra,      digits=digits )
  result$Fmax      <- round( refPointObj$Fmax,      digits=digits )
  result
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
# Author:     A.R. Kronlund
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
	
	helpFiles <- c( "mseRabout.txt" )
	
	srcFiles <- paste( wkDir,   helpFiles, sep="/" )    # Source files.
	tarFiles <- paste( helpDir, helpFiles, sep="/" )    # Target files.
	file.copy( srcFiles, tarFiles, overwrite = TRUE )
	
	# Create .FTEMP directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .FTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  tempFiles <- c(
                   "mseRopMod.r",
                   "mseRguiOpModWin.txt",
                   "mseRabout.txt"
                )

  srcFiles <- paste( wkDir,   tempFiles, sep="/" )    # Source files.
  tarFiles <- paste( tempDir, tempFiles, sep="/" )    # Target files.
  file.copy( srcFiles, tarFiles, overwrite = TRUE)
  
  return(wkDir)
}

#------------------------------------------------------------------------------#
#-- GUI Error Checking                                                       --#
#------------------------------------------------------------------------------#

# .validGuiOpModPars (valid parameters for guiOpMod):
# Purpose:      Check whether the parameters supplied are valid.
#               If invalid, display an error message in the R console and
#               clear the invalid field.
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid.
# Parameters:   None
# GUI inputs:   Parameters taken directly from active guiRef. 
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields, corrects correctable fields (if any)
# Author:       A.R. Kronlund
.validGuiOpModPars <- function()
{
  # Get the GUI  values and make them local to this function.
  getWinVal( scope="L" )
                    
  isValid <- TRUE
  
  # This will be a list of the parameters that require changing.                                                     
  changes <- list()
  
  # The algorithm is to evalute the validity of each GUI parameter.
  # If a change is required then:
  # (i)   Make the change;
  # (ii)  Assign any Global parameters to the parent frame.
  # (iii) Set isValid to FALSE if a change cannot be made.

  # Example for GUI parameter "x".  
  # if ( is.na(x) || (x <= 1) || (x > 10) )
  # {
  #   cat( "Parameter x must be 1 <= x <= 10".\n" )
  #   changes$x <- NA
  #   isValid <- FALSE
  # }

  # If there are invalid parameters, bring the console to the top.  
  if ( !isValid )
    bringToTop( -1 )
  
  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( changes )
  return( isValid )
}

#-----------------------------------------------------------------------------##
#-- GUI Submit Functions                                                    --##
#-----------------------------------------------------------------------------##

# .subGuiOpMod  (Processes guiOpMod submit actions, e.g., buttons, checkboxes...)
# Purpose:    This is the function that directs the guiTemp program flow:
#             - Attempts to check validity of the GUI parameters;
#             - processes any button actions;
#             - processes any radio button actions;
#             - process any file open/save actions;
#             - processes any entry field actions.
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiOpMod <- function()
{
  win        <- .getWinName()                      # Get the current window name
  gvar       <- paste( ".", win, sep="" )          # Global variable name
  
  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiChanges <- list()                             # List for any changes.
  act        <- getWinAct()[1]                     # Get last menu window action

  valid <- .validGuiOpModPars()

  # *** Logic here is wrong.
  if ( valid  )
  {
    # No need to recalculate reference points unless pars was diddled.
    # But you do need to update pars and add to changes.
    # I don't know if this is needed...
    if ( act=="opModPars" )
    {
      print( guiInfo$pars )
      parsList   <- .convertToList( guiInfo$pars )
      tmp        <- calcRefPoints( parsList )
      tmp$pars   <- guiInfo$pars
      
      guiChanges <- .updateRefGUI( tmp )
      
      guiChanges$pars <- guiInfo$pars
      
      simulation[[iSim]] <- tmp
      assign( "simulation",simulation,pos=1 )
      
      setWinVal( guiChanges, winName=win )    
      assign( "refPoints",tmp,pos=1 )
    }
   
   .doGuiOpModPlots( simulation[[iSim]]$om )
   
  }     # if valid.
  
  if ( act=="chkFcra" | act=="chkF0" | act=="chkF01" | act=="chkF40" | act=="chkFmsy" |
       act=="chkFmax" )
    .doGuiOpModPlots( simulation[[iSim]]$om )
  
  if ( act=="plotOptions" )
    .doGuiOpModPlots( simulation[[iSim]]$om )
  
  # Rename a simulation
  if ( act=="simName" )
  {
    names( simulation )[guiInfo$iSim] <- guiInfo$simName
    assign( "simulation",simulation,pos=1 )
  }
  
  # Add a simulation to the list.
  if ( act=="addSim" )
  {
    n <- length( simulation )
    n <- n + 1
    simulation[[n]] <- refPoints
    names(simulation)[[n]] <- paste( "Simulation",n,sep="" )
    assign( "simulation",simulation,pos=1 )
    guiChanges <- list( simName=names(simulation)[n],
                    iSim=n, nSim=n, pars=guiInfo$pars )
                     
    setWinVal( guiChanges, winName=win )
    # Enable any widgets as required.
    if ( length(simulation) > 1 )
      setWidgetState( varname="opModPlotType", state="normal", radiovalue="Fvsh", win )
      
    # Update the plots.
    .doGuiOpModPlots( simulation[[iSim]]$om )       
  }
  
  # Next simulation (>).
  if ( act=="nextSim" )
  {
    # Do not permit iSim > nSim.
    guiInfo$iSim <- min( (iSim+1), nSim )
    
    # Update the reference points on the GUI from the next simulation.
    guiChanges <- .updateRefGUI( simulation[[guiInfo$iSim]] )   
    
    # Update the parameter object widget.
    guiChanges$pars <- simulation[[guiInfo$iSim]]$pars    
    
    # Update the simulation information.
    guiChanges$iSim <- guiInfo$iSimulation
    guiChanges$simName <- names(simulation[guiInfo$iSim] )
    
    # Update the GUI.
    setWinVal( guiChanges,winName=win )
    
    .doGuiOpModPlots( simulation[[guiInfo$iSim]]$om )
  }  
  
  # Previous simulation (<).
  if ( act=="prevSim" )
  {
    # Do not permit iSim < 1.
    guiInfo$iSim <- max( 1, (iSim-1) )
    
    # Update the reference points GUI from the previous simulation.
    guiChanges <- .updateRefGUI( simulation[[guiInfo$iSim]] )   

    # Update the parameter object widget.
    guiChanges$pars <- simulation[[guiInfo$iSim]]$pars
    
    # Update the simulatino information.
    guiChanges$iSim <- guiInfo$iSim
    guiChanges$simName <- names(simulation[guiInfo$iSim] )
    
    # Update the GUI.
    setWinVal( guiChanges, winName=win )
    
    .doGuiOpModPlots( simulation[[guiInfo$iSim]]$om )    
  }
     
  # REMOVE a simulation.
  if ( act=="remSim" )
  {
    # Rules:
    # (a) If there is only 1 simulation, nothing gets done.
    # (b) If simulation (1,...,n-1) is removed, show the next simulation.
    # (c) If simulation n is removed, show simulation n-1.
    
    if ( length(simulation)> 1 )
    { 
      # Find the index of the simulation to be removed.
      id <- c(1:length(simulation))[ guiInfo$simName==names(simulation) ]

      # Rules b,c: Adjust the current simulation to account for the removal.
      if ( id==length(simulation) )
        iSim <- length(simulation) - 1
      else
        iSim <- id
      
      # Remove the simulation and re-assign simulation to global.
      simulation[[guiInfo$simName]] <- NULL
      assign( "simulation",simulation,pos=1 )
      
      # Update the reference points from the previous simulation.
      guiChanges <- .updateRefGUI( simulation[[iSimulation]] )   
      
      # Update the simulation information.
      guiChanges$iSim    <- iSim
      guiChanges$nSim    <- length(simulation)
      guiChanges$simName <- names(simulation)[iSim]
      
      # Update the GUI.
      setWinVal( guiChanges,winName=win )
      if ( length(simulation)==1 )
        setWidgetState( varname="opModPlotType", state="disabled", radiovalue="Fvsh", win )
      
      # Update the plots.
      .doGuiOpModPlots( simulation[[iSim]]$om )
    }
    else
      cat( "(.subGuiOpMod) Cannot remove last simulation.\n" )

    # Update the GUI.    
    setWinVal( guiChanges,winName=win )
  }
 
  # EXIT the GUI (leave graphics on).
  if ( act=="exit" )
  {
    closeWin()
  }

  # LOAD the GUI parameters, force an update, and refresh plots.
  if ( act=="load" )
  {
    # Read the reference point parameter file and set GUI parameters.
    refPointPars <- readRefPointPars()
      
    parsList   <- .convertToList( refPointPars )
    tmp        <- calcRefPoints( parsList )
     
    guiChanges <- .updateRefGUI( tmp )
    
    guiChanges$pars <- refPointPars
    
    simulation <- list()  
    simulation[[1]] <- tmp
    guiChanges$iSim <- 1
    guiChanges$nSim <- 1
    guiChanges$simName <- "Simulation1"
    
    names( simulation ) <- "Simulation1"
    
    assign( "simulation",simulation,pos=1 )
    
    setWinVal( guiChanges, winName=win )    

    # HACK: Need to check validation before updating GUI.
    assign( "refPoints",tmp,pos=1 )      
          
    # Update the plots.
    .doGuiOpModPlots( simulation[[iSim]]$om )    
    
    valid <- .validGuiOpModPars()
      
    if ( valid )
    {
      #guiInfo <- .updateStats( guiInfo, win )
      assign( "guiOpModPars",guiInfo,pos=1 )
    }
  }
    
  # SAVE the GUI parameters, force an update, and refresh plots.
  if ( act=="save" )
  {
    # Don't save invalid parameters.
    valid <- .validGuiOpModPars()
    if ( valid )
    {
      #guiInfo <- .updateStats( guiInfo, win )
      assign( "guiOpModPars",guiInfo$pars,pos=1 )
      .saveOpModPars()
    }
  }
  
  # EXPORT the simulation list to an R directory file.
  if ( act=="export" )
  {
    .saveSimulationToRdata()
  }
  
  # IMPORT the simulation list to the working directory.
  if ( act=="import" )
  {
    .loadSimulationFromRdata()
    
    # Update the reference points from first scenario.
    guiChanges <- .updateRefGUI( simulation[[1]] )
    
    # Update the pars object.
    guiChanges$pars <- simulation[[1]]$pars   
      
    # Update the simulation information.
    guiChanges$iSim    <- 1
    guiChanges$nSim    <- length(simulation)
    guiChanges$simName <- names(simulation)[1]
      
    # Update the GUI.
    setWinVal( guiChanges,winName=win )
    if ( length(simulation)==1 )
      setWidgetState( varname="opModPlotType",state="disabled",radiovalue="Fvsh",win )
    else
      setWidgetState( varname="opModPlotType",state="normal",radiovalue="Fvsh",win )    
    
    # Update the plots.
    .doGuiOpModPlots( simulation[[iSim]]$om )   
  }
  
  if ( act=="tables" )
  {
    .saveSimulationToExcel( .FXLS )
    openFile( .FXLS )
  }
    
  return( invisible() )  
}

.loadSimulationFromRdata <- function( simFile=.FSIM )
{
  tmpFile <- selectFile( initialfile=simFile,
                filetype=list( c(".Rdata","Rdata files") ), mode="open" )
  if ( tmpFile=="" )
    return( invisible() )      # No such file exists, bale.
  else
    simFile <- tmpFile 
    
  load( simFile, .GlobalEnv )
  
  return( invisible() ) 
}

.saveSimulationToRdata <- function( simFile=.FSIM, overWrite=FALSE )
{
  # Need to check if there are simulations to save before calling this...
  if ( !overWrite )
  {
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    tmpFile <- selectFile( initialfile=simFile,
                 filetype=list( c(".Rdata","Rdata files") ), mode="save" )                 
    if ( tmpFile=="" )
      return( invisible() )     # User did not select a file, bale.
    else
      simFile <- tmpFile
  }
  else
  {
    # Force an overwrite of .FTRLS.
    cat( "\n(.saveSimulationToRdata) Overwriting :",simFile,"\n" )
  }

  save( simulation, file=simFile )
 
  return( invisible() )  
}
  
#-----------------------------------------------------------------------------##
#-- Helper Functions (some HIDDEN, e.g., .foo)                              --##
#-----------------------------------------------------------------------------##

.convertToList <- function( obj )
{
  # Construct the list for the GUI.
  result <- list()
  
  # Loop thru the rows, plucking out the parameter and value columns.
  # Find the name after the last "$" and assign it to guiList.
  for ( i in 1:nrow(obj) )
  {
     parName <- obj[ i,"parameter" ]
     parVal  <- obj[ i,"value" ]
     
     tokenPos <- max(which(strsplit(parName,'')[[1]]=='$')) + 1
     
     listName <- substring( parName, tokenPos,nchar(parName) )
     listText <- paste( "result$",listName,"=",parVal,sep="" )
     eval( parse( text=listText ) )
  }
  result
}

# .readParFile  (reads an ASCII file with header as a data frame)
# Purpose:      Reads an ASCII file: 1 header row, space-delimited
#               data frame usually containing columns "parameter" and "value".
#               Note that lines beginning with a pound (#) character are
#               treated as comments.
# Parameters:   parFile is a character string indicating the input file.
# Returns:      result, a data frame.
# Source:       A.R. Kronlund
.readParFile <- function( parFile="inputFile.par" )
{
  # Read the file and store as a dataframe.
  result <- read.table( file=parFile, as.is=TRUE, header=TRUE, comment.char="#",
                        quote="",sep=" " )
  result
}

# *** Update to selectFile.
# .saveRefPars  (saves the ref GUI parameters to a file).
# Purpose:      Saves an ASCII file with reference point parameters.
# Parameters:   Output file name for ASCII file containing ref GUI parameters.
# Returns:      NULL
# Side effects: Writes an ASCII control file configured for read usng .readParFile.
# Source:       A.R. Kronlund
.saveRefPars <- function( parFile=.FPARS, overWrite=FALSE )
{
  if ( !overWrite )
  {
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    tmpFile <- promptSaveFile( initialfile=parFile,
                 filetype=list( c(".par","par files") ) )
    if ( tmpFile=="" )
      return( invisible() )
    else
      parFile <- tmpFile
  }
  else
  {
    # Force an overwrite of .FREFS.
    cat( "\n(.saveRefPars) Overwriting :",parFile,"\n" )
  }

  win <- .getWinName()                           # Get the current window name
  guiInfo <- getWinVal( scope="L", winName=win ) # GUI information local scope

  cat( file=parFile, paste( "# mseRrefPoints GUI parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )

  for ( i in 1:nrow(guiInfo$pars) )
  {
    cat( file=parFile, guiInfo$pars[i,1]," ",guiInfo$pars[i,2],"\n", sep="",
         append=TRUE )
  }
  return( invisible() )  
}


# .saveSimulationToExcel (Save data in Excel format)
# Purpose:    Saves data in obj to a Microsoft Excel .xls file.
# Parameters: fname - a character string file name.
#             statObj - the result of a call to .calcPerfStats.
# Returns:    NULL
# Source:     Modified from saveExcel by T.K. Deering (PopSim.r)
.saveSimulationToExcel <- function( fname=.FXLS )
{
	# No name provided, or "Cancel" selected.
	if ( fname == "" )
    return(invisible())
    
	# If fname already exists, then remove it.
	fileGone <- TRUE
	if ( file.exists(fname) )
    fileGone <- file.remove( fname )
    
  if ( fileGone )
  {
  	conn <- RODBC::odbcConnectExcel( fname, readOnly=FALSE )

    # Write the parameters by simulation.
    parNames <- c( "B0","rSteepness","gammaR","sigmaR","A50","A95","M","c1",
                   "c2","Linf","L1","vonK","sigmaLinf","qg","powerg","nAges",
                   "nGrps","nGear","sizeLim",
                   "L50Cg1","L50Cg2","L95Cg1","L95Cg2","L50Dg","L95Dg",
                   "dg","fg","piOne","R0","rec.a","rec.b","B20","R20" )
    
    tmp <- unlist( simulation[[1]][parNames] )
    tmpNames <- names(tmp)
        
    result <- matrix( NA, nrow=length(tmp),ncol=length(simulation) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simulation[[i]][parNames] )
      result[,i] <- tmp
    }
    
	  # Save the simulation conditions.
	  .excelTable( conn, result, "Parameters", names(simulation), tmpNames )
  
    # Write the reference points by simulation.
    refNames <- c( "F0","yprF0","ssbprF0","landedF0","yieldF0",
                   "discardedF0","ssbF0","recruitsF0",
                   "F01","yprF01","ssbprF01","landedF01","yieldF01",
                   "discardedF01","ssbF01","recruitsF01",
                   "Fmsy","yprFmsy","ssbprFmsy","landedFmsy","yieldFmsy",
                   "discardedFmsy","ssbFmsy","recruitsFmsy",
                   "F40","yprF40","ssbprF40","landedF40","yieldF40",
                   "discardedF40","ssbF40","recruitsF40",
                   "Fmax","yprFmax","ssbprFmax","landedFmax","yieldFmax",
                   "discardedFmax","ssbFmax","recruitsFmax",
                   "Fcra","yprFcra","ssbprFcra","landedFcra","yieldFcra",
                   "discardedFcra","ssbFcra","recruitsFcra" )
                   
    result <- matrix( NA, nrow=length(refNames),ncol=length(simulation) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simulation[[i]][refNames] )
      result[,i] <- tmp
    }
    
    # Save the reference points.
    .excelTable( conn, result, "Reference_Points", names(simulation), refNames )
    
    # Write the reference points by gear.
    refGearNames <- c( "yprLF0","yprDF0","yprLF01","yprDF01",
                       "yprLFmsy","yprDFmsy","yprLF40","yprDF40",
                       "yprLFmax","yprDFmax","yprLFcra","yprDFcra" )
    
    tmp <- unlist( simulation[[1]][refGearNames] )
    tmpNames <- names(tmp)
        
    result <- matrix( NA, nrow=length(tmp),ncol=length(simulation) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simulation[[i]][refGearNames] )
      result[,i] <- tmp
    }
    
	  # Save the simulation conditions.
	  .excelTable( conn, result, "Ref_Points_Gear", names(simulation), tmpNames )                       
    
    
    # Write the theoretical curves.
    curveNames <- c( "ssbpr","yprL","yprD","ypr","F","ssb","recruits","yield",
                     "landed","discarded" )
                     
    nPoints <- length( simulation[[1]]$ssbpr )
                        
    for ( i in 1:length(simulation) )
    {
      result <- matrix( NA, nrow=nPoints,ncol=length(curveNames) )
      for ( j in 1:length(curveNames) )
        result[,j] <- simulation[[i]][[ curveNames[j] ]]
        
      .excelTable( conn, result, names(simulation)[i], curveNames, as.character(1:nPoints) )
    }
  
    odbcClose(conn)
  }
  else
  {
    cat( "mseRguiRef (.saveToExcel): Simulations results NOT saved to Excel, permission denied\n" )
    cat( "mseRguiRef (.saveToExcel): Close or rename file ",fname,"\n " )
  }
	return( fileGone )
}

.updateGUI <- function()
{
   parentList <- ls( name=parent.frame(n=1) )
   
   win     <- .getWinName()                       # Get the current window name
   guiList <- getWinVal( scope="L", winName=win ) # GUI information local scope
  
   # Check for parent environment variables that match the GUI list.
   isMatch <- is.element( parentList,names(guiList) )
   parentList <- parentList[isMatch]
  
   # Now evaluate the variables into a list.
   nVals <- length( parentList )
   vals  <- as.list( 1:nVals )
   names( vals ) <- parentList
   
   for ( i in 1:length(vals) )
     vals[[i]] <- get( parentList[i], parent.frame(n=1) )
   
   setWinVal( vals )  
}

#------------------------------------------------------------------------------#
#-- Plotting Functions                                                       --#
#------------------------------------------------------------------------------#

.doGuiOpModPlots <- function( obj )
{
  win     <- .getWinName()
  guiInfo <- getWinVal(scope="L", winName=win ) # GUI information local scope

  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )
  
  #----------------------------------------------------------------------------#
  # Life history plots.                                                        #
  #----------------------------------------------------------------------------#
  
  if ( opModPlotType=="lenAtAge" )
  {
    .plotLenAtAge( obj, gfx=list( annotate=annotate,
                   setXaxis=setXaxis, setYaxis=setYaxis,
                   xLim=c(minAge,maxAge),yLim=c(minLen,maxLen) ) )
  }
  
  if ( opModPlotType=="matAtAge" )
  {
    .plotMatAtAge( obj, gfx=list( annotate=annotate,
                   setXaxis=setXaxis, setYaxis=setYaxis,
                   xLim=c(minAge,maxAge),yLim=c(0.0,1.0) ) )
  }
  
  if ( opModPlotType=="numAtAge" )
  {
    .plotNumAtAge( obj, gfx=list( annotate=annotate ) )
  }
  
  if ( opModPlotType=="wgtAtAge" )
  {
    .plotWgtAtAge( obj, gfx=list( setXaxis=setXaxis, setYaxis=setYaxis,
                   xLim=c(minAge,maxAge),yLim=c(minWgt,maxWgt) ) )
  }
  
  if ( opModPlotType=="wgtLen" )
  {
     .plotWgtLen( obj, gfx=list( setXaxis=setXaxis, setYaxis=setYaxis,
                   xLim=c(minLen,maxLen),yLim=c(minWgt,maxWgt) ) )
  }
  
  # Combined life history plot.
  if ( opModPlotType=="lifeHist" )
  {
    .plotLifeHist( obj )
  }
  
  #----------------------------------------------------------------------------#
  # Equilibrium reference point plots.                                         #
  #----------------------------------------------------------------------------#
  
  # Arguments are passed instead of extracting from the GUI within the plot
  # function so that functions can be used without the GUI, or altered at the
  # time of the function call.
  
  checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
  names( checked ) <- c( "Fmsy",  "F0",  "F01", "F40",   "Fmax",  "Fcra"  )
                  
  par( oma=c(2,1.5,2,1), mar=c(3,4,1,1), mfrow=c(1,1) )
  
  if ( opModPlotType=="recSSB" )
  {
    .plotRecSSB( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minRec,maxRec) ) )
  }
  
  if ( opModPlotType=="ssbF" )
  {
    .plotSsbF( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )
  }
  
  if ( opModPlotType=="ssbRF" )
  {
    .plotSsbPerRecF( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  }
  
  if ( opModPlotType=="yieldF" )
  {
    .plotYieldF( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  }
  
  if ( opModPlotType=="yieldSSB" )
  {
    .plotYieldSSB( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minYield,maxYield) ) )
  }
  
  if ( opModPlotType=="yprF" )
  {
    .plotYprF( obj,gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYPR,maxYPR) ) )
  }
  
  # Combined reference point plot.
  if ( opModPlotType=="refPoints" )
  {
    .plotRefPoints( obj, checked )
  }
  
  #----------------------------------------------------------------------------#
  # Selectivity and discard proportions by gear.                               #
  #----------------------------------------------------------------------------#
  
  # Selectivity by length and gear.
  if ( opModPlotType=="Slg" )
  {
    .plotSlg( obj, gfx=list(autolayout=TRUE, annnotate=TRUE, bygear=TRUE,
                       doLegend=TRUE, xLim=NULL, yLim=NULL ) )
  }
                           
  # Selectivity by age, length and gear.
  if ( opModPlotType=="Salg" )
  {
    .plotSalg( obj, gfx=list(autolayout=TRUE, annnotate=TRUE, bygear=TRUE,
                       doLegend=TRUE, xLim=NULL, yLim=NULL ) )
  }

  # Proportion discarded at length and gear.
  if ( opModPlotType=="Plg" )
  {
    if ( setXaxis )
      xLim <- c(minLen,maxLen)
    else
      xLim <- NULL
  
    .plotPlg( obj, gfx=list(autolayout=autolayout, annnotate=annotate, bygear=bygear,
                       doLegend=doLegend, xLim=xLim, yLim=NULL ) )
  }

  # Proportion discarded at age, length and gear.
  if ( opModPlotType=="Palg" )
  {
    .plotPalg( obj, gfx=list(autolayout=TRUE, annnotate=TRUE, bygear=TRUE,
                       doLegend=TRUE, xLim=NULL, yLim=NULL ) )
  }
  
  # Mortality rates of discards by gear.
  if ( opModPlotType=="dg" )
  {
    .plotDisMortRate( obj, gfx=list(autolayout=TRUE, annnotate=TRUE, bygear=TRUE,
                       doLegend=TRUE, xLim=NULL, yLim=NULL ) )
  }

  # Gear impacts.
  if ( opModPlotType=="yieldGear" )
  {
    .plotYieldByGear( obj, gfx=list( annotate=annotate, checked=checked ) )
  }

  # Compare scenarios.
  if ( opModPlotType=="Fvsh" )
  {
    .plotFvsh( simulation )
  }
  
  if ( opModPlotType=="yieldVsF" )
  {
    .plotYieldVsF( simulation, refGear=3, gfx=list( annotate=annotate, checked=checked) )
  }

  # Save plot(s) as an enhanced Windows metafile.
  if ( saveemf )
    savePlot( filename=paste( simName,opModPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
 # Save plot(s) as a PDF file.
  if ( savepdf )
    savePlot( filename=paste( simName,opModPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                
    
  # Restore the graphics settings.
  par( oldpar )  
  return( invisible() )
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Life History                                         --#
#------------------------------------------------------------------------------#

.plotLenAtAge <- function( obj, gfx )
{
  annotate <- gfx$annotate
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Length at age.
  xRange <- c( 0,max(obj$ages) )
  if ( setXaxis )
    xRange <- xLim
  yRange <- c( 0,max(obj$Lal) )
  if ( setYaxis )
    yRange <- yLim
  
  plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  for( l in 1:ncol(obj$Lal) )  
    lines( obj$ages, obj$Lal[,l], lty=1 )
  
  if ( annotate )
    abline( h=obj$sizeLim,lty=2, lwd=2 )
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Length-at-age" )
}

.plotMatAtAge <- function( obj, gfx )
{
  annotate <- gfx$annotate
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( 0,max(obj$ages) )
  if ( setXaxis )
    xRange <- xLim
  yRange <- c( 0,max(obj$Ma) )
  if ( setYaxis )
    yRange <- yLim
    
  # Maturity at age.
  plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$Ma, lty=1 )

  if ( annotate )
  {  
    A50 <- max( obj$ages[ obj$Ma <= 0.5001 ] )
    A95 <- max( obj$ages[ obj$Ma <= 0.9501 ] )
    segments( A50, 0.0,  A50, 0.5,  lty=2 )
    segments( 0.0, 0.5,  A50, 0.5,  lty=2 )
    segments( A95, 0.0,  A95, 0.95, lty=2 )
    segments( 0.0, 0.95, A95, 0.95, lty=2 )
  }
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Maturity-at-age" )
}

.plotNumAtAge <- function( obj, gfx )
{
  annotate <- gfx$annotate
 
  yRange <- c( 0,max(obj$numAgeYr1 ) )
  yRange[2] <- round( yRange[2],digits=1 )
  
  # Numbers at age in year 1.
  barplot(obj$numAgeYr1,names.arg=obj$ages, axes=FALSE, xlab="", ylab="", ylim=yRange )
  axis (side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()

  if ( annotate )
  {    
    legend( "topright",legend=c(paste("Max:",round(max(obj$numAgeYr1),digits=4)),
            paste("Min:", round(min(obj$numAgeYr1), digits=4))), bty="n")
  }
            
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Number at Age-1" )
}

.plotWgtAtAge <- function( obj, gfx )
{
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( 0,max(obj$ages) )
  if ( setXaxis )
    xRange <- xLim
  yRange <- c( 0,max(obj$Wal) )
  if ( setYaxis )
    yRange <- yLim  
  
  # Weight at age.
  plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  for( l in 1:ncol(obj$Wal) )  
    lines( obj$ages, obj$Wal[,l], lty=1 )
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Weight-at-age" )
}

.plotWgtLen <- function( obj, gfx )
{
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( min(rowMeans(obj$Lal)),max(rowMeans(obj$Lal)) )
  if ( setXaxis )
    xRange <- xLim
  yRange <- c( min(rowMeans(obj$Wal)),max(rowMeans(obj$Wal)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Weight against length.
  plot( rowMeans(obj$Lal), rowMeans(obj$Wal), type="n", axes=FALSE,
        xlab="",xlim=xRange,ylab="",ylim=yRange )
  lines( rowMeans(obj$Lal), rowMeans(obj$Wal), lty=1 )
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Length" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Weight" )    
}

# .plotLifeHist (Plot the operating model life history parameters)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund  
.plotLifeHist <- function( obj )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
    
  par( oma=c(2,1,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )
    
  .plotNumAtAge( obj, gfx=list( annotate=annotate ) )
  .plotMatAtAge( obj, gfx=list( annotate=annotate,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minAge,maxAge),yLim=c(0.0,1.0) ) )
  .plotLenAtAge( obj, gfx=list( annotate=annotate,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minAge,maxAge),yLim=c(minLen,maxLen) ) )
  .plotWgtAtAge( obj, gfx=list( setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minAge,maxAge),yLim=c(minWgt,maxWgt) ) )
  .plotWgtLen( obj, gfx=list( setXaxis=setXaxis, setYaxis=setYaxis,
               xLim=c(minLen,maxLen),yLim=c(minWgt,maxWgt) ) )

  mtext( side=3, line=0, cex=.REFCEX, outer=TRUE, "Life History" )
  return( invisible() )  
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Equilibrium Reference Points                         --#
#------------------------------------------------------------------------------#

.addRefPointsLegend <- function( x=0.5, y=0.5, checked )
{
  # Is there anything in the legend, or is nothing checked?
  if ( sum(checked)==0 )
  {
    cat( "\nmseRrefPoints (.addRefPointsLegend): Legend vector of length 0.\n" )
    return()
  }
  
  cax <- .REFCAX
  pex <- .REFPEX
  
  labels <- c( "F0","F0.1","Fmsy","Fspr40","Fmax","Fcrash" )
  names(labels) <- c("F0","F01","Fmsy","F40","Fmax","Fcra" )
  
  pchVec <- rep( 21,length(labels) )
  names(pchVec) <- names(labels)
  
  ptBg   <- c(.REFCOLF0,.REFCOLF01,.REFCOLFMSY,.REFCOLF40,.REFCOLFMAX,.REFCOLFCRA)
  names(ptBg) <- names(labels)

  # Now show only those reference points that are checked in the GUI.
  # This is tricky, we want the reference points where checked=TRUE, but
  # we have to get the names(checked) where checked=TRUE in case the order
  # of the reference points in the vectors differ.
  
  labels <- labels[ names(checked)[checked] ]
  pchVec <- pchVec[ names(checked)[checked] ]
  ptBg   <- ptBg[ names(checked)[checked] ]  
      
  # Add a legend.
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=pex, cex=cax )
}

.plotRecSSB <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX

  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim

  xRange <- range( c(0,max(obj$ssb)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$recruits)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot recruits against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$recruits), lwd=2 )
    
  # Adding steepness lines at B20=0.2*B0, R20, 
  # and steepness R20/B0.
  lines( c(obj$B20,obj$B20), c(0,      obj$R20), lty=2 )
  lines( c(0,      obj$B20), c(obj$R20,obj$R20), lty=2 )
  
  lines( c(obj$B0,obj$B0), c(0,     obj$R0), lty=2 )
  lines( c(0,     obj$B0), c(obj$R0,obj$R0), lty=2 )
  
  # Adding steepness label
  h <- round( obj$R20/obj$R0, digits=2 )
  xPos <- obj$B20
  yPos <- obj$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",obj$rSteepness,sep="") )
  
  if ( checked["F0"] )  
    points( obj$ssbF0,   obj$recruitsF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$ssbF01,  obj$recruitsF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$ssbFcra, obj$recruitsFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$ssbFmax, obj$recruitsFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$ssbFmsy, obj$recruitsFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$ssbF40,  obj$recruitsF40,  cex=pex, bg=.REFCOLF40,  pch=21 )      
    
  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "SSB" )
  mtext( side=2, line=3.5, cex=.REFCEX, "Recruits" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.7, y=0.4, checked=checked )
}

.plotSsbF <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$F)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ssb)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot spawning stock biomass against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$F), .posVal(obj$ssb), lwd=2 )
  
  if ( checked["F0"] )  
    points( obj$F0,   obj$ssbF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$ssbF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$ssbFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$ssbFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$ssbFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$ssbF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
    
  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "F" )
  mtext( side=2, line=3.5, cex=.REFCEX, "SSB" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotSsbPerRecF <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$F)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ssbpr)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot spawning stock biomass per recruit against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$F), .posVal(obj$ssbpr), lwd=2 )

  if ( checked["F0"] )  
    points( obj$F0,   obj$ssbprF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$ssbprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$ssbprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$ssbprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$ssbprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$ssbprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )       
    
  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "F" )
  mtext( side=2, line=3.5, cex=.REFCEX, "SSB per Recruit" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotYieldF <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX

  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$Fcra)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$yield)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$F), .posVal(obj$yield), lwd=2 )

  if ( checked["F0"] )  
    points( obj$F0,   obj$yieldF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )    

  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "F" )
  mtext( side=2, line=3.5, cex=.REFCEX, "Yield" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}  

.plotYieldSSB <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$ssb)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$yield)) )
  if ( setYaxis )
    yRange <- yLim
    
  # Plot yield against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$yield), lwd=2 )
  
  if ( checked["F0"] )  
    points( obj$ssbF0,   obj$yieldF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$ssbF01,  obj$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$ssbFcra, obj$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$ssbFmax, obj$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$ssbFmsy, obj$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$ssbF40,  obj$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )
   
  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "SSB" )
  mtext( side=2, line=3.5, cex=.REFCEX, "Yield" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.5, y=0.4, checked )
}  

.plotYprF <- function( obj, gfx )
{
  cax <- .REFCAX
  pex <- .REFPEX

  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim

  xRange <- range( c(0,max(obj$F)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ypr)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot yield per recruit against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$F), .posVal(obj$ypr), lwd=2 )

  if ( checked["F0"] )  
    points( obj$F0,   obj$yprF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$yprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$yprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$yprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$yprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$yprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )    
    
  axis( side=1, cex.axis=cax )
  axis( side=2, cex.axis=cax, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=.REFCEX, "F" )
  mtext( side=2, line=3.5, cex=.REFCEX, "Yield per Recruit" )
  box()
  
  if ( annotate )
    .addRefPointsLegend( x=0.7, y=0.4, checked )
}

# .plotRefPts    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPoints <- function( obj, checked )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  par( oma=c(2,1.5,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )    
   
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYPR,maxYPR) ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minRec,maxRec) ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minYield,maxYield) ) )

  mtext( side=3, line=0, cex=.REFCEX, outer=TRUE, "Reference Points" )
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Selectivity                                          --#
#------------------------------------------------------------------------------#

.plotSlg <- function( obj, gfx=list( autolayout=TRUE, annnotate=TRUE ) )
{
  xRange <- c( obj$L1,trunc(max(obj$Lal)) )
  yRange <- c(0,1)
  
  nGear  <- obj$nGear
  L50Cg1 <- obj$L50Cg1
  L50Cg2 <- obj$L50Cg2
  L95Cg1 <- obj$L95Cg1
  L95Cg2 <- obj$L95Cg2
  
  len <- seq( xRange[1],xRange[2],0.25 )

  Slg <- matrix( NA, nrow=length(len), ncol=nGear )
  for( g in 1:nGear )
  {
    tmp1 <- exp( (-1.)*log(19)*(len-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
    tmp2 <- exp( (-1.)*log(19)*(len-L50Cg2[g])/(L95Cg2[g] - L50Cg2[g]) )
    tmpS <- (1./(1.+tmp1))*(1./(1.+tmp2))
    Slg[,g] <- tmpS/max( tmpS )
  }

  # HACK: ARK 25-Nov-09
  if ( nGear==2 )
    mfRow <- c(2,1)
  else if ( nGear==3 )
    mfRow <- c(3,1)
  else
    mfRow <- c(2,2)

  if ( gfx$autolayout )  
    par( oma=c(3,3,2,1), mar=c(2,2,1,1), mfrow=mfRow )
  
  for ( g in 1:nGear  )
  {
    plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
    lines( len,Slg[,g], lty=1 )
    panLab( 0.05,0.95, adj=0, obj$gNames[g] )  
    axis( side=1, cex.axis=.REFCAX )
    axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
    box()
  }
  
  mtext( side=1, line=1, cex=.REFCEX, outer=TRUE, "Length" )
  mtext( side=2, line=1, cex=.REFCEX, outer=TRUE, "Selectivity" )
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Gear Proportion Discarded and Mortality Rates.       --#
#------------------------------------------------------------------------------#

.plotPlg <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE, bygear=TRUE,
                           doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # obj: input as a list of parameters.
  
  xLim <- gfx$xLim
  if ( is.null(gfx$xLim) )
    xLim <- c( obj$L1,trunc(max(obj$Lal)) )

  yLim <- c(0,1)
  
  dg      <- obj$dg
  nGear   <- obj$nGear
  L50Dg   <- obj$L50Dg
  L95Dg   <- obj$L95Dg
  sizeLim <- obj$sizeLim
  
  # Vector for plotting range of lengths.  
  len <- seq( xLim[1],xLim[2],0.25 )

  Plg <- matrix( NA, nrow=length(len), ncol=nGear )
  for( g in 1:nGear )
  {
    tmp <- exp( (-1.)*log(19)*(len-L50Dg[g])/(L95Dg[g] - L50Dg[g]) )
    tmpP <- (1./(1.+ tmp))
    tmpP[ len < sizeLim ] <- 1.0
    Plg[,g] <- tmpP
  }
 
  # Separate plot for each gear type.
  if ( gfx$bygear )
  {
    # HACK: ARK 25-Nov-09
    if ( nGear==2 )
      mfRow <- c(2,1)
    else if ( nGear==3 )
      mfRow <- c(3,1)
    else
      mfRow <- c(2,2)
  
    par( oma=c(4,3,2,1), mar=c(2,2,1,1), mfrow=mfRow )
    
    for ( g in 1:nGear  )
    {
      plot( xLim,yLim,type="n",axes=F,xlab="",ylab="" )
      lines( len,Plg[,g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )

      browser()
      if ( gfx$annotate )
        abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=LWDSIZE )
    
      panLab( 0.8,0.90, adj=0, cex=1.2,
              paste( obj$gNames[g],"(dg =",round( obj$dg[g],digits=3 ),")"  ) )
      axis( side=1, cex.axis=.REFCAX )
      axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
      box()
    }
  }
  # Single plot with gear overlay.
  else
  {
    par( oma=c(4,3,2,1), mar=c(2,2,1,1), mfrow=c(1,1) )
    
    plot( xLim, yLim, type="n",axes=F,xlab="",ylab="" )
    for ( g in 1:nGear  )
      lines( len,Plg[,g], col=.COLPlg[g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )

    if ( gfx$annotate )
      abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=LWDSIZE )
    
    axis( side=1, cex.axis=.REFCAX )
    axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
    box()
      
    if ( gfx$doLegend )
    {
      tmp <- paste( obj$gNames, "(dg =",round( obj$dg, digits=3 ),")" )
      panLegend( 0.6,0.95, legTxt=tmp, bg="white",
                 col=.COLPlg, lty=.LTYPlg, lwd=.LWDPlg, pt.cex=1.2 )    
    }
  }

  mtext( side=1, line=1, cex=.REFCEX, outer=TRUE, "Length" )
  mtext( side=2, line=1, cex=.REFCEX, outer=TRUE, "Proportion Discarded at Length" )
}

.plotPalg <- function( obj, annotate=TRUE )
{
  Palg    <- obj$Palg
  dimPalg <- dim( Palg )
  nAges   <- dimPalg[1]
  nGear   <- dimPalg[3]
  nGrps   <- dimPalg[2]

  # HACK: ARK 25-Nov-09
  if ( nGear==2 )
    mfRow <- c(2,1)
  else if ( nGear==3 )
    mfRow <- c(3,1)
  else
    mfRow <- c(2,2)
  
  par( oma=c(3,3,2,1), mar=c(2,2,1,1), mfrow=mfRow )
  
  xRange <- c(0,nAges)
  yRange <- c(0,1)

  for ( g in 1:nGear )
  {  
    plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  
    for( i in 1:nGrps )  
      lines( c(1:nAges),Palg[,i,g], lty=1 )
      
    panLab( 0.05,0.90, adj=0, obj$gNames[g] )  
        
    axis( side=1, cex.axis=.REFCAX )
    axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
    box()
  }
  
  mtext( side=1, line=1, cex=.REFCEX, outer=TRUE, "Age" )
  mtext( side=2, line=1, cex=.REFCEX, outer=TRUE, "Proportion Discarded at Age" )
}

.plotDisMortRate <- function( obj, annotate=TRUE )
{
  # Discard mortality rates.
  par( oma=c(2,3,1,1), mar=c(2,2,1,1), mfrow=c(1,1) )
  
  nGear <- obj$nGear
  xRange <- range( 1:nGear )
  yRange <- c(0,1)
  
  plot( xRange,yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:length(obj$dg)), obj$dg, type="h", lwd=10 )
  #points( c(1:length(obj$dg)), obj$dg, pch=16 )
  
  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames
    
  axis( side=1, at=c(1:nGear), labels=gearLabels, cex.axis=.REFCEX )
  axis( side=2 )
  box()
  mtext( side=1, line=2, cex=.REFCEX, "Gear" )
  mtext( side=2, line=2, cex=.REFCEX, "Discard Mortality Rate" )
}

.plotYieldByGear <- function( obj, gfx )
{
  annotate <- gfx$annotate
  checked <- gfx$checked
  if ( sum( gfx$checked ) > 1 || sum( gfx$checked)==0 )
  {
    for ( i in 1:length( checked ) )
      checked[i] <- FALSE
    checked["Fmsy"] <- TRUE
    Fval <- obj$Fmsy
  }
 
  if ( checked["F0"] )
  {
    Fval <- obj$F0
    yprL <- obj$yprLF0
    yprD <- obj$yprDF0
    yieldL <- obj$recruitsF0*obj$yprLF0
    yieldD <- obj$recruitsF0*obj$yprDF0      
  }
  
  if ( checked["F01"] )
  {
    Fval   <- obj$F01
    yprL   <- obj$yprLF01
    yprD   <- obj$yprDF01
    yieldL <- obj$recruitsF01*obj$yprLF01
    yieldD <- obj$recruitsF01*obj$yprDF01
  }
      
  if ( checked["Fcra"] )
  {
    Fval   <- obj$Fcra
    yprL   <- obj$yprLFcra
    yprD   <- obj$yprDFcra
    yieldL <- obj$recruitsFcra*obj$yprLFcra
    yieldD <- obj$recruitsFcra*obj$yprDFcra
  }
  
  if ( checked["Fmax"] )
  {
    Fval   <- obj$Fmax
    yprL   <- obj$yprLFmax
    yprD   <- obj$yprDFmax
    yieldL <- obj$recruitsFmax*obj$yprLFmax
    yieldD <- obj$recruitsFmax*obj$yprDFmax
  }
  
  if ( checked["Fmsy"] )
  {
    Fval   <- obj$Fmsy
    yprL   <- obj$yprLFmsy
    yprD   <- obj$yprDFmsy
    yieldL <- obj$recruitsFmsy*obj$yprLFmsy
    yieldD <- obj$recruitsFmsy*obj$yprDFmsy
  }
  
  if ( checked["F40"] )
  {
    Fval   <- obj$F40
    yprL   <- obj$yprLF40
    yprD   <- obj$yprDF40
    yieldL <- obj$recruitsF40*obj$yprLF40
    yieldD <- obj$recruitsF40*obj$yprDF40
  }
  
  par( oma=c(2,3,1,1), mar=c(2,2,1,1), mfrow=c(2,1) )
  
  # Plot the yield per recruit for landed and discarded.
  nGear  <- obj$nGear
  xRange <- range((1-1):(nGear+1))
  yRange <- c(0, max(c(yprL,yprD)) )
  
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  points( c(1:length(yprL)),yprL, cex=1.2, pch=16 )
  points( c(1:length(yprD)),yprD, cex=1.2, pch=1  )
  
  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames  
  axis( side=1, at=c(1:nGear),labels=gearLabels, cex.axis=.REFCEX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  
  if ( annotate )
  {
    panLegend( 0.6,0.8, legTxt=c("Landed","Discarded"),pch=c(16,1) )
  }
  
  box()
  mtext( side=2, line=3, cex=.REFCEX, "Yield Per Recruit" )
  
  # Plot the yield for landed and discarded.
  xRange <- range((1-1):(nGear+1))
  yRange <- c( 0,max( c(yieldL,yieldD) ) )
  
  plot( xRange, yRange, type="n", axes=FALSE, xlab="",ylab="" )
  points( c(1:length(yieldL)),yieldL, cex=1.2, pch=16 )
  points( c(1:length(yieldD)),yieldD, cex=1.2, pch=1  )
  
  if ( annotate )
  {
     text( c(1:length(yieldL)),yieldL, labels=round(yieldL,digits=3), cex=1.0, pos=4 )
     text( c(1:length(yieldD)),yieldD, labels=round(yieldD,digits=3), cex=1.0, pos=4 )
  }
  
  if ( annotate )
  {
    panLegend( 0.6,0.9,
      legTxt=paste( c(names(checked)[checked],"Landed","Discarded"),
             round( c(Fval,sum(yieldL),sum(yieldD)),digits=3) ) )
  }

  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames
    
  axis( side=1, at=c(1:nGear), labels=gearLabels, cex.axis=.REFCEX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
  
  mtext( side=2, line=3, cex=.REFCEX, "Yield" )
  
  mtext( side=1, line=0, cex=.REFCEX, outer=TRUE, "Gear" )
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Comparing simulatons                                 --#
#------------------------------------------------------------------------------#

.plotFvsh <- function( simObj )
{
  nSim  <- length( simulation )
  FmsyVec <- numeric( nSim )
  hVec    <- numeric( nSim )
  
  for ( i in 1:nSim )
  {
    FmsyVec[i] <- simulation[[i]]$Fmsy
    hVec[i]    <- simulation[[i]]$rSteepness
  }
  # Sort x=axis ascending.
  idx     <- order( hVec )
  FmsyVec <- FmsyVec[idx]
  hVec    <- hVec[idx]  

  plot( range( hVec ), range( FmsyVec ), type="n", axes=FALSE, xlab="", ylab="" )
  lines( hVec, FmsyVec, lty=1 )
  points( hVec, FmsyVec, cex=1.2, pch=16 )
  axis( side=1 )
  axis( side=2 )
  box()
  
  mtext( side=1, line=2, cex=1, "Steepness" )
  mtext( side=2, line=2, cex=1, "Fmsy" )
}

.plotYieldVsF <- function( simObj, refGear=3, gfx )
{
  annotate <- gfx$annotate
  checked <- gfx$checked

  nSim   <- length( simulation )
  nGear  <- simObj[[1]]$nGear
  gNames <- simObj[[1]]$gNames
  Fval   <- numeric( nSim )
  Fvec   <- numeric( nSim )
  yieldL <- numeric( nGear )
  yieldD <- numeric( nGear )
  
  landed  <- rep( 0, nSim )
  discard <- rep( 0, nSim )

#  for ( i in 1:nSim )
#  {
#    recruits <- trialObj[[i]]$recruitsFmsy
#    yprL     <- trialObj[[i]]$yprLFmsy
#    yprD     <- trialObj[[i]]$yprDFmsy
    
    # This is the yield of the killed landed and discarded fish.
#    yieldL <- recruits*yprL
#    yieldD <- recruits*yprD
#    Fvec[i]    <- trialObj[[i]]$fg[refGear]
    
#    print( rbind( yieldL, yieldD ) )
    
#    if ( i==1 )
#      result <- rbind( yieldL, yieldD )
#    else
#    {
#      tmp <- rbind( yieldL, yieldD )
#      result <- cbind( result, tmp )
#    }
#  }

  result <- matrix( NA, nrow=2, ncol=nGear*nSim )
  
  icol <- 0
  for ( g in 1:nGear )
  {
    for ( i in 1:nSim )
    {
      if ( sum( gfx$checked ) > 1 || sum( gfx$checked)==0 )
      {
        for ( iChk in 1:length( checked ) )
          checked[iChk] <- FALSE
        checked["Fmsy"] <- TRUE
      }
         
      icol <- icol + 1
      recruits <- simObj[[i]]$recruitsFmsy
      yprL     <- simObj[[i]]$yprLFmsy
      yprD     <- simObj[[i]]$yprDFmsy
 
      if ( checked["F0"] )
      {
        Fval[i]  <- simObj[[i]]$F0
        recruits <- simObj[[i]]$recruitsF0
        yprL     <- simObj[[i]]$yprLF0
        yprD     <- simObj[[i]]$yprDF0
      }
  
      if ( checked["F01"] )
      {
        Fval[i]  <- simObj[[i]]$F01
        recruits <- simObj[[i]]$recruitsF01
        yprL     <- simObj[[i]]$yprLF01
        yprD     <- simObj[[i]]$yprDF01

      }
      
      if ( checked["Fcra"] )
      {
        Fval[i]  <- simObj[[i]]$Fcra
        recruits <- simObj[[i]]$recruitsFcra
        yprL     <- simObj[[i]]$yprLFcra
        yprD     <- simObj[[i]]$yprDFcra

      }
  
      if ( checked["Fmax"] )
      {
        Fval[i]  <- simObj[[i]]$Fmax
        recruits <- simObj[[i]]$recruitsFmax
        yprL     <- simObj[[i]]$yprLFmax
        yprD     <- simObj[[i]]$yprDFmax

      }
  
      if ( checked["Fmsy"] )
      {
        Fval[i]  <- simObj[[i]]$Fmsy
        recruits <- simObj[[i]]$recruitsFmsy
        yprL     <- simObj[[i]]$yprLFmsy
        yprD     <- simObj[[i]]$yprDFmsy

      }
  
      if ( checked["F40"] )
      {
        Fval[i]  <- simObj[[i]]$F40
        recruits <- simObj[[i]]$recruitsF40
        yprL     <- simObj[[i]]$yprLF40
        yprD     <- simObj[[i]]$yprDF40
      }
    
      # This is the yield of the killed landed and discarded fish.
      yieldL[g] <- recruits*yprL[g]
      yieldD[g] <- recruits*yprD[g]
      Fvec[i]   <- simObj[[i]]$fg[refGear]
      
      result[ 1,icol ] <- yieldL[g]
      result[ 2,icol ] <- yieldD[g]
      
      landed[i]  <- landed[i] + yieldL[g]
      discard[i] <- discard[i] + yieldD[g]
    }
  }
  
  barplot( height=result, beside=TRUE,
           space=c(0.1,0.5),
           names.arg=paste( rep( gNames, rep(nSim,nGear) ),
                            rep( c(1:nSim),nGear),sep=""),
           col=c("black","darkgray")  )
  
  if ( annotate )
  {
    Flabel <- paste( names(checked)[checked],c(1:nSim),"  ",round(Fval,digits=3),sep="" )
#    Llabel <- paste( "Landed",c(1:nSim)  round( sum(yieldL), digits=3 ), sep="" )
#    Dlabel <- paste( "Discard", round( sum(yieldD), digits=3 ), sep="" )

    panLegend( 0.3,0.95, legTxt=Flabel, bty="n", cex=1 )
    panLegend( 0.55,0.95, legTxt=paste( "Land",c(1:nSim)," ",
               round(landed,digits=3), sep="" ),
               cex=1, bty="n", pch=15, col="black" )
  
    panLegend( 0.75,0.95, legTxt=paste( "Disc",c(1:nSim)," ",
               round(discard,digits=3), sep="" ),
               cex=1, bty="n", pch=15, col="darkgray" )
  }  
  
  box()
  
  mtext( side=2, line=2.5, cex=1.4, "Yield" )
}