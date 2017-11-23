#------------------------------------------------------------------------------#
# TO DO:                                                                       #
#                                                                              #
# 1. Translate "About" into a pop-up dialog box with a Close button.           #
# 2. Review globals, decide which should go into mseRglobals.r                 #
# 3. In .calcWa, where is bias correction?  Should c1, c2 be hard-wired?       #
# 6. Parameter description.                                                    #
# 7. The initial simulation loaded from the GUI - do we need "New".            #
# 8. Need to prevent deletion of simulations when there is only 1.             #
# 17. Add layout functioning.                                                  #
# 18. Add settings report.                                                     #
# 23. Review code for all occurences of ***                                    #
# 24. Annotation of current sim, etc to appear on all plots.                   #
# 26. Save gui iSim, nSim, etc to global but include in save (this is 22).     #
# 28. Check out the load and save GUI options functions suppied in PBSmod.     #
# 29. Check out why selectFile is not enforcing file types.                    #
# 30. Slowness comes from passing all the Palg, Plg, Slg, Salg, La1, and       #
#     reference curve values ~7000 numbers within simTracker.                  #
#                                                                              #
#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 2010                  #
#                                                                              #
#     Copyright 2008, 2009, 2010 by A.R. Kronlund, S.P. Cox                    #
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
#--                                                                          --#
#-- mseRgui.r: A mseR module for managing the GUI suite.                     --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 12-Nov-09 from mseRrefpt_funs.r and mseR 2.0       --#
#--                                                                          --#
#-- 12-Nov-09: mseRrefpt_funs developed by Sean Cox from mseR 2.0.           --#
#-- 12-Nov-09: mseRrefPoints.r implementation by A.R. Kronlund               --#
#-- 26-Jan-10: mseRopMod.r by combining mseRrefPoints.r and mseRsim.r        --#
#-- 03-Feb-10: Modifications to tracking list.                               --#
#-- 31-Mar-10: Renovations to adopt notebook widget.                         --#
#-- 01-Jun-10: Added in Cox's new mseRsystem code, see notes below.          --#
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
#    prefix referenced specified in .mseRguiSetup.                             #
# 7. The insertion of a function call in notebook means that function is       #
#    called upon createWin call.  This means variables may not be defined so   #
#    a default should be established.                                          #
# 8. Detecting the OS - use the variable .Platform$OS.type.                    #
#                                                                              #
# WHAT'S SAVED?                                                                #
#                                                                              #
# A mseR session leave tracks:                                                 #
#                                                                              #
# simTracker:    the simulation tracking list, BUT the current behavior is to  #
#                load the list from a saved .FSIM file if it exists, NOT the   #
#                simulation tracking list stored in the working directory.     #
#                If the simulation tracking file .FSIM does not exist, then    #
#                parameters are loaded from .FPARS and a new simulation        #
#                tracking object is created.                                   #
#                                                                              #
# .guiPars: the current parameters values for the mseR() GUI.                  #
# allPars:       the data frame with "parameter" and "value" fields required   #
#                for the mseR() GUI "object" widget.                           #
#                                                                              #
# TRACKING LOGIC:                                                              #
#                                                                              #
# Synchronization of results is determined by a date/time stamp.  There is a   #
# stamp for the Fit, Operating model (OM), Management Procedure (MP), the      #
# simulation results (Sim), and the Performance statistics (Perf).  A function #
# called .getStamp obtained a date/time stamp via R from the operating system. #
# The call the .getStamp assigns a date/time stamp using the following code:   #
#                                                                              #
#    paste( "sim", format( Sys.time(), format="%d%m%Y%H%M%S" ), sep="" )       #
#                                                                              #
# 1. Fit stamp - when the operating model is conditioned a "Fit" is performed. #
#    Side Effects - the date/time stamp for the "Fit" is updated.              #
#                 - the date/time stamps for other components are not updated. #
#                   so there is no synchronization between the Fit and others. #
#                 - CURRENTLY NOT IMPLEMENTED.                                 #
#                 - invalid parameters set the stamp to "NO_FIT".              #
# 2. OM stamp - when the operating model is adjusted                           #
#    Side Effects - the date/time stamp for the "OM" is updated.               #
#                 - the date/time stamp for the "MP" is updated.               #
#                 - the date/time stamps for other components are not updated. #
#                   so there is no synchronization with Fit, Sim, and Perf.    #
#                 - invalid parameters set the stamp to "NO_OM".               #
#                 - valid stamp means OM plots can be viewed.                  #
# 3. MP stamp - when the management procedure is adjusted                      #
#    Side Effects - the date/time stamp for the "MP" is updated.               #
#                 - the date/time stamp for the "OM" is updated.               #
#                 - the date/time stamps for other components are not updated  #
#                   so there is no synchronization with Fit, Sim, and Perf.    #
#                 - invalid parameters set the stamp to "NO_MP".               #
#                 - valid stamp means management procedure plots can be viewed.#
# 4. Sim stamp - when the feedback simulation is run.                          #
#    Side Effects - the date/time stamp for the "Sim" is updated.              #
#                 - the date/time stamp for the "OM","MP" is updated.          #
#                 - the "Fit" stamp is updated if it matches the "OM" and the  #
#                   "MP" stamps - CURRENTLY NOT IMPLEMENTED.                   #
#                 - invalid parameters set the stamp to "NO_SIM".              #
#                 - valid stamp means that the replicates can be viewed.       #
# 5. Perf stamp - when the performance statistics settings are adjusted.       #
#    Side Effects - the "Perf" stamp is set to match "OM","MP", "Sim" stamps,  #
#                   which should be the date/time stamp used to name the blob. #
#                 - invalid parameters set the stamp to "NO_PERF".             #
#                 - valid stamp means the performance plots and stats can be   #
#                   viewed.                                                    #
#                                                                              #
#-- PERFORMANCE STATISTICS                                                   --#
#                                                                              #
# Q1 - When does the GUI calculate performance statistics?                     #
#                                                                              #
# 1. Performance statistics are calculated for each simulation as the feedback #
#    loop end (by definition they have a valid "Sim" stamp.                    #
# 2. Performance statistics are calculated and saved whenever the "Perf" page  #
#    is adjusted with valid parameters for the simulations that have a valid   #
#    "Sim" stamp and are selected in the GUI.                                  #
# 3. Performance statistics are calculated for every simuation with a valid    #
#    "Sim" stamp as mseR() is exited and stored in the simTracker.             #
#
# Performance Statistics Functions (Hidden):                                   #
#                                                                              #
# .calcPerfStats      : Calculate performance statistics (calls .calcStatsXXX) #
#                       where XXX refers to one of the functions below.        #
#                                                                              #
# .calcStatsAAV       : Calculate quantiles of AAV statistics.                 #
# .calcStatsCatch     : Calculate quantiles of average catch statistics.       #
# .calcStatsDepletion : Calculate quantiles of average depletion statistics.   #
# .calcStatsFinalDep  : Calculate quantiles of final depletion statistics.     #
# .calcStatsLowCat    : Calculate quantiles of low catch statistics.           #
# .calcStatsLowDep    : Calculate quantiles of low depletion statistics.       #
# .calcStatsPolicy    : Calculate statistics related to Limit and Bmsy policy. #
# .calcStatsTarget    : Calculate target statistics WRT dep, time, certainty.  #
# .calcStatsZones     : Calculate prop. of time SSB in each stock status zone. #
#                                                                              #
# USE INSTRUCTIONS:                                                            #
#                                                                              #
# Enter mseR() at the R command console.                                       #
#                                                                              #
# MODIFYING mseR:                                                              #
#                                                                              #
# 1. How to add plots to GUI pages.                                            #
#                                                                              #
# 2. How to modify the plot selection widgets on GUI pages.                    #
#                                                                              #
# REQUIRES:                                                                    #
#                                                                              #
#   MASS         :                                                             #
#   MatrixSum    : Efficient matrix operations - see MatrixSum folder for zip. #
#   PBSmodelling :                                                             #
#   ref          : Pass large objects by reference (instead of by value).      #
#   RODBC        :                                                             #
#                                                                              #
# References:                                                                  #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., Egeli,A. and Kronlund, A.R. 2009.#
#   PBSmodelling 2.20: user’s guide revised from Canadian Technical Report of  #
#   Fisheries and Aquatic Science 2674: vi + 173 p. Last updated July 28, 2009 #
#                                                                              #
# GUI Functions (listed in order found in code):                               #
#                                                                              #
#   Flow: mseR -> .mseRguiSetup -> .wkDirSetup -> createWin -> .subGui         #
#                                                                              #
# mseR             : Run the mseR Operating Model GUI.                         #
#                                                                              #
# .guiOpmodSetup   : Set up and run the GUI.                                   #
# .wkDirSetup      : Working directory set-up.                                 #
# .validGuiSimPars : Check if the GUI parameters are valid.                    #
# .subGui     : Processes guiOpMod submit actions, e.g. buttons, entries.      #
#                                                                              #
# Helper Functions (Hidden, to show hidden function ls(all.names=TRUE)):       #
#                                                                              #
#------------------------------------------------------------------------------#

source( "mseRglobals.r" )
source( "mseRrefPoints.r" )
source( "mseRsystem.r" )
source( "mseRtools.r" )
source( "mseRplots.r" )

#------------------------------------------------------------------------------#
#-- GUI Functions                                                            --#
#------------------------------------------------------------------------------#

# mseR        (Run GUI)
# Purpose:    Run the mseR GUI.
# Parameters: None
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
mseR <- function()
{
  return( .mseRguiSetup("mseRgui") )
}

# .mseRguiSetup  (Setup for mseR GUI creation)
# Purpose:       Set up and run the mseR GUI.
# Parameters:    win is a character containing the name of the window to setup
# Returns:       NULL (invisibly)
# Source:        PBSref (modified), guiRef (December 2009), A.R. Kronlund
.mseRguiSetup <- function( win )
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  
  require( MatrixSum )
  require( MASS )
  require( ref )
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  dir <- .wkDirSetup()
  cat( "\nMSG (.mseRguiSetup) Working directory setup in ",dir,"\n" )
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  graphics.off()
  
  # Can a menu be created?
  goMenu <- TRUE
  
  simTrackerFileExists <- file.exists( .FSIM )
  
  # A simTracker Rdata file exists.
  if ( simTrackerFileExists )
  {
    # Load a default simTracker list.
    load( .FSIM )
    cat( "\nMSG (.mseRguiSetup) Found .FSIM simTracker file: ",.FSIM,"\n" )
    
    #--------------------------------------------------------------------------#
    # Create om, mp (data, assess, hcr) entry objects and overall par object.  #
    #--------------------------------------------------------------------------#    

    # *** Make these assignments a function.
    # These assignments create dataframes to support GUI widgets.
    
    iSim <- .guiPars$iSim
    
    assign ( "omPars", omPars, pos=1 )
    
    assign( "dataPars", dataPars, pos=1 )
    
    assign( "assessPars", assessPars, pos=1 )
    
    assign( "hcrPars", hcrPars, pos=1 )
    
    # Simulation list object for "Perf" note book page.
    tmp <- .getSimulationList( simTracker, simList=simTracker[[1]]$gui$pfSimList,
              init=FALSE )
    assign( "pfSimList", tmp, pos=1 )
    
    # Get the "Perf" page period list.
    assign( "pfPeriodList",simTracker[[1]]$gui$pfPeriodList,pos=1 )
    
    # Convert parameters from dataframe to a list for reference point calcs.
    simParsList <- .convertToList( rbind( omPars,dataPars,assessPars,hcrPars) )
  }
  # There is no default simTracker Rdata file, set up a null list.
  else
  {
    if ( exists( ".guiPars" ) )        # Remove lurking .guiPars list.
      rm( .guiPars, pos=1 )
      
    # Read a parameter file and save copy to working directory for GUI to use.    
    simPars <- .readParFile( .FPAR )
    cat( "\nMSG (.mseRguiSetup) Found simulation parameter file: ",.FPAR,"\n" )
    
    #--------------------------------------------------------------------------#
    # Create om, mp (data, assess, hcr) entry objects and overall par object.  #
    #--------------------------------------------------------------------------#    

    # *** Make these assignments a function.
    # These assignments create dataframes to support GUI widgets.
    
    assign( "omPars",
      .extractPars( simPars,parType=c("pars$","opModData$","opModEst$")), pos=1 )
    
    assign( "dataPars",
      .extractPars( simPars,parType="mp$data$"), pos=1 )
    
    assign( "assessPars",
      .extractPars( simPars,parType="mp$assess$"), pos=1 )
    
    assign( "hcrPars",
      .extractPars( simPars,parType="mp$hcr$"), pos=1 )
    
    # Create a new simTracker list and save to the working directory.
    # simTracker is a tree-like list, where the i-th element holds the i-th
    # simulation structure for the operating model, management procedure,
    # conditioning fit, performance measures, gui and option settings.
    # The feedback simulations will be held in separate Rdata directories
    # i=1,...,nSim with the i-th simulation results.
    
    simTracker          <- list()
    simTracker[[1]]     <- .createSimNode()
    names( simTracker ) <- "Simulation1"
   
    # Convert parameters from dataframe to a list for reference point calcs.
    simParsList <- .convertToList( simPars )
    
    # *** Validity check here, assume valid for now.
      
    # Calculate reference points from the operating model parameter list.
    cat( "\nMSG (.mseGuiSetup) Calculating reference points, please wait...\n" )
    tmpRef <- calcRefPoints( as.ref( simParsList ) )
    tmp    <- deref( tmpRef )
    cat( "\nMSG (.mseRguiSetup) Reference points calculated.\n" )
    
    # Add the updated reference points to the simTracker list.
    # Here is where we need to snip off the refCurves to cut bulk.
    #tmp <- .snipRefCurves( tmp )$refPoints
    
    simTracker[[1]]$om <- tmp
    
    # Add the date-time stamp for the OM and MP.
    stamp <- .getStamp()
    simTracker[[1]]$om$stamp <- stamp
    simTracker[[1]]$mp$stamp <- stamp
    
    # Add the simulation parameters to the simTracker list.
    #simTracker[[1]]$sim$simPars <- simPars
    
    assign( "simTracker", simTracker, pos=1 )
    
    # *** Replace these with .initPeriodList, .initMpData functions.
    
    # Simulation list object for "Perf" note book page.
    tmp <- .getSimulationList( simTracker, init=TRUE )
    assign( "pfSimList", tmp, pos=1 )    
    
    nT <- simTracker[[1]]$om$nT
    tMP <- simTracker[[1]]$om$tMP
    nProj <- nT - tMP + 1
    
    pfPeriodList <- data.frame( Period=c("Short","Medium","Long"),
                    Year1=rep(tMP,3),
                    Year2=c(tMP+10, tMP+15,nT) )
    assign( "pfPeriodList",pfPeriodList,pos=1 )
  }

  # Valid conditions exist for menu creation.
  if ( goMenu )
  {
    # Initialize the GUI from the PBSmodelling windows description file.
    createWin( paste( dir, "/", win, "Win.txt", sep="" ) )

    # Get the GUI parameters and make scope local to this function.
    guiInfo    <- getWinVal( scope="L", winName=win )
    guiChanges <- list()
    
    # .guiPars derived from .FSIM hidden variable if simTrackerFileExists.
    if ( exists( ".guiPars"  ) )
    {
      iSim   <- .guiPars$iSim    # Index of last used simulation.
      iRep   <- .guiPars$iRep    # Index of last used replicate.
      nRep   <- .guiPars$nRep    # Number of replicates.
      nbPage <- .guiPars$nbPage  # Index of last used notebook page.
    }
    else
    {
      iSim   <- 1
      iRep   <- 1
      nRep   <- simParsList$nReps
      nbPage <- 1
    }
    
    if ( !simTrackerFileExists )
      simTracker[[iSim]]$gui <- guiInfo
      
    guiChanges         <- simTracker[[iSim]]$gui
    guiChanges$iSim    <- iSim
    guiChanges$iRep    <- iRep
    guiChanges$nRep    <- nRep
    #guiChanges$simReps <- simParsList$nReps
    guiChanges$nbPage  <- nbPage
    
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                              init=FALSE )
    guiChanges$pfShow    <- sum( guiChanges$pfSimList$Select )
    guiChanges$pfNsim    <- length(simTracker)
    
    # Set periods.
    
    
    # Check to ensure all simulations, scenarios and mp's are named.
    for ( i in 1:length(simTracker) )
    {
      if ( simTracker[[i]]$gui$simName =="" )
      {
        simTracker[[i]]$gui$simName <- paste( "Simulation",i,sep="" )
      }
      if ( simTracker[[i]]$gui$scenarioName =="" )
      {
        simTracker[[i]]$gui$scenarioName <- paste( "Scenario",i,sep="" )
      }
      if ( simTracker[[i]]$gui$mpName =="" )
      {
        simTracker[[i]]$gui$mpName <- paste( "MP",i,sep="" )
      }
    }
    
    guiChanges$simName      <- simTracker[[iSim]]$gui$simName
    guiChanges$scenarioName <- simTracker[[iSim]]$gui$scenarioName
    guiChanges$mpName       <- simTracker[[iSim]]$gui$mpName
        
    # Regardless of whether an simulation file existed at startup,
    # there is now a simulation object available to update the GUI.  However,
    # if loaded from an .FPAR file then the reference points may be incorect.
    # Reference point in a saved simulation file will be correct.
      
    # Update and Round the reference points for display.
    # *** But the reference points were updated in the if branch for
    #     simTrackerFileExits==FALSE !!!
    # THIS DOES NOT RECALCULATE REFERENCE POINTS, JUST ROUNDS REF POINTS.
    
    guiChanges <- .updateGuiRefPoints( guiChanges,
                  simTracker[[iSim]]$om, pass=TRUE, digits=4 )
            
    setWinVal( guiChanges, win )
    
    status <- .setGuiState( simTracker[[iSim]], length(simTracker) )
    
    # Is there a sync'ed feedback simulation?
    if ( status$sim==TRUE )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simTracker[[guiChanges$iSim]]$om$stamp,".Rdata",sep="" )
      cat( "\nMSG (.mseRguiSetup) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
      
      # *** Change to "gui" when done, make nReps and nRep consistent.
      #guiChanges$nRep <- blob$simGuiPars$pars$nReps
      # Try to avoid dependence on simGuiPars
      guiChanges$nRep <- blob$pars$nReps
    }
    
    # Update the GUI, and re-extract the GUI parameters.
    setWinVal( guiChanges, win )
    guiInfo <- getWinVal( winName=win, scope="L" )

    # Save all the GUI parameters, including ref points to working directory.
    assign( ".guiPars",guiInfo,pos=1 )
      
    # Save the visible global list of simTracker nodes.
    assign( "simTracker",simTracker,pos=1 )

    # Create initial plots, the active notebooks page matters.
    if ( exists("iSim" ) )
    {
      if ( nbPage==1 )
        if ( status$om )
          .doGuiOmPlots( simTracker[[iSim]] )
      if ( nbPage==2 )
        if ( status$mp )
          .doGuiMpPlots( simTracker[[iSim]] )
      if ( nbPage==4 )
        if ( status$sim )
           .doGuiViewPlots( simTracker[[iSim]] )
      if ( nbPage==5 )
        .doGuiPerfPlots( simTracker )
    }
  }
  else
    cat( "\nERROR (.mseRguiSetup): GUI creation not possible.\n ")

  return( invisible() )
}     # .mseRguiSetup

# .wkDirSetup (Working directory set-up):
# Purpose:    Creates three (3) sub-directories if they do not exist:
#             (a) .DTEMP - contains R code and PBSModelling GUI control files
#                 copied from working directory.
#             (b) .DHELP - contains help files copied from working directory.
#             (c) .DDOCS - contains code and model documentation.
# Notes:      In "shell", translate=TRUE turns forward slash / to backslash \.
# Parameters: None
# Returns:    The full path to the new directory
# Source:     mseR V2.0, modified from PBSref
# Author:     A.R. Kronlund
.wkDirSetup <- function()
{
  pckg  <- .PACKAGE                      # Current package name.
  fdocs <- .DDOCS                        # Directory for documentation files.
  fhelp <- .DHELP                        # Directory for help files.
  ftemp <- .DTEMP                        # Directory for temporary files.
  wkDir <- getwd()                       # Current R working directory.
  
  # These two will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)          # Path to the R directory
  #fnam <- paste(rdir, dname, fils, sep = "/")  # the files you want to copy
  	
	# Create .FDOC directory if needed, then copy any required files.
	docDir <- paste( wkDir, .DDOCS, sep="/" )
	if ( !file.exists(docDir) )
	  shell( paste( "mkdir", docDir ), translate=TRUE )
	
	# Create. FHELP directory if needed, then copy any required files.
	helpDir <- paste( wkDir, .DHELP, sep="/" )
	if ( !file.exists(helpDir) )
	  shell( paste( "mkdir", helpDir ), translate=TRUE )
	
	helpFiles <- c( "mseRabout.txt" )
	
	srcFiles <- paste( wkDir,   helpFiles, sep="/" )    # Source files.
	tarFiles <- paste( helpDir, helpFiles, sep="/" )    # Target files.
	file.copy( srcFiles, tarFiles, overwrite = TRUE )
	
	# Create .FTEMP directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .DTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  tempFiles <- c(
                   "mseRgui.r",
                   "mseRguiWin.txt",
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

# .validGuiPars (valid parameters for guiOpMod):
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
#.validGuiPars <- function()
#{
  # Get the GUI  values and make them local to this function.
#  getWinVal( scope="L" )
                    
#  isValid <- TRUE
  
  # This will be a list of the parameters that require changing.                                                     
#  changes <- list()
  
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
#  if ( !isValid & .Platform$OS.type=="windows" )
#    bringToTop( -1 )
  
  # This takes the accumulated list of changes and applies them to the GUI.
#  setWinVal( changes )
#  return( isValid )
#}

# .validGuiPars (valid parameters for guiOpMod):
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
.validGuiPars <- function()
{
  # Get the GUI  values and make them local to this function.
  getWinVal( scope="L" )
  
  if ( !exists("iSim") )
    return()
                      
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

  # Simulation index.  
  if ( is.na(iSim) || (iSim < 1) || (iSim > nSim ) )
  {
    cat( "\nSimulation ID must be 1 <= iSim <= nSim.\n" )
    changes$iSim <- 1
    isValid <- FALSE
  }  

  # If there are invalid parameters, bring the console to the top.  
  if ( !isValid & .Platform$OS.type=="windows" )
    bringToTop( -1 )
  
  # This takes the accumulated list of changes and applies them to the GUI.
  if ( !isValid )
    setWinVal( changes )
  return( isValid )
}

# .validGuiViewPars (valid parameters for simulation GUI):
# Purpose:       Check whether the parameters supplied in View GUI are valid.
#                If invalid, display an error message in the R console and
#                clear the invalid field.
#                If it is a correctable field, corrects it in the GUI and does
#                not flag it as invalid
# Parameters:    None
# GUI inputs:    Parameters taken directly from active guiSim. 
# Returns:       TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:   Clears invalid fields
#                Corrects correctable fields (if any)
.validGuiViewPars <- function( maxReps=.MAXREP )
{
  # Get the GUI  values and make them local to this function.
  getWinVal( scope="L" )
  
  changes <- list()                  
  isValid <- TRUE
  
  # Replicate index.
  if ( is.na(iRep) || (iRep < 1) || (iRep > maxReps) )
  {
    cat( "\nERROR (.validGuiViewPars) Replicate ID must be 0 < iRep <= ",maxReps,".\n" )
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
  if ( !isValid )  
    setWinVal( changes )
  return( isValid )
}

.subSaveSim <- function()
{
  win <- .getWinName()     # Get the current window name
  act <- getWinAct()[1]    # Last menu window action. 
  
  doSave <- FALSE
  if ( act=="simExit" )
    doSave <- TRUE
  else 
    if ( getYes( "mseR2010: Do you want to save the simTracker file?" ) )
      doSave <- TRUE

  if ( doSave )
  {
    guiInfo <- getWinVal( winName=win )
    
    # Save the GUI settings.
    simTracker[[guiInfo$iSim]]$gui <- guiInfo
  
    # Save the simulation tracking list to R working directory, Rdata file.
    assign( "simTracker",simTracker,pos=1 )
    .saveSimulationToRdata( overWrite=TRUE )
    
    # Update .guiPars.    
    assign( ".guiPars", guiInfo, pos=1 )
    
    cat( "\nMSG (.subSaveSim) Simulation saved to Rdata file:",.FSIM,"\n" )
  }
  else
    cat( "\nMSG (.subSaveSim) Exiting without saving results... \n" )
}     # .subSaveSim function

#------------------------------------------------------------------------------#
#-- GUI Submit Functions                                                     --#
#                                                                              #
# .subGui manages the calls to individual gui Pages in notebook widget:        #
#   .subGuiOM
#   .subGuiMP
#   .subGuiSim
#   .subGuiView
#   .subGuiPerf
#   .subGuiOpt
#------------------------------------------------------------------------------#

# .subGui     (Main function to process GUI submit actions.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGui <- function()
{
  win        <- .getWinName()         # Get the current window name
  act        <- getWinAct()[1]        # Get last menu window action

  # Extract only the GUI values that are needed.  
  guiInfo <- getWinVal( c("iRep","iSim","nRep","nSim","nbPage","scenarioName","simName",
               "mpName"), scope="L", winName=win )

  guiChanges <- list()                # List for any changes

  valid <- TRUE
  if ( exists("iSim") )
    valid <- .validGuiPars()            # Are GUI parameters valid?
  else
    return()
    
  updateSim <- FALSE                  # Should "simTracker" be re-assigned?
  
  #----------------------------------------------------------------------------#
  #--                           HEADER Menu Actions                          --#
  #----------------------------------------------------------------------------#
     
  # LOAD a parameter file to create a new node.
  if ( act=="simLoad" )
  {
    #if ( exists( ".guiPars" ) )        # Remove lurking .guiPars list.
    #  rm( .guiPars, pos=1 )
      
    # Read a parameter file and save copy to working directory for GUI to use.
    #simPars <- .readParFile( .FPAR )
    simPars <- readSimPars()
    
    #cat( "\nMSG (.subGui) Found operating model parameter file: ",.FPAR,"\n" )
    
    #------------------------------------------------------------------------#
    # Create om, mp (data, assess, hcr) entry objects and overall par object.#
    #------------------------------------------------------------------------#    

    #assign( "simPars", simPars, pos=1 )    
    #assign( "vwPars", simPars, pos=1 )

    if ( !is.null(simPars) )
    {
      # Operating model parameters.
      assign( "omPars",
        .extractPars( simPars,parType=c("opModData$","opModEst$","pars$")), pos=1 )
    
      # Data parameters.
      assign( "dataPars",
        .extractPars( simPars,parType="mp$data$"), pos=1 )
    
      # Assessment parameters.
      assign( "assessPars",
        .extractPars( simPars,parType="mp$assess$"), pos=1 )
    
      # Harvest control rule parameters.
      assign( "hcrPars",
        .extractPars( simPars,parType="mp$hcr$"), pos=1 )

    nSim <- length( simTracker ) + 1     # Set current simTracker index.
    
    # Create a new simTracker tree.
    simTracker[[nSim]] <- .createSimNode()

    tmpSimName <- paste( "Simulation",nSim,sep="" )
    
    # Prevent a duplicate name.
    j <- 1
    while( any( tmpSimName==names(simTracker)[-nSim] ) )
    {
      j <- j + 1
      tmpSimName <- paste( tmpSimName,"-",j,sep="" )
    }    
    
    names(simTracker)[[nSim]] <- tmpSimName
    guiInfo$simName           <- tmpSimName
    guiChanges$simName        <- tmpSimName
    
    # Update scenarioName and mpName.
    guiChanges$scenarioName <- paste( "Scenario",nSim,sep="" )
    guiChanges$mpName       <- paste( "MP",nSim,sep="" )
    
    simParsList <- .convertToList( simPars )
    cat( "\nMSG (.subGui) Calculating reference points, please wait...\n" )
    tmpRef     <- calcRefPoints( as.ref( simParsList ) )
    tmp        <- deref( tmpRef )
    cat( "\nMSG (.subGui) Reference points calculated.\n" )
      
    # Update the reference points on the GUI.
    guiChanges <- .updateGuiRefPoints( guiChanges,
                   tmp, pass=TRUE, digits=4 )    
    
    # Update the simTracker list using the currently displayed OM and MP.
    simTracker[[nSim]]$fit <- simTracker[[iSim]]$fit
    simTracker[[nSim]]$om  <- tmp
    simTracker[[nSim]]$mp  <- simTracker[[iSim]]$mp
    simTracker[[nSim]]$sim <- simTracker[[iSim]]$sim
    simTracker[[nSim]]$gui <- guiInfo
    simTracker[[nSim]]$opt <- simTracker[[iSim]]$opt
    
    stamp <- .getStamp()
    simTracker[[nSim]]$om$stamp   <- stamp
    simTracker[[nSim]]$mp$stamp   <- stamp
    simTracker[[nSim]]$sim$stamp  <- "NO_SIM"
    simTracker[[nSim]]$perf$stamp <- "NO_PERF"
    
    # Now update the om and mp.
      # Operating model parameters.
      assign( "omPars",
        .extractPars( simPars,parType=c("opModData$","opModEst$","pars$")), pos=1 )
    
      # Data parameters.
      assign( "dataPars",
        .extractPars( simPars,parType="mp$data$"), pos=1 )
    
      # Assessment parameters.
      assign( "assessPars",
        .extractPars( simPars,parType="mp$assess$"), pos=1 )
    
      # Harvest control rule parameters.
      assign( "hcrPars",
        .extractPars( simPars,parType="mp$hcr$"), pos=1 )    
   
    guiChanges$omPars     <- omPars
    guiChanges$dataPars   <- dataPars
    guiChanges$assessPars <- assessPars
    guiChanges$hcrPars    <- hcrPars
    
    # Update the GUI simulation tracking variables.
    guiChanges$iSim    <- nSim
    guiChanges$nSim    <- nSim
    
    iSim <- nSim
    
    status <- .setGuiState( simTracker[[nSim]], nSim )
    
    # Simulation list object for "Perf" note book page.
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                              init=FALSE )
                   
    # Update the GUI, and re-extract the GUI parameters.
    setWinVal( guiChanges, win )
    guiInfo <- getWinVal( winName=win, scope="L" )                            
    simTracker[[nSim]]$gui <- guiInfo
                              
    updateSim <- TRUE                   
    }
  }    # endif simLoad.
    
  # SAVE the GUI parameters, force an update, and refresh plots.
  if ( act=="simSave" )
  {
    # Don't save invalid parameters.
    valid <- .validGuiPars()
    if ( valid )
    {
      # Save current GUI parameters to hidden global.
      assign( ".guiPars",guiInfo$pars,pos=1 )
      
      # Save current GUI parameters to ASCII text file.
      .saveSimPars( iSim )
      
      updateSim <- TRUE
    }
  }

  #----------------------------------------------------------------------------#
  #--                 Simulation Tracking Control Widgets                    --#
  #----------------------------------------------------------------------------#
    
  # Add a simulation to the list.
  if ( act=="newSim" )
  {
    nSim <- length( simTracker ) + 1     # Set current simTracker index.
    
    # Create a new simTracker tree.
    simTracker[[nSim]] <- .createSimNode()

    tmpSimName <- paste( "Simulation",nSim,sep="" )
    
    # Prevent a duplicate name.
    j <- 1
    while( any( tmpSimName==names(simTracker)[-nSim] ) )
    {
      j <- j + 1
      tmpSimName <- paste( tmpSimName,"-",j,sep="" )
    }    
    
    names(simTracker)[[nSim]] <- tmpSimName
    guiInfo$simName           <- tmpSimName
    guiChanges$simName        <- tmpSimName
    
    # Update scenarioName and mpName.
    guiChanges$scenarioName <- paste( "Scenario",nSim,sep="" )
    guiChanges$mpName       <- paste( "MP",nSim,sep="" )
    
    # Update the simTracker list using the currently displayed OM and MP.
    simTracker[[nSim]]$fit <- simTracker[[iSim]]$fit
    simTracker[[nSim]]$om  <- simTracker[[iSim]]$om
    simTracker[[nSim]]$mp  <- simTracker[[iSim]]$mp
    simTracker[[nSim]]$sim <- simTracker[[iSim]]$sim
    simTracker[[nSim]]$gui <- guiInfo
    simTracker[[nSim]]$opt <- simTracker[[iSim]]$opt
    
    stamp <- .getStamp()
    simTracker[[nSim]]$om$stamp   <- stamp
    simTracker[[nSim]]$mp$stamp   <- stamp
    simTracker[[nSim]]$sim$stamp  <- "NO_SIM"
    simTracker[[nSim]]$perf$stamp <- "NO_PERF"
    
    # Update the GUI simulation tracking variables.
    guiChanges$iSim    <- nSim
    guiChanges$nSim    <- nSim
    
    iSim <- nSim
    
    status <- .setGuiState( simTracker[[nSim]], nSim )
    
    # Simulation list object for "Perf" note book page.
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                              init=FALSE )
                              
    updateSim <- TRUE               
  }
  
  # Arbitrary simulation.
  if ( act=="iSim" )
  {
    # Increment the simulation counter, but do not permit iSim > nSim.
    #iSim <- min( (iSim+1), nSim )
    iSim <- ifelse( iSim < 1, 1, iSim )
    iSim <- ifelse( iSim > nSim, nSim, iSim )
    
    guiChanges$simName      <- simTracker[[iSim]]$gui$simName
    guiChanges$scenarioName <- simTracker[[iSim]]$gui$scenarioName
    guiChanges$mpName       <- simTracker[[iSim]]$gui$mpName
    
    # Update the reference points on the GUI from the next simulation.
    # *** Can we remove this?
    guiChanges <- .updateGuiRefPoints( guiChanges, simTracker[[iSim]]$om,
                  pass=TRUE, digits=4 )   
    
    # Don't use the saved iSim, nSim, or plot types, graphics settings.
    guiChanges$omPars     <- simTracker[[iSim]]$gui$omPars
    guiChanges$dataPars   <- simTracker[[iSim]]$gui$dataPars
    guiChanges$assessPars <- simTracker[[iSim]]$gui$assessPars
    guiChanges$hcrPars    <- simTracker[[iSim]]$gui$hcrPars
    guiChanges$nRep       <- simTracker[[iSim]]$gui$nRep
    
    # Update the simulation information.
    guiChanges$iSim <- iSim
    guiChanges$nSim <- length(simTracker)
    
    status <- .setGuiState( simTracker[[iSim]], length(simTracker) )   
    if ( status$sim==TRUE )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simTracker[[guiChanges$iSim]]$om$stamp,".Rdata",sep="" )
      cat( "\nMSG (.subGui) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
    
      # *** Change to "gui" when done, make nReps and nRep consistent.
      #guiChanges$nRep <- blob$simGuiPars$pars$nReps
      # Try to avoid dependence on simGuiPars
      guiChanges$nRep <- blob$pars$nReps
      
      # Now if the replicates differ among simulations, then we need to make
      # sure we are not out of range.
      guiChanges$iRep <- ifelse( iRep > guiChanges$nRep, guiChanges$nRep, iRep )
    }  
  }     # act=="iSim"
  
  # Next simulation (>).
  if ( act=="nextSim" )
  {
    # Increment the simulation counter, but do not permit iSim > nSim.
    iSim <- min( (iSim+1), nSim )
    
    guiChanges$simName      <- simTracker[[iSim]]$gui$simName
    guiChanges$scenarioName <- simTracker[[iSim]]$gui$scenarioName
    guiChanges$mpName       <- simTracker[[iSim]]$gui$mpName
    
    # Update the reference points on the GUI from the next simulation.
    # *** Can we remove this?
    guiChanges <- .updateGuiRefPoints( guiChanges, simTracker[[iSim]]$om,
                  pass=TRUE, digits=4 )   
    
    # Don't use the saved iSim, nSim, or plot types, graphics settings.
    guiChanges$omPars     <- simTracker[[iSim]]$gui$omPars
    guiChanges$dataPars   <- simTracker[[iSim]]$gui$dataPars
    guiChanges$assessPars <- simTracker[[iSim]]$gui$assessPars
    guiChanges$hcrPars    <- simTracker[[iSim]]$gui$hcrPars
    guiChanges$nRep       <- simTracker[[iSim]]$gui$nRep
    
    # Update the simulation information.
    guiChanges$iSim <- iSim
    guiChanges$nSim <- length(simTracker)
    
    status <- .setGuiState( simTracker[[iSim]], length(simTracker) )   
    if ( status$sim==TRUE )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simTracker[[guiChanges$iSim]]$om$stamp,".Rdata",sep="" )
      cat( "\nMSG (.subGui) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
    
      # *** Change to "gui" when done, make nReps and nRep consistent.
      #guiChanges$nRep <- blob$simGuiPars$pars$nReps
      # Try to avoid dependence on simGuiPars
      guiChanges$nRep <- blob$pars$nReps
      
      # Now if the replicates differ among simulations, then we need to make
      # sure we are not out of range.
      guiChanges$iRep <- ifelse( iRep > guiChanges$nRep, guiChanges$nRep, iRep )
    }
  }  
  
  # Previous simulation (<).
  if ( act=="prevSim" )
  {
    # Decrement the iSim counter, but do not permit iSim < 1.
    iSim <- max( 1, (iSim-1) )

    guiChanges$simName      <- simTracker[[iSim]]$gui$simName
    guiChanges$scenarioName <- simTracker[[iSim]]$gui$scenarioName
    guiChanges$mpName       <- simTracker[[iSim]]$gui$mpName
    
    # Round the reference points GUI from the previous simulation.
    guiChanges <- .updateGuiRefPoints( guiChanges, simTracker[[iSim]]$om,
                  pass=TRUE, digits=4 )   

    # Don't use the saved iSim, nSim, or plot types, graphics settings.
    guiChanges$omPars     <- simTracker[[iSim]]$gui$omPars
    guiChanges$dataPars   <- simTracker[[iSim]]$gui$dataPars
    guiChanges$assessPars <- simTracker[[iSim]]$gui$assessPars
    guiChanges$hcrPars    <- simTracker[[iSim]]$gui$hcrPars
    guiChanges$nRep       <- simTracker[[iSim]]$gui$nRep
    
    # Update the simulation tracking information.
    guiChanges$iSim <- iSim
    #guiChanges$nSim <- length(simTracker)
    #guiChanges$simName <- names(simTracker)[iSim]
    
    status <- .setGuiState( simTracker[[iSim]], length(simTracker) )   
    if ( status$sim==TRUE )
    {
      # Load an Rdata working directory containing a list called blob.
      simFile <- paste( "sim",simTracker[[guiChanges$iSim]]$om$stamp,".Rdata",sep="" )
      cat( "\nMSG (.subGui) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
      
      # *** Change to "gui" when done, make nReps and nRep consistent.
      # guiChanges$nRep <- blob$simGuiPars$pars$nReps
      # Try to avoid dependence on simGuiPars
      guiChanges$nRep <- blob$pars$nReps
      
      # Now if the replicates differ among simulations, then we need to make
      # sure we are not out of range.
      guiChanges$iRep <- ifelse( iRep > guiChanges$nRep, guiChanges$nRep, iRep )      
    }
  }
  
  # ADD a simulation from a saved blob, e.g., simDDMMYYYhhmmss.Rdata.
  if ( act=="addSim" )
  {
    # Select blob file, this call invokes a GUI.
    simFile <- .loadSimulationFromRdata()
    
    if ( !is.null(simFile) )
    {
      cat( "\nMSG (.subGui) Loading",simFile,"...\n" )    
      load( file=simFile )
      assign( "blob", blob, pos=1 )
    
      # Now make a copy of the simFile with a date-time stamp so it can be
      # located from the simTracker list.
    
      stamp <- .getStamp()    
      newFile <- paste( "sim",stamp,".Rdata", sep="" )
      file.copy( simFile, newFile, overwrite=TRUE )
      cat( "\nMSG (.subGui) Simulation results copied to ",newFile,"\n" )
    
      # Extract the runMSEpar element.
      simPars <- blob$runMSEpar
    
      omPars     <- .extractPars( simPars,parType=c("opModData$","opModEst$","pars$") )
      dataPars   <- .extractPars( simPars,parType="mp$data$" )
      assessPars <- .extractPars( simPars,parType="mp$assess$" )
      hcrPars    <- .extractPars( simPars,parType="mp$hcr$" )
 
      nSim <- length( simTracker ) + 1     # Set current simTracker index.
    
      # Create a new simTracker tree.
      simTracker[[nSim]] <- .createSimNode()

      tmpSimName <- paste( "Simulation",nSim,sep="" )
      
      # Prevent a duplicate name.
      j <- 1
      while( any( tmpSimName==names(simTracker)) )
      {
        j <- j + 1
        tmpSimName <- paste( tmpSimName,"-",j,sep="" )
      }
      
      names(simTracker)[[nSim]] <- tmpSimName
      
      guiInfo$simName           <- tmpSimName
      guiChanges$simName        <- tmpSimName
    
      # Update scenarioName and mpName.
      guiChanges$scenarioName <- paste( "Scenario",nSim,sep="" )
      guiChanges$mpName       <- paste( "MP",nSim,sep="" )
    
      # Update the pars.
      guiChanges$omPars     <- omPars
      guiChanges$dataPars   <- dataPars
      guiChanges$assessPars <- assessPars
      guiChanges$hcrPars    <- hcrPars
    
      # Update the simTracker list using the currently displayed OM and MP.
      stamp <- .getStamp()
      simTracker[[nSim]]$fit <- simTracker[[iSim]]$fit
      simTracker[[nSim]]$fit$stamp <- "NO_FIT"
    
      simParsList <- .convertToList( simPars )
      cat( "\nMSG (.subGui) Calculating reference points, please wait...\n" )
      tmpRef     <- calcRefPoints( as.ref( simParsList ) )
      tmp        <- deref( tmpRef )
      cat( "\nMSG (.subGui) Reference points calculated.\n" )
      
      # Update the reference points on the GUI.
      guiChanges <- .updateGuiRefPoints( guiChanges,
                     tmp, pass=TRUE, digits=4 )      
      
      simTracker[[nSim]]$om <- tmp
    
      simTracker[[nSim]]$om$stamp <- stamp
    
      simTracker[[nSim]]$mp  <- simTracker[[iSim]]$mp
      simTracker[[nSim]]$mp$stamp <- stamp
      simTracker[[nSim]]$sim <- simTracker[[iSim]]$sim
      simTracker[[nSim]]$sim$stamp <- stamp
      simTracker[[nSim]]$perf <- simTracker[[iSim]]$perf
      simTracker[[nSim]]$perf$stamp <- "NO_PERF"
      simTracker[[nSim]]$opt <- NULL

      # Update the GUI simulation tracking variables.
      guiChanges$iRep    <- 1
      guiChanges$nRep    <- simTracker[[nSim]]$om$nReps        
      guiChanges$iSim    <- nSim
      guiChanges$nSim    <- nSim
      #guiChanges$simReps  <- simTracker[[nSim]]$om$nReps
    
      iSim <- nSim
    
      setWinVal( guiChanges, winName=win )
      guiInfo <- getWinVal( winName=win  )
      simTracker[[nSim]]$gui <- guiInfo
    
      status <- .setGuiState( simTracker[[nSim]], nSim )
    
      # Simulation list object for "Perf" note book page.
      guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                              init=FALSE )
      updateSim <- TRUE                 
    }  
    else
      cat( "\nMSG (.subGui) File not selected, aborting Add blob.\n" )
  }
     
  # REMOVE a simulation.
  if ( act=="remSim" )
  {
    if ( getYes( "Are you sure you want to delete the simulation?" ) )  
    {
      # Rules:
      # (a) If there is only 1 simulation, nothing gets done.
      # (b) If simulation (1,...,n-1) is removed, show the next simulation.
      # (c) If simulation n is removed, show simulation n-1.
    
      if ( length(simTracker) > 1 )
      { 
        # Find the index of the simulation to be removed.
        iRem <- c(1:length(simTracker))[ guiInfo$simName==names(simTracker) ]
      
        # Adjust the current simulation to account for the removal.
        # iSim is the "new" index in the reduced simulation list.
      
        if ( iRem==length(simTracker) )
          iSim <- length(simTracker) - 1      # (c) simulation n removed, show n-1.
        else if ( iRem < length(simTracker) )
          iSim <- max( 1, iRem-1 )
      
        # Remove the iRem simulation.
        simTracker[[iRem]] <- NULL            
      
        # Update the reference points from the previous simulation.
        guiChanges <- .updateGuiRefPoints( guiChanges,
                      simTracker[[iSim]]$om, pass=TRUE, digits=4 )
      
        guiChanges$simName      <- simTracker[[iSim]]$gui$simName
        guiChanges$scenarioName <- simTracker[[iSim]]$gui$scenarioName
        guiChanges$mpName       <- simTracker[[iSim]]$gui$mpName
    
        # Don't use the saved iSim, nSim, or plot types, graphics settings.
        guiChanges$omPars     <- simTracker[[iSim]]$gui$omPars
        guiChanges$dataPars   <- simTracker[[iSim]]$gui$dataPars
        guiChanges$assessPars <- simTracker[[iSim]]$gui$assessPars
        guiChanges$hcrPars    <- simTracker[[iSim]]$gui$hcrPars
        guiChanges$nRep       <- simTracker[[iSim]]$gui$nRep
    
        # Update the simulation tracking information.
        guiChanges$iSim <- iSim
        guiChanges$nSim <- length(simTracker)
        guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                                   init=FALSE )
    
        status <- .setGuiState( simTracker[[iSim]], length(simTracker) )   
        if ( status$sim==TRUE )
        {
          # Load an Rdata working directory containing a list called blob.
          simFile <- paste( "sim",simTracker[[guiChanges$iSim]]$om$stamp,".Rdata",sep="" )
          cat( "\nMSG (.subGui) Loading",simFile,"...\n" )    
          load( file=simFile )
          assign( "blob", blob, pos=1 )
      
          guiChanges$nRep <- blob$pars$nReps
        }

        updateSim <- TRUE
      }
    }
    else
      cat( "\nMSG (.subGui) Cannot remove last simulation.\n" )
  }
    
  # Rename a simulation
  if ( act=="simName" )
  {
    simName <- gsub( " ","", simName )
  
    if ( any( guiInfo$simName==names(simTracker)[-iSim] ) )
      cat( "\nERROR (.subGui) Duplicate simulation name, setting unchanged.\n" )
    else
    {
      names( simTracker )[iSim]      <- simName
      simTracker[[iSim]]$gui$simName <- simName
      names( simTracker )[ iSim ]    <- simName
      guiChanges$simName             <- simName
      
      # Simulation list object for "Perf" note book page.
      guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                                 init=FALSE )      
      
      cat( "\nMSG (.subGui) Simulation node renamed.\n " )
      updateSim <- TRUE
    }
  }
  
  # Rename the scenario or the procedure.
  if ( act=="scenarioName" || act=="mpName" )
  {
    # Simulation list object for "Perf" note book page.
    guiChanges$scenarioName <- scenarioName
    guiChanges$mpName       <- mpName
    
    simTracker[[iSim]]$gui$scenarioName <- scenarioName
    simTracker[[iSim]]$gui$mpName       <- mpName
    
    cat( "\nMSG (.subGui) Scenario or Procedure name updated...\n" )
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                               init=FALSE )
    updateSim <- TRUE    
  }    
    
  # EXPORT the simulation list to an R directory file.
  if ( act=="export" )
  {
    if ( exists( "simTracker" ) )
      .saveSimulationToRdata()
    else
      cat( "\nMSG (.subGui) No simulation tracking list available to export.\n" )
  }
  
  # IMPORT the simulation list to the working directory.
  if ( act=="import" )
  {
    # *** Need to put a check in to ensure that a valid simulation tracking
    #     list was loaded...
    
    .loadSimulationFromRdata()
    
    # At this time I will assume that the old iSim position should not be
    # restored, rather we will start fresh.
    
    # Update the reference points from first scenario.
    guiChanges <- .updateGuiRefPoints( guiChanges, simTracker[[1]]$om,
                  pass=TRUE, digits=4 )
    
    # Update the simPars object.
    #guiChanges$simPars <- simTracker[[1]]$gui$simPars
      
    # Update the simulation information.
    guiChanges$iSim    <- 1
    guiChanges$nSim    <- length(simTracker)
    guiChanges$simName <- names(simTracker)[1]
    
    updateSim <- TRUE
  }
  
  # ADD a simulation from a saved batcj job, e.g., myBatchJob.design
  if ( act=="batch" )
  {
    # Select a design file.
    designFileName <- .loadDesignFileName()
   
    if ( !is.null(designFileName) )
    {
      if ( getYes( "Are you sure you want to overwrite the tracking file?" ) )
      {
        # Load the designFile.
        batchDesign <- read.table( file=designFileName, as.is=TRUE, header=TRUE,
                         sep=",", stringsAsFactors=FALSE )
                         
        # Initialize a simTracker list.
        tmpTracker          <- list()
      
        # Loop over the simulations.
        for ( i in 1:nrow(batchDesign) )
        {
          # Make a temporary simTracker object, overwrite at the end.
          # Loop over entries in design file, load ends at last batch blob.
        
          #simFile <- paste( batchDesign$prefixName[i],batchDesign$stampName[i],
          #                  ".Rdata", sep="" ) 
          
          # Check to see if the desired blob file exists.
          blobFileExists <- FALSE
          if ( file.exists( batchDesign$blobName[i] ) )
          {
            # Blob file exists, load it.
            simFile <- batchDesign$blobName[i]
            cat( "\nMSG (.subGui) Loading",simFile,"...\n" )
            load( file=simFile )
            assign( "blob", blob, pos=1 )
            blobFileExists <- TRUE
            
            # Extract the runMSEpar element.
            simPars <- blob$runMSEpar
          }
          else
          {
            # Blob file does not exist, read par file.
            simPars <- .readParFile( parFile=batchDesign$parFile[i] )
          }
    
          omPars     <- .extractPars( simPars,parType=c("opModData$","opModEst$","pars$") )
          dataPars   <- .extractPars( simPars,parType="mp$data$" )
          assessPars <- .extractPars( simPars,parType="mp$assess$" )
          hcrPars    <- .extractPars( simPars,parType="mp$hcr$" )
 
          nSim <- length( tmpTracker ) + 1     # Set current simTracker index.
    
          # Create a new simTracker tree.
          tmpTracker[[nSim]] <- .createSimNode()

          tmpSimName <- paste( "Simulation",nSim,sep="" )
        
          # Prevent a duplicate name.
          j <- 1
          while( any( tmpSimName==names(tmpTracker)) )
          {
            j <- j + 1
            tmpSimName <- paste( tmpSimName,"-",j,sep="" )
          }        
       
          names(tmpTracker)[[nSim]] <- tmpSimName
          guiInfo$simName           <- tmpSimName
          guiChanges$simName        <- tmpSimName
    
          # Update scenarioName and mpName.
          guiChanges$scenarioName <- batchDesign$scenarioName[i]
          guiChanges$mpName       <- batchDesign$mpName[i]
    
          # Update the pars.
          guiChanges$omPars     <- omPars
          guiChanges$dataPars   <- dataPars
          guiChanges$assessPars <- assessPars
          guiChanges$hcrPars    <- hcrPars
    
          simParsList <- .convertToList( simPars )
          cat( "\nMSG (.subGui) Calculating reference points, please wait...\n" )
          tmpRef     <- calcRefPoints( as.ref( simParsList ) )
          tmp        <- deref( tmpRef )
          cat( "\nMSG (.subGui) Reference points calculated.\n" )
          tmpTracker[[nSim]]$om <- tmp
    
          # Update the simTracker list stamp values.
          if ( blobFileExists )
          {
            nPrefix <- nchar( batchDesign$prefix[i] )
            stamp <- substring( simFile,first=nPrefix+1,last=nPrefix+14 )
            tmpTracker[[nSim]]$fit$stamp  <- "NO_FIT"
            tmpTracker[[nSim]]$om$stamp   <- stamp
            tmpTracker[[nSim]]$mp$stamp   <- stamp          
            tmpTracker[[nSim]]$sim$stamp  <- stamp
            tmpTracker[[nSim]]$perf$stamp <- "NO_PERF"
            tmpTracker[[nSim]]$opt        <- NULL
          }
          else
          {
            #nPrefix <- nchar( batchDesign$prefix[i] )
            #stamp <- substring( batchDesign$stampName,start=nPrefix+1)
            stamp <- .getStamp()
            tmpTracker[[nSim]]$fit$stamp  <- "NO_FIT"
            tmpTracker[[nSim]]$om$stamp   <- stamp
            tmpTracker[[nSim]]$mp$stamp   <- stamp          
            tmpTracker[[nSim]]$sim$stamp  <- "NO_SIM"
            tmpTracker[[nSim]]$perf$stamp <- "NO_PERF"
            tmpTracker[[nSim]]$opt        <- NULL          
          }
          
          # Update the GUI simulation tracking variiables.
          guiChanges$iRep    <- 1
          guiChanges$nRep    <- tmpTracker[[nSim]]$om$nReps        
          guiChanges$iSim    <- nSim
          guiChanges$nSim    <- nSim
          #guiChanges$simReps <- tmpTracker[[nSim]]$om$nReps
    
          iSim <- nSim
    
          setWinVal( guiChanges, winName=win )
          guiInfo <- getWinVal( winName=win  )
          tmpTracker[[nSim]]$gui <- guiInfo
    
          status <- .setGuiState( tmpTracker[[nSim]], nSim )
    
          # Simulation list object for "Perf" note book page.
          guiChanges$pfSimList <- .getSimulationList( tmpTracker, simList=pfSimList,
                                  init=FALSE )
                                
          cat( "\nMSG (.subGui) Loaded batch job ",i," of ",nrow(batchDesign),"\n" )
        }     # Loop over simulations.
      
        # Replace simTracker with tmpTracker
        simTracker <- tmpTracker
        updateSim <- TRUE
      }  
    }
    else
      cat( "\nMSG (.subGui) File not selected, aborting Add blob.\n" )
  }
  
  if ( act=="moveL" )
  {
    # Move current simulation Left (up in simTracker).
    if ( iSim > 1 )
    {
      # There is room to move the simulation node up.
      tmp <- simTracker[[iSim-1]]
      tmpName <- names(simTracker)[iSim-1]
      simTracker[[iSim-1]] <- simTracker[[iSim]]
      simTracker[[iSim]]   <- tmp
      
      names( simTracker )[[iSim-1]] <- names(simTracker)[[iSim]]
      names( simTracker )[[iSim]]   <- tmpName
      
      guiChanges$iSim <- iSim - 1
      
      updateSim <- TRUE
    }
    else
      cat( "\nMSG (.subGuiPerf) Cannot move left, already at Simulation 1.\n" )
  }
  
  if ( act=="moveR" )
  {
    # Move current simulation Right (down in simTracker).
    if ( iSim < nSim )
    {
      # There is room to move the simulation node right.
      tmp <- simTracker[[iSim+1]]
      tmpName <- names(simTracker)[iSim+1]
      simTracker[[iSim+1]] <- simTracker[[iSim]]
      simTracker[[iSim]]   <- tmp
      
      names( simTracker )[[iSim+1]] <- names(simTracker)[[iSim]]
      names( simTracker )[[iSim]]   <- tmpName
      
      guiChanges$iSim <- iSim + 1
      
      updateSim <- TRUE
    }
    else
      cat( "\nMSG (.subGuiPerf) Cannot move right, already at last Simulation.\n" )
  }    
  
  # Write a data set to file from the operating model for conditioning fit.
  if ( act=="dataSim" )
  {
    if ( simTracker[[iSim]]$sim$stamp!="NO_SIM" )
    {
      # Write the current parameters to inputparameters.par.
      .saveSimPars( iSim, parFile=.FMSE, overWrite=TRUE )      
    
      cat( "\nMSG (.subGui) Writing ADMB pin and dat files for replicate",iRep,".\n" )    
      .writePinDat( parFile=.FMSE, blob, iRep=iRep )
      cat( "\nMSG (.subGui) ADMB pin and dat file write complete.\n" )
    }
    else
      cat( "\nMSG (.subGui) Simulation data NOT available - cannot write ADMB files.\n" )
  }  
  
  #----------------------------------------------------------------------------#
  #--                           Footer Actions                               --#
  #----------------------------------------------------------------------------#    

  if ( act=="simStats" )
  {
    cat( "\nMSG (.subGui) Writing simulation descriptions to file...\n\n" )
    .saveSimStatsToFile( simTracker )
  }

  if ( act=="perfStats" )
  {
    cat( "\nMSG (.subGui) Writing performance statistics to file(s)...\n" )
    if ( .saveSimulationToExcel( .FXLS ) )
      openFile( .FXLS )
  }
  
  # EXIT the GUI (leave graphics on).
  if ( act=="simExit" )
  {
    #guiInfo <- getWinVal( winName=win )
    
    # Save the GUI settings.
    #simTracker[[iSim]]$gui <- guiInfo
  
    # Save the simulation tracking list to R working directory, Rdata file.
    #assign( "simTracker",simTracker,pos=1 )
    #.saveSimulationToRdata( overWrite=TRUE )
    
    # Update .guiPars.    
    #assign( ".guiPars", guiInfo, pos=1 )
    
    #cat( "\nMSG (.subGuiOpMod) Simulation saved to Rdata file:",.FSIM,"\n" )
    
    on.exit( closeWin() )     # Don't close window until .subGui exits.
  }
 
  if ( !is.null(iSim) )
  {
    setWinVal( guiChanges, winName=win )
  
    guiInfo <- getWinVal( scope="L", winName=win )
    .setGuiState( simTracker[[iSim]], length(simTracker) )
    simTracker[[iSim]]$gui <- guiInfo
    
    if ( updateSim==TRUE )
      .saveSimulation( simTracker )

    assign( ".guiPars", guiInfo, pos=1 )

  #----------------------------------------------------------------------------#
  #--                        Notebook Page Actions                           --#
  #----------------------------------------------------------------------------#    
 
    if ( nbPage==1 )        # "OM" notebook page.
    {
      simTracker <- .subGuiOM( simTracker )
      updateSim <- TRUE
    }
    else if ( nbPage==2 )   # "MP" notebook page.
    {
      simTracker <- .subGuiMP( simTracker )
      updateSim <- TRUE
    }
    else if ( nbPage==3 )   # "Sim" notebook page.
    {
      simTracker <- .subGuiSim( simTracker )
      updateSim <- TRUE
    }
    else if ( nbPage==4 )   # "View" notebook page, does not modify "simTracker".
    {
      #.subGuiView()
      
      # NEXT replicate.
      if ( act=="vwNextRep" )
      {
        guiChanges$iRep <- min( (iRep + 1), nRep )     # Don't let iRep > nRep.
      }
    
      # PREVIOUS replicate.
      if ( act=="vwPrevRep" )
      {
        guiChanges$iRep <- max( 1, (iRep-1) )          # Don't let iRep < 1.
      }  

      # Clear the graphics windows.
      if ( act=="vwGrfxOff" )
      {
        graphics.off()
      }

      setWinVal( guiChanges, win )

      # *** Why is this necessary, nothing on View page that should alter GUI state.
      # ahhh, it is here to check that there is a valid simulation to plot.
      status <- .setGuiState( simTracker[[iSim]], length( simTracker ) )

      if ( status$sim )
      {
        .doGuiViewPlots( simTracker[[iSim]] )
      }
      else
        .plotStatus( plotLabel="No Feedback Simulation Results Available" )
    }
    else if ( nbPage==5 )   # "Perf" notebook page.
      simTracker <- .subGuiPerf( simTracker )
    else if ( nbPage==6 )   # "Options" note book page.
      simTracker <- .subGuiOpt( simTracker )
       
    if ( updateSim==TRUE )
      .saveSimulation( simTracker )
      
    guiInfo <- getWinVal( winName=win )
    assign( ".guiPars", guiInfo, pos=1 )
      
  }    # if !null iSim.
  
  return( invisible() )  
}     # END .subGui function.

# .subGuiOM   (Function to process GUI submit actions from OM Notebook Page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiOM <- function( simTracker )
{
  win        <- .getWinName()                      # Get current window name.
  act        <- getWinAct()[1]                     # Last GUI action.  
  #guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope.
  guiChanges <- list()                             # List for any changes.

  valid <- .validGuiPars()
  
  #if ( omAuto )
  par( oma=c(2,2,1,1), mar=c(2,2,1,1), mfrow=c(1,1) )
  
  # *** Here is where plot layout control must be handled.
  
  #else
  #{
  #  if ( omPlotByRow )
  #    par( oma=c(2,2,1,1), mar=c(2,2,1,1), mfrow=c(omNrows,omNcols) )
  #  else
  #    par( oma=c(2,2,1,1), mar=c(2,2,1,1), mfcol=c(omNrows,omNcols) )
  #}

  if ( valid  )
  {
    guiInfo <- getWinVal( c("omPars","iSim","pfSimList"), scope="L", winName=win )
    
    # No need to recalculate reference points unless omPars was diddled.
    # This action must come first in the function as it clobbers guiChanges.
    if ( act=="omPars" )
    {
      omParsList <- .convertToList( omPars )
      cat( "\nMSG (.subGuiOM) Calculating reference points, please wait...\n" )
      tmpRef     <- calcRefPoints( as.ref( omParsList ) )
      tmp        <- deref( tmpRef )
      cat( "\nMSG (.subGuiOM) Reference points calculated.\n" )
            
      simTracker[[iSim]]$om <- tmp
      stamp <- .getStamp()
      simTracker[[iSim]]$om$stamp <- stamp
      simTracker[[iSim]]$mp$stamp <- stamp

      guiChanges <- .updateGuiRefPoints( guiChanges, tmp, pass=FALSE, digits=4 )
      guiChanges$nRep <- simTracker[[iSim]]$om$nReps
      #guiChanges$simReps <- simTracker[[iSim]]$om$nReps
      
      # Update the simPars object.
      #guiChanges$simPars <- rbind( omPars,dataPars,assessPars,hcrPars )
      #guiChanges$vwPars  <- guiChanges$simPars
    }     # if act=omPars

    # Reset the graphics windows.
    if ( act=="omGfxOff" )
    {
      graphics.off()
    }
    
    # Simulation list object for "Perf" note book page.
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                               init=FALSE )        
   
    status <- .setGuiState( simTracker[[iSim]], length(simTracker ) )
    #status <- list( fit=FALSE, om=TRUE, mp=TRUE, sim=FALSE, perf=FALSE )
   
    if ( status$om )
      .doGuiOmPlots( simTracker[[iSim]] )
    else
      .plotStatus( plotLabel="OM Parameters are Invalid" )
  
    setWinVal( guiChanges, winName=win )
    
    guiInfo <- getWinVal( winName=win )
 
    #assign( ".guiPars", guiInfo, pos=1 )    

    simTracker[[iSim]]$gui <- guiInfo
    #.saveSimulation( simTracker )
    
  }     # endif valid.
  
  return( simTracker )
}     # END .subGuiOM function.

# .subGuiMP     (Main function to process GUI submit actions for MP notebook page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiMP <- function( simTracker )
{
  win        <- .getWinName()                      # Get the current window name
  act        <- getWinAct()[1]                     # Get last menu window action  
  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiChanges <- list()                             # List for any changes

  valid    <- .validGuiPars()
  updateMP <- FALSE
  
  if ( valid )
  {
    if ( act=="dataPars" )
    {
      #guiChanges$simPars <- rbind( omPars,dataPars,assessPars,hcrPars )
      #guiChanges$vwPars  <- guiChanges$simPars
      stamp <- .getStamp()
      simTracker[[iSim]]$om$stamp <- stamp
      simTracker[[iSim]]$mp$stamp <- stamp
      updateMP <- TRUE
    }
      
    if ( act=="assessPars" )
    {
      #guiChanges$simPars <- rbind( omPars,dataPars,assessPars,hcrPars )    
      #guiChanges$vwPars  <- guiChanges$simPars
      stamp <- .getStamp()      
      simTracker[[iSim]]$om$stamp <- stamp
      simTracker[[iSim]]$mp$stamp <- stamp
      updateMP <- TRUE
    }
      
    if ( act=="hcrPars" )
    {
      #guiChanges$simPars <- rbind( omPars,dataPars,assessPars,hcrPars )    
      #guiChanges$vwPars  <- guiChanges$simPars
      stamp <- .getStamp()            
      simTracker[[iSim]]$om$stamp <- stamp
      simTracker[[iSim]]$mp$stamp <- stamp
      updateMP <- TRUE
    }
  
    if ( updateMP )
    {
      # Simulation list object for "Perf" note book page.
      guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                                 init=FALSE )      
  
      setWinVal( guiChanges, winName=win )
    
      guiInfo <- getWinVal( winName=win )
    
      simTracker[[iSim]]$gui <- guiInfo
      .saveSimulation( simTracker )
      assign( ".guiPars", guiInfo, pos=1 )
    }     # updateMP
    
    status <- .setGuiState( simTracker[[iSim]], length(simTracker ) )   
    if ( status$mp )
      .doGuiMpPlots( simTracker[[iSim]] )
    else
      .plotStatus( plotLabel="Management Procedure Parameters Invalid" )        
  }     # endif valid.

  return( simTracker )
}     # END .subGuiMP function.

# .subGuiSim  (Main function to process GUI submit actions for Sim notebook page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiSim <- function( simTracker )
{
  win        <- .getWinName()                      # Get the current window name
  act        <- getWinAct()[1]                     # Get last menu window action  
  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiChanges <- list()                             # List for any changes

  valid <- .validGuiPars()
  
  if ( act=="simInvis" )
  {
    assign( ".INVISIBLE", simInvis, pos=1 )
  }
  
  if ( act=="simShow" )
  {
    assign( ".SHOWOUTPUTONCONSOLE", simShow, pos=1 )
  }
  
  if ( act=="simRun" )
  {
    simPars <- rbind( omPars,dataPars,assessPars,hcrPars )
  
    simParsList <- .convertToList( simPars )
    cat( "\nMSG (.subGuiSim) Calculating reference points, please wait...\n" )
    tmpRef      <- calcRefPoints( as.ref( simParsList ) )
    tmp         <- deref( tmpRef )
    cat( "\nMSG (.subGuiSim) Reference points calculated.\n" )
    
    # Assemble the ADMB command line options.
    admbOpts <- ""
    if ( simNox )
      admbOpts <- paste( admbOpts,"-nox" )
    if ( simEst )
      admbOpts <- paste( admbOpts,"-est" )
    if ( simPrint > 0 )
      admbOpts <- paste( admbOpts,"-iprint",simPrint )
    assign( ".ADMBOPTS",admbOpts,pos=1 )

    simTracker[[iSim]]$om       <- tmp
    stamp <- .getStamp()
    simTracker[[iSim]]$om$stamp <- stamp
    simTracker[[iSim]]$mp$stamp <- stamp
    guiChanges <- .updateGuiRefPoints( guiChanges, tmp, pass=FALSE, digits=4 )

    setWinVal( guiChanges, winName=win )    
    guiInfo <- getWinVal( scope="L", winName=win )

    cat( "\nMSG (.subGuiSim) Running the simulation...\n" )
    
    # Write the current parameters to inputparameters.par.
    .saveSimPars( iSim, parFile=.FMSE, overWrite=TRUE )
    
    #--------------------------------------------------------------------------#
    #-----                      Run the feedback loop                     -----#
    #--------------------------------------------------------------------------#
    runMSE()
    
    # Now save the results, blob is global and therefore not passed.
    # .saveFeedbackResults gets a NEW date/time stamp and returns it.
    stamp <- .saveFeedbackResults()
    
    # Update the stamp on the om and the sim status.
    simTracker[[iSim]]$om$stamp  <- stamp
    simTracker[[iSim]]$mp$stamp  <- stamp
    simTracker[[iSim]]$sim$stamp <- stamp
    
    # Update the GUI info for the simulation.
    simTracker[[iSim]]$gui <- guiInfo
        
    # Now update the GUI status.
    status <- .setGuiState( simTracker[[iSim]], length(simTracker) )
    
    # Simulation list object for "Perf" note book page.
    guiChanges$pfSimList <- .getSimulationList( simTracker, simList=pfSimList,
                              init=FALSE )    
    setWinVal( guiChanges, winName=win )
    guiInfo <- getWinVal( winName=win )
    assign( ".guiPars",guiInfo,pos=1 )
  }     # endif simRun.

  return( simTracker )
}     # END .subGuiSim function.

# .subGuiView (Main function to process GUI submit actions for View notebook page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiView <- function()
{
  win        <- .getWinName()                      # Get the current window name
  act        <- getWinAct()[1]                     # Get last menu window action
  #guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiInfo <- getWinVal( c("iRep","nRep","iSim"), scope="L", winName=win )
  guiChanges <- list()                             # List for any changes
  
  #guiChanges$iSim <- iSim

  valid <- .validGuiPars()
  
  if (  valid )
  {
    # NEXT replicate.
    if ( act=="vwNextRep" )
    {
      guiChanges$iRep <- min( (iRep + 1), nRep )     # Don't let iRep > nRep.
    }
    
    # PREVIOUS replicate.
    if ( act=="vwPrevRep" )
    {
      guiChanges$iRep <- max( 1, (iRep-1) )          # Don't let iRep < 1.
    }  

    # Clear the graphics windows.
    if ( act=="vwGrfxOff" )
    {
      graphics.off()
    }

    setWinVal( guiChanges, win )

    # *** Why is this necessary, nothing on View page that should alter GUI state.
    # ahhh, it is here to check that there is a valid simulation to plot.
    status <- .setGuiState( simTracker[[iSim]], length( simTracker ) )

    if ( status$sim )
    {
      .doGuiViewPlots( simTracker[[iSim]] )
    }
    else
      .plotStatus( plotLabel="No Feedback Simulation Results Available" )

    # Update .guiPars.    
    #guiInfo <- getWinVal( winName=win )
    #assign( ".guiPars", guiInfo, pos=1 )      
             
  }     # endif valid
  
  return( invisible() )
}     # END .subGuiView function.

# .subGuiPerf (Main function to process GUI submit actions for Perf notebook page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiPerf <- function( simObj )
{
  win        <- .getWinName()                      # Get the current window name
  act        <- getWinAct()[1]                     # Get last menu window action 
  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiChanges <- list()                             # List for any changes

  valid <- .validGuiPars()
  updatePerf <- FALSE
  
  if ( act=="pfSimList" )
  {
    updatePerf <- TRUE
    for ( i in 1:length(simTracker) )
      simObj[[i]]$gui$pfSimList <- pfSimList
      
    guiChanges$pfSimList <- pfSimList
    guiChanges$pfShow <- sum( pfSimList$Select )
    guiChanges$pfNsim <- length( simTracker )
  }
  
  if ( act=="pfSelectAll" )
  {
    updatePerf <- TRUE
    pfSimList$Select[ pfSimList$Stamp!="" ] <- TRUE
    for ( i in 1:length(simTracker) )
      simObj[[i]]$gui$pfSimList
    guiChanges$pfSimList <- pfSimList
    guiChanges$pfShow <- sum( pfSimList$Select )    
  }
  
  if ( act=="pfClearAll" )
  {
    updatePerf <- TRUE
    
    pfSimList$Select <- rep( FALSE, length(pfSimList$Select) )
    for ( i in 1:length(simTracker) )
      simObj[[i]]$gui$pfSimList
    guiChanges$pfSimList <- pfSimList
    guiChanges$pfShow <- sum( pfSimList$Select )
  }
  
  if ( act=="pfPeriodList" )
  {
    updatePerf <- TRUE
    for ( i in 1:length(simObj) )
      simObj[[i]]$gui$pfPeriodList <- pfPeriodList
    guiChanges$pfPeriodList <- pfPeriodList
  }
  
  if ( updatePerf )
  {
    setWinVal( guiChanges, winName=win )
   .setGuiState( simObj[[iSim]], length(simObj) )
   assign( "simTracker", simObj, pos=1 )    
  }
  
  # Any valid simulations to plot?
  if ( any( pfSimList$Select ) )
    .doGuiPerfPlots( simTracker )
  else
    .plotStatus( plotLabel="No Feedback Simulation Results Selected" )
    
  return( simObj )
}     # END .subGuiPerf function.

# .subGuiOpt (Main function to process GUI submit actions for Options notebook page.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subGuiOpt <- function( simTracker )
{
  win        <- .getWinName()                      # Get the current window name
  act        <- getWinAct()[1]                     # Get last menu window action 
  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI info local scope
  guiChanges <- list()                             # List for any changes

  valid <- .validGuiPars()
  
  return( simTracker )
}     # END .subGuiOpt function.
  
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

.createSimNode <- function()
{
  # We will probably not need gui and opt.  They will be globals saved to the blob
  # file, and mseRsimulation.xls.
  result <- list( fit=NULL, om=NULL, mp=NULL, sim=NULL, perf=NULL, gui=NULL, opt=NULL )
  
  # Oddly, setting stamp to NULL seems to delete the first sub-node of the tree.
  # So, set it to an arbitrary string that can also be used to determine if the
  # node has been updated at all from initial conditions.
  
  result$fit$stamp  <- "NO_FIT"
  result$om$stamp   <- "NO_OM"
  result$mp$stamp   <- "NO_MP"
  result$sim$stamp  <- "NO_SIM"
  result$perf$stamp <- "NO_PERF"
  result
}

.extractPars <- function( obj, parType=NULL )
{
  result <- NULL
  
  if ( is.null(parType) )
    result <- obj
    
  if ( !is.null(parType) )
  {
    for ( i in 1:length(parType) )
    {
      idx <- grep(parType[i],obj[,1],fixed=TRUE )
      if ( is.null(result) )
        result <- obj[ idx, ]
      else
        result <- rbind( result, obj[idx,] )
    }
  }
  result
}

.getGearRowCol <- function( nGear=1 )
{
  if ( nGear<=3 )
    mfRow <- c(nGear,1)
  else if ( nGear<=6 )
    mfRow <- c(round((nGear+0.1)/2),2)
  else
    mfRow <- c(3,2)
  mfRow
}

.getSimulationList <- function( simObj, simList=NULL, init=FALSE, maxSims=100 )
{
  Simulation <- character( maxSims )
  Scenario   <- character( maxSims )
  Procedure  <- character( maxSims )
  Stamp      <- character( maxSims )
  Select     <- logical( maxSims )
  
  nSims <- length( simObj )
  
  # Loop over the number of simulations, populating the simulation list for guiPerf
  # by extracting the contents of the simulation tracking list.   
  for ( i in 1:min(nSims,maxSims) )
  {
    if ( init )
    {
      Simulation[i] <- names(simObj)[i]
      Scenario[i]  <- ""
      Procedure[i] <- ""
    }
    else
    {
      Simulation[i] <- simObj[[i]]$gui$simName    
      Scenario[i]   <- simObj[[i]]$gui$scenarioName
      Procedure[i]  <- simObj[[i]]$gui$mpName
    }
    Stamp[i] <- simObj[[i]]$sim$stamp
  }
  
  result <- data.frame( Simulation, Scenario, Procedure, Stamp, Select,
                        stringsAsFactors=FALSE )
                        
  # Now check to see if any of the simulations were selected in Perf.
  if ( !is.null(simList) )
  {
    for ( i in 1:nSims )
    {
      if ( result$Stamp[i] != "NO_SIM" )
      {
        # Found a valid stamp, see if it was in the pfSimList.
        if ( is.element( result$Stamp[i], simList$Stamp ) )
        {
          # The stamp was in pfSimList$Stamp, so find the value of Select.
          idx <- c(1:length(simList$Stamp))[ result$Stamp[i]==simList$Stamp ]
          result$Select[i] <- simList$Select[idx]
        }
      }   # if not NO_SIM
    }  # i in 1,nSims
  }  #  if not null
  result
}     # .getSimulationList function

.getStatus <- function( dtStamps )
{
  # Update status - dtStamps is a list of stamps.
  # Enforce an OM-centric view, rather than cross-classified view. OM dominates.
  # If default om==NO_OM, then no sync possible.

  # If a dtStamp is NULL, then it gets a "NO_XXX" value.
  if ( is.null(dtStamps$fit) )
    dtStamps$fit <- "NO_FIT"
  if ( is.null(dtStamps$om) )
    dtStamps$om <- "NO_OM"
  if ( is.null(dtStamps$mp) )
    dtStamps$mp <- "NO_MP"
  if ( is.null(dtStamps$sim) )
    dtStamps$sim <- "NO_SIM"
  if ( is.null(dtStamps$perf) )
    dtStamps$perf <- "NO_PERF"  

  sync <- dtStamps
  sync <- lapply( sync, function(x) { x <- FALSE } )

  # These go through and turn things ON, they are assumed OFF until checked.
  if ( dtStamps$om != "NO_OM" )
  {
    sync$om <- TRUE
  
    if ( dtStamps$fit == dtStamps$om )     # Fit sync?
      sync$fit  <- TRUE

    if ( dtStamps$mp == dtStamps$om )      # MP sync?
      sync$mp  <- TRUE
      
    if ( dtStamps$sim  == dtStamps$om )    # Sim sync?
      sync$sim   <- TRUE
      
    if ( dtStamps$perf==dtStamps$om )      # Perf sync?
      sync$perf <- TRUE
  }
  sync
}

.makeTrackerFromBlobs <- function( blobNames )
{
  # blobNames is a character vector of *.Rdata files containing blobs.
  # A blob has the following 1-st level elements:
  #
  # pars        : the $pars input list, plus reference points via calcRefPoints
  # om          :
  # mp          :
  # runMSEpar   : the .FMSE (e.g., runMSE.par) input file as a dataframe.
  #               This can be converted to a list using .createList.
  #             : pars - the $pars elments of the input parameter file.
  #             : mp   - list with elements data, assess, and hcr containing the
  #                      corresponding $data, $assess, and $hcr components of 
  #                      the input parameter file.
  #
  # Only pars and runMSEpar are relevant to reconstructing a tracking entry.
  #
  # The simTracker list (default name "simulation") has the following elements:
  # Simulation1, Simulation2, ..... SimulationN.
  # Each 1st-level element has the following list elements with sub-elements:
  #
  # fit  : stamp
  # om   : All om, data, method, and hcr parameters, reference points, stamp
  # sim  : stamp
  # perf : stamp (not used)
  # gui  : mseR GUI list
  # opt  : NULL (not used)
  #
  # If the blob file, e.g., simDateStamp.Rdata was created by runMSE() without
  # the GUI, then much of the required GUI information is not available.
  #
  # We need to be able to rebuild the guiInfo list.  Most of the settings can
  # be default values, but in particular the function has to do the following:
  #
  # 1. Create $omPars
  # 2. Create $dataPars
  # 3. Create $assessPars
  # 4. Create $hcrPars
  # 5. Set simName, scenarioName, mpName via the batch design file.
  # 6. Update pfSimList with appropriate simName, scenarioName, mpName, stamp
  #    and set "Select" to TRUE.
  # 7. Set the appropriate OM, MP, Sim, and Perf stamps in the tracker.
  #
  # This is essentially an "newSim" action where the blob is immediately available.
  # so, (i) Invoke the GUI, (ii) go to Load batchJob, (iii) load design file,
  # (iv) iterate over blobs, updating the GUI with repeated addSim operations and
  # taking the simName, scenarioName, and mpName from the batch design file.
  #
  # Could add a page to mseR "Batch" to establish the simTracker file? Yes.
  # This would allow a new simTracker.Rdata file to be created with the appropriate
  # GUI entries simply by updating the GUI in the usual way.  Essentially a loop
  # for runMSE as usual, but instead of writing the text file we are writing the
  # tracker as a list.
  # 
}

.setGuiState <- function( simNode, nSims=1 )
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.
      
  # Check to see what GUI widgets should be disabled, enabled, etc.
  if ( nSims==1 )
    setWidgetState( varname="omPlotType",state="disabled",radiovalue="omFvsh",win )
  else
    setWidgetState( varname="omPlotType",state="normal",radiovalue="omFvsh",win )    

  dtStamps <- list(  fit=simNode$fit$stamp,
                     om=simNode$om$stamp,
                     mp=simNode$mp$stamp,
                     sim=simNode$sim$stamp,
                     perf=simNode$perf$stamp )
        
  # A list of TRUE/FALSE indicating synchronization with OM.
  status <- .getStatus( dtStamps )
        
  # Update the GUI synchronization status.
  if ( status$fit==TRUE )
    setWidgetColor( "indFit", winName=win, entrybg="green" )
  else
    setWidgetColor( "indFit", winName=win, entrybg="red" )
        
  if ( status$om==TRUE )
  {
    setWidgetState( varname="omPlotType",state="normal", winname=win )
    setWidgetColor( "indOM", winName=win, entrybg="green" )
  }
  else
  {
    setWidgetState( varname="omPlotType",state="disabled", winname=win )
    setWidgetColor( "indOM", winName=win, entrybg="red" )
  }
  
  if ( status$mp==TRUE )
  {
    setWidgetState( varname="omPlotType",state="normal", winname=win )
    setWidgetColor( "indMP", winName=win, entrybg="green" )
  }
  else
  {
    setWidgetState( varname="omPlotType",state="disabled", winname=win )
    setWidgetColor( "indMP", winName=win, entrybg="red" )
  }
       
  if ( status$sim==TRUE )
  {
    setWidgetState( varname="vwPlotType", state="normal", winname=win )  
    setWidgetColor( "indSim", winName=win, entrybg="green" )
  }
  else
  {
    setWidgetState( varname="vwPlotType", state="disabled", winname=win )
    setWidgetColor( "indSim", winName=win, entrybg="red" )
  }
      
  if ( status$perf==TRUE )
  {
    setWidgetState( varname="pfPlotType", state="normal", winname=win )  
#    setWidgetColor( "indPrf", winName=win, entrybg="green" )
  }
  else                                   
  {
    setWidgetState( varname="pfPlotType", state="disabled", winname=win )  
#    setWidgetColor( "indPrf", winName=win, entrybg="red" )
  }
  
  # Performance statistics page.
    if ( any( pfSimList$Select ) )
    {
      # Now check to distinguish between cases with all NO_SIM and at least
      # one valid feedback simulation.  Remember there can be "" stamps.
      if ( all(pfSimList$Stamp[ pfSimList$Select]=="NO_SIM") )
      {
        # Turn off all buttons.
        setWidgetState( varname="pfPlotType", state="disabled", winname=win )
        # Turn on equilibrium analysis buttons and set the active button to pfEqYprF.
        if ( !is.element( pfPlotType, c("pfEqYprF","pfEqSsbRF","pfEqYieldF",
                                        "pfEqSsbF","pfEqRecSSB","pfEqYieldSSB" ) ) )
          guiChanges$pfPlotType <- "pfEqYprF"
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqYprF",     win )
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqSsbRF",    win )
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqYieldF",   win )
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqSsbF",     win )
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqRecSSB",   win )         
        setWidgetState( varname="pfPlotType", state="normal", radiovalue="pfEqYieldSSB", win )
      }
      else
        setWidgetState( varname="pfPlotType", state="normal", winname=win )        
    }
    else
      setWidgetState( varname="pfPlotType", state="disabled", winname=win )
#  }
  
  # Disable the simTracker if notebook page "Perf" or "Options", i.e., 5 or 6.
  if ( nbPage==5 | nbPage==6 )
  {
    setWidgetState( varname="scenarioName", state="disabled", winname=win )
    setWidgetState( varname="simName",      state="disabled", winname=win )
    setWidgetState( varname="mpName",       state="disabled", winname=win )
    
    setWidgetState( varname="indFit",       state="disabled", winname=win )
    setWidgetState( varname="indOM",        state="disabled", winname=win )
    setWidgetState( varname="indMP",        state="disabled", winname=win )                         
    setWidgetState( varname="indSim",       state="disabled", winname=win )
#    setWidgetState( varname="indPrf",       state="disabled", winname=win )

    setWidgetState( varname="iSim",         state="disabled", winname=win )                    
    
    setWidgetState( varname="newSim",       state="disabled", winname=win )
    setWidgetState( varname="addSim",       state="disabled", winname=win )
    setWidgetState( varname="remSim",       state="disabled", winname=win )
    setWidgetState( varname="prevSim",      state="disabled", winname=win )
    setWidgetState( varname="nextSim",      state="disabled", winname=win )
    setWidgetState( varname="moveL",        state="disabled", winname=win )
    setWidgetState( varname="moveR",        state="disabled", winname=win )        
    setWidgetState( varname="import",       state="disabled", winname=win )                         
    setWidgetState( varname="export",       state="disabled", winname=win )
    setWidgetState( varname="batch",        state="disabled", winname=win )
    setWidgetState( varname="dataSim",      state="disabled", winname=win )            
  }
  else
  {
    setWidgetState( varname="scenarioName", state="normal", winname=win )
    setWidgetState( varname="simName",      state="normal", winname=win )
    setWidgetState( varname="mpName",       state="normal", winname=win )
    
    setWidgetState( varname="indFit",       state="normal", winname=win )    
    setWidgetState( varname="indOM",        state="normal", winname=win )
    setWidgetState( varname="indMP",        state="normal", winname=win )
    setWidgetState( varname="indSim",       state="normal", winname=win )
#    setWidgetState( varname="indPrf",       state="normal", winname=win )

    setWidgetState( varname="iSim",         state="normal", winname=win )    
    
    setWidgetState( varname="newSim",       state="normal", winname=win )
    setWidgetState( varname="addSim",       state="normal", winname=win )
    setWidgetState( varname="remSim",       state="normal", winname=win )
    setWidgetState( varname="prevSim",      state="normal", winname=win )
    setWidgetState( varname="nextSim",      state="normal", winname=win )
    setWidgetState( varname="moveL",        state="normal", winname=win )
    setWidgetState( varname="moveR",        state="normal", winname=win )        
    setWidgetState( varname="import",       state="normal", winname=win )                         
    setWidgetState( varname="export",       state="normal", winname=win )
    setWidgetState( varname="batch",        state="normal", winname=win )
    setWidgetState( varname="dataSim",      state="normal", winname=win )                            
  }
      
  # Disable the Animate button on View.
  setWidgetState( varname="vwAnimate", state="disabled", win )
  
  # Disable the replicates entry on Sim.
  #setWidgetState( varname="simReps", state="disabled", win )
  setWidgetState( varname="nRep", state="disabled", win )
  
  # Disable the random number seed entry on Sim.
  setWidgetState( varname="simSeed", state="disabled", win )
      
  # Disable incomplete plots.
  #setWidgetState( varname="vwPlotType", state="disabled", radiovalue="vwEstSurvey", win )
  setWidgetState( varname="vwPlotType", state="disabled", radiovalue="vwObsSurvey", win )
  
  # Disable base radio button.
  setWidgetState( varname="pfBase", state="disabled", radiovalue="B0", win )  

  # Update the GUI.
  setWinVal( guiChanges, win )
  
  return( status )
}    # END .setGuiState function.


.loadDesignFileName <- function( desFile="mseRbatch.design" )
{
  val <- NULL
  tmpFile <- selectFile( initialfile=desFile,
                filetype=list( c(".design","Design files") ), mode="open" )
  if ( is.null(tmpFile) )
    return( val )      # No such file exists, bale.
  else
    desFile <- tmpFile 
  
  return( desFile ) 
}


.loadSimulationFromRdata <- function( simFile=.FSIM )
{
  val <- NULL
  tmpFile <- selectFile( initialfile=simFile,
                filetype=list( c(".Rdata","Rdata files") ), mode="open" )
  if ( is.null(tmpFile) )
    return( val )      # No such file exists, bale.
  else
    simFile <- tmpFile 
    
  load( simFile, .GlobalEnv )
  
  return( simFile ) 
}

# readSimPars   (reads guiOpMod GUI parameters from file written by saveSimPars)
# Purpose:      Reads an ASCII file with reference points parameters.
# Parameters:   GUI parameters for guiRef. Output GUI parameter list.
# Returns:      val, a dataframe if file found, else NULL.
# Side effects: Reads an ASCII control file written by saveRefPars and resets
#               the guiRef GUI parameters.
# Source:       A.R. Kronlund
readSimPars <- function( parFile=.FPAR )
{
  val <- NULL
  tmpFile <- selectFile( initialfile=parFile,
               filetype=list( c(".par","par files") ), mode="open" )                       
  if ( is.null(tmpFile) )
  {
    cat( "\nMSG (readSimPars) No valid file selected...\n\n" )
    return( val )      # No such file exists, bale.
  }
  else
    parFile <- tmpFile
    
  # Read file, return a data frame with headers "parameter" and "value".
  val <- .readParFile( parFile )
  cat( "\nMSG (readSimPars) Loaded mseR par file ",parFile,"\n\n" )
  
  return( val )
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

# .saveFeedbackResults (Saves the simulations results and updates list)
# Purpose:        Saves the simulations results stored in blob to a .RData file.
#                 Updates the simulation tracker list.
# Parameters:     obj is a "blob" object holding simulation results.
# Returns:        File name of the saved results.
# Source:         A.R. Kronlund
.saveFeedbackResults <- function()
{
  # Get a unique file name based on system date and time and save the results.
  stamp  <- .getStamp()
  
  fbFile <- paste( "sim",stamp,".Rdata",sep="" )
  cat( "\nMSG (.saveFeedbackResults) Writing feedback simulation results to file: ",fbFile,"\n" )
  
  save( blob, file=fbFile )

  return( stamp )
}

# .saveSimPars (saves the ref GUI parameters to a file).
# Purpose:       Saves an ASCII file with reference point parameters.
# Parameters:    Output file name for ASCII file containing ref GUI parameters.
# Returns:       NULL
# Side effects:  Writes an ASCII control file configured for read usng .readParFile.
# Source:        A.R. Kronlund
.saveSimPars <- function( iSim, parFile=.FPAR, overWrite=FALSE )
{
  if ( !overWrite )
  {
    val <- NULL
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    tmpFile <- selectFile( initialfile=parFile,
                 filetype=list( c(".par","par files") ), mode="save" )                 

    if ( is.null(tmpFile) )
      return( val )
    else
      parFile <- tmpFile
  }
  else
  {
    # Force an overwrite of parFile.
    cat( "\nMSG (.saveSimPars) Overwriting :",parFile,"\n" )
  }

  cat( file=parFile, paste( "# Operating model parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )

  simPars <- rbind( simTracker[[iSim]]$gui$omPars,
                    simTracker[[iSim]]$gui$dataPars,
                    simTracker[[iSim]]$gui$assessPars,
                    simTracker[[iSim]]$gui$hcrPars )
  for ( i in 1:nrow(simPars) )
  {
    cat( file=parFile, simPars[i,1]," ",
                       simPars[i,2],"\n",
                       sep="", append=TRUE )
  }
  return( invisible() )  
}

.saveSimulation <- function( simObj )
{
  for ( i in 1:length(simObj) )
  {
    simObj[[i]]$gui$iSim <- i
    simObj[[i]]$gui$nSim <- length(simObj)
  }
  assign( "simTracker", simObj, pos=1 )
  return( invisible() )
}

# .saveSimulationToExcel (Save data in Excel format)
# Purpose:    Saves data in obj to a Microsoft Excel .xls file.
# Parameters: fname - a character string file name.
#             statObj - the result of a call to .calcPerfStats.
# Returns:    NULL
# Source:     A.R. Kronlund, modified from saveExcel by T.K. Deering (PopSim.r)
.saveSimulationToExcel <- function( fname=.FXLS )
{
  .stripQuotesLR <- function( txt )
  {
    txt <- iconv( txt, to="ASCII//TRANSLIT" )
    txt <- gsub( "\"","",txt )
    txt
  }

  fileGone <- TRUE
  OStype <- .Platform$OS.type
  
  if ( OStype!="windows" )
    fileGone <- FALSE
  
  if ( OStype=="windows" )
  {
  	# No name provided, or "Cancel" selected.
	  if ( fname == "" || is.null(fname) )
      return(invisible())
    
  	# If fname already exists, then remove it, but permission may be denied
	  # if the file is currently open.
	  if ( file.exists(fname) )
      fileGone <- file.remove( fname )
    
    if ( fileGone )
     	conn <- RODBC::odbcConnectExcel( fname, readOnly=FALSE )
 	}
  else
  {
 	  #fileName <- .getFileName( fname )$fileName
 	  cat( "\nMSG (.saveSimulationToExcel) Windows not detected, saving to CSV files.\n" )
  }

    # PAGE: GUI settings - parameters (rows) by simulation (columns).
    #       No need to check for a simulation - all sims have these values.
    
    # Some of the parameters are vectors so must use unlist.  For example,
    # x <- c(1,2,3) means that we need rows in the Excel sheet labeled
    # x1, x2, x3 to give unique row names.

    tmp <- rbind( simTracker[[1]]$gui$omPars,simTracker[[1]]$gui$dataPars,
                  simTracker[[1]]$gui$assessPars,simTracker[[1]]$gui$hcrPars )
    tmpNames <- tmp[,"parameter"]
    
    tmpList      <- unlist( .createList( tmp ) )
    tmpListNames <- names( tmpList )
            
    #result <- matrix( NA, nrow=length(tmp),ncol=length(simTracker) )
    result <- matrix( "",nrow=nrow(tmp), ncol=length(simTracker) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- rbind( simTracker[[i]]$gui$omPars,simTracker[[i]]$gui$dataPars,
                    simTracker[[i]]$gui$assessPars,simTracker[[i]]$gui$hcrPars )    

      #result[,i] <- unlist( .createList( tmp ) )
      result[ ,i ] <- tmp[,"value"]
    }
    
    if ( OStype=="windows" )	  
  	  .excelTable( conn, result, "Sim_Parameters", names(simTracker), tmpNames )
 	  else
 	  {
 	    dimnames( result ) <- list( tmpNames, names(simTracker) )
 	    fName <- "simTrackerParameters.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Parameters saved to ",fName,"\n" )
    }
  
    # PAGE: Reference_Points - Reference points (rows) by simulation (columns).
    #       No need to check for a simulation - all sims have these values.
      
    refNames <- c( "F0","U0","yprF0","ssbprF0","landedF0","yieldF0",
                   "discardedF0","ssbF0","recruitsF0",
                   "F01","U01","yprF01","ssbprF01","landedF01","yieldF01",
                   "discardedF01","ssbF01","recruitsF01",
                   "Fmsy","Umsy","yprFmsy","ssbprFmsy","landedFmsy","yieldFmsy",
                   "discardedFmsy","ssbFmsy","recruitsFmsy",
                   "F40","U40","yprF40","ssbprF40","landedF40","yieldF40",
                   "discardedF40","ssbF40","recruitsF40",
                   "Fmax","Umax","yprFmax","ssbprFmax","landedFmax","yieldFmax",
                   "discardedFmax","ssbFmax","recruitsFmax",
                   "Fcra","Ucra","yprFcra","ssbprFcra","landedFcra","yieldFcra",
                   "discardedFcra","ssbFcra","recruitsFcra" )
    
    tmp      <- unlist( simTracker[[1]]$om[refNames] )
    tmpNames <- names( tmp )
                   
    result <- matrix( NA, nrow=length(tmp),ncol=length(simTracker) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simTracker[[i]]$om[refNames] )
      result[,i] <- tmp
    }
    
    # Save the reference points.
    if ( OStype=="windows" )
      .excelTable( conn, result, "Reference_Points", names(simTracker), tmpNames )
    else
 	  {
 	    dimnames( result ) <- list( tmpNames, names(simTracker) )
 	    fName <- "simTrackerRefPoints.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Reference points saved to ",fName,"\n" )
    }
    
    
    # PAGE: Ref_Points_By_Gear: Reference points by gear (rows) and simulation (columns).
    #       No need to check for a simulation - all sims have these values.
        
    refGearNames <- c( "yprLF0","yprDF0","yprLF01","yprDF01",
                       "yprLFmsy","yprDFmsy","yprLF40","yprDF40",
                       "yprLFmax","yprDFmax","yprLFcra","yprDFcra" )
    
    tmp      <- unlist( simTracker[[1]]$om[refGearNames] )
    tmpNames <- names(tmp)
        
    result <- matrix( NA, nrow=length(tmp),ncol=length(simTracker) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simTracker[[i]]$om[refGearNames] )
      result[,i] <- tmp
    }
    
	  # Save the simulation conditions.
    if ( OStype=="windows" )	  
	    .excelTable( conn, result, "Ref_Points_By_Gear", names(simTracker), tmpNames )
    else
 	  {
 	    dimnames( result ) <- list( tmpNames, names(simTracker) )
 	    fName <- "simTrackerRefPtsByGear.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Reference points saved to ",fName,"\n" )
    }                             
    
    # PAGE: Statistics: Aggregate performance statistics - "spreadsheet" format.
    #       Need to check for a valid simulation.  Use simTracker[[i]]$sim$stamp.
    #       This is done in .calcPerfStats.
    
    perfStats <- .calcPerfStats()
    
    if ( OStype=="windows" )
      .excelTable( conn, perfStats$summary1, "Statistics", 
        dimnames( perfStats$summary1 )[[2]],dimnames( perfStats$summary1 )[[1]] )
    else
 	  {
 	    result <- perfStats$summary1
# 	    dimnames( result ) <- list( tmpNames, names(simTracker) )
 	    fName <- "simTrackerStatistics.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Performance statistics saved to ",fName,"\n" )
    }

    # PAGE: Statistics_normalized - aggregate performance statistics by period
    #       in normalized format.  Need to check for a valid simulation.
    
    if ( OStype=="windows" )        
      .excelTable( conn, perfStats$summary2, "Statistics_normalized",
        dimnames( perfStats$summary2 )[[2]],dimnames( perfStats$summary2 )[[1]] )
    else
 	  {
 	    result <- perfStats$summary2
# 	    dimnames( result ) <- list( tmpNames, names(simTracker) )
 	    fName <- "simTrackerStatisticsNorm.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Normalized performance statistics saved to ",fName,"\n" )
    }
    
    # ARK (05-Dec-10) Added output for Table to support working paper based on perfStats.
    # Catch statistics are 10-year time horizons - set using the "Short" period option.
    # Obj. 1-3 are 2 generations ~ 15 yrs for sablefish - use "Long" period option.
    
    # Extract Average median catch.
    #varNames <- c( "Simulation","Scenario","Procedure","Period","t1","t2",
    #               "medAvgCatch","Q1AvgCatch","Q2AvgCatch","medAAV","pHealthy","t1AvgDep","t1AvgCatch" )
    
    # ARK (13-Dec-10) As per phone conversation with SPC use minCatch, maxCatch.
    varNames <- c( "Simulation","Scenario","Procedure","Period","t1","t2",
                   "medAvgCatch","medLowCatch","medHighCatch","medAAV","pHealthy","t1AvgDep","t1AvgCatch" )                   
                   
    wpTable <- perfStats$summary1[ ,varNames ]
    
    wpTable[ ,"Scenario" ] <- .stripQuotesLR( wpTable[ ,"Scenario" ] )
    wpTable$Obj1 <- ifelse( perfStats$summary1[,"pObj1"] >= 0.95, 999,
                            perfStats$summary1[,"pObj1"] )
    wpTable$Obj2 <- ifelse( perfStats$summary1[,"obsPdecline"] <= perfStats$summary1[,"pDecline"],
                            999, perfStats$summary1[,"pDecline"] )
    wpTable$Obj3 <- ifelse( perfStats$summary1[,"pObj3"] > 0.5, 999,
                            perfStats$summary1[,"pObj3"] )
    
    # Now take "Long" for Obj1-Obj3, pHealthy.
    tmp <- wpTable[ wpTable[,"Period"]=="Long",c("Obj1","Obj2","Obj3","pHealthy" ) ]
    
    # Now take "Short" for everything else.
    wpTable <- wpTable[ wpTable[,"Period"]=="Short", ]
    
    # Stuff the "Long" into the table.
    wpTable[ ,c("Obj1","Obj2","Obj3","pHealthy") ] <- tmp
    
    # Re-order columns.                        
    wpTable <- wpTable[ ,c( varNames[1:6], "Obj1","Obj2","Obj3",
                        varNames[ 7:length(varNames) ] ) ]    
    
    if ( OStype=="windows" )
      .excelTable( conn, wpTable, "WorkingPaperTable", 
        dimnames( wpTable )[[2]],dimnames( wpTable )[[1]] )
    else
 	  {
 	    result <- perfStats$summary1
 	    fName <- "simTrackerWPtable.csv"
      write.csv( result, file = fName, append = FALSE,
        quote = TRUE, row.names = TRUE )
 	  
 	    cat( "\nMSG (.saveSimulationToExcel) Working paper table saved to ",fName,"\n" )
    }    
    
    # PAGE(S): Deterministic_Lines: Reference point lines for each simulation.
    #       No need to check for a simulation - all sims have these values.    
    
    curveNames <- c( "ssbpr","yprL","yprD","ypr","F","ssb","recruits","yield",
                     "landed","discarded" )
                     
    nPoints <- length( simTracker[[1]]$om$ssbpr )
                        
    for ( i in 1:length(simTracker) )
    {
      result <- matrix( NA, nrow=nPoints,ncol=length(curveNames) )
      for ( j in 1:length(curveNames) )
        result[,j] <- simTracker[[i]]$om[[ curveNames[j] ]]
      
      if ( OStype=="windows" )  
        .excelTable( conn, result, paste( "Deterministic",i,sep="" ),
                     curveNames, as.character(1:nPoints) )
      else
   	  {
 	      dimnames( result ) <- list( NULL, curveNames )
   	    fName <- paste( "simTrackerDeterministic",i,".csv", sep="" )
        write.csv( result, file = fName, append = FALSE,
          quote = TRUE, row.names = TRUE )
 	  
   	    cat( "\nMSG (.saveSimulationToExcel) Deterministic curves saved to ",fName,"\n" )
      }                     
    }
  
    if ( OStype=="windows" )
      odbcClose(conn)
    else
 	  {
 	    cat( "\nMSG (.saveSimulationToExcel) Finished writing CSV files, somebody has a Mac...\n" )
    }    
  
  if ( OStype=="windows" && !fileGone )
  {
    cat( "\nMSG (.saveSimulationToExcel): Simulations results NOT saved to Excel, permission denied\n" )
    cat( "\nMSG (.saveSimulationToExcel): Close or rename file ",fname,"\n " )
  }
  
	return( fileGone )
}

.saveSimulationToRdata <- function( simFile=.FSIM, overWrite=FALSE )
{
  if ( !overWrite )
  {
    val <- NULL
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    tmpFile <- selectFile( initialfile=simFile,
                 filetype=list( c(".Rdata","Rdata files") ), mode="save" )                 
    if ( is.null(tmpFile) )
      return( val )     # User did not select a file, bale.
    else
      simFile <- tmpFile
  }
  else
  {
    # Force an overwrite of simFile.
    cat( "\nMSG (.saveSimulationToRdata) Overwriting :",simFile,"\n" )
  }
  
  save( .guiPars, simTracker, file=simFile )
 
  return( invisible() )  
}

.setGuiSync <- function( simNode, winName )
{
  # Update the synchronization status.
  dtStamps <- list(  fit=simNode$fit$stamp,
                     om=simNode$om$stamp,
                     sim=simNode$sim$stamp,
                     perf=simNode$perf$stamp )
        
  # A list of TRUE/FALSE indicating synchronization with OM.
  status <- .getStatus( dtStamps )
        
  # Update the GUI synchronization status.
  if ( status$fit==TRUE )
    setWidgetColor( "indFit", winName=winName, entrybg="green" )
  else
    setWidgetColor( "indFit", winName=winName, entrybg="red" )
        
  if ( status$om==TRUE )
    setWidgetColor( "indOM", winName=winName, entrybg="green" )
  else
    setWidgetColor( "indOM", winName=winName, entrybg="red" )

  if ( status$om==TRUE )
    setWidgetColor( "indMP", winName=winName, entrybg="green" )
  else
    setWidgetColor( "indMP", winName=winName, entrybg="red" )
       
  if ( status$sim==TRUE )
    setWidgetColor( "indSim", winName=winName, entrybg="green" )
  else
    setWidgetColor( "indSim", winName=winName, entrybg="red" )
      
#  if ( status$perf==TRUE )
#    setWidgetColor( "indPrf", winName=winName, entrybg="green" )
#  else
#    setWidgetColor( "indPrf", winName=winName, entrybg="red" )
    
  status
}

.snipRefCurves <- function( refPointObj )
{
  result <- list()
  refCurveNames <- c( "F","ssbpr","ssb","legalb","sublegalb",
  "recruits","ypr","yprL","yprD","yprLeg","yprSubLeg","yield","landed",
  "discarded","legal","sublegal","legalHR","sublegalHR","fg" )
     
  idx <- is.element( names(refPointObj), refCurveNames )
  result$refCurves <- refPointObj[  idx ]
  result$refPoints <- refPointObj[ !idx ]
  result
}

.stripQuotes <- function( txt )
{
  txt <- iconv( txt, to="ASCII//TRANSLIT" )
  txt <- gsub( "\"","",txt )
  txt
}

.updateGuiRefPoints <- function( guiObj, refPointObj, pass=FALSE, digits=4 )
{
  # Round the reference points for display in a GUI.
  # This takes a guiObj as input, and rounds the listed elements to digits.
  # If "pass" is TRUE, then the entire guiObj is returned,
  # If "pass" is FALSE then only the listed elements are returned.
  
  if ( pass )
    result <- guiObj
  else
    result <- list()

  #result$ssbFmsy   <- round( refPointObj$ssbFmsy,   digits=digits )
  #result$yieldFmsy <- round( refPointObj$yieldFmsy, digits=digits )
  #result$F0        <- round( refPointObj$F0,        digits=digits )
  #result$F01       <- round( refPointObj$F01,       digits=digits )
  #result$F40       <- round( refPointObj$F40,       digits=digits )
  #result$Fmsy      <- round( refPointObj$Fmsy,      digits=digits )
  #result$Fcra      <- round( refPointObj$Fcra,      digits=digits )
  #result$Fmax      <- round( refPointObj$Fmax,      digits=digits )
  
  # Changed to U-based 08-Nov-10.
  result$ssbFmsy   <- round( refPointObj$ssbFmsy,   digits=digits )
  result$yieldFmsy <- round( refPointObj$yieldFmsy, digits=digits )
  result$F0        <- round( refPointObj$U0,        digits=digits )
  result$F01       <- round( refPointObj$U01,       digits=digits )
  result$F40       <- round( refPointObj$U40,       digits=digits )
  result$Fmsy      <- round( refPointObj$Umsy,      digits=digits )
  result$Fcra      <- round( refPointObj$Ucra,      digits=digits )
  result$Fmax      <- round( refPointObj$Umax,      digits=digits )  
  
  result
}

#------------------------------------------------------------------------------#
#-- Plotting Functions                                                       --#
#------------------------------------------------------------------------------#

.doGuiOmPlots <- function( obj )
{
  obj <- obj$om

  win     <- .getWinName()
  act     <- getWinAct()[1]
  guiInfo <- getWinVal(scope="L", winName=win ) # GUI information local scope

  if ( !omAuto )
  {
    if ( omPlotByRow )
      par( mfrow=c(omNrows,omNcols) )
    else
      par( mfcol=c(omNrows,omNcols) )
  }
    
  #----------------------------------------------------------------------------#
  # Life history plots.                                                        #
  #----------------------------------------------------------------------------#
  
  if ( omPlotType=="omLenAtAge" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  
    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minLen,maxLen )
    .plotLenAtAge( obj, gfx=list( annotate=omAnnotate, xLim=xLim, yLim=yLim ) )
  }
  
  if ( omPlotType=="omMatAtAge" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( 0.0,1.0 )
    .plotMatAtAge( obj, gfx=list( annotate=omAnnotate, xLim=xLim, yLim=yLim ) )
  }
  
  if ( omPlotType=="omNumAtAge" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    xLim <- NULL
    yLim <- NULL
    .plotNumAtAge( obj, gfx=list( annotate=omAnnotate, doLegend=omLegend,
                   xLim=xLim, yLim=yLim ) )
  }
  
  if ( omPlotType=="omWgtAtAge" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtAtAge( obj, gfx=list( xLim=xLim, yLim=yLim ) )
  }
  
  if ( omPlotType=="omWgtLen" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minLen,maxLen )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtLen( obj, gfx=list( xLim=xLim, yLim=yLim ) )
  }
  
  # Combined life history plot.
  if ( omPlotType=="omLifeHist" )
  {
    if ( omAuto )
    {
      myOma <- .OMA
      myOma[3] <- 3
      par( oma=myOma, mar=c(3,4,1,1), mfrow=c(3,2) )   
    }

    .plotNumAtAge( obj, gfx=list( annotate=omAnnotate, doLegend=omLegend ) )

    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( 0.0, 1.0 )
    .plotMatAtAge( obj, gfx=list( annotate=omAnnotate, xLim=xLim, yLim=yLim ) )
  
    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minLen,maxLen )
    .plotLenAtAge( obj, gfx=list( annotate=omAnnotate, xLim=xLim, yLim=yLim ) )

    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minAge,maxAge )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtAtAge( obj, gfx=list( xLim=xLim, yLim=yLim ) )

    xLim <- NULL
    if ( omSetXaxis )
      xLim <- c( minLen,maxLen )
    yLim <- NULL
    if ( omSetYaxis )
      yLim <- c( minWgt,maxWgt )
    .plotWgtLen( obj, gfx=list( xLim=xLim, yLim=yLim ) )
               
    if ( omAnnotate )
      mtext( side=3, line=0, cex=.REFCEX, outer=TRUE, "Life History" )                     
      
    par( mfrow=c(1,1) )
  }
  
  #----------------------------------------------------------------------------#
  # Equilibrium reference point plots.                                         #
  #----------------------------------------------------------------------------#
  
  # Arguments are passed instead of extracting from the GUI within the plot
  # function so that functions can be used without the GUI, or altered at the
  # time of the function call.
  
  checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
  names( checked ) <- c( "Fmsy",  "F0",  "F01", "F40",   "Fmax",  "Fcra"  )
                  
  # Do the F-based plots.
  
  if ( omPlotType=="omRecSSB" | omPlotType=="omRecSSB2" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotRecSSB( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minRec,maxRec) ) )
  }
  
  if ( omPlotType=="omSsbF" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    .plotSsbF( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )
  }
  
  if ( omPlotType=="omSsbRF" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotSsbPerRecF( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  }
  
  if ( omPlotType=="omYieldF" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotYieldF( obj,gfx=list( annotate=omAnnotate,checked=checked,doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  }
  
  if ( omPlotType=="omYieldSSB" | omPlotType=="omYieldSSB2" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotYieldSSB( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minYield,maxYield) ) )
  }
  
  if ( omPlotType=="omYprF" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotYprF( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minYPR,maxYPR) ) )
  }
  
  # Combined reference point plot.
  if ( omPlotType=="omRefPoints" )
  {
    if ( omAuto )
    {
      myOma <- .OMA
      myOma[3] <- 2
      myMar <- c(3,4,1,1)
      par( oma=myOma, mar=myMar, mfrow=c(3,2) )
    }
  
    .plotRefPoints( obj, gfx=list( annotate=omAnnotate, checked=checked, doLegend=omLegend,
                      setXaxis=omSetXaxis, setYaxis=omSetYaxis ) )
  }
  
  # Do the U-based plots.
  
  #checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
  names( checked ) <- c( "Umsy",  "U0",  "U01", "U40",   "Umax",  "Ucra" )
  
  if ( omPlotType=="omSsbU" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    .plotSsbU( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )
  }
  
  if ( omPlotType=="omSsbRU" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotSsbPerRecU( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  }
  
  if ( omPlotType=="omYieldU" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotYieldU( obj,gfx=list( annotate=omAnnotate,checked=checked,doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  }
  
  if ( omPlotType=="omYprU" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )     
  
    .plotYprU( obj,gfx=list( annotate=omAnnotate,checked=checked, doLegend=omLegend,
                 setXaxis=omSetXaxis, setYaxis=omSetYaxis,
                 xLim=c(minF,maxF),yLim=c(minYPR,maxYPR) ) )
  }
  
  # Combined reference point plot.
  if ( omPlotType=="omRefPointsU" )
  {
    if ( omAuto )
    {
      myOma <- .OMA
      myOma[3] <- 2
      myMar <- c(3,4,1,1)
      par( oma=myOma, mar=myMar, mfrow=c(3,2) )
    }
  
    .plotRefPointsU( obj, gfx=list( annotate=omAnnotate, checked=checked, doLegend=omLegend,
                      setXaxis=omSetXaxis, setYaxis=omSetYaxis ) )
  }
  
  #----------------------------------------------------------------------------#
  # Selectivity and discard proportions by gear.                               #
  #----------------------------------------------------------------------------#
  
  # Selectivity by length and gear.
  if ( omPlotType=="omSlg" )
  {
    if ( omAuto )
    {
      if ( omGears )
      {
        mfRow <- .getGearRowCol( obj$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }    
    
     if ( omSetXaxis )
      xLim <- c(minLen,maxLen)
    else
      xLim <- NULL
      
    .plotSlg( obj, gfx=list(autolayout=omAuto, annotate=omAnnotate,
                     bygear=omGears, doLegend=omLegend, xLim=xLim, yLim=NULL ) )
  }
                           
  # Selectivity by age, length and gear.
  if ( omPlotType=="omSalg" )
  {
    if ( omAuto )
    {
      mfRow <- .getGearRowCol( obj$nGear )
      par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    }
  
    if ( omSetXaxis )
      xLim <- c(minAge,maxAge)
    else
      xLim <- NULL
      
    .plotSalg( obj, gfx=list(autolayout=omAuto, annotate=omAnnotate,
                       doLegend=omLegend, xLim=xLim, yLim=NULL ) )
  }

  # Proportion discarded at length and gear.
  if ( omPlotType=="omPlg" )
  {
 
    if ( omSetXaxis )
      xLim <- c(minLen,maxLen)
    else
      xLim <- NULL
  
    .plotPlg( obj, gfx=list(autolayout=omAuto, annotate=omAnnotate,
                     bygear=omGears, doLegend=omLegend, xLim=xLim, yLim=NULL ) )
  }

  # Proportion discarded at age, length and gear.
  if ( omPlotType=="omPalg" )
  {
    if ( omAuto )
    {
      mfRow <- .getGearRowCol( obj$nGear )
      par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    }
  
    if ( omSetXaxis )
      xLim <- c(minAge,maxAge)
    else
      xLim <- NULL
      
    .plotPalg( obj, gfx=list(autolayout=omAuto, annotate=omAnnotate,
                       doLegend=omLegend, xLim=xLim, yLim=NULL ) )
  }
  
  # Mortality rates of discards by gear.
  if ( omPlotType=="omdg" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  
    .plotDisMortRate( obj, annotate=omAnnotate )
  }

  # Gear impacts.
  if ( omPlotType=="omYieldGear" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  
    .plotYieldByGear( obj, gfx=list( annotate=omAnnotate, checked=checked ) )
  }

  # Compare scenarios.
  if ( omPlotType=="omFvsh" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
    .plotFvsh( simTracker )
  }
  
  if ( omPlotType=="omYieldVsF" )
  {
    if ( omAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
        
    .plotYieldVsF( simTracker, refGear=3, gfx=list( annotate=omAnnotate, checked=checked) )
  }
  
  if ( omAnnotate )
  {
    #txt <- paste( simName,": ",scenarioName, " (",simTracker[[iSim]]$om$stamp,")",
    #         sep="" )
    mtext( side=1, adj=0, line=0.5, cex=0.7, font=3, outer=TRUE,
           paste( simName," (",simTracker[[iSim]]$om$stamp,")", sep="" ) )
    mtext( side=1, adj=1, line=0.5, cex=0.7, font=3, outer=TRUE, scenarioName )
  }

  # Save plot(s) as an enhanced Windows metafile.
  if ( act=="omSaveEMF" )
    savePlot( filename=paste( simName,omPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
 # Save plot(s) as a PDF file.
  if ( act=="omSavePDF" )
    savePlot( filename=paste( simName,omPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                
    
  return( invisible() )
}     # .doGuiOmPlots function

# .doGuiMpPlots (Wrapper to get "MP" page parameters and call the required plot)
# Purpose:      Calls the plot specified by mpPlotType that is returned from
#               "MP" GUI page.
# Parameters:   obj - a simulation node, e.g. simTracker[[i]].
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.doGuiMpPlots <- function( obj  )
{
  win     <- .getWinName()
  guiInfo <- getWinVal(scope="L", winName=win )  # GUI information local scope.
  act     <- getWinAct()[1]                      # Last menu window action.
  
  simPars     <- rbind( omPars, dataPars, assessPars, hcrPars )
  simParsList <- .createList( simPars )
  
  if ( is.null(act) )
    act <- ""

#  if ( !mpAuto )
#  {
#    if ( mpPlotByRow )
#      par( mfrow=c(mpNrows,mpNcols) )
#   else
#      par( mfcol=c(mpNrows,mpNcols) )
#  }
  
  if ( mpPlotType=="mpDataPlot" )
  {
#    if ( mpAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
      
    if ( mpSetXaxis )
      xLim <- c( mpMinYr,mpMaxYr )
    else
      xLim <- c( 1,simParsList$pars$nT )
    
    #yLim <- c( 1, length(simParsList$mp$data$t1Index ) )
    
    .plotMpData( simParsList, gfx=list( annotate=mpAnnotate, doLegend=mpLegend,
                 xLim=xLim, yLim=NULL ) )
  }
  
  if ( mpPlotType=="mpHcrPlot" )
  {
#    if ( mpAuto )
      par( oma=c(3,3,3,1), mar=c(2,2,2,1), mfrow=c(1,1) )

    if ( mpSetXaxis )
      xLim <- c(minSSB,maxSSB)
    else
      xLim <- NULL  
      
    if ( mpSetYaxis )
      yLim <- c(minF,maxF)
    else
      yLim <- NULL
  
    .plotMpHCR( obj, gfx=list( annotate=mpAnnotate, doLegend=mpLegend,
                xLim=xLim, yLim=yLim ) )
  }
  
  if ( mpPlotType=="mpPriors" )
  {
    #if ( mpAuto )
    par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
    
    xLim <- NULL
    yLim <- NULL
    
    .plotPriors( simParsList, gfx=list( annotate=mpAnnotate, doLegend=mpLegend,
                 xLim=xLim, yLim=yLim ) )
  }
  
  if ( mpAnnotate )
  {
    #txt <- paste( simName,": ",mpName, " (",simTracker[[iSim]]$mp$stamp,")",
    #         sep="" )
    mtext( side=1, adj=0, line=0.5, cex=0.7, font=3, outer=TRUE,
           paste( simName," (",simTracker[[iSim]]$om$stamp,")", sep="" ) )
    mtext( side=1, adj=1, line=0.5, cex=0.7, font=3, outer=TRUE, mpName )
  }  

  # Save plot(s) as an enhanced Windows metafile.
  if ( act=="mpSaveEMF" )
    savePlot( filename=paste( simName,mpModPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
  # Save plot(s) as a PDF file.
  if ( act=="mpSavePDF" )
    savePlot( filename=paste( simName,mpModPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                
    
}     # .doGuiMpPlots function.


# .doGuiViewPlots  (Wrapper to get guiView parameters and call the required plot)
# Purpose:      Calls the plot specified by vwPlotType that is returned from
#               view GUI (guiView).
# Parameters:   obj - simTracker[[i]]
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.doGuiViewPlots <- function( obj )
{
  win     <- .getWinName()
  guiInfo <- getWinVal(scope="L", winName=win )  # GUI information local scope.
  act     <- getWinAct()[1]                      # Last menu window action.
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

  if ( !vwAuto )
  {
    if ( vwPlotByRow )
      par( mfrow=c(vwNrows,vwNcols) )
    else
      par( mfcol=c(vwNrows,vwNcols) )
  }

  #----------------------------------------------------------------------------#
  # Selectivity by gear, discard rate by gear, discard mortality rate by gear. #
  #----------------------------------------------------------------------------#

  # Selectivity by length and gear.
  if ( vwPlotType=="vwSlg" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }    

    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinLen, vwMaxLen )

    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinSel,vwMaxSel )
    .plotSlg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
              bygear=vwGears, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Selectivity by age, length and gear.
  if ( vwPlotType=="vwSalg" )
  {
    if ( vwAuto )
    {
      mfRow <- .getGearRowCol( blob$pars$nGear )
      par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinSel,vwMaxSel )
    .plotSalg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
               doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Proportion discarded at length and age.
  if ( vwPlotType=="vwPlg" )
  {
    # No omAuto setting because plot panels are determined by .plotSlg.

    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c(vwMinLen,vwMaxLen)
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinDis,vwMaxDis )

    .plotPlg( blob$pars, gfx=list( annotate=vwAnnotate, autolayout=vwAuto,
              bygear=vwGears, doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  # Proportion discarded at age, length and gear.
  if ( vwPlotType=="vwPalg" )
  {
    if ( vwAuto )
    {
      mfRow <- .getGearRowCol( blob$pars$nGear )
      par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    }
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinDis,vwMaxDis )
    .plotPalg( blob$pars, gfx=list( autolayout=vwAuto, annotate=vwAnnotate,
               doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }
  
  #----------------------------------------------------------------------------#
  # Age Structure Plots                                                        #
  #----------------------------------------------------------------------------#
  
  if ( vwPlotType=="vwCatgProp" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinAge,vwMaxAge )
      
    # Pass the nGear catch-at-age proportions for the replicate.
    .plotCatAgeBubbles( blob$om$uCatg[iRep,,,], gearNames=blob$pars$gNames,
      gfx=list( annotate=vwAnnotate, bygears=vwGears, doLegend=vwLegend,
      xLim=xLim, yLim=yLim ) )
  }
  
  if ( vwPlotType=="vwCatgFreq" )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinAge,vwMaxAge )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinYr,vwMaxYr )
    
    .plotCatAgeFreq( blob$om$uCatg[iRep,,,], gearNames=blob$pars$gNames,
      gfx=list( annotate=vwAnnotate, bygears=vwGears, doLegend=vwLegend,
      showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  #----------------------------------------------------------------------------#
  # Combination Plots                                                          #
  #----------------------------------------------------------------------------#

  if ( (vwPlotType == "vwBioCatF") && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim1 <- NULL
    yLim2 <- NULL
    yLim3 <- NULL      
    if ( vwSetYaxis )
    {
      yLim1 <- c( vwMinSSB,vwMaxSSB )
      yLim2 <- c( vwMinCat,vwMaxCat )
      yLim3 <- c( vwMinF,  vwMaxF   )
    }
    .plotBioCatF( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim1=yLim1, yLim2=yLim2,
      yLim3=yLim3 ) )
  }
  else if ( (vwPlotType == "vwBioCatF") && (animate==TRUE) )
  {
    .doAnimate( blob, vwPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName=win)
    .plotBioCatF( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
    animate <- FALSE
  }

  if ( (vwPlotType == "vwBioCatR") && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim1 <- NULL
    yLim2 <- NULL
    yLim3 <- NULL      
    if ( vwSetYaxis )
    {
      yLim1 <- c( vwMinSSB,vwMaxSSB )
      yLim2 <- c( vwMinCat,vwMaxCat )
      yLim3 <- c( vwMinRec,vwMaxRec )
    }
    .plotBioCatR( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim1=yLim1, yLim2=yLim2,
      yLim3=yLim3 ) )
  }
  else if ( (vwPlotType == "vwBioCatR") && (animate==TRUE) )
  {
    .doAnimate( blob, vwPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRviewGui")
    plotBioCatR( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
    animate <- FALSE
  }

  if ( (vwPlotType == "vwBioHrR") && (animate==FALSE) )
  {
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
      
    yLim1 <- NULL
    yLim2 <- NULL
    yLim3 <- NULL      
    if ( vwSetYaxis )
    {
      yLim1 <- c( vwMinSSB,vwMaxSSB )
      yLim2 <- c( vwMinHR,vwMaxHR )
      yLim3 <- c( vwMinRec,vwMaxRec )
    }
    .plotBioHrRec( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim1=yLim1, yLim2=yLim2,
      yLim3=yLim3 ) )
  }
  else if ( (vwPlotType == "vwBioHrR") && (animate==TRUE) )
  {
    .doAnimate( blob, vwPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRviewGui")
    plotBioHrRec( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
    animate <- FALSE
  }

  #----------------------------------------------------------------------------#
  # Time-series plots                                                          #
  #----------------------------------------------------------------------------#

  if ( vwPlotType == "vwBiomass" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr, vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinSSB, vwMaxSSB )
      
    .plotBiomass( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                  doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwSSBIndex" )
  {
    if ( vwAuto )
    {
      mfRow <- .getGearRowCol( blob$pars$nGear )
      par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    }  
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinIdx,vwMaxIdx )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinSSB,vwMaxSSB )
    .plotSSBIndex( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwCatch" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
      
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinCat,vwMaxCat )

    .plotCatch( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwCtg" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }
    
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinCat,vwMaxCat )
    .plotCtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwDtg" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }  
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinDis,vwMaxDis )
    .plotDtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
       doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwCatDis" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        myOma <- .OMA
        myOma[4] <- 3
        par( oma=myOma, mar=.MAR, mfrow=c(blob$pars$nGear,2) )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }    
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinCat, vwMaxCat )
    .plotCtgDtg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      bygears=vwGears, doLegend=vwLegend, showProj=vwProj,xLim=xLim,yLim=yLim ))
  }

  if ( vwPlotType == "vwFmort" )
  {
    # No vwAuto setting because plot panels are determined by .plotF.  
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF,vwMaxF )
    .plotF( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( ( vwPlotType == "vwOmHCR" ) && (animate==FALSE) )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[3] <- 3
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
      
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinSSB,vwMaxSSB )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF,vwMaxF )
      
    .plotOmHCR( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, xLim=xLim, yLim=yLim, colorZones=TRUE ) )
  }
  else if ( (vwPlotType == "vwOmHCR") && (animate==TRUE) )
  {
    if ( vwAuto )
      par( oma=c(3,3,3,1), mar=c(2,2,2,1), mfrow=c(1,1) )
      
    .doAnimate( blob, vwPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRviewGui")
    .plotOmHCR( blob, iSim, iRep, vwAnnotate, vwSet, vwSetYaxis )
    animate <- FALSE
  }
  
  if ( ( vwPlotType == "vwMpHCR" ) && (animate==FALSE) )
  {
    if ( vwAuto )
    {
      myMar <- .MAR
      myMar[3] <- 3
      par( oma=.OMA, mar=myMar, mfrow=c(1,1) )
    }
      
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinSSB,vwMaxSSB )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinF,vwMaxF )
      
    .plotPmodHCR( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      doLegend=vwLegend, xLim=xLim, yLim=yLim, colorZones=TRUE ) )
  }
  else if ( (vwPlotType == "vwMpHCR") && (animate==TRUE) )
  {
    if ( vwAuto )
      par( oma=c(3,3,3,1), mar=c(2,2,2,1), mfrow=c(1,1) )
      
    .doAnimate( blob, vwPlotType, iRep )
    guiInfo <- getWinVal(scope="L", winName="mseRviewGui")
    .plotMgmtProcHCR( blob, iSim, iRep, vwAnnotate, vwSet, vwSetYaxis )
    animate <- FALSE
  }  
  
  if ( vwPlotType == "vwItg" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }    
  
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinIdx,vwMaxIdx )
    .plotItg( blob, iSim, iRep, gfx=list( annotate=vwAnnotate, bygears=vwGears,
      doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwNumbers" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
        
    xLim <- NULL
    yLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    if ( vwSetYaxis )
      yLim <- c( vwMinNum,vwMaxNum )
    .plotNumbers( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                  doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwObsSurvey" )
  {
    .plotObsSurvey( blob, iSim, iRep, vwAnnotate, vwProj, vwSetYaxis )
  }

  if ( vwPlotType == "vwRecruits" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
        
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinRec,vwMaxRec )
    .plotRecruits( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                   doLegend=vwProj, showProj=vwProj, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType =="vwRecSpawn" )
  {
    if ( vwAuto )
      par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
        
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinSSB,vwMaxSSB )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinRec,vwMaxRec )
    .plotRecSpawners( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
                      doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
  }

  if ( vwPlotType == "vwRefPoints" )
  {
    if ( vwAuto )
    {
      myOma <- .OMA
      myOma[3] <- 2
      myMar <- c(3,4,1,1)
      par( oma=myOma, mar=myMar, mfrow=c(3,2) )
    }
  
    checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
    names( checked ) <- c( "Umsy",  "U0",  "U01", "U40",   "Umax",  "Ucra"  )
  
    .plotRefPointsU( obj$om, gfx=list( annotate=omAnnotate, checked=checked, doLegend=omLegend,
                      setXaxis=omSetXaxis, setYaxis=omSetYaxis ) )
  }

  #-- Assessment model plots.

  if ( vwPlotType == "vwSSBFit" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }
    
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinSSB,vwMaxSSB )
        
    .plotBtFit( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      bygears=vwGears, doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
      useYears=vwUseYears ) )
  }
  
  if ( vwPlotType == "vwHR" )
  {
    if ( vwAuto )
    {
      if ( vwGears )
      {
        mfRow <- .getGearRowCol( blob$pars$nGear )
        myMar <- .MAR
        myMar[2] <- 3
        par( oma=.OMA, mar=myMar, mfrow=mfRow )
      }
      else
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    }
      
    xLim <- NULL
    if ( vwSetXaxis )
      xLim <- c( vwMinYr,vwMaxYr )
    yLim <- NULL
    if ( vwSetYaxis )
      yLim <- c( vwMinHR,vwMaxHR )
        
    .plotHarvRateSeries( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
      bygears=vwGears, doLegend=vwLegend, showProj=vwProj, xLim=xLim, yLim=yLim,
      useYears=vwUseYears ) )
  }  

  if ( obj$om$assessMethod=="pMod" )
  {
    if ( vwPlotType == "vwParCor" )
    {
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinSSB, vwMaxSSB )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinF,vwMaxF )
        
      .plotPmodEstCor( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
        doLegend=vwProj, showProj=vwProj,
        xLim=xLim, yLim=yLim ) )
    }  
  
    if ( vwPlotType == "vwParEst" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
        
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinYr,vwMaxYr )
      yLimB <- NULL
      if ( vwSetYaxis )
        yLimB <- c( vwMinSSB,vwMaxSSB )
      yLimF <- NULL
      if ( vwSetYaxis )
        yLimF <- c( vwMinF,vwMaxF )
      yLimC <- NULL
      if ( vwSetYaxis )
        yLimC <- c( vwMinCat,vwMaxCat )
     
      .plotPmodEstPars( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
        doLegend=vwLegend, showProj=vwProj,
        xLim=xLim, yLimB=yLimB, yLimF=yLimF, yLimC=yLimC ) )
    }
    
    if ( vwPlotType == "vwRefSeries" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
    
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinYr,vwMaxYr )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinSSB,vwMaxSSB )
      yLim2 <- NULL
      if ( vwSetYaxis )
        yLim2 <- c( vwMinF, vwMaxF )
        
      .plotHCRrefPtSeries( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
         colorZones=FALSE, doLegend=vwLegend, showProj=vwProj,
         xLim=xLim, yLim=yLim, yLim2=yLim2, useYears=vwUseYears ) )
    }    

  #----------------------------------------------------------------------------#
  # Model Convergence and ADMB plots                                           #
  #----------------------------------------------------------------------------#
  
    if ( vwPlotType == "vwDiags" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(4,1) )
    
      xLim <- NULL
      yLim <- NULL
        
      .plotDiagnostics( blob, BmsyCrit=.BMSYCRIT, FmsyCrit=.FMSYCRIT,
         iSim=iSim, iRep=iRep,
         gfx=list( annotate=vwAnnotate,doLegend=vwLegend, xLim=xLim, yLim=yLim ) )
    }    
  }     # end if assessMethod=="Pmod".
  
  if ( obj$om$assessMethod=="perfect" )
  {
    if ( vwPlotType == "vwRefSeries" )
    {
      if ( vwAuto )
        par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
    
      xLim <- NULL
      if ( vwSetXaxis )
        xLim <- c( vwMinYr,vwMaxYr )
      yLim <- NULL
      if ( vwSetYaxis )
        yLim <- c( vwMinSSB,vwMaxSSB )
      yLim2 <- NULL
      if ( vwSetYaxis )
        yLim2 <- c( vwMinF, vwMaxF )
        
      .plotHCRrefPtSeries( blob, iSim, iRep, gfx=list( annotate=vwAnnotate,
         colorZones=TRUE, doLegend=vwLegend, showProj=vwProj,
         xLim=xLim, yLim=yLim, yLim2=yLim2 ) )
    }      

    doPlotStatus <- FALSE
    if ( vwPlotType == "vwParCor" )
      doPlotStatus <- TRUE
    if ( vwPlotType == "vwParEst" )
      doPlotStatus <- TRUE
    if ( vwPlotType=="vwDiags" )
      doPlotStatus <- TRUE
    if ( vwPlotType=="vwParCor" )
      doPlotStatus <- TRUE

    if ( doPlotStatus )
      .plotStatus( "No Estimated Parameters to Plot" )
  }

  # Add a legend for the simulation for specified plots.
  if ( vwAnnotate && !animate )
  {
    if ( vwAnnotate )
    {
      #txt <- paste( simName,": ",scenarioName,"-",mpName,
      #        " (",simTracker[[iSim]]$om$stamp,")", sep="" )
      mtext( side=1, adj=0, line=0.5, cex=0.7, font=3, outer=TRUE,
             paste( simName," (",simTracker[[iSim]]$om$stamp,")",
                    " Rep ",iRep, sep="" ) )
      mtext( side=1, adj=1, line=0.5, cex=0.7, font=3, outer=TRUE,
             paste( scenarioName,"-",mpName, sep="" ) )
    }  
  
#    if ( (vwPlotType=="vwBioCatF") || (vwPlotType=="vwBioCatR") )
#      simLegend( iSim, iRep, nReps,
#                 simID=guiInfo$viewHeader$simID[iSim],
#                 scenario=guiInfo$viewHeader$scenario[iSim],
#                 procedure=guiInfo$viewHeader$procedure[iSim],
#                 side=3, line=0 )
#    else if ( vwPlotType=="vwRefPoints" )
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
  if ( act=="vwSaveEMF" )
    savePlot( filename=paste( simName,vwPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
  # Save plot(s) as a PDF file.
  if ( act=="vwSavePDF" )
    savePlot( filename=paste( simName,vwPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                

  return( invisible() )
}     # .doGuiViewPlots function

.calcFailedReps <- function( obj, assessMethod="pMod",
    checkGrad=TRUE, checkExit=TRUE, checkFun=TRUE )
{
  # Input is the estPars data.frame from blob$mp$assess.
  # Following steps find time steps in which the assessment fails.
  
  failedReps <- NULL
  
  if ( assessMethod=="pMod" )
  {
    # Check whether any gradients exceed .MAXGRADCRIT.
    goodGrad <- obj$maxGrad <= .MAXGRADCRIT - 1.0E-8
  
    # Check whether any function calls exceed .MAXFUNCALLS
    goodFun  <- obj$nEval <= .MAXFUNCALLS
  
    # Check whether any iExit codes != 1.
    goodExit <- obj$iExit == 1
 
    # Check for sensible Bmsy value.
    goodBmsy <- obj$Bmsy > 10.0
    
    # Create a logical vector that combines failed status.
    goodRep <- as.logical(goodGrad * goodFun * goodExit * goodBmsy)
  }    # assessMethod=="pMod"
    
  # Return a list of good replicates.
  failedReps <- unique( obj$iRep[ !goodRep ] )
  
  failedReps
}     # .calcFailedReps

# .doGuiPerfPlots (Wrapper to get "Perf" page parameters and call the required plot)
# Purpose:      Calls the plot specified by pfPlotType that is returned from
#               "Perf" GUI page.
# Parameters:   obj - simTracker list.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.doGuiPerfPlots <- function( obj, iSim=1  )
{
  win     <- .getWinName()
  guiInfo <- getWinVal(scope="L", winName=win )  # GUI information local scope.
  act     <- getWinAct()[1]                      # Last menu window action.
  
  if ( is.null(act) )
    act <- ""

  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )
    
  nSim   <- sum( pfSimList$Select )          # Number of selected simulations.
  nPer   <- nrow( pfPeriodList )             # Number of summary periods.
  nStat  <- 4                                # Depletion, catch, discard, AAV Cat.
  if ( nSim < 1 ) return( invisible() )      # Nothing to plot, bale out.
  
  select <- pfSimList$Select
  idNum  <- c(1:length(select))[select]
  
  simList <- pfSimList[ pfSimList$Select, ]  # data.frame of selected simulations.
  
  quantVals <- c( 0.05, pfLoQuant, 0.5, pfUpQuant, 0.95 )
  
  # Coerce factors back to factors.
  simList$Simulation <- as.character( simList$Simulation )
  simList$Scenario   <- as.character( simList$Scenario )
  simList$Procedure  <- as.character( simList$Procedure )
  simList$Stamp      <- as.character( simList$Stamp )
  
  # Are there any simulations with Stamp==NO_SIM selected?
  # If so, can only do an equilibrium analysis and not performance statistics.
  isNoSim <- any( simList$Stamp=="NO_SIM" )
  
  # Determine the type of performance plot requested.
  doTulip <- FALSE
  doBar   <- FALSE
  doQbox  <- FALSE
  doOther <- FALSE
  doEquil <- FALSE
  
  if ( substring( pfPlotType,3,7 )=="Tulip" )
  {
    doTulip <- TRUE
    
    quantVals <- c( 0.05,pfLoQuant,0.5,pfUpQuant,0.95 )
    
    # Get pfTraces random replicates and save indices to working directory.  
    # Note that if the simulations contain unequal numbers of replicates then
    # the trace indices can be out of range.  This is caught by plot functions.
    if ( pfTraces > 0 )
      iTraces <- sample( blob$pars$nReps,min(pfTraces,blob$pars$nReps) )
    else
      iTraces <- 0
      
    # *** Why is this happening?  Maybe just to save for reference?
    assign( ".iTraces",iTraces, pos=1 )   
  }
  else if ( substring( pfPlotType,3,7 )=="Trend" )
  {
    # This is intentional because the Trend plot is based on Tulip.
    doTulip <- TRUE
    
    # Get pfTraces random replicates and save indices to working directory.  
    # Note that if the simulations contain unequal numbers of replicates then
    # the trace indices can be out of range.  This is caught by plot functions.
    if ( pfTraces > 0 )
      iTraces <- sample( blob$pars$nReps,min(pfTraces,blob$pars$nReps) )
    else
      iTraces <- 0    
  }
  else if ( substring( pfPlotType,3,5 )=="Bar" )
  {
    doBar  <- TRUE
  }
  else if ( substring( pfPlotType,3,6 )=="Qbox" )
  {
    doQbox <- TRUE
  }
  else if ( substring( pfPlotType,3,4 )=="Eq" )
  {
    doEquil <- TRUE
  }
  else
    doOther <- TRUE
    
  # Set up the number of rows and columns required for plot layout.
  # Set the row and column layout parameters (This is a HACK that doesn't
  # handle things when more plots than available panels are specfied, plots
  # will be over-written after 10).
  if ( nSim<=2 )
    mfRow <- c( nSim, 1 )
  else if (nSim > 2 & nSim <= 4 )
    mfRow <- c( 2, 2 )
  else if (nSim > 4 & nSim <=6 )
    mfRow <- c( 3, 2 )
  else if ( nSim > 6 & nSim <=8 )
    mfRow <- c( 4, 2 )
  else
    mfRow <- c( 5, 2 )
  
  if ( pfPlotType=="pfTulipDepCat" )
    mfRow <- c( 2,nSim )
    
  if ( pfPlotType=="pfTulipBmsyMSY" )
    mfRow <- c( 2,nSim )
    
  if ( pfPlotType=="pfTrend" )
    mfRow <- c( nSim, 2 )
    
  # Set the plot region parameters.
  if ( pfPlotType=="pfTulipBmsy" )
    par( oma=.TULOMA, mar=.TULMAR, mfrow=mfRow )
  else if ( pfPlotType=="pfTulipBmsyMSY" )
    par( oma=.TULOMA, mar=.TULMAR, mfcol=mfRow )
  else if ( pfPlotType=="pfTulipDep" ) {
    par( oma=.TULOMA, mar=.TULMAR, mfrow=mfRow )
  } else if ( pfPlotType=="pfTulipCat" ) {
    par( oma=.TULOMA, mar=.TULMAR, mfrow=mfRow )
  } else if ( pfPlotType=="pfTulipDis" ) {
    par( oma=.TULOMA, mar=.TULMAR, mfrow=mfRow )
  } else if ( pfPlotType=="pfTulipDepCat" ) {
    par( oma=.TULOMA, mar=.TULMAR, mfcol=mfRow )
  } else if ( pfPlotType=="pfTulipF" ) {
    par( oma=.TULOMA, mar=.TULMAR, mfrow=mfRow )
  } else if ( pfPlotType=="pfFvsSSB" ) {
    par( oma=.OTHOMA, mar=.OTHMAR, mfcol=mfRow )        
  } else if ( pfPlotType=="pfBarAllStats" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfcol=c(nStat,nPer) )
  } else if ( pfPlotType=="pfBarDepPer" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfcol=c(1,nPer) )
  } else if ( pfPlotType=="pfBarCatPer" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfcol=c(1,nPer) )
  } else if ( pfPlotType=="pfBarDisPer" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfcol=c(1,nPer) )
  } else if ( pfPlotType=="pfBarAAVPer" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfcol=c(1,nPer) )
  } else if ( pfPlotType=="pfBarAllPer" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(nPer,nStat) )
  } else if ( pfPlotType=="pfBarAllPerSL" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(2,nStat) )  
  } else if ( pfPlotType=="pfBarShort" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(1,nStat) )
  } else if ( pfPlotType=="pfBarMedium" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(1,nStat) )
  } else if ( pfPlotType=="pfBarLong" ) {
    par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(1,nStat) )
  } else if ( pfPlotType=="pfQboxDep" ) {
    par( oma=c(2,5,2,2), mar=c(3,2,2,2), mfrow=c(nPer,1) )  
    # par( oma=.PERFOMA, mar=.PERFMAR, mfrow=c(nPer,1) )
  } else if ( pfPlotType=="pfQboxSSB" ) {
    par( oma=c(2,5,2,2), mar=c(3,2,2,2), mfrow=c(nPer,1) )  
  } else if ( pfPlotType=="pfEqRecSSB" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  } else if ( pfPlotType=="pfEqSsbU" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  } else if ( pfPlotType=="pfEqSsbRU" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  } else if ( pfPlotType=="pfEqYieldU" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )  
  } else if ( pfPlotType=="pfEqYieldSSB" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )                           
  } else if ( pfPlotType=="pfEqYprU" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
  } else if ( pfPlotType=="pfTrend" ) {
    par( oma=.OMA, mar=.MAR, mfrow=c(nSim, 2) )
  }
  
  failedReps <- NULL
  nFail <- 0
  
  # This block loops over valid simulations and finds the union of failed reps.
  if ( pfFailType=="pfFailAcross" )
  {
    for ( i in 1:nrow(simList ) )
    {
      if ( simList$Stamp[i]!="NO_SIM" )
      {
        fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
        # Load an Rdata working directory containing a list called blob.
        cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
        load( file=fileName )      
      
        failedReps <- c( failedReps,.calcFailedReps( blob$mp$assess$estPars ) )
      }
    }
    failedReps <- unique( failedReps )
    nFail <- length(failedReps)
  }    
  
  if ( doTulip & !isNoSim )
  {
    for ( i in 1:nSim )
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
      load( file=fileName )

      # This block determines failed reps within current simulation.  
      if ( pfFailType=="pfFailWithin" )
        failedReps <- .calcFailedReps( blob$mp$assess$estPars )

      if ( .Platform$OS.type=="windows" )
        txt <- paste( .stripQuotes(simList$Scenario[i]),"\n", simList$Procedure[i] )
      else
        txt <- paste( simList$Scenario[i],"\n",simList$Procedure[i] )
        
      if ( pfStamp )
        txt <- paste( txt," (",simList$Stamp[i],")", sep="" )  

      if ( pfPlotType=="pfTulipBmsy" )    
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinBase,pfMaxBase )
            
        .plotTulipBmsy( blob, guiInfo$multLrpBmsy, guiInfo$multUsrBmsy,
                         label=txt, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                         doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                         useYears=pfUseYears ) )
        
        if ( pfAnnotate )
        {
          if ( pfBase=="Bmsy" )
            baseVal <- blob$pars$ssbFmsy
          else if ( pfBase=="B0" )
            baseVal <- blob$pars$B0
            
          tmp <- .addObjectives( blob, failedReps=failedReps, base=pfBase,
                                 targMult, targYear, targProb )
          if ( pfLegend )
          {
            obj1 <- paste( "Year ",tmp$yearAtTargetProb,"~(M ",targMult,
                      ", P ",targProb,")",sep="" )
            obj2 <- paste( "Prob ",round(tmp$probAtTargetYear,digits=2),
                      "~(M ",targMult,
                      ", Y ",targYear,")", sep="" )
            obj3 <- paste( "Mult ",round(tmp$targetAtYearProb/baseVal,digits=1),
                      "~(Y ",targYear,
                      ", P ",targProb,")",sep="" )
            legTxt <- c( obj1, obj2, obj3 )
            panLegend( 0.65, 0.9, bg="white", cex=0.8, pt.cex=1.1, legTxt=legTxt,
              col=c(.TARGYEARCOL,.TARGPROBCOL,.TARGCOL),
              pch=c(16,16,16) )
          }
        }
                
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "SSB/Bmsy" )
        }                                                      
      }
      
      if ( pfPlotType=="pfTulipBmsyMSY" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinBase,pfMaxBase )
            
        .plotTulipBmsy( blob, guiInfo$multLrpBmsy, guiInfo$multUsrBmsy,
                         label=txt, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                         doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                         useYears=pfUseYears ) )
        
        if ( pfLegend )
        {
          if ( pfBase=="Bmsy" )
            baseVal <- blob$pars$ssbFmsy
          else if ( pfBase=="B0" )
            baseVal <- blob$pars$B0
            
          tmp <- .addObjectives( blob, failedReps=failedReps, base=pfBase,
                                 targMult, targYear, targProb )
          if ( pfLegend )
          {
            obj1 <- paste( "Year ",tmp$yearAtTargetProb,"~(M ",targMult,
                      ", P ",targProb,")",sep="" )
            obj2 <- paste( "Prob ",round(tmp$probAtTargetYear,digits=2),
                      "~(M ",targMult,
                      ", Y ",targYear,")", sep="" )
            obj3 <- paste( "Mult ",round(tmp$targetAtYearProb/baseVal,digits=1),
                      "~(Y ",targYear,
                      ", P ",targProb,")",sep="" )
            legTxt <- c( obj1, obj2, obj3 )
            panLegend( 0.65, 0.9, bg="white", cex=0.8, pt.cex=1.1, legTxt=legTxt,
              col=c(.TARGYEARCOL,.TARGPROBCOL,.TARGCOL),
              pch=c(16,16,16) )
          }
        }
                
        mfg <- par ( "mfg" )                                              
        if ( (mfg[1]==1) && (mfg[2]==1) )
        {
          mtext( side=2, line=3, cex=.TULCEX, "Bt / Bmsy" ) 
        }                            
        
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinMSY,pfMaxMSY )
        
        .plotTulipCatchMSY( blob, label=NULL,
                         traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=FALSE, doLegend=pfLegend,
                           doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                           useYears=pfUseYears ) )
     
        mfg <- par ( "mfg" )                                              
        if ( (mfg[1]==2) && (mfg[2]==1) )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=3, cex=.TULCEX, "Ct / MSY" ) 
        }
      }   # pfTulipBmsyCmsy            
    
      if ( pfPlotType=="pfTulipCat" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinCat,pfMaxCat )

        .plotTulipCatch( blob, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         label=NULL, failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                           doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                           useYears=pfUseYears ) )
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "Legal Catch (t)" )
        }                                    
      }
      
      if ( pfPlotType=="pfTulipDep" )    
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinDep,pfMaxDep )
        
        .plotTulipDepletion( blob, guiInfo$multLrpBmsy, guiInfo$multUsrBmsy,
                         label=NULL, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                         doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                         useYears=pfUseYears ) )
        
        if ( pfAnnotate )
        {
          #target <- targMult * blob$pars$ssbFmsy
          #tmp <- .addObjectives( blob, pfBase, targMult,targYear,targProb )
          #if ( pfLegend )
          #{
          #  obj1 <- paste( "Year ",tmp$yearAtTargetProb,"~(T ",targMult,
          #            ", P ",targProb,")",sep="" )
          #  obj2 <- paste( "Prob ",round(tmp$probAtTargetYear,digits=2),
          #            "~(T ",targMult,
          #            ", Y ",targYear,")", sep="" )
          #  obj3 <- paste( "Targ ",round(tmp$targetAtYearProb,digits=1),"~(Y ",targYear,
          #            ", P ",targProb,")",sep="" )
          #  legTxt <- c( obj1, obj2, obj3 )
          #  panLegend( 0.65, 0.9, bg="white", cex=0.8, pt.cex=1.1, legTxt=legTxt,
          #    col=c(.TARGYEARCOL,.TARGPROBCOL,.TARGCOL),
          #    pch=c(16,16,16) )
          #}
        }
                                 
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "Depletion" )
        }                                                      
      }      
      
      if ( pfPlotType=="pfTulipDepCat" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinDep,pfMaxDep )
          
        .plotTulipDepletion( blob, guiInfo$multLrpBmsy, guiInfo$multUsrBmsy,
                         label=txt, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                         doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                         useYears=pfUseYears ) )

        mfg <- par( "mfg" )
        if ( (mfg[1]==1) && (mfg[2]==1) )
          mtext( side=2, line=3, cex=.CEXLAB, "Depletion" )

        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinCat,pfMaxCat )
        
        .plotTulipCatch( blob, label=NULL,
                         traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=FALSE, doLegend=pfLegend,
                           doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                           useYears=pfUseYears ) )
     
        mfg <- par ( "mfg" )                                              
        if ( (mfg[1]==2) && (mfg[2]==1) )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=3, cex=.CEXLAB, "Catch (000s t)" ) 
        }
      }   # pfTulipDepCat
      
      if ( pfPlotType=="pfTulipDis" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinDis,pfMaxDis )
        
        .plotTulipDis( blob, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
                         failedReps=failedReps,
                         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                         doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim ) )
                         
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Year" )
          mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "Discards (t)" )
        }                                    
      }            
    
      if ( pfPlotType=="pfTulipF" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinHR,pfMaxHR )
      
        gear <- 1      
        .plotTulipHR( blob, gear=gear, label=txt,
                      traces=iTraces, qProbs=quantVals,
                      allQuants=TRUE, failedReps=failedReps,
                      gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                      doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                      useYears=pfUseYears ) )
                   
        #if ( pfAnnotate )
        #  panLab( 0.05, 0.95, cex=1.0, blob$pars$gNames[gear] )                   
                   
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.CEXLAB, outer=TRUE, "Year" )
          mtext( side=2, line=1.5, cex=.CEXLAB, outer=TRUE, "Realized legal harvest rate" )
        }                              
      }
      
      # This call is like "Tulip" so placed here for convenience.
      if ( pfPlotType=="pfTrend" )
      {
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinYr,pfMaxYr )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinSSB,pfMaxSSB )

        .plotTrend( blob, delta=trendPeriod, multLrp=multLrpBmsy, multUsr=multUsrBmsy,
           label=txt, traces=iTraces, failedReps=failedReps,
           gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
           doGrid=pfGrid, showProj=pfProj, xLim=xLim, yLim=yLim,
           useYears=pfUseYears ) )
           
        if ( i==nSim )
        {
          #mtext( side=1, line=0.5, cex=.TULCEX, "Year" )
          mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "Spawning Biomass (000s t)" )
        }                                    
      }     # if pfTrend      

      # This is done for all tulip plots.
      if ( pfAnnotate & pfStamp )
      {
#        txt <- paste( simList$Simulation[i],": ",
#               .stripQuotes(simList$Scenario[i]),"-",
#               simList$Procedure[i]," (",simList$Stamp[i],")", sep="" )

        if ( .Platform$OS.type=="windows" )
          txt <- paste( .stripQuotes(simList$Scenario[i]),"\n",simList$Procedure[i] )
        else
          txt <- paste( simList$Scenario[i],"\n",simList$Procedure[i] )
          
        if ( pfStamp )
          txt <- paste( txt," (",simList$Stamp[i],")", sep="" )

        panLab( 0.5,0.9,adj=0.5,cex=0.8, font=3, txt )
      }      
    }      # i=1,nSim
  }     # doTulip
  else if ( doTulip & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }

  if ( doBar & !isNoSim )
  {
    result <- .calcBarPlotStats( simList, pfPeriodList, failedReps )
    
    # Call the requested plot.
    if ( pfPlotType=="pfBarDepPer" )
    {
      .plotBarsByPeriod( result, xvar="depletion", refPoints=NULL )
    }
    else if ( pfPlotType=="pfBarCatPer" )
    {
      .plotBarsByPeriod( result, xvar="catch", refPoints=NULL )
    }
    else if ( pfPlotType=="pfBarDisPer" )
    {
      .plotBarsByPeriod( result, xvar="discard", refPoints=NULL )
    }
    else if ( pfPlotType=="pfBarAAVPer" )
    {
      .plotBarsByPeriod( result, xvar="AAV", refPoints=NULL )
    }
    else if ( pfPlotType=="pfBarAllStats" )
    {
      .plotBarsByPeriod( result, xvar=c("depletion","catch","discard","AAV"),
                         refPoints=NULL )
    }
    else if ( pfPlotType=="pfBarShort" )
    {
      .plotBarsByStats( result, periodList="Short", refPoints=NULL, pfUseYears )
    }
    else if ( pfPlotType=="pfBarMedium" )
    {
      .plotBarsByStats( result, periodList="Medium", refPoints=NULL, pfUseYears )
    }
    else if ( pfPlotType=="pfBarLong" )
    {
      .plotBarsByStats( result, periodList="Long", refPoints=NULL, pfUseYears )
    }
    else if ( pfPlotType=="pfBarAllPer" )
    {
      .plotBarsByStats( result, periodList=c("Short","Medium","Long"),
                        refPoints=NULL, pfUseYears )
    }
    else if ( pfPlotType=="pfBarAllPerSL" )
    {
      .plotBarsByStats( result, periodList=c("Short","Long"),
                        refPoints=NULL, pfUseYears )    
    }
    
    # *** HACK ARK 15-Aug-10, not quite kosher, but should work.
    nReps <- blob$pars$nReps   
    
    if ( pfFailType=="pfFailAcross" )
      mtext( side=1, line=0.5, cex=1, outer=TRUE, adj=0,
        paste( "Removed ",length(failedReps)," of ",nReps," replicates." ) )
    else if ( pfFailType=="pfFailWithin" )
      mtext( side=1, line=0.5, cex=1, outer=TRUE, adj=0,
        "All replicates kept, removals within not allowed." )
      
  }      # doBar
  else if ( doBar & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }  
  
  if ( doQbox & !isNoSim )
  {
    iRow <- 0
    
    # Make an output object for replicate performance statistics.  There are
    # nSim*nrow(pfPeriodList) rows and nRep columns because of Short, Medium, Long
    # summary periods. Number of columns is determined by the Simulation,
    # Scenario, Procedure, Period, t1, t2, plus the nRep statistics.
  
    nResults    <- nSim * nrow(pfPeriodList)
    headerNames <- c("Simulation","Scenario","Procedure","Period","t1","t2",
                     "B0","Bmsy","zoneLimit","zoneUpper" )
    result      <- data.frame(matrix( NA,nrow=nResults,ncol=length(headerNames)))
    names( result ) <- headerNames
    
    # Loop over the selected simulations.
    for ( i in 1:nSim )
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
      load( file=fileName )
    
      # This block determines failed reps within current simulation.  
      #if ( pfFailType=="pfFailWithin" )
      #  failedReps <- .calcFailedReps( blob$mp$assess$estPars )    
    
      nReps <- blob$pars$nReps

      # Depletion at MSY.
      B0   <- blob$pars$B0
      Bmsy <- blob$par$ssbFmsy
      
      # Take the zone boundaries from perfGui.
      zoneLimit <- multLrpBmsy
      zoneUpper <- multUsrBmsy

      # Get the SSB and depletion matrix to be summarized, nReps by nT matrix.
      Bt <- blob$om$Bt
  
      # Determine which reps to remove.
      if ( is.null(failedReps) )
      {
        Bt <- Bt[ ,c(2:ncol(Bt)) ]
        nFail <- 0
      }
      else
      {
        isFailed <- is.element( Bt[,"iRep"], failedReps )
        nFail <- length(failedReps)
        Bt <- Bt[ !isFailed,c(2:ncol(Bt)) ]
      }
      
      # Depletion and scaled SSB/Bmsy.
      Dept     <- Bt / B0
      scaledBt <- Bt / Bmsy               

      if ( i==1 )
      {
        depStats <- matrix( NA, nrow=nrow(result), ncol=nReps-nFail )
        if ( is.null(failedReps) )
          dimnames( depStats ) <- list( NULL, paste( "Rep",c(1:nReps),sep="" ) )
        else
          dimnames( depStats ) <- list( NULL, paste( "Rep",c(1:nReps)[!isFailed],sep="" ) )
        ssbStats <- depStats
      }
      
      # Accumulate statistics for each period - may need them later for barplots.
      for ( j in 1:nrow(pfPeriodList) )
      {
        # Get the header information for this simulation and period.
        iRow <- iRow + 1
        result[ iRow, "Simulation" ] <- simList$Simulation[i]
        result[ iRow, "Scenario" ]   <- simList$Scenario[i]
        result[ iRow, "Procedure" ]  <- simList$Procedure[i]
        result[ iRow, "Period" ]     <- pfPeriodList$Period[j]
        result[ iRow, "t1"     ]     <- pfPeriodList$Year1[j]
        result[ iRow, "t2"     ]     <- pfPeriodList$Year2[j]
        result[ iRow, "B0"   ]       <- B0
        result[ iRow, "Bmsy" ]       <- Bmsy
        result[ iRow, "zoneLimit" ]  <- zoneLimit
        result[ iRow, "zoneUpper" ]  <- zoneUpper

        # Get the time index values that span this period.
        tdx <- c( pfPeriodList$Year1[j]:pfPeriodList$Year2[j] )

        #--- Depletion Statistics                                           ---#
        depStats[ iRow, ] <- apply( Dept[,tdx],1,mean )
        
        #---- Scaled SSB Statistics.
        ssbStats[ iRow, ] <- apply( scaledBt[,tdx],1,mean )

      }     # Loop over j periods.
    }     # Loop over i simulations.
    
    resultDep <- data.frame( result, depStats )
    resultSSB <- data.frame( result, ssbStats )
  
    if ( pfPlotType=="pfQboxDep" )
    {
      .plotQboxDep( resultDep, pfPeriodList, quantVals, gfx=list( doLegend=pfLegend ) )
      if ( pfFailType=="pfFailAcross" )
        mtext( side=1, line=0.5, cex=1, outer=TRUE,
          paste( "Removed ",length(failedReps)," of ",nReps," replicates." ) )
      else if ( pfFailType=="pfFailWithin" )
        mtext( side=1, line=0.5, cex=1, outer=TRUE, "All replicates kept, removals within not allowed." )
    }
    
    if ( pfPlotType=="pfQboxSSB" )
    {
      .plotQboxSSB( resultSSB, pfPeriodList, quantVals, gfx=list( doLegend=pfLegend ) )
      if ( pfFailType=="pfFailAcross" )
        mtext( side=1, line=0.5, cex=1, outer=TRUE,
          paste( "Removed ",length(failedReps)," of ",nReps," replicates." ) )
      else if ( pfFailType=="pfFailWithin" )
        mtext( side=1, line=0.5, cex=1, outer=TRUE, "All replicates kept, removals within not allowed." )
    }    
                             
  }    # if doQbox
  else if ( doBar & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }  
  
  if ( doOther & !isNoSim )
  {
    # Loop over the selected simulations.
    for ( i in 1:nSim )
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
      load( file=fileName )

      # This block determines failed reps within current simulation.  
      if ( pfFailType=="pfFailWithin" )
        failedReps <- .calcFailedReps( blob$mp$assess$estPars )
        
      nReps <- blob$pars$nReps
      
      if ( pfPlotType=="pfFvsSSB" )
      {
      
        xLim <- NULL
        if ( pfSetXaxis )
          xLim <- c( pfMinSSB,pfMaxSSB )
        yLim <- NULL
        if ( pfSetYaxis )
          yLim <- c( pfMinHR,pfMaxHR )
                
        .plotFvsSSB( blob, zones=list(zoneLimit=multLrpBmsy,zoneUpper=multUsrBmsy),
                     failedReps=failedReps,
                     gfx=list( annotate=pfAnnotate, doColors=FALSE, doLegend=pfLegend,
                     doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim ) )
                           
        if ( i==nSim )
        {
          mtext( side=1, line=0.5, cex=.TULCEX, outer=TRUE, "Spawning Biomass" )
          mtext( side=2, line=1.5, cex=.TULCEX, outer=TRUE, "Legal Harvest Rate" ) 
        }
     
        if ( pfAnnotate )
        {
          txt <- paste( simList$Simulation[i],": ",simList$Scenario[i],"-",
                 simList$Procedure[i]," (",simList$Stamp[i],")", sep="" )
          #mtext( side=1, line=2, cex=0.7, font=3, adj=1, txt )
          panLab( 0.975,0.95,adj=1,cex=0.7, font=3, txt )
        }
        
      }     # if pfFvsSSB plot
        
    }     # i in 1:nSim loop.
   
  }     # if doOther
  else if ( doOther & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }  
     
  if ( doEquil )
  {
    checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
    names( checked ) <- c( "Umsy",  "U0",  "U01", "U40",   "Umax",  "Ucra" )
    
    
      if ( pfPlotType=="pfEqYprU" )
      {
        .plotEqYprU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
           xLim=NULL,yLim=NULL ) )
      }
    
      if ( pfPlotType=="pfEqYieldU" )
      {
          
        .plotEqYieldU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
          xLim=NULL,yLim=NULL ) )
      }
    
      if ( pfPlotType=="pfEqSsbRU" )
      {
        .plotEqSsbPerRecU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
           xLim=NULL,yLim=NULL ) )
      }
      
      if ( pfPlotType=="pfEqSsbU" )
      {
        .plotEqSsbU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
           xLim=NULL,yLim=NULL ) )
      }
      
      if ( pfPlotType=="pfEqRecSSB" )
      {
        .plotEqRecSSBU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
           xLim=NULL,yLim=NULL ) )
      }
      
      if ( pfPlotType=="pfEqYieldSSB" )
      {
        .plotEqYieldSSBU( obj[select], idNum,
           gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
           setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
           xLim=NULL,yLim=NULL ) )
      }      
  }   # if doEquil
  
  if ( pfPlotType=="pfDiag" & !isNoSim )
  {
    graphics.off()
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )

    # Loop over the selected simulations.
    for ( i in 1:nSim )
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
      load( file=fileName ) 

      if ( i > 1 )
        dev.new()      
      
      .plotDiagSim( blob, BmsyCrit=.BMSYCRIT, FmsyCrit=.FMSYCRIT,
        gfx=list( annotate=pfAnnotate, doLegend=pfLegend ) )
      
      txt <- paste( simList$Simulation[i],": ",simList$Scenario[i],"-",
               simList$Procedure[i]," (",simList$Stamp[i],")", sep="" )
      mtext( side=1, line=2, cex=0.7, font=3, adj=1, txt )
    }
  }
  else if ( pfPlotType=="pfDiag" & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }
  
  if ( pfPlotType=="pfParPairs" & !isNoSim )
  {
    graphics.off()
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )

    # Loop over the selected simulations.
    for ( i in 1:nSim )
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.doGuiPerfPlots) Loading",fileName,"...\n" )     
      load( file=fileName ) 
           
      if ( i > 1 )
      {
        dev.new()
        par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
      }      
      
      if ( any( names(blob$mp$assess$estPars)=="MSY" ) )
        .estParPairs( blob )
      else
        .plotStatus( "No MSY - is this a perfect information simulation?" )
      
      if ( pfAnnotate )
      {
        txt <- paste( simList$Simulation[i],": ",simList$Scenario[i],"-",
                 simList$Procedure[i]," (",simList$Stamp[i],")", sep="" )
        mtext( side=1, line=2, cex=0.7, font=3, adj=1, txt )
      }
    }
  }
  else if ( pfPlotType=="pfParPairs" & isNoSim )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    .plotStatus( "No plot - selections include NO_SIM results.\n" )
  }          

  # Save plot(s) as an enhanced Windows metafile.
  if ( act=="pfSaveEMF" )
    savePlot( filename=paste( simName,pfPlotType,sep="" ),
              type="emf", restoreConsole = TRUE )
              
  # Save plot(s) as a PDF file.
  if ( act=="pfSavePDF" )
    savePlot( filename=paste( simName,pfPlotType,sep="" ),
              type="pdf", restoreConsole = TRUE )                
    
  # Restore the graphics settings.
  par( oldpar )
}     # END .doGuiPerfPlots function.

.calcBarPlotStats <- function( hdr, period, failedReps=NULL )
{
  # Get the guiPerf parameters so that plot controls available.
  # Remember that hdr is the list of SELECTED simulations.
  
  guiInfo <- getWinVal(scope="L")  
  
  iRow <- 0
  nSim <- nrow( hdr )
    
  # qLower and qUpper are user-specified from the guiPerf GUI.
  quantVals <- c( 0.05,pfLoQuant,0.5,pfUpQuant,0.95 )
  
  # Make an output object for performance statistics, there are
  # nSim*nrow(pfPeriodList) rows because of Short, Medium, Long, etc summary
  # periods.  The number of columns is determined by the columns of pfSimList:
  # Simulation, Scenario, Procedure, Period, t1, t2, plus the number of stats.
  
  nResults <- nSim * nrow( period )
  headerNames <- c( "Simulation","Scenario","Procedure","Period","t1","t2" )
  statNames <- c( "medAvgDep","Q1AvgDep","Q2AvgDep",
                  "medFinalDep","Q1finalDep","Q2finalDep",
                  "medAAV", "Q1AAV","Q2AAV",
                  "medAvgCatch","Q1AvgCatch","Q2AvgCatch",
                  "medAvgDiscard","Q1AvgDiscard","Q2AvgDiscard" )
  colNames <- c( headerNames, statNames )
  result <- data.frame( matrix( NA, nrow=nResults,ncol=length(colNames) ) )
  names( result ) <- colNames

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    fileName <- paste( "sim",hdr$Stamp[i],".Rdata", sep="" )
     
    # Load an Rdata working directory containing a list called blob.
    cat( "\nMSG (.calcBarPlotStats) Loading",fileName,"...\n" )     
    load( file=fileName )
    
    nReps <- blob$pars$nReps

    # Accumulate statistics for each period - may need them later for barplots.
    for ( j in 1:nrow(period) )
    {
      # Get the header information for this simulation and period.
      iRow <- iRow + 1
      result[ iRow, "Simulation" ] <- hdr$Simulation[i]
      result[ iRow, "Scenario" ]   <- hdr$Scenario[i]
      result[ iRow, "Procedure" ]  <- hdr$Procedure[i]
      result[ iRow, "Period" ]     <- period$Period[j]
      result[ iRow, "t1"     ]     <- period$Year1[j]
      result[ iRow, "t2"     ]     <- period$Year2[j]
      
      # Get the time index values that span this period.
      tdx <- c( period$Year1[j]:period$Year2[j] )

      Bt <- blob$om$Bt
      Ct <- blob$om$Ct
      
      # There is no blob$om$Dt, so sum over the gears, also note that om$Dtg
      # does not have replicate numbers as the first column.
      Dt <- apply( blob$om$Dt[ ,c(1:ncol(blob$om$Dt)), ], c(1,2), sum )

      # Determine which reps to remove.
      if ( is.null(failedReps) )
      {
        # Get the variables to be summarized, these are nReps by nT matrices.
        Bt <- Bt[ ,c(2:ncol(Bt)) ]
        Ct <- Ct[ ,c(2:ncol(Ct)) ]
      
        nFail <- 0
      }
      else
      {
        isFailed <- is.element( Bt[,"iRep"], failedReps )
        Bt <- Bt[ !isFailed,c(2:ncol(Bt)) ]
        Ct <- blob$om$Ct[ !isFailed,c(2:ncol(blob$om$Ct)) ]
        Dt  <- Dt[ !isFailed, ] 
        
        nFail <- length(failedReps)        
      }      
      
      # Get the variables to be summarized, these are nReps by nT matrices.
      #Bt   <- blob$om$Bt[ ,c(2:ncol(blob$om$Bt)) ]
      #Ct   <- blob$om$Ct[ ,c(2:ncol(blob$om$Ct)) ]
      
      # There is no blob$om$Dt, so sum over the gears, also note that om$Dtg
      # does not have replicate numbers as the first column.
      #Dt   <- apply( blob$om$Dt[ ,c(1:ncol(blob$om$Dt)), ], c(1,2), sum )
      
      Dept <- Bt / blob$pars$B0
    
      #--- Depletion Statistics                                             ---#

      # Aggregate depletion for period.
      tmp <- .calcStatsDepletion( Dept[,tdx], probs=quantVals )
      result[ iRow, "medAvgDep" ] <- tmp$medAvgDep
      result[ iRow, "Q1AvgDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgDep" ]  <- tmp$qVals[4]
        
      # Final depletion for period.
      tmp <- .calcStatsFinalDep( Dept[,tdx], quantVals )
      result[ iRow, "medFinalDep" ] <- tmp$medFinalDep
      result[ iRow, "Q1finalDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2finalDep" ]  <- tmp$qVals[4]

      #--- Catch Statistics                                                 ---#

      tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
      result[ iRow, "medAvgCatch" ] <- tmp$medAvgCatch
      result[ iRow, "Q1AvgCatch" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgCatch" ]  <- tmp$qVals[4]
      
      #---- Discard Statistics                                              ---#
      tmp <- .calcStatsDiscard( Dt[,tdx], quantVals )
      result[ iRow, "medAvgDiscard" ] <- tmp$medAvgDiscard
      result[ iRow, "Q1AvgDiscard" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgDiscard" ]  <- tmp$qVals[4]      
 
      #--- AAV Catch Statistics                                             ---#

      tmp <- .calcStatsAAV( Ct, tdx, quantVals )   
      result[ iRow,"medAAV" ] <- tmp$medAAV
      result[ iRow,"Q1AAV" ]  <- tmp$qVals[2]
      result[ iRow,"Q2AAV" ]  <- tmp$qVals[4]

    }     # Loop over j periods.
  }     # Loop over i simulations.  

  result
}


#------------------------------------------------------------------------------#
#-- Performance statistics functions                                         --#
#------------------------------------------------------------------------------#

# .calcPerfStats (Calculate performance statistics by calling .calcStatsXXX).
# Purpose:       Calculate various performance statistics with the function
#                name .calcStatsXXX, where XXX determined the statistics.
#                (1) Read the simulation tracking file to get the simulation
#                    ID, simulation label, scenario label, and procedure label.
#                (2) Make a data frame that holds the period definitions from
#                    guiPerf entry fields.
#                (3) Build a result data frame that has the simulation identifier
#                    fields, period definitions and statistics as columns. Each
#                    row respresents the peformance statistics for one simulation
#                    and time period.
#                (4) Loop through the time periods for each simulation and
#                    calculate the performance statistics for each combination.
# Parameters:    NONE.
# Returns:       A list with components simSummary1 (spreadsheet "view") and
#                simSummary2 (normalized "Access view").
# Source:        A.R. Kronlund
.calcPerfStats <- function()
{
  # Get the GUI parameters to calculate periods, local scope.
  win     <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )

  noSimVal <- -999

  # Get the simulation list, regardless of whether it has a valid Sim.
  simList <- pfSimList[ c(1:pfNsim), ]
  
  # Get the period list.
  period <- pfPeriodList
  
  quantVals <- c( 0.05, pfLoQuant, 0.5, pfUpQuant, 0.95 )
  
  # Make an output object for performance statistics, there are a total of 
  # nSim*nrow(pfPeriodList) rows because of Short, Medium, Long summaries, etc.
  # The number of columns is determined by the Simulation, Sceanario, Procedure,
  # Stamp, Period, t1, t2, plus the number of statistics calculated.
  
  nSim        <- nrow( simList )
  nResults    <- nSim * nrow( period )
  headerNames <- c("Simulation","Scenario","Procedure","Stamp","Period","t1","t2")
  statNames   <- c( "medAvgDep","Q1AvgDep","Q2AvgDep",
                    "medFinalDep","Q1finalDep","Q2finalDep",
                    "medLowDep","Q1LowDep","Q2LowDep",
                    "medAAV", "Q1AAV","Q2AAV",
                    "medAvgCatch","Q1AvgCatch","Q2AvgCatch",
                    "medLowCatch","Q1LowCatch","Q2LowCatch",
                    "medHighCatch","Q1HighCatch","Q2HighCatch",
                    "medAvgDiscard","Q1AvgDiscard","Q2AvgDiscard",
                    "criticalP","cautiousP","healthyP",
                    "yearAtTargetProb","probAtTargetYear","targetAtYearProb",
                    "probGteDepMSY","probGteLimit",
                    "pObj1","pObj3","pHealthy",
                    "t1Trend","trendPeriod","avgExpSlope",
                    "trendDec","trendInc","obsPdecline","pDecline",
                    "pGTlrp","pGTtarg","t1AvgCatch","t1AvgDep" )

  colNames    <- c( headerNames, statNames )
  result      <- data.frame( matrix( NA, nrow=nResults,ncol=length(colNames) ) )
  names( result ) <- colNames
  
  # Set the results to noSimVal
  result[ ,statNames ] <- noSimVal
    
  # Initialize row counter.
  iRow    <- 0
              
  # Load and calculate statistics for each simulation in tracking object.
  for ( i in 1:nSim )
  {
    validSim <- FALSE
    if ( simList$Stamp[i] !="NO_SIM" )
      validSim <- TRUE
    
    if ( validSim )  
    {
      fileName <- paste( "sim",simList$Stamp[i],".Rdata", sep="" )
     
      # Load an Rdata working directory containing a list called blob.
      cat( "\nMSG (.calcPerfStats) Loading",fileName,"...\n" )     
      load( file=fileName )
    
      tMP    <- blob$pars$tMP
      nT     <- blob$pars$nT
    
      B0     <- blob$pars$B0
      Bmsy   <- blob$pars$ssbFmsy
      DepMSY <- Bmsy / B0
    
      limitB <- multLrpBmsy * Bmsy
      upperB <- multUsrBmsy * Bmsy
    
      # Zone boundaries on the depletion scale.
      limitDep <- limitB / B0
      upperDep <- upperB / B0
    }
    
    # Accumulate statistics for each period.
    for ( j in 1:nrow(period) )
    {
      # Get the header information for this simulation and period.
      iRow <- iRow + 1
      result[ iRow, "Simulation" ] <- simList$Simulation[i]
      result[ iRow, "Scenario" ]   <- simList$Scenario[i]
      result[ iRow, "Procedure" ]  <- simList$Procedure[i]
      result[ iRow, "Stamp" ]      <- simList$Stamp[i]
      result[ iRow, "Period" ]     <- period$Period[j]
      result[ iRow, "t1"     ]     <- period$Year1[j]
      result[ iRow, "t2"     ]     <- period$Year2[j]
      
      # Usefull for trend statistics.
      t1 <- result[ iRow, "t1" ]
      t2 <- result[ iRow, "t2" ]
      
      # Get the time index values that span this period.
      tdx <- c( period$Year1[j]:period$Year2[j] )
      
      if ( validSim )
      {
        # Get the variables to be summarized, these are nReps by nT matrices.
        Bt   <- blob$om$Bt[ ,c(2:ncol(blob$om$Bt)) ]
        Ct   <- blob$om$Ct[ ,c(2:ncol(blob$om$Ct)) ]
        Dt   <- apply( blob$om$Dt,c(1,2),sum )
        Dept <- Bt / blob$pars$B0
      }
    
      #--- Depletion Statistics                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsDepletion( Dept[,tdx], quantVals )       
        result[ iRow, "medAvgDep" ] <- tmp$medAvgDep
        result[ iRow, "Q1AvgDep" ]  <- tmp$qVals[2]
        result[ iRow, "Q2AvgDep" ]  <- tmp$qVals[4]
      }
      
      #--- Final depletion for period                                       ---#
      if ( validSim )
      {
        tmp <- .calcStatsFinalDep( Dept[,tdx], quantVals )
        result[ iRow, "medFinalDep" ] <- tmp$medFinalDep
        result[ iRow, "Q1finalDep" ]  <- tmp$qVals[2]
        result[ iRow, "Q2finalDep" ]  <- tmp$qVals[4]
      }

      #--- Low depletion for period                                         ---#
      if ( validSim )
      {
        tmp <- .calcStatsLowDep( Dept[,tdx], quantVals )
        result[ iRow, "medLowDep" ] <- tmp$medLowDep
        result[ iRow, "Q1LowDep" ]  <- tmp$qVals[2]
        result[ iRow, "Q2LowDep" ]  <- tmp$qVals[4]
      }
      
      #--- Catch Statistics                                                 ---#
      if ( validSim )
      {
        tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
        result[ iRow, "medAvgCatch" ] <- tmp$medAvgCatch
        result[ iRow, "Q1AvgCatch" ]  <- tmp$qVals[2]
        result[ iRow, "Q2AvgCatch" ]  <- tmp$qVals[4]
      }

      #--- Low catch for period                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsLowCat( Ct[,tdx], quantVals )
        result[ iRow, "medLowCatch" ] <- tmp$medLowCat
        result[ iRow, "Q1LowCatch" ]  <- tmp$qVals[2]
        result[ iRow, "Q2LowCatch" ]  <- tmp$qVals[4]
      }
      
      #--- Low catch for period                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsHighCat( Ct[,tdx], quantVals )
        result[ iRow, "medHighCatch" ] <- tmp$medHighCat
        result[ iRow, "Q1HighCatch" ]  <- tmp$qVals[2]
        result[ iRow, "Q2HighCatch" ]  <- tmp$qVals[4]
      }      
        
      #--- Discard Statistics                                               ---#
      if ( validSim )
      {
        tmp <- .calcStatsDiscard( Dt[,tdx], quantVals )
        result[ iRow, "medAvgDiscard" ] <- tmp$medAvgDiscard
        result[ iRow, "Q1AvgDiscard" ]  <- tmp$qVals[2]
        result[ iRow, "Q2AvgDiscard" ]  <- tmp$qVals[4]
      }

      #--- AAV Catch Statistics                                             ---#
      if ( validSim )                                                       
      { 
        tmp <- .calcStatsAAV( Ct, tdx, quantVals )
        result[ iRow,"medAAV" ] <- tmp$medAAV
        result[ iRow,"Q1AAV" ]  <- tmp$qVals[2]
        result[ iRow,"Q2AAV" ]  <- tmp$qVals[4]
      }
      
      #--- Zone Objectives Statistics                                       ---#
      
      # Note that the limitD and upperD are taken from the multLrpBmsy
      # and multUsrBmsy, respectively, not the HCR reference points.
      # They are translated from the SSB scale to the depletion scale above.
      
      if ( validSim )
      {
        tmp <- .calcStatsZones( Dept[,tdx],limitDep,upperDep, quantVals )
        #result[ iRow,"medCriticalP" ] <- tmp$medCriticalP
        #result[ iRow,"medCautiousP" ] <- tmp$medCautiousP
        #result[ iRow,"medHealthyP"  ] <- tmp$medHealthyP
        result[ iRow,"criticalP"    ] <- tmp$pAllZone[1]
        result[ iRow,"cautiousP"    ] <- tmp$pAllZone[2]
        result[ iRow,"healthyP"     ] <- tmp$pAllZone[3]
      }
      
      #--- Objective Statistics by Period                                   ---#
      
      if ( validSim )
      {
        tmp <- .calcStatsRefPoints( Bt[,tdx], target=Bmsy, targMult=multLrpBmsy, refProb=0.95 )
        result[ iRow,"pGTlrp" ] <- tmp
        tmp <- .calcStatsRefPoints( Bt[,tdx], target=Bmsy, targMult=1, refProb=0.5 )
        result[ iRow,"pGTtarg" ] <- tmp
      }
      
      #--- Objective Statistics from GUI.
      
      if ( validSim )
      {
        # Note that the entire projection period is required here, not
        # just the current period of interest.
        baseVal <- 0
        if ( pfBase=="Bmsy" )
          baseVal <- blob$pars$ssbFmsy
        else if ( pfBase=="B0" )
          baseVal <- blob$pars$B0
          
        tmp <- .calcStatsTarget( Bt, baseVal, tMP, targMult, targYear, targProb )
        result[ iRow,"yearAtTargetProb"  ] <- tmp$yearAtTargetProb
        result[ iRow,"probAtTargetYear"  ] <- tmp$probAtTargetYear
        result[ iRow,"targetAtYearProb"  ] <- tmp$targetAtYearProb
      }
      
      # Objectives 1-3 by period (use long period for paper, 30 years).
      .calcPropGtTarget <- function( x, target, FUN=mean )
      {
        # Calculate for each replicate (row of x), whether GE target or not.
        gtTarget <- function( x, target )
        {
          result <- rep( 0, length(x) )
          result <- ifelse( x > target, 1, result )
          result
        }
        
        # Number of time steps (years) in time slice.
        n <- ncol( x )
        
        # Matrix of (0,1) to show when x is GT target for each replicate.
        tmp <- t( apply( x,1,FUN=gtTarget, target ) )
        
        # Calculate sum of each row, which is the number GT target.
        nTarget <- apply( tmp,1,sum )
        
        # Proportion of time steps (years) GT target.
        pTarget <- nTarget / n
        
        # Mean proportion above target.
        avgPtarget <- mean( pTarget )
        
        result <- list( avgPtarget=avgPtarget, n=n, pTarget=pTarget, nTarget=nTarget )
        result
      }
      
      if ( validSim )
      {
        # Objective 1 - the mean proportion of projected years where SSB is in
        # the Cautious or Healthy zones.
        tmpObj1 <- .calcPropGtTarget( Bt[,tdx], target=limitB, FUN=mean )
        tmpObj3 <- .calcPropGtTarget( Bt[,tdx], target=Bmsy,   FUN=mean )
        tmpHealthy <- .calcPropGtTarget( Bt[,tdx], target=upperB, FUN=mean )
        
        result[ iRow,"pObj1" ] <- tmpObj1$avgPtarget
        result[ iRow,"pObj3" ] <- tmpObj3$avgPtarget
        result[ iRow,"pHealthy" ] <- tmpHealthy$avgPtarget
      }
      
      #--- Policy Statistics                                                ---#
      
      if ( validSim )
      {
        tmp <- .calcStatsPolicy( Dept[,tdx], DepMSY, limitDep, tMP )
        #result[ iRow,"medProbGteDmsy"  ] <- tmp$medProbGteDmsy
        #result[ iRow,"medProbGteLimit" ] <- tmp$medProbGteLimit
        result[ iRow,"probGteDepMSY" ] <- tmp$probGteDepMSY
        result[ iRow,"probGteLimit"  ] <- tmp$probGteLimit
      }
      
      #--- Trend statistics - this is set to start at the first year of the period
      #--- for an interval of "delta" years to a maximum of nT-t1 years.
      
      if ( validSim )
      {
        tmp <- .calcStatsTrend( Bt, t1=t1, delta=trendPeriod )

        SSB    <- as.numeric( Bt[,unique(tmp$t1) ] )
        target <- blob$pars$ssbFmsy
        tmp$pDecline <- .calcStatsAccDecline( SSB, target, lowProb=0.05,
                          hiProb=0.5, multLrp=multLrpBmsy, multUsr=multUsrBmsy  )        
        
        result[ iRow, "t1Trend" ] <- t1
        result[ iRow, "trendPeriod" ] <- trendPeriod      
        result[ iRow, "avgExpSlope" ] <- mean( tmp$beta )
        nInc <- sum( tmp$beta >= 0.0 )
        nDec <- length( tmp$beta ) - nInc
        pObs <- nDec / length( tmp$beta )        
        result[ iRow, "trendInc" ]    <- nInc
        result[ iRow, "trendDec" ]    <- nDec
        result[ iRow, "obsPdecline" ] <- pObs
        result[ iRow, "pDecline" ]    <- mean( tmp$pDecline )
      }
      
      #--- Convenience - Depletion and Catch at t1 for each period.         ---#
      if ( validSim )
      {
        tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
        result[ iRow, "t1AvgCatch" ] <- mean( Ct[,tdx[1] ] )
        result[ iRow, "t1AvgDep" ]   <- mean( Dept[,tdx[1] ] )
      }
      

    }     # Loop over j periods.
    
    # Do garbage collection to clean memory of loose ends, accumulated blobs.    
    gc()
           
   if ( !validSim )
      cat( "\nMSG (.calcPerfStats) No feedback results for simulation ",i,"\n" )
        
  }   # Loop over i simulations.
  
  result2 <- data.frame( matrix( NA, nrow=(nrow(result)*length(statNames)),
                           ncol=(length(headerNames)+2) ) )
                           
  names( result2 ) <- c( headerNames,"statistic","value" )
   
  # Now format simulation summary in normalized form.
  iRow <- 0
  for ( i in 1:nrow( result ) )
  {
    for ( j in 1:length(statNames) )
    {
      iRow <- iRow + 1
      result2[ iRow,c(1:length(headerNames)) ] <- result[ i,c(1:length(headerNames)) ]
      result2[ iRow,"statistic" ] <- statNames[j]
      result2[ iRow,"value" ]     <- result[ i,(length(headerNames)+j) ]
    }
  }
  
  perResult   <- 0     # Not used at this time.
  summary1 <- result
  summary2 <- result2

  return( list( summary1=summary1, summary2=summary2,
                perResult=perResult ) )
}


# .calcStatsAAV (Calculate quantiles of Average Annual Varability statistics)
# Purpose:      Calculate quantiles of AAV statistics over simulation replicates.
# Parameters:   Dt    - catch biomass as an nRep by nT matrix.
#               tdx   - indices of time period for statistics.
#               probs - probabilities for quantiles.
# Returns:      val, a list with the median AAV over replicates and a vector
#               with the quantiles specified by probs.
# Notes:        In contrast to other .calcStatsXXX functions, this function
#               requires the entire 1:nT time period because of the lag in the
#               AAV calculations.
# Source:       A.R. Kronlund
.calcStatsAAV <- function( Dt, tdx, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # The period must be of length >= 2.  Can't form the difference otherwise.
  if ( length(Dt) >= 2 )
  {  
    # Form the absolute catch differences over 1:nT by replicate.      
    diffDt    <- t( apply( Dt,1,diff ) )
    absDiffDt <- abs( diffDt )

    # Shift differences by 1 to catch transition from previous period
    # to current period, i.e., sum(|C_t-C_t-1|) for t=t1,..,t2.  For example,
    # if the current year is 2008, you have to get the first catch difference
    # of 2008-2007 to respect C_t - C_t-1, i.e., when t=2008, t-1=2007.

    # Sum the absolute differences of the shifted absolute catch differences.
    sumAbsDiffDt <- apply( absDiffDt[,(tdx-1)],1,sum )
      
    # Sum the catch by replicate over the summary period specified by tdx.
    sumCatch <- apply( Dt[,tdx], 1, sum )
      
    # Compute the AAV by replicate.
    AAV <- ifelse( sumCatch > 0.0, sumAbsDiffDt / sumCatch * 100.0, 0.0 )
    # Compute the median of the AAV values for each replicate.
    medAAV <- median( AAV )
    # Compute the quantiles of the distribution.
    # Use the quantiles specified in the interface (qLower, qUpper).
    qVals <- quantile( AAV, probs=probs )
  }
  else
  {
    # If the length of Dt is insufficent to form AAV, everything gets -1.
    medAAV <- -1
    qVals <- rep( -1, length(probs) )
  }
  val <- list( medAAV=medAAV, qVals=qVals )
  val
}


# .calcStatsCatch (Calculate quantiles of average catch statistics)
# Purpose:        Calculate quantiles of average catch statistics over
#                 simulation replicates.
# Parameters:     Dt    - catch biomass as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average catch over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsCatch <- function( Ct, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Compute average catch by replicate over the range specified in tdx.
  avgCatch <- apply( Ct,1,mean )
  # Compute the median of the average catch values.
  medAvgCatch <- median( avgCatch )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgCatch, probs=probs )
  val <- list( medAvgCatch=medAvgCatch, qVals=qVals )
  val
}

# .calcStatsDiscard (Calculate quantiles of average discards statistics)
# Purpose:        Calculate quantiles of average catch statistics over
#                 simulation replicates.
# Parameters:     Dt    - catch biomass as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average catch over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsDiscard <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Compute average catch by replicate over the range specified in tdx.
  avgDiscard <- apply( Dt,1,mean )
  # Compute the median of the average catch values.
  medAvgDiscard <- median( avgDiscard )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgDiscard, probs=probs )
  val <- list( medAvgDiscard=medAvgDiscard, qVals=qVals )
  val
}

# .calcStatsDepletion (Calculate quantiles of average depletion statistics)
# Purpose:        Calculate quantiles of average depletion statistics over
#                 simulation replicates.
# Parameters:     Dept  - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsDepletion <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Average depletion by replicate (avgDept is vector len=ncol(Dept)).
  avgDep <- apply( Dept,1,mean )
  # Compute the median of the average depletion values.
  medAvgDep <- median( avgDep ) 
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgDep, probs=probs )
  val <- list( medAvgDep=medAvgDep, qVals=qVals )
  val
}


# .calcStatsFinalDep (Calculate quantiles of final depletion statistics)
# Purpose:        Calculate quantiles of final depletion statistics over
#                 simulation replicates.
# Parameters:     Dep   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median final depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsFinalDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the final depletion in the period passed to function.
  # The passed Dept is an nRep by length(t1:t2) matrix.  Thus, ncol(Dept)
  # is the last column (t2) in the matrix - the final depletion for the period.
  finalDep <- Dept[ ,ncol(Dept) ]
  # Compute the median of the final depletion values, final year of period over reps.
  medFinalDep <- median( finalDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( finalDep, probs=probs )
  val <- list( medFinalDep=medFinalDep, qVals=qVals )
  val
}

# .calcStatsHighCat (Calculate quantiles of low catch statistics)
# Purpose:         Calculate quantiles of low catch statistics over
#                  simulation replicates.
# Parameters:      Dt  - catch as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low catch over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsHighCat <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low catch values from the replicate.
  # This apply operation returns a vector of length=nRep.
  highCat <- apply( Dt,1,max )
  # Compute the median of the low depletion values for the period over reps.
  medHighCat <- median( highCat )
  # Compute the quantiles of the distribution.
  qVals <- quantile( highCat, probs=probs )
  val <- list( medHighCat=medHighCat, qVals=qVals )
  val
}

# .calcStatsLowCat (Calculate quantiles of low catch statistics)
# Purpose:         Calculate quantiles of low catch statistics over
#                  simulation replicates.
# Parameters:      Dt  - catch as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low catch over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowCat <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low catch values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowCat <- apply( Dt,1,min )
  # Compute the median of the low depletion values for the period over reps.
  medLowCat <- median( lowCat )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowCat, probs=probs )
  val <- list( medLowCat=medLowCat, qVals=qVals )
  val
}


# .calcStatsLowDep (Calculate quantiles of low depletion statistics)
# Purpose:         Calculate quantiles of low depletion statistics over
#                  simulation replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low depletion over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low depletion values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowDep <- apply( Dept,1,min )
  # Compute the median of the low depletion values for the period over reps.
  medLowDep <- median( lowDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowDep, probs=probs )
  val <- list( medLowDep=medLowDep, qVals=qVals )
  val
}


# .calcStatsPolicy (Calculate statistics related to Limit and Bmsy policy)
# Purpose:         (1) Calculate median probability of depletion > Dmsy for
#                      the time period over replicates,
#                  (2) Calculate the median probability of depletion >
#                      Dep at the LRP for the time period over replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix,
#                          where (t1:t2) defines the summary period.
#                  Dmsy  - depletion at Bmsy.
#                  zoneLimit - depletion at Critical-Cautious zone boundary.
#                  tMP   - start year of management procedure.
# Returns:         val, a list with the median statistics for the following:
#                  Pr(Dep>=Dmsy), Pr(Dep<Dmsy), Pr(Dep>=Dlimit), Pr(Dep<Dlimit).
# Source:          A.R. Kronlund
.calcStatsPolicy <- function( Dept, DepMSY, zoneLimit, tMP )
{
  isGTE <- function( x, bound )
  {
    result <- rep( "Below",length(x) )
    result <- ifelse( x >= bound, "Above", result )
    result
  }
  
  calcPcats <- function( x, bound )
  {
    # Calculate number and proportion GTE specified bound for each replicate.
    tmp <- t( apply( x,1,FUN=isGTE, bound ) )

    catLabels <- c( "Below","Above" )
    nCats <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(catLabels) ) )
    names( nCats ) <- catLabels
    pCats <- nCats
  
    for ( i in 1:nrow(tmp) )
      nCats[ i, ] <- table( factor(tmp[i,],levels=catLabels ) )
  
    # Calculate the proportions.
    pCats <- nCats / apply( nCats,1,sum )
    
    result <- list( nCats=nCats, pCats=pCats )
    result
  }
  
  # Calculate the median proportion GTE Bmsy.
  tmp <- calcPcats( Dept, DepMSY )
  medProbGteDepMSY <- median( tmp$pCats[,"Above"] )
  medProbLtDepMSY  <- median( tmp$pCats[,"Below"] )

  # Calculate the median proportion GTE zoneLimit.
  tmp <- calcPcats( Dept, zoneLimit )
  
  medProbGteLimit <- median( tmp$pCats[,"Above"] )
  medProbLtLimit  <- median( tmp$pCats[,"Below"] )
  
  # Now calculate irrespective of replicates.
  probGteLimit <- sum( Dept >= zoneLimit ) / length( Dept )
  probGteDepMSY  <- sum( Dept >= DepMSY      ) / length( Dept )
  
  val <- list( medProbGteDepMSY=medProbGteDepMSY,   medProbLtDepMSY=medProbLtDepMSY,
               medProbGteLimit=medProbGteLimit, medProbLtLimit=medProbLtLimit,
               probGteLimit=probGteLimit, probGteDepMSY=probGteDepMSY )
  val    
}

.calcStatsRefPoints <- function( Bt, target, targMult=0.4, refProb=0.95 )
{
  refPt <- targMult * target
  
  # Find all years in period where Bt > refPt.
  tmp <- matrix( 0, nrow=nrow(Bt), ncol=ncol(Bt) )
  tmp[ Bt >= refPt ] <- 1
  
  # Count the number of years in each replicate where where Bt > refPt.
  val <- apply( tmp,1,sum )
  
  # Calculate the proportion of years in each replicate where Bt > refPt.
  pVal <- val / ncol( Bt )
  
  # Compute the mean proportion of years GT refPt.
  result <- mean( pVal )
  result
}


# .calcStatsTarget (Calculate statistics for Xt WRT target, time, certainty)
# Purpose:         Calculate statistics determined by target objectives entry
#                  fields on guiPerf:
#      (1) yearAtTargetProb: year where Xt >= target with specified probablity,
#      (2) probAtTargetYear: probability of Xt >= target at the specified year,
#      (3) targetAtYearProb: target at probability at the specified time such
#                             that Pr(Xt >= target)=1-prob.
#
# Parameters:  Xt         - entire nRep by length(1:nT) matrix of depletion.
#              baseVal    - base value (Bmsy or B0).
#              tMP        - start year of management procedure.
#              targetMult - user specified target multiplier
#              targetYear - user specified target year.
#              targetProb - user specified target probability (certainty).     
# Returns:     result, a list calculated yearAtTargetProb, probAtTargetYear, and
#              targetAtYearProb values, along with baseVal, targMult, targetYear, targetProb.
# Source:          A.R. Kronlund
.calcStatsTarget <- function( Xt, baseVal, tMP, targetMult,targetYear,targetProb )
{
  # Assume that the entire time series Xt is passed as nRep by nT matrix.
  
  # Calculate the target.
  target <- targetMult * baseVal
  
  nT   <- ncol( Xt )             # Total number of time steps.
  tVec <- c( 1:nT )              # Vector of time index values.
  tdx  <- c( tMP:nT )            # Time index values for projection period.
  
  # (1) Calculate time when target is first attained with targetProb certainty.
  
  # Calculate the quantile corresponding to 1-targetProb at each time step.
  # This means that targetProb*100% of the distribution of target values
  # is greater than the quantile at each time step. This is a 1:nT vector.
  
  qTarget <- apply( Xt, 2, quantile, probs=c(1.0-targetProb) )
  
  # Find the index of the first PROJECTION time step where qTarget >= target,
  # i.e., where the target quantile corresponding to targetProb is greater
  # than the target.  This means you are targetProb*100% sure that the
  # target has been attained.  No guarantee it will remain attained.
  
  tSteps    <- c(1:nT)               # t=1,..., nT
  isTarget  <- qTarget >= target     # TRUE if quantile >= target.
  tdxTarget <- tSteps[ isTarget ]    # t values where quantile >= target.
  
  # Now determine if there are any in the projection period that exceed the target.
  if ( length(tdxTarget) > 0 )
  {
    tdxTarget <- tdxTarget[ tdxTarget >=tMP ]
    if ( length(tdxTarget) > 0 )
      yearAtTargetProb <- min(tdxTarget)
    else
      yearAtTargetProb <- -1                   # No values GTE the target.
  }
  else
    yearAtTargetProb <- -1                     # No values GTE the target.
   
  # (2) Calculate the quantile of target at the targetYear index (tMP,...,nT).
  
  # Here we find the proportion of target values at time targetYear that
  # exceed the targetDep, i.e., certainty of exceeding targetDep at targetYear.

  targetVals       <- Xt[ ,targetYear ]
  probAtTargetYear <- sum( targetVals >= target ) / nrow(Xt)
  
  # (3) Calculate the target at targetYear and targetProb.
  targetAtYearProb <- quantile( targetVals,probs=(1.0-targetProb) )

  # Return a list result.  
  result <- list( yearAtTargetProb=yearAtTargetProb, probAtTargetYear=probAtTargetYear,
                  targetAtYearProb=targetAtYearProb,
            baseVal=baseVal, target=target,
            targetMult=targetMult, targetYear=targetYear, targetProb=targetProb )
  result
}

.calcStatsTrend <- function( y, t1, delta=10 )
{
  # y is a matrix of dimension nRep by nT.
  y1 <- as.numeric( y[ ,t1 ] )
  y2 <- as.numeric( y[ ,t1+delta ] )
  
  beta <- ( log(y2) - log(y1) ) / delta
  yPrime <- exp( (beta * delta) + log(y1) )
  result <- data.frame( t1=t1, t2=t1+delta, beta=beta, y1=y1, y2=y2, yPrime=yPrime )
  result
}

.calcStatsAccDecline <- function( SSB, target, lowProb=0.05, hiProb=0.5,
   multLrp=0.4, multUsr=0.8  )
{
  LRP     <- multLrp * target
  USR     <- multUsr * target
  pDecline <- lowProb + (hiProb-lowProb) * (SSB-LRP)/(target-LRP)
  pDecline <- ifelse( SSB < LRP,    lowProb, pDecline )
  pDecline <- ifelse( SSB > target, hiProb,  pDecline )    
  pDecline
}

# .calcStatsZones (Calculate proportion of time SSB in each stock status zone)
# Purpose:        Calculate quantiles of the proportion of time (probability)
#                 that the SSB was in the Critical, Cautious, and Healthy stock
#                 status zones during time period over the simulation replicates.
# Parameters:     Dept   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 limitD - boundary Crit-Cautious zones on the depletion scale.
#                 upperD - boundary Cautious-Healthy zones on depletion scale.
#                 probs  - probabilities for quantiles (not used yet).
# Returns:        val, a list with the median final depletion over replicates
# Source:         A.R. Kronlund
.calcStatsZones <- function( Dept, limitD, upperD, probs )
{
  # At this time the probs are not used. Only median statistics are computed.

  findZone <- function( x, limitD, upperD )
  {
    zone <- rep( "Critical",length(x) )
    zone <- ifelse( x > limitD, "Cautious",zone )
    zone <- ifelse( x > upperD, "Healthy", zone )
    zone
  }
  
  n <- ncol(Dept)
  
  # Calculate number in zone (Critical, Cautious Healthy) for each replicate.
  tmp <- t( apply( Dept,1,FUN=findZone, limitD, upperD ) )

  zoneLabels <- c( "Critical","Cautious","Healthy" )
  nZone <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(zoneLabels) ) )
  names( nZone ) <- zoneLabels
  pZone <- nZone
  
  for ( i in 1:nrow(tmp) )
    nZone[ i, ] <- table( factor(tmp[i,],levels=zoneLabels ) )
  
  # Calculate the proportions.
  pZone <- nZone / apply( nZone,1,sum )
  
  # Now calculate the medians.
  medCriticalP <- median( pZone[,"Critical"] )
  medCautiousP <- median( pZone[,"Cautious"] )
  medHealthyP  <- median( pZone[,"Healthy" ] )
  
  # Now calculate the proportions ignoring replicates.
  nAllZone <- table( cut( Dept, breaks=c(0,limitD,upperD,max(Dept) ) ) )
  pAllZone <- nAllZone / length(Dept)
  
  val <- list( medCriticalP=medCriticalP, medCautiousP=medCautiousP,
               medHealthyP=medHealthyP, nZone=nZone, pZone=pZone,
               nAllZone=nAllZone, pAllZone=pAllZone )
  val    
}

.saveSimStatsToFile <- function( obj )
{
  # obj is a simTracker list.
  
  nSim <- length(obj)
  
  varNames <- c( "Sim","Scenario","MP",
                 "h","M","B0",
                 "MSY","Umsy","Bmsy","Dmsy","Dlrp",
                 "avgSSBtMP","avgDeptMP","avgLegalBtMP","avgLegalHRtMP","avgCtMP" )
                 
  result <- data.frame( matrix( NA, nrow=nSim, ncol=length(varNames) ) )
  names( result ) <- varNames
  
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile <- paste( "sim",obj[[i]]$om$stamp,".Rdata",sep="" )
    cat( "\nMSG (.saveSimStatsToFile) Loading",simFile,"...\n" )    
    load( file=simFile )
    #assign( "blob", blob, pos=1 )
    
    scenarioName <- obj[[i]]$gui$scenarioName
    if (.Platform$OS.type=="windows" )
    { 
      scenarioName <- iconv( scenarioName, to="ASCII//TRANSLIT" )
      scenarioName <- gsub( "\"","",scenarioName )
    }
    
    mpName <- obj[[i]]$gui$mpName
    
    result[ i,"Sim"      ] <- paste( "Sim",i,sep="" )
    result[ i,"Scenario" ] <- scenarioName
    result[ i,"MP"       ] <- mpName
    result[ i,"MSY"      ] <- blob$pars$landedFmsy
    
    # ARK (17-Dec-10) Changed Umsy to legalHRFmsy.
    #result[ i,"Umsy"     ] <- blob$pars$Umsy    
    result[ i,"Umsy"     ] <- blob$pars$legalHRFmsy
    result[ i,"h"        ] <- blob$pars$rSteepness
    result[ i,"M"        ] <- blob$pars$M
    result[ i,"B0"       ] <- blob$pars$B0
    result[ i,"Bmsy"     ] <- blob$pars$ssbFmsy
    result[ i,"Dmsy"     ] <- result[ i,"Bmsy" ] / result[ i, "B0" ]
    result[ i,"Dlrp"     ] <- (0.4 * result[ i,"Bmsy" ] ) / result[ i, "B0" ]
    #result[ i,"Dusr"     ] <- (0.8 * result[ i,"Bmsy" ] ) / result[ i, "B0" ]
    
    tMP <- blob$pars$tMP
    result[ i,"avgSSBtMP"     ] <- mean( blob$om$Bt[ ,paste("B",tMP,sep="") ] )
    result[ i,"avgDeptMP"     ] <- mean( blob$om$Bt[ ,paste("B",tMP,sep="") ]/result[i,"B0"] )
    result[ i,"avgLegalBtMP"  ] <- mean( blob$om$legalB[ ,paste("lBiomass",tMP,sep="") ] )
    result[ i,"avgLegalHRtMP" ] <- mean( blob$om$legalHR[ ,paste("lHR",tMP,sep="" ) ] )
    
    # For some reason Ct is not named, but add one for the replicate ID.
    result[ i,"avgCtMP"       ] <- mean( blob$om$Ct[ ,tMP+1 ] )   
  }
  cat( "\n" )
  
  # Save to file here.
  OStype <- .Platform$OS.type
  if ( OStype=="windows" )
  {
    fName <- "mseRsimStats.xls"
	  if ( file.exists(fName) )
      fileGone <- file.remove( fName )    
    
    conn <- RODBC::odbcConnectExcel( fName, readOnly=FALSE )  	  
    .excelTable( conn, result, "SimStats", varNames, dimnames(result)[[1]] )
    close( conn )
  }
  else
  {
    fName <- "mseRsimStats.csv"
    write.csv( result, file = fName, append=FALSE, quote=TRUE, row.names=TRUE )
  }
  cat( "\nMSG (.saveSimStatsToFile) Parameters saved to ",fName,"\n" )    
    
  tmp <- result
  tmp[ , c(4:ncol(tmp)) ] <- round( tmp[ ,c(4:ncol(tmp)) ], digits=3 )
  print( tmp )

  result
}

.plotWhatIfSurvey <- function( obj, tIndex=47, deltaSurvey=c(-1,seq(0.5,2,0.1)) )
{
  # 1. Get catch  data.
  # 2. Get survey data.
  # 3. Get tIndex survey point.  Apply deltaSurvey.
  
  varNames <- c( "Delta","tIndex","Index","estMSY","estBmsy","estFmsy",
                 "projBiomass","precF","lastCatch","legalHarv" )
  
  result <- matrix( NA, nrow=length(deltaSurvey), ncol=length(varNames) )
  result <- data.frame( result )
  names( result ) <- varNames
  
  parList <- .createList( obj$runMSEpar )
  
  for ( i in 1:nrow(result) )
  {
    result$Delta[i]  <- deltaSurvey[ i ]
    result$tIndex[i] <- tIndex
    
    #--------------------------------------------------------------------------#
    # Perform the production model stock assessment.                           #
    #--------------------------------------------------------------------------#
    
    tmp <- list()

    # Get the index values
    tmp$nIndices <- length( parList$mp$data$useIndex )
    
    #tmp$Itg       <- obj$mp$data$Itg[1:(t-1),parList$mp$data$useIndex]
    Itg <- parList$opModData$index$idxSeries[ ,1:(tIndex-1) ]
  
    # Run assessment
    if( parList$mp$assess$assessMethod=="pMod" )
    {
      tmp$Itg <- Itg
      
      if ( deltaSurvey[i]!=-1 )
        tmp$Itg[ 3,tIndex-1 ] <- Itg[ 3, tIndex-2 ] * deltaSurvey[ i ]
      
      result$Index[i] <- tmp$Itg[ 3,tIndex-1 ]
     
      tmp$Itg <- t( tmp$Itg )     
     
      # SPC 19June2010: need to assemble and fill in the data required by pMod assessment
	    # If t == tMP, use init values of 0 for Omega and user inputs for Bo and r
      #tmp$initMSY         <- mp$assess$initMSY
      #tmp$initFmsy        <- mp$assess$initFmsy
      tmp$lnOmega         <- rep(0,(tIndex-3-1))
      tmp$surveyFirstYear <- parList$mp$data$t1Index
      tmp$surveyLastYear  <- rep((tIndex-1),tmp$nIndices)
      tmp$IndexType       <- parList$mp$data$IndexType
      tmp$useIndex        <- parList$mp$data$useIndex
      tmp$rhoEIV          <- parList$mp$assess$rhoEIV
      tmp$muPriorMSY      <- parList$mp$assess$muPriorMSY
      tmp$sdPriorMSY      <- parList$mp$assess$sdPriorMSY
      tmp$muPriorFmsy     <- parList$mp$assess$muPriorFmsy
      tmp$sdPriorFmsy     <- parList$mp$assess$sdPriorFmsy
      
      #tmp$Dt              <- obj$om$Ct[1:(t-1)] #rowSums( om$Ctg[ (1:(t-1)), ] )
      retC <- parList$opModData$catch$landCatchMatrix
      tmp$Dt <- apply( retC[,3:ncol(retC)],1,sum ) / 1000.0
      #tmp$Dt <- obj$om$legalC[ 1,c(1:(tIndex-1))+1 ]
      
      stockAssessment       <- assessModPMod( tmp )

      result$estBmsy[i]     <- stockAssessment$estPars$Bmsy
      result$estMSY[i]      <- stockAssessment$estPars$MSY
      result$estFmsy[i]     <- stockAssessment$estPars$Fmsy
      result$projBiomass[i] <- stockAssessment$biomass
      result$lastCatch[i]   <- stockAssessment$estPars$dep
      
      # If the "always replace failures with last successful year" was selected in
      # guiSim, the following series of if statements implement this functionality
      fail <- FALSE
      ## Test for failure in production model to converge in current year:
      #if (stockAssessment$estPars$iExit !=1 | stockAssessment$estPars$maxGrad >= 0.0001) 
      #  fail <- TRUE
      
      # Build harvest control rule object.
      # Most recent survey spawning biomass estimate (i.e.,from t-1)
      rule           <- obj$mp$hcr
      rule$assess    <- obj$mp$assess$assessMethod
      rule$biomass   <- stockAssessment$biomass
      rule$lastCatch <- stockAssessment$estPars$dep
      rule$remRate   <- stockAssessment$estPars$Fmsy
      
      rule$lowerB <- rule$specs$limitRefMult * stockAssessment$estPars$Bmsy
      rule$upperB <- rule$specs$upperRefMult * stockAssessment$estPars$Bmsy
      
      #rule$maxF      <- qlnorm(p=0.95,meanlog=log(tmp$muPriorFmsy),sdlog=tmp$sdPriorFmsy)
      
      .calcLegalCatch <- function( obj )
      {
        # Extract assessment quantities from harvest control rule list.
        biomass     <- obj$biomass
        lastCatch   <- obj$lastCatch

        # Extract fixed members of input harvest control rule list
        lastTAC     <- obj$lastCatch          # TAC[t-1]
        lam1        <- obj$specs$lam1         # Weight factor on TAC[t-1].
        remRate     <- obj$remRate            # Instantaneous fishing mortality rate reference

        lowerB <- obj$lowerB  # Limit reference point, where remRate=0
        upperB <- obj$upperB  # Upper stock reference, where remRate=Fmsy

        # Calculate effective removal rate based on biomass relative to ref pts
        adjF     <- remRate*(biomass-lowerB)/(upperB-lowerB)
        
        #adjF     <- min( adjF, maxF )
        if( adjF < 0 ) adjF <- 0    
          adjF <- ifelse( biomass < upperB, adjF, remRate  )

        catchLim              <- lam1 * lastCatch + (1.0-lam1)*adjF*biomass
        
        result                <-list()
        result$precautionaryF <- adjF
        result$catchLimit     <- max( 0., catchLim )
        result
      }
      
      # Calculate catch limit
      targetHarv   <- .calcLegalCatch( rule )

      result$precF[i]   <- targetHarv$precautionaryF
      result$legalHarv[i] <- targetHarv$catchLimit
            
    }     # if method is pMod.
  }     # for i=1:nrow(result)
  
  xLim <- range( deltaSurvey[ deltaSurvey!=-1 ] )
  yLim <- range( 0,result$legalHarv )
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )

  idx <- c(1:nrow(result))[ result$Delta==-1 ]
  #points( 1,result$legalHarv[idx], cex=2, pch=3 )
  abline( h=result$legalHarv[idx], lty=2, lwd=2 )
  abline( h=2.35, lty=1 )
  
  abline( h=seq(0,2.5,0.1), col="lightgray" )
  
  abline( v=1, lty=3, lwd=2 )
  
  points( result$Delta, result$legalHarv, bg="white", cex=1.4 )
  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  panLegend( 0.6,0.1, legTxt="No 2010 StRS Value", lty=2, lwd=2 )
  
  mtext( side=1, line=2, cex=.CEXLAB, "Multiplier of 2009 StRS Survey CPUE" )
  mtext( side=2, line=3, cex=.CEXLAB, "Legal Harvest 2011" )
  
  result
}
