#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 4.x                   #
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
#                                                                              #
#-------------------- mseRsim.r: mseR Simulatin GUI Functions -----------------#
#--                                                                          --#
#-- mseRsim.r: A mseR template for a GUI to choose and run a simulation.     --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 06-Dec-09 from mseR V2.0 (mseRgui_funs.r)          --#
#-- 14-Jan-10  Revised GUI and started implementing widget functions.        --#
#-- 26-Jan-10  First implementation from mseRmod.r.                          --#
#--                                                                          --#
# To do:                                                                       #
# 1. Put a legend/help info at the top of guiSim to remind user of gear/survey #
#    order in the vectors.                                                     #
# 3. Consider sub-directories for .Rdata files containing simulations.         #
# 4. Consistent error handling - msg to console and GUI message box?           #
# 5. When changing models, you need to close and re-open GUI to resize object  #
#    widget.                                                                   #
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
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. 2008. PBS          #
#   Modelling 2.50: user’s guide revised from Canadian Technical Report of     #
#   Fisheries and Aquatic Science 2674: v+146 p. Last updated October 23, 2008 #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
#   Flow: guiSim -> .guiSimSetup -> .wkDirSetup -> createWin -> .subguiSim     #
#                                                                              #
# guiSim        : Run the mseR Modelling GUI.                                  #
# guiSimSetup   : Initialize GUI settings.                                     #
# .wkDirSetup   : Setup the working directory and sub-directories as required. #
# createWin     : Create the GUI window.                                       #
# .subGuiSim    : Processes GUI submit actions, e.g., buttons, entry fields... #
#                                                                              #
# GUI Hidden Functions (to show hidden function ls(all.names=TRUE)):           #
#                                                                              #
# .getWinName    : Get the current GUI window name.                            #
# .viewFile      : View a file saved in the mseRtemp directory.                #
# .viewHdrStatus : Updates the status of the header table in guiView.          #
# .viewHelp      : View a file saved in the mseRhelp directory.                #
#                                                                              #
# Some useful tricks for later:                                                #
#  tmpPars[ parType!=types, ] <- ""                                            #
#------------------------------------------------------------------------------#

source( "mseRtools.r" )

# General global variables.
.PACKAGE <- ""

# Following directory names usually prefixed by .PACKAGE so that, for example,
# the directory name for .FDOCS becomes "mseRdocs".
.FDOCS    <- "docs"              # Directory containing documents.
.FHELP    <- "help"              # Directory containing help files.
.FTEMP    <- "temp"              # Directory containing temporary files.

.FSIMPARS <- "mseRsim.par"       # Parameter file for mseRsim.
.FSIMS    <- "mseRsims.Rdata"    # R working directory (a file) containing a list
                                 # of simulations to track.
.FSXLS    <- "mseRsims.xls"      # Excel output file for simulations.

#-----------------------------------------------------------------------------##
#-- GUI Functions                                                           --##
#-----------------------------------------------------------------------------##

# guiSim      (Run GUI)
# Purpose:    Run the GUI.
# Parameters: None
# Returns:    NULL (invisibly)
guiSim <- function()
{
  return( .guiSimSetup("mseRguiSim") )
}

#------------------------------------------------------------------------------#
#-- Gui Error Checking                                                       --#
#------------------------------------------------------------------------------#

# .validGuiSimPars (valid parameters for myGUI):
# Purpose:      Check whether the parameters supplied are valid.
#               If invalid, display an error message in the R console and
#               clear the invalid field.
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid.
# Parameters:   None
# GUI inputs:   Parameters taken directly from active myGui. 
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields, corrects correctable fields (if any)
.validGuiSimPars <- function()
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

# .subGuiSim  (Processes guiSim submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiSim program flow:
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
.subGuiSim <- function()
{
  win      <- .getWinName()                       # Get the current window name.
  gvar     <- paste( ".", win, sep="" )           # Global variable name.
  
  valid    <- TRUE
  guiInfo  <- getWinVal( scope="L", winName=win ) # GUI information local scope.
  act      <- getWinAct()[1]                      # Get last menu window action.
  
  guiChanges <- guiInfo                            # Make a copy for changes.
  updateMsg <- FALSE

  if ( valid )
  {
    # Asssign the list of parameters to the Global environment.
    simParsList <- .createList( simPars )
    assign( "simParsList", simParsList, pos=1 )
    
    # Assign the GUI settings to the Global environment.
    assign( "guiSimPars",  guiInfo,     pos=1 )
    
    #----------------------------------------------------------------#
    # Main Menu actions.                                             #
    #----------------------------------------------------------------#
    
    # OPEN the model parameters, set the GUI.
    if ( act=="simParOpen" )
    {
      simPars <- .readSimParFile( .FSIMPARS )

      # Did we successfully get a new simPars, or null?
      if ( !is.null( simPars ) )
      {
        assign( "simPars",    simPars,pos=1 )
        assign( "initSimPars",simPars,pos=1 )
      
       # Validation checks?
        #valid <- .validGuiSimPars()

        guiChanges$simPars <- simPars
     
        # Reset Sorting.
        guiChanges$parType <- "DEFAULT"
        guiChanges$sortName <- FALSE
        guiChanges$sortType <- FALSE
      
        updateMsg <- TRUE
        errMsg <- "(.subGuiSim) New model parameter file loaded."
      
        # Close the window and re-open 
        closeWin()
      
        # Initialize the GUI from the description file using PBSmodelling createWin.
        # Interesting, this appears to work, but suspect it for later problems.
        # Also, when we move to a project approach, the guiInfo parameters should
        # be loaded.
        createWin( paste( win, "Win.txt", sep="" ) )
            
        setWinVal( guiChanges, winName=win )    
      }
      else
      {
        updateMsg <- TRUE
        errMsg <- "(.subGuiSim) Model parameter file unchanged.\n"
      }
    }
    
    if ( act=="simParSave" )
    {
       .saveSimParFile()
    }
    
    # Process any parameter input actions.
    
    # Sort the parameter display.
    if ( act=="parType" || act=="sortName" || act=="sortType" )
    {
      # Establish current ordering of types.
      if ( parType=="DEFAULT" )
      {
        typeOrder <- parType.values
        typeOrder <- typeOrder[ typeOrder!="DEFAULT" ]
        typeOrder <- sort( typeOrder )
      }
      else
      {
        typeOrder <- parType.values
        typeOrder <- typeOrder[ typeOrder!="DEFAULT" ]
        typeOrder <- typeOrder[ typeOrder!=parType ]
        typeOrder <- c( parType,typeOrder )
      }

      # Sort by Parameter name within Type using current typeOrder.
      if ( guiInfo$sortType && guiInfo$sortName )
      {
        # Find the position of the first "$" in parameter vector.      
        # This solution breaks down when there is more than one $.
        #posDollar <- unlist( gregexpr( "\\$",simPars$parameter ) )
        # Try this solution, which may be general for unequal length vectors.
        tmp <- gregexpr( "\\$",simPars$parameter )
        posDollar <- unlist( sapply(tmp, function(x){ x[1] } ) )
        # Extract the parameter & type using the "$" position.
        types     <- substring( simPars$parameter,1,posDollar-1 )
        parameter <- substring( simPars$parameter,(posDollar+1), )
      
        guiChanges$simPars <- simPars[order(ordered(types,levels=typeOrder),
                                parameter),]
      }

      # Sort by Type using current typeOrder, leaving Parameter as input.      
      if ( guiInfo$sortType && !guiInfo$sortName )
      {
        # Restore input sort order.
        simPars <- simPars[ dimnames(initSimPars)[[1]], ]
      
        # Find the position of the first "$" in parameter vector.      
        # This solution breaks down when there is more than one $.
        #posDollar <- unlist( gregexpr( "\\$",simPars$parameter ) )
        # Try this solution, which may be general for unequal length vectors.
        tmp <- gregexpr( "\\$",simPars$parameter )
        posDollar <- unlist( sapply(tmp, function(x){ x[1] } ) )
        # Extract the type using the "$" position.
        types     <- substring( simPars$parameter,1,posDollar-1 )
       
        guiChanges$simPars <- simPars[ order(ordered(types,levels=typeOrder)), ]
      }

      # Sort by Parameters, leaving Type as input.
      if ( !guiInfo$sortType && guiInfo$sortName )
      {
        # Restore input sort order.
        simPars            <- simPars[ dimnames(initSimPars)[[1]], ]      
 
        # Find the position of the first "$" in parameter vector.      
        # This solution breaks down when there is more than one $.
        #posDollar <- unlist( gregexpr( "\\$",simPars$parameter ) )
        # Try this solution, which may be general for unequal length vectors.
        tmp <- gregexpr( "\\$",simPars$parameter )
        posDollar <- unlist( sapply(tmp, function(x){ x[1] } ) )
        # Extract the parameter using the first "$" position.
        parameter <- substring( simPars$parameter,(posDollar+1), )
        
        guiChanges$simPars <- simPars[ order(parameter), ]
      }

      # No sorting, restore input sort order.      
      if ( !guiInfo$sortType && !guiInfo$sortName )
      {
        # Restore input sort order.
        guiChanges$simPars <- simPars[ dimnames(initSimPars)[[1]], ]
      }        
      
      setWinVal( guiChanges, winName=win )
    }     # sortType or sortName.

    #----------------------------------------------------------------# 
    # Sim tracker widget.                                            #
    #----------------------------------------------------------------#
    
    # Add a Sit to the tracking list.
    if ( act=="addSim" )
    {
      if ( exists( "currentSim" ) )
      {
        nSim <- length( simTracker ) + 1
        guiInfo$iSim    <- nSim
        guiInfo$nSim    <- nSim
        simName <- paste( "Sim",nSim,sep="" )
        i <- 0
        while( any(simName==names(simTracker) ) )
        {
          i <- i + 1
          simName <- paste( simName,"v",i,sep="" )
        }
        guiInfo$simName <- simName
      
        # Add the GUI and ADMB information.
        simTracker[[nSim]] <- list( guiInfo=guiInfo, result=currentSim )
        # *** Ideally the list elements should attach directly to simTracker.
      
        names( simTracker )[[nSim]] <- guiInfo$simName
      
        # Update nSim in all sims and save the simTracker.
        for ( i in 1:length(simTracker) )
          simTracker[[i]]$guiInfo$nSim <- nSim
        assign( "simTracker", simTracker, pos=1 )
      
        guiChanges <- guiInfo
        setWinVal( guiChanges, winName=win )
      }
      else
      {
        updateMsg <- TRUE
        errMsg    <- "(.subGuiSim) No current simulation is available.\n"
      }
    }
    
    # Next Sim.
    if ( act=="nextSim" )
    {
      # Do not permit iSim > nSim.
      idxSim <- min( (iSim+1), nSim )
 
      # Update the Sim on the GUI.
      guiChanges <- simTracker[[idxSim]]$guiInfo    
    
      # Update the GUI.
      setWinVal( guiChanges,winName=win )
    }
    
    # Previous Sim.
    if ( act=="prevSim" )
    {
      # Get the index of the previous sim.  Do not permit iSim < 1.
      idxSim <- max( 1, (iSim-1) )
    
      # Update the parameter object widget.
      guiChanges <- simTracker[[idxSim]]$guiInfo

      # Update the GUI.
      setWinVal( guiChanges, winName=win )
    }
    
    # Remove a Sim from the tracking list.
    if ( act=="remSim" )
    {
      # Rules:
      # (a) Unlike trials, you can have 0 Sims saved in simTracker.
      # (b) If Sim (1,...,n-1) is removed, show the next Sim.
      # (c) If Sim n is removed, show Sim n-1.
    
      # Only take action if there is at least one Sim.
      if ( length(simTracker)> 0 )
      { 
        # Find the index of the Sim to be removed.
        id <- c(1:length(simTracker))[ guiInfo$simName==names(simTracker) ]

        # Rules b,c: Adjust the current Sim to account for the removal.
        if ( id==length(simTracker) )
          iSim <- length(simTracker) - 1
        else
          iSim <- id
      
        # Remove the Sim, re-number, and re-assign simTracker to global.
        simTracker[[guiInfo$simName]] <- NULL
        for ( i in 1:length(simTracker) )
        {
          simTracker[[i]]$iSim <- i
          simTracker[[i]]$nSim <- length(simTracker)
        }
        assign( "simTracker",simTracker,pos=1 )
      
        # Update the Sim information.
        guiChanges$iSim    <- iSim
        guiChanges$nSim    <- length(simTracker)
        guiChanges$simName <- names(simTracker)[iSim]
      
        # Update the GUI.
        setWinVal( guiChanges,winName=win )
      }
    }
        
    # Export Sim list to an R directory file.
    if ( act=="expSim" )
    {
      .saveSimToRdata()
    }
    
    # Import Sim list from an R directory file.
    if (act=="impSim" )
    {
      .loadSimFromRdata()

      # Update the trial information.
      guiChanges         <- simTracker[[1]]$guiInfo
      guiChanges$iSim    <- 1
      guiChanges$nSim    <- length( simTracker )
      guiChanges$simName <- names( simTracker )[1]
      
      # Update the GUI.
      setWinVal( guiChanges,winName=win )
    }
    
    #----------------------------------------------------------------#
    # Control buttons.                                               #
    #----------------------------------------------------------------#
    
    # EXIT the GUI (leave graphics on).
    if ( act=="exit" )
    {
      # Don't close until .subGuiSim has terminated.
      on.exit( closeWin() )
    }
    
    # TABLES requested.
    if ( act=="tables" )
    {
      saveStatus <- .saveSimToExcel( .FSXLS )
      if ( saveStatus$fileSaved )
        openFile( .FSXLS )
      else
      {
        errMsg    <- saveStatus$msg
        updateMsg <- TRUE 
      }
    }
    
    # guiView requested.
    if ( act=="guiView" )
    {
     doGuiView <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subGuiSim has to exit first!!!
        closeWin()
        guiView()
      }
      on.exit( doGuiView() )
    }
    
    # GUIREF requested.
    if ( act=="guiRef" )
    {
      doGuiRef <- function()
      {
        # This function required because you can't close a GUI while you
        # are still using it - the .subGuiSim has to exit first!!!
        closeWin()
        guiRef()
      }
      on.exit( doGuiRef() )
    }
    
    # Update message box.
    if ( updateMsg )
    {
      guiChanges$errMsg <- errMsg
      #setWidgetColor( errMsg, bg=updateMsgCol )
      setWinVal( guiChanges )
    }
    else
    {
      guiChanges$errMsg <- ""
      #setWidgetColor( errMsg, .getWinName(), bg="lightgreen" )
      setWinVal( guiChanges )
    }
    
  }     # if valid.
  else
  {
    # Invalid GUI parameters so bring R console to top.
    bringToTop(-1)
  }     # invalid.

  return( invisible() )  
}

#------------------------------------------------------------------------------#
#-- GUI Helper Functions                                                     --#
#------------------------------------------------------------------------------#

# .guiSimSetup (Setup for guiSim creation)
# Purpose:    Set up and run guiSim
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.guiSimSetup <- function(win)
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  
  .admbReadOptions()                 # ADMB option list to global environment.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  dir <- .wkDirSetup()
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  
  if ( exists( "currentSim" ) )
    rm( currentSim, pos=1 )
  
  # This finds the names of all ADMB tpl files in the working directory.
  # *** Consider making a sub-directory just for TPL files.
  tplList <- .findFileName( suffix=".tpl" )
  assign( "tplList", tplList, pos=1 )
  
  # This finds the name of all *.dat files in the working directory.
  datList <- .findFileName( suffix=".dat" )
  assign( "datList", datList, pos=1 )
  
  # *** Default parameter file - let a check happen later.
  simPars <- readParFile( .FSIMPARS )
  assign( "simPars",     simPars, pos=1 )
  assign( "initSimPars", simPars, pos=1 )
  
  # Initalize the Sim tracker list.
  assign( "simTracker", list(), pos=1 )
  
  # Can a menu be created?
  goMenu <- TRUE
  
  #*** Test here whether conditions are valid for menu creation. ***
  
  # Valid conditions exist for menu creation.
  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( paste( dir, "/", win, "Win.txt", sep="" ) )
    
    # Get the GUI parameters and make scope local to this function.
    guiInfo <- getWinVal( scope="L", winName=win )
          
    # Initialize myGui action.
    if ( win=="guiSim" )
    {
      # Perform GUI initialization.
    }
    
  }
  else
    cat( "\nERROR (.guiSimSetup): GUI creation not possible:\n ")

  return( invisible() )
}     # .guiSimSetup

.loadSimFromRdata <- function( simFile=.FSIMS )
{
  tmpFile <- selectFile( initialfile=simFile,
                filetype=list( c(".Rdata","Rdata files") ), mode="open" )
  if ( is.null(tmpFile) )
    return( invisible() )      # No such file exists, bale.
  else
    simFile <- tmpFile 
    
  load( simFile, .GlobalEnv )
  
  return( invisible() ) 
}

.createList <- function( obj )
{
  # Input  a data frame with columns "parameter" and "value".
  # Output a list with elements named as parameter and values in "value".

  result <- list()

  # Shut off whining, then coerce to numeric to let NA indicate non-numerics.
  options( warn=-1 )
  numericVal <- as.numeric( obj[,"value"] )
  options( warn=0 )

  for ( i in 1:nrow(obj) )
  {
    # Value is numeric, build the parse string.
    if ( !is.na(numericVal[i]) )
      listText <- paste( "result$",obj[i,"parameter"],"=",
                    obj[i,"value"],sep="" )
    # Value is character, build the parse string.
    else
      listText <- paste( "result$",obj[i,"parameter"],"=",
                  obj[i,"value"], sep="" )

    # ARK: At one point I had this code, presumably to handle a different
    #      input format convention, perhaps assuming "value" was all character.
    #                   sQuote(obj[i,"value"]),sep="" )
    
    # Evaluate the parse string.
    eval( parse( text=listText ) )
  }
  result
}

# .readParFile   (reads an ASCII file with 1 comment line, header, data frame)
# Purpose:      Reads an ASCII file: 1 comment, 1 header, space-delimited
#               data frame usually containing columns "parameter" and "value".
# Parameters:   parFile is a character string indicating the input file.
# Returns:      result, a data frame.
# Source:       A.R. Kronlund
.readParFile <- function( parFile="inputFile.par" )
{
  # Read the file and store as a dataframe.
  result <- read.table( file=parFile, as.is=TRUE, header=TRUE, skip=1,
                        quote="",sep=" " )
  result
}

# .readSimParFile (reads guiRef GUI parameters from file written by saveRefPars)
# Purpose:      Reads an ASCII file with reference points parameters.
# Parameters:   GUI parameters for guiRef. Output GUI parameter list.
# Returns:      NULL (invisibly)
# Side effects: Reads an ASCII control file written by saveRefPars and resets
#               the guiRef GUI parameters.
# Source:       A.R. Kronlund
.readSimParFile <- function( parFile=.FSIMPARS )
{
  val <- NULL
  
  tmpFile <- selectFile( initialfile=parFile,
               filetype=list( c(".par","par files") ), mode="open" ) 
  if ( is.null(tmpFile) )
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

# .saveSimToExcel (Save Sims in Excel format)
# Purpose:    Saves data in obj to a Microsoft Excel .xls file.
# Parameters: fname - a character string file name.
#             statObj - the result of a call to .calcPerfStats.
# Returns:    NULL
# Source:     Modified from saveExcel by T.K. Deering (PopSim.r)
.saveSimToExcel <- function( fname=.FFXLS )
{
  fileSaved <- TRUE
  
	# No name provided, or "Cancel" selected.
	if ( fname == "" )
	{
	  fileSaved <- FALSE
    msg       <- "Sims not saved, no file name specified"	
    return( saveStatus <- list( fileSaved=fileSaved, msg=msg ) )
  }
    
	# If fname already exists, then remove it.
	fileGone <- TRUE
	if ( file.exists( fname) )
    fileGone <- file.remove( fname )
    
  if ( fileGone )
  {
  	conn <- RODBC::odbcConnectExcel( fname, readOnly=FALSE )

    # Write the parameters by Sim.
    parNames <- c( "B0","rSteepness","gammaR","sigmaR","A50","A95","M","c1",
                   "c2","Linf","L1","vonK","sigmaLinf","qg","powerg","nAges",
                   "nGrps","nGear","sizeLim",
                   "L50Cg1","L50Cg2","L95Cg1","L95Cg2","L50Dg","L95Dg",
                   "dg","fg" )
    
    tmp <- unlist( simTracker[[1]][parNames] )
    tmpNames <- names(tmp)
        
    result <- matrix( NA, nrow=length(tmp),ncol=length(simTracker) )
    
    for ( i in 1:ncol(result) )
    {
      tmp <- unlist( simTracker[[i]][parNames] )
      result[,i] <- tmp
    }
    
	  # Save the Sim parameter estimates.
	  .excelTable( conn, result, "Parameters", names(simTracker), tmpNames )
  
    # Write the ADMB run variables and return codes by Sim.
    #admbNames <- 
                   
    #result <- matrix( NA, nrow=length(admbNames),ncol=length(simTracker) )
    
    #for ( i in 1:ncol(result) )
    #{
    #  tmp <- unlist( simTracker[[i]][admbNames] )
    #  result[,i] <- tmp
    #}
    
    # Save the ADMB run variables and return codes.
    #.excelTable( conn, result, "ADMB", names(simTracker), admbNames )
    
    # Write the model residuals.
    #residNames <- 
    
    #tmp <- unlist( simTracker[[1]][residNames] )
    #tmpNames <- names(tmp)
        
    #result <- matrix( NA, nrow=length(tmp),ncol=length(simTracker) )
    
    #for ( i in 1:ncol(result) )
    #{
    #  tmp <- unlist( trial[[i]][residNames] )
    #  result[,i] <- tmp
    #}
    
	  # Save the model residuals.
	  #.excelTable( conn, result, "Resduals", names(simTracker), tmpNames )                       
    
    odbcClose(conn)
    
    fileSaved <- TRUE
    msg <- paste( "mseRSim (.saveSimToExcel): Sims saved to Excel ",fname )
  }
  else
  {
    fileSaved <- FALSE
    msg <- c( "(.saveSimToExcel): Sims NOT saved to Excel, permission denied" )
    msg <- c( msg, paste( "(.saveSimToExcel): Close or rename file ",fname) )
  }
	return( list( fileSaved=fileSaved, msg=msg ) )
}

.saveSimToRdata <- function( simFile=.FSIMS, overWrite=FALSE )
{
  # Need to check if there are trials to save before calling this...
  if ( !overWrite )
  {
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    #tmpFile <- promptSaveFile( initialfile=simFile,
    #             filetype=list( c(".Rdata","Sim files") ) )
    tmpFile <- selectFile( initialfile=simFile,
                  filetype=list( c(".Rdata","Sim files")), mode="save" )

    simFile <- tmpFile
  }
  else
  {
    # Force an overwrite of .FTRLS.
    cat( "\n(.saveSimsToRDir) Overwriting :",simFile,"\n" )
  }

  save( simTracker, file=simFile )
 
  return( invisible() )  
}

# .saveSimParFile (saves the sim GUI parameters to a file).
# Purpose:      Saves an ASCII file with model parameters.
# Parameters:   Output file name for ASCII file containing sim GUI parameters.
# Returns:      NULL
# Side effects: Writes an ASCII control file configured for read usng .readSimParFile.
# Source:       A.R. Kronlund
.saveSimParFile <- function( parFile=.FSIMPARS, overWrite=FALSE )
{
  if ( !overWrite )
  {
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    #tmpFile <- promptSaveFile( initialfile=parFile,
    #             filetype=list( c(".par","par files") ) )
    tmpFile <- selectFile( initialfile=parFile,
                  filetype=list( c(".par","par files") ), mode="save" )
    if ( is.null(tmpFile) )
      return( invisible() )
    else
      parFile <- tmpFile
  }
  else
  {
    # Force an overwrite of .FSIMPARS.
    cat( "\n(.saveSimParFile) Overwriting :",parFile,"\n" )
  }

  win <- .getWinName()                           # Get the current window name
  guiInfo <- getWinVal( scope="L", winName=win ) # GUI information local scope

  cat( file=parFile, paste( "# mseRSim GUI parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )

  for ( i in 1:nrow(guiInfo$simPars) )
  {
    cat( file=parFile, guiInfo$simPars[i,1]," ",guiInfo$simPars[i,2],"\n", sep="",
         append=TRUE )
  }
  return( invisible() )  
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
	
	helpFiles <- c( "mseRabout.txt" )
	
	srcFiles <- paste( wkDir,   helpFiles, sep="/" )    # Source files.
	tarFiles <- paste( helpDir, helpFiles, sep="/" )    # Target files.
	file.copy( srcFiles, tarFiles, overwrite = TRUE )
	
	# Create .FTEMP directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .FTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  tempFiles <- c(
                   "mseRsim.r",
                   "mseRguiSimWin.txt",
                   "mseRabout.txt"
                )

  srcFiles <- paste( wkDir,   tempFiles, sep="/" )    # Source files.
  tarFiles <- paste( tempDir, tempFiles, sep="/" )    # Target files.
  file.copy( srcFiles, tarFiles, overwrite = TRUE)
  
  return(wkDir)
}