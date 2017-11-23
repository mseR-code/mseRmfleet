#------------------------------------------------------------------------------#
# (c) mseR-FinFish: Management Strategy Evaluation in R, Finfish Version 1.0   #
#                                                                              #
#     Copyright 2012 by A.R. Kronlund and S.P. Cox.                            #
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
#------------------------------------------------------------------------------#
#--                                                                          --#
#-- mseRguiTrackFuns.r: An mseR module to provides graphical user interface  --#
#-- capabilities for tracking project simulation results.  This relies on    --#
#-- the operating system (Windows, MAC, Linux etc) to manage folders.        --#
#--                                                                          --#
#-- Usage:        guiTrack()                                                 --#
#-- Side Effects: Involkes GUI for Tracking the feedback simulation results. --#
#--                                                                          --#
#-- Flow: guiTrack -> .mseRsetupTrack -> .wdSetup -> createWin -> .subTrack  --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--                                                                          --#
#-- First Implementation: 08-Aug-12.                                         --#
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
#   Modelling 2.00: users guide revised from Canadian Technical Report of      #
#   Fisheries and Aquatic Science 2674: v+146 p. Last updated October 23, 2008 #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
# guiTrack       : Run mseR simulation Tracking GUI.                           #
#                                                                              #
# GUI Error Checking/Validation:                                               #
#                                                                              #
# .validTrackPars: Check if the View GUI parameters are valid.                 #
#                                                                              #
# GUI Submit Control Functions:                                                #
#                                                                              #
# .subTrack      : Processes guiView submit actions, e.g., buttons.            #
#                                                                              #
# Note that it may be much more efficient to write an info file to each folder #
# that is updated every time the sim file is saved.  This could be in lisread  #
# format which would be very fast to read. Variables like iRank and iGroup are #
# more a property of the GUI than the simulation in any case.                  #
#                                                                              #
#     ## Information file for simulation  sim301220121205354  updated...       #
#     # simTime                                                                #
#     301220121205354                                                          #
#     # scenarioLabel                                                          #
#     S1                                                                       #
#     # mpLabel                                                                #
#     MP1                                                                      #
#     # tMP                                                                    #
#     50                                                                       #
#     # nT                                                                     #
#     60                                                                       #
#     # nReps                                                                  #
#     3                                                                        #
#     # rank                                                                   #
#     1                                                                        #
#     # group                                                                  #
#     1                                                                        #
#                                                                              #
# NOTE: When the trkObj updated, the ranking must be updated in case a folder  #
# was added or deleted.                                                        #
#                                                                              #
# Functions available in R for path and folder management:                     #
#                                                                              #
# dir.create   file.create   file.exists   file.remove  file.rename            #
# file.append  file.copy     file.info     path.expand  list.files             #
# basename     file.path     normalizePath                                     #
#                                                                              #
#                                                                              #
# trackData    - working directory copy of tracking data.                      #
# trackDataGui - subset of trackData for guiTrack window.                      #
# trackObj     - function argument.                                            #
#------------------------------------------------------------------------------#

# Approach:

# 1. mseR will assign folder names, but user may rename.
# 2. mserR must assign *.Rdata name and *.info file name within a folder.
# 3. The filename must be the same for *.Rdata and *.info.
# 4. guiTrack displays folder name, simulation filename, scenario label and
#    MP label.

#-----------------------------------------------------------------------------##
#-- mseR guiTrack Functions                                                 --##
#-----------------------------------------------------------------------------##

# guiTrack    (Run mseR feedback simulation tracker )
# Purpose:    Set up and run the Tracker GUI to manage simulation folders.
# Parameters: None
# Returns:    NULL (invisibly)
guiTrack <- function()
{
  return( .mseRsetupTrack("mseRguiTrack") )
}     # END function guiTrack


# .mseRsetupTrack  (mseR setup for GUI creation)
# Purpose:    Set up and run the specified GUI
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.mseRsetupTrack <- function( win )
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  require( tools )                   # For the file extension function
  options( useFancyQuotes=FALSE )    # Required for saveSimPars function.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  closeWin()                         # Close all open windows to prevent crashes.
  graphics.off()                     # Turn off any open graphics.
  
  goMenu <- TRUE                     # Assume menu cannot be created.

  wkDir <- .wdSetup()                # Setup working directory.

  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.
 
  trackData <- .getTrackingData( projDir=.PRJFLD )

  .saveTracking( trackData )

  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( file.path( wkDir, paste( win, "Win.txt", sep="" ) ) )
    
    # Get the GUI parameters and make scope local to this function.
    guiList <- getWinVal( scope="L", winName=win )
    
    # Initialize the guiView action.
    if ( win=="mseRguiTrack" )
    {
      
      guiChanges <- guiList
      guiChanges$trkFolder <- .PRJFLD
      
      setWinVal( guiChanges, winName=win )
      .setGuiTrackState()
    }
  }
  else
    cat( "\nERROR (.mseRsetupTrack): Menu creation not possible:\n ")

  return( invisible() )
}     # END function .mseRsetupTrack


.setGuiTrackState <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Disable the simTracker if notebook page "Perf" or "Options", i.e., 5 or 6.
  
  isValid <- .validTrackPars()

  if ( isValid )
  {
    setWidgetColor( name="trackDataGui", winName=win, entrybg="lightgreen" )
  }
  else
  {
    setWidgetColor( name="trackDataGui",  winName=win, entrybg="coral1" )
  }
  
  if ( sum( trackDataGui$simFile!="" )==0 )
  {
    setWidgetColor( name="trackDataGui",  winName=win, entrybg="coral1" )
  }

  isValid  
}     # END function .setGuiTrackState


# .subTrack   (Processes guiView submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiTrack program flow:
#             - Attempts to check validity of the GUI parameters;
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subTrack <- function()
{
  win        <- .getWinName()                      # Get the current window name
  gvar       <- paste( ".", win, sep="" )          # Global variable name
  act        <- getWinAct()[1]                     # Get last menu window action

  guiInfo    <- getWinVal( scope="L",winName=win ) # GUI information local scope
  guiChanges <- guiInfo                            # Make a copy for changes.

  # trackDataGui was updated on invoking guiTrack via mseRsetupTrack function.
  # But while GUI was idling, somebody might have moved folders, so check the
  # trackDataGui against the project folder and update as necessary.
  
  trackDataNew  <- .getTrackingData( .PRJFLD )
  nSims         <- .getNumSims( trackDataNew )
  sameTrackData <- TRUE

  # Are the number of simulations the same?
  if ( .getNumSims(trackDataGui)!=nSims )
  {
    cat( "\nMSG (.subTrack) Number of simulations in project folder has changed...\n" )
    sameTrackData <- FALSE
  }      
  # Are the simulation folder names the same?
  else
  {
    if ( !all( trackDataGui$simFile[1:nSims]==trackDataNew$simFile[1:nSims] ) )
    {
       cat( "\nMSG (.subTrack) Simulation folders in project have changed...\n" )
       sameTrackData <- FALSE
    }
  } 

  if ( !sameTrackData )
  {
    cat( "\nWARNING (.subTrack) Tracking information has changed, review ranks and groups.\n" )
    guiChanges$trackDataGui <- .saveTracking( trackDataNew )
    setWinVal( guiChanges, win )
  }

  valid      <- .setGuiTrackState()                 # Are guiTrack parameters valid?
  
  if ( valid )
  {
    update <- FALSE
  
    # Was the trkObj touched?
    if ( act=="trackDataGui" || act=="trkUpdate" )
    {
      update <- TRUE
    }
    
    # Sorting options.
    if ( act=="trkName" || act=="trkRank" || act=="trkGrp" )
    {
      if ( sum(c(trkName, trkRank,trkGrp))==0 )
      {
        sortVars <- c( TRUE, FALSE, FALSE )
        guiChanges$trkName <- TRUE
        trkName <- TRUE
        setWinVal( guiChanges, win )
      }
      update <- TRUE
    }
    
    if ( act=="trkBatch" )
    {
      update <- TRUE
      on.exit( guiBatch() )
    }
    
    if ( act=="trkSim" )
    {
      update <- TRUE
      on.exit( guiSim() )
    }
    
    if ( act=="trkView" )
    {
      update <- TRUE
      on.exit( guiView() )
    }                

    if ( act=="trkPerf" )
    {
      update <- TRUE
      on.exit( guiPerf() )
    }      
  
    # Cancel the Tracker GUI (leave graphics on).
    if ( act=="trkExit" )
    {
      cat( "\nMSG (.subTrack) Exiting guiTrack at ",date(),"\n" )
      closeWin()
    }
    
    # Select another project folder.
    if ( act=="trkSelect" )
    {
      tmpDir <- selectDir( initialdir=.PRJFLD, title="Select project folder", usewidget="trkFolder" )
      
      if ( tmpDir!="" )
      {
        assign( ".PRJFLD", tmpDir, pos=1 )
        guiChanges$trkFolder <- .PRJFLD
        update <- TRUE
      }
      else
      {
        alarm()
        cat( "\nMSG (.subTrack) No folder was selected, project folder unchanged.\n ")
      }
    }
    
    if ( update )
    {
      trkObj <- .getTrackingData( .PRJFLD )
      trkObj <- .sortTrackingData( trkObj, sortVars=c(trkName,trkRank,trkGrp) )
      trkObj <- .rankSims( projDir=.PRJFLD, trkObj )
      guiChanges$trackDataGui <- .saveTracking( trkObj )
      setWinVal( guiChanges, win )
    }     # if update
    
  }     # if valid.
  else
  {
    bringToTop(-1)
    # Cancel the guiTrack GUI (leave graphics on).
    if ( act=="trkCancel" )
    {
      cat( "\nMSG (.subTrack) Tracking object not saved, invalid entries.\n" )
      closeWin()
    }
  }
  return( invisible() )  
}     # END function .subTrack


.saveTracking <- function( trkObj,
  trackGuiVars=c( "simFolder","scenario","mp","simFile","nReps","rank","group" ) )
# Save the tracking data for guiTrack and copy of full tracking to working dir.
# Return the tracking data for gui.
{
  result <- trkObj[ ,trackGuiVars ]
  assign( "trackDataGui", result, pos=1 )
  assign( "trackData", trkObj, pos=1 )
  
  result
}     # END function .saveTracking


# .validTrackPars (valid parameters for Tracker GUI):
# Purpose:       Check whether the parameters supplied in Tracker GUI are valid.
#                If invalid, display an error message in the R console and
#                clear the invalid field.
#                If it is a correctable field, corrects it in the GUI and does
#                not flag it as invalid
# Parameters:    None
# GUI inputs:    Parameters taken directly from active guiTrack.
# Returns:       TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:   Clears invalid fields
#                Corrects correctable fields (if any)
.validTrackPars <- function()
{
  # Get the GUI  values and make them local to this function.
  guiInfo <- getWinVal( scope="L" )
  
  guiChanges <- guiInfo
  isValid <- TRUE

  # Check to ensure there is at least one simFile.
  if ( sum( trackDataGui$simFile!="" )==0 )
  {
    cat( "\nWARNING (.validTrackPars) No valid simulation folder is in ",.PRJFLD,"\n" )
    #isValid <- FALSE
  }

  # Check to ensure ranks are integer non-negative.
  if ( any( is.na(trackDataGui$rank ) ) )
  {
    cat( "\nWARNING (.validTrackPars) Missing values for ranks not allowed, set to 0.\n" )
    trackDataGui$rank <- ifelse( is.na(trackDataGui$rank),0,trackDataGui$rank )
    guiChanges$trackDataGui <- trackDataGui
  }
  
  if ( any( !is.wholenumber(trackDataGui$rank) ) )
  {
    cat( "\nERROR (.validTrackPars) Integer ranks are required.\n" )
    trackDataGui$rank <- ifelse( !is.wholenumber(trackDataGui$rank), NA,
                                 trackDataGui$rank )
    guiChanges$trackDataGui <- trackDataGui
    isValid <- FALSE
  }
  
  if ( all( !is.na( trackDataGui$rank ) ) && any( trackDataGui$rank < 0 ) )
  {
    cat( "\nERROR (.validTrackPars) Ranks must be non-negative.\n" )
    trackDataGui$rank <- ifelse( trackDataGui$rank < 0, NA, trackDataGui$rank )
    guiChanges$trackDataGui <- trackDataGui
    isValid <- FALSE
  }
  
  # Check to ensure groups are not missing.
  if ( any( is.na(trackDataGui$group ) ) )
  {
    cat( "\nWARNING (.validTrackPars) Missing values for groups not allowed, set to 0.\n" )
    trackDataGui$rank <- ifelse( is.na(trackDataGui$rank),0,trackDataGui$rank )
    guiChanges$trackDataGui <- trackDataGui
  }
  
  # Check to ensure all nT are equal.
  if ( any( trackDataGui$nT != trackDataGui$nT[1] ) )
  {
    cat( "\nERROR (.validTrackPars) Number of years nT differs among simulations.\n" )
    isValid <- FALSE
  }
  
  # Check to ensure all tMP are equal.
  if ( any( trackDataGui$tMP != trackDataGui$tMP[1] ) )
  {
    cat( "\nERROR (.validTrackPars) First year of procedure tMP differs among simulations.\n" )
    isValid <- FALSE
  }
  
  # Do not force all nReps to be equal.
  
  # Set the changes into the GUI.  
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validTrackPars

#------------------------------------------------------------------------------#
#-- Tracking object management functions                                     --#
#------------------------------------------------------------------------------#

.getNumSims <- function( trackObj )
{
  # This queries the tracking object, not the project folder.
  # If there is no tracking objective, return nSim=0.
  
  nSim <- 0
  if ( nrow(trackObj) > 0 )
  {
    idx <- trackObj[,"simFile"]!=""
    nSim <- sum( idx )
  }
  nSim 
}     # END function .getNumSims


.getTrackingData <- function( projDir=.PRJFLD )
{
  # To be a valid sim, a folder must contain a *.Rdata file and *.info file
  # with the same filename.
  
  # Get the list of directories in the project folder, but not project folder.
  # Also, the full.names argument does not appear to work in list.dirs, so use
  # basename to extract just the directory name, not the path.  This approach
  # has to be done since the User may put files other than simulation folders
  # in the project folder.
  
  folderList <- basename( list.dirs( projDir, recursive=FALSE ) )

  # Get the number of folders in the project directory.
  nFolders <- length( folderList )
  
  # Make a dataframe of the required size, 0 rows if project folder empty.
  if ( nFolders > .MAXSIM-10 )
  {
    assign( ".MAXSIM", nFolders+50, pos=1 )
    cat( "\nMSG (.getTrackingData) Maximum number of simulations increased to",
         .MAXSIM,"\n" )
  }
  
  # Initialize the tracking object to .MAXSIM rows.
  result      <- .initTrackingObj()
  finalResult <- result
  finalResult[ ,"simFile" ] <- ""  

  if ( nFolders > 0 )
  {
    # Loop through folders gathering data and loading into result.
    for ( i in 1:nFolders )
    {
      validSim <- TRUE                 # Assume simulation valid, until otherwise.
 
      # Find all the files in the simulation folder.
      folderFiles <- list.files( file.path( projDir,folderList[i] ) )
      
      # Step 1: Is there an  *.info file?
      
      infoFileIdx <- file_ext( folderFiles )=="info"
      
      # Enforce only one *.info file.
      if ( sum( infoFileIdx ) == 1 )
      {
        infoFile     <- folderFiles[ infoFileIdx ]
        infoFilePath <- file.path( projDir, folderList[i], infoFile )
        simName      <- substring( infoFile, 1, nchar(infoFile)-5 )        
        
        tmp <- .readSimInfo( infoFilePath )
        
        # Is the *.info file valid?
        infoNames <- c( "simTime","scenarioLabel","mpLabel","tMP","nT","nReps",
                        "rank","group" )
                        
        if ( length(tmp)!=length(infoNames) )
          validSim <- FALSE
        else
        {
          if ( !all( names( tmp )==infoNames ) )
             validSim <- FALSE
        }
  
        if ( validSim )
        {
          # Copy rank and group to tracking object.
          result[ i,"simFolder" ] <- folderList[i]
          result[ i, "simName" ]  <- simName        
          result[ i,"simDate" ]   <- tmp$simTime
          result[ i,"scenario" ]  <- tmp$scenarioLabel
          result[ i,"mp" ]        <- tmp$mpLabel  
          result[ i,"tMP" ]       <- tmp$tMP
          result[ i,"nT"  ]       <- tmp$nT
          result[ i,"nReps" ]     <- tmp$nReps
          result[ i,"rank" ]      <- tmp$rank
          result[ i,"group" ]     <- tmp$group
        }
      }
      else
      {
        validSim <- FALSE
      }

      # Step 2: Is there a *.Rdata file that matches the *.info file?
      if ( validSim )
      {
        # Get the filename without extension of the *.info file.
        rDataFile     <- paste( simName,".Rdata",sep="" )
        rDataFilePath <- file.path( projDir, folderList[i], rDataFile )
        
        if ( file.exists( rDataFilePath ) )
        {
          result[ i, "simFile" ] <- rDataFile
        }
        else
        {
          result[ i,"simFile" ] <- "Invalid"
          validSim <- FALSE
        }
      }
    }     # End loop over folders.
  }     # if nFolders > 0

  # Make a copy of result that eliminates all invalid entries.
  iSim <- 0
  for ( i in 1:nrow(result) )
  {
    if ( result$simFile[i]!="Invalid" )
    {
      iSim <- iSim + 1
      finalResult[ iSim, ] <- result[ i, ]
    }
    else
    {
      finalResult$simFile[i] <- ""
    }
  }
  finalResult
}     # END function .getTrackingData


.initTrackingObj <- function( n=.MAXSIM )
{
  result <- data.frame( 
              simFolder=character(n),
              simName=character(n),
              simFile=rep( "Invalid", n ),
              simDate=character(n),
              scenario=character(n), mp=character(n),
              tMP=numeric(n), nT=numeric(n), nReps=numeric(n),
              rank=numeric(n), group=numeric(n),
              stringsAsFactors=FALSE )
  result
}     # END function .initTrackingObj


.rankSims <- function( projDir=.PRJFLD, trackObj )
{
  nSims <- .getNumSims( trackObj )
  
  if ( nSims > 0 )
  {
    tmp <- trackObj[ c(1:nSims), ]
    
    # Get the current ranks from tracking object.
    currRank <- tmp$rank
  
    # Make ranks less than 0 equal to a very large number.
    iRank <- ifelse( currRank < 0, 1e6, currRank   )
    iRank <- ifelse( currRank ==0,   1, currRank+1 )
  
    # Rationalize the ranks.
    newRank <- rank( iRank, ties.method="first" )
    
    # Update the rank in the tracking object.
    trackObj$rank[1:nSims] <- newRank    
  
    # Now change ranks in the info files in each simulation folder in project.
    for ( i in 1:nSims )
    {
      # Read the info file.
      infoFilePath <- file.path( .PRJFLD, trackObj$simFolder[i],
                                 paste( trackObj$simName[i],".info",sep="" ) )
      
      tmp <- .readSimInfo( infoFilePath )
      
      # Update the rank variable.
      tmp$rank <- trackObj$rank[i]
      
      # Re-write the info file.
      .writeSimInfo( info=tmp, trackObj$simFolder[i], infoFilePath )
    }
    if ( .WHINE > 0 )
      cat( "\nMSG (.rankSims) Simulations re-ranked...\n" )
  }
  trackObj
}     # END function .rankSims


.readSimInfo <- function( fileName )
{
  # Function writes information about the simulation in lisread format.
  # Comments preceded by ##, variable names preceded by #

  result <- lisread( fname=fileName )
  result
}     # END function .readSimInfo


.sortTrackingData <- function( trackObj,
                 sortNames=c("simName","simDate","rank","group"),
                 sortVars=c(TRUE, FALSE, FALSE, FALSE) )
{
  nSims <- .getNumSims( trackObj )
  
  # Extract completed rows of tracking object.    
  tmp <- trackObj[ c(1:nSims), ]

  # This is a bit of trickery to allow the sequence of columns to be
  # built using the sortNames where sortVars is TRUE.
            
  token <- paste( "order(",paste( "tmp",sortNames[sortVars],sep="$", collapse=","),")" )
  idx   <- eval( parse( text=token ) )
  
  # Now sort and replace the current rows of the tracking object with new sort.
  tmp <- tmp[ idx, ]
  trackObj[ c(1:nSims),] <- tmp

  if ( .WHINE > 0 )  
    cat( "\nMSG (.sortTrackingData) Tracking object sorted...\n" )
  
  trackObj
}     # END function .sortTrackingData


.writeSimInfo <- function( info=list( simTime=stamp, scenarioLabel="None",
                   mpLabel="None", tMP=0,  nT=0, rank=0, group=0, nReps=0 ),
                   simFolder, fileName )
{
  # Function writes information about the simulation in lisread format.
  # Comments preceded by ##, variable names preceded by #

  cat( file=fileName,
       "## Information file for simulation ",simFolder," updated ",date(),".\n" )
  
  infoNames <- names( info )

  for ( i in 1:length(info) )
  {
    cat( file=fileName, paste( "# ",infoNames[i],"\n", sep="" ), append=TRUE )
    if ( infoNames[i]=="simTime" )
      cat( file=fileName, as.character( info[[i]] ), "\n",       append=TRUE )
    else
      cat( file=fileName, info[[i]],"\n",                        append=TRUE )
  }
  
  return( invisible() )
}     # END function .writeSimInfo

#------------------------------------------------------------------------------#
#-- Helper Functions                                                         --#
#------------------------------------------------------------------------------#

.getStamp <- function()
{
  stamp <- paste( format(Sys.time(),format="%d%m%Y%H%M%OS1" ),sep="" )
  
  # The period must be double escaped with \\.
  stamp <- gsub( "\\.","", stamp )
  return( stamp )
}

is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)
{
  result <- abs(x - round(x)) < tol
  result
}
