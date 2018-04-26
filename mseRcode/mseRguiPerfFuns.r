# Tasks:
# 1. Add report to console from .getEstimationFails to inform user.

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
#-- mseRguiPerfFuns.r: An mseR module that provides graphical user interface --#
#-- capablities for calculating and viewing performance statistics from the --#
#-- feedback simulation of a fisheries management strategy evaluation.       --#
#--                                                                          --#
#-- Usage:        guiPerf()                                                  --#
#-- Side Effects: Invokes GUI for Viewing the feedback simulation results.   --#
#--                                                                          --#
#-- Logic Flow: guiPerf -> .mseRsetup -> .wdSetup -> createWin -> .subPerf   --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 27-Jul-12 from mseR V2, mseR-Prawn and mseR2011.   --#
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
# guiPerf       : Run mseR simulation Performance measures GUI.                #
#                                                                              #
# GUI Error Checking/Validation:                                               #
#                                                                              #
# .validPerfPars: Check if the Perf GUI parameters are valid.                  #
#                                                                              #
# GUI Submit Control Functions:                                                #
#                                                                              #
# .subPerf      : Processes guiPerf submit actions, e.g., buttons.             #
#                                                                              #
#------------------------------------------------------------------------------#

##----------------------------------------------------------------------------##
#-- mseR guiPerf Functions                                                  --##
#-----------------------------------------------------------------------------##

# guiPerf     (Run mseR performance statistics GUI )
# Purpose:    Set up and run the performance statistics prospective evaluation.
# Parameters: None
# Returns:    NULL (invisibly)
guiPerf <- function()
{
  return( .mseRsetupPerf("mseRguiPerf") )
}

# .mseRsetupPerf  (mseR setup for GUI creation)
# Purpose:    Set up and run the specified GUI
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.mseRsetupPerf <- function(win)
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  require( KernSmooth )              # For kernel density estimation.
  require( tools )                   # For file_ext etc.
  options( useFancyQuotes=FALSE )    # Required for saveSimPars function.
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  wkDir <- .wdSetup()
  cat( "\nMSG (.mseRsetupPerf) Working directory setup in ",wkDir,"\n" )   
  
  closeWin()                         # Close all open windows to prevent crashes.
  graphics.off()                     # Turn off any open graphics.
  
  .mseRinitProject( wkDir )          # Initialize mseR project, paths, options.  
  
  goMenu <- TRUE                     # Assume menu cannot be created.

  # Get the tracking data for the project folder.
  trackData <- .getTrackingData( projDir=.PRJFLD )
  nSims     <- .getNumSims( trackData )
  
  if ( nSims == 0 )
  {
    if ( exists( ".guiPerfPars" ) )
      rm( ".guiPerfPars", pos=1 )
    goMenu <- FALSE
  }
  
  # Save the simulation list for guiPerf initialization, local and global.
  pfSimList <- .initPfSimList( trackData )
  assign( "pfSimList", pfSimList, pos=1 )
  
  # There are valid simulations, go ahead and initialize guiView.
  # if ( goMenu )
  #{

  # Initialize the GUI from the description file using PBSmodelling createWin.
  createWin( paste( wkDir, "/", win, "Win.txt", sep="" ) )
   
  if ( goMenu )
  { 
    # Get the GUI parameters and make scope local to this function.
    guiInfo    <- getWinVal( scope="L", winName=win )
    guiChanges <- list()
    
    # Need a restore of .guiPerfPars here...
    if ( exists( ".guiPerfPars" ) )
    {
      guiChanges <- .guiPerfPars
      guiChanges$pfSimList <- pfSimList
      
      if ( sum( guiChanges$pfSimList$Select ) > .getNumSims( trackData ) )
      {
        pfSimList$Select    <- rep( FALSE, nrow(trackData) )
        pfSimList$Select[1] <- TRUE
      }
    }

    nT  <- unique( trackData$nT[1:nSims] )
    tMP <- unique( trackData$tMP[1:nSims] )
    
    guiChanges$pfNt      <- nT
    guiChanges$pfTmp     <- tMP
    
    
    #Revised by KHolt so that default is to use time periods I want for 2017 MSE ===========
    
    # Original:
    #guiChanges$pfShort1  <- tMP
    #guiChanges$pfShort2  <- tMP + trunc( (nT-tMP+1) / 3.0 )
    #guiChanges$pfMed1    <- guiChanges$pfShort2 + 1
    #guiChanges$pfMed2    <- guiChanges$pfMed1 + trunc( (nT-tMP+1) /3.0 )
    #guiChanges$pfLong1   <- guiChanges$pfMed2 + 1
    #guiChanges$pfLong2   <- nT
    
    # Revised:
    guiChanges$pfShort1  <- tMP
    guiChanges$pfShort2  <- tMP+9
    guiChanges$pfMed1    <- guiChanges$pfShort2 + 1
    guiChanges$pfMed2    <- nT
    guiChanges$pfLong1   <- tMP
    guiChanges$pfLong2   <- nT
    
    # =========================================================================

    setWinVal( guiChanges, winName=win )
    .setGuiPerfState( nSims )
      
    # Save a copy of the GUI parameters in the working directory.
    guiInfo <- getWinVal( scope="L", winName=win )    
    assign( ".guiPerfPars",guiInfo,pos=1 )

    # Force plotting action on menu creation with default selection.
    valid <- .validPerfPars()
    if ( valid )
      .doGuiPerfPlots( trackData )
      #.plotStatus( "mseRsetupPerf called...\n" )
    else
      cat( "\nMSG (mseRsetupPerf) Invalid GUI parameters...\n" )
  }
  else
  {
    setWidgetState( varname="pfPlotType", state="disabled" )  
    cat( "\nWARNING (.mseRsetupPerf): No simulations found in project folder",
         .PRJFLD, "\n" )
  }

  return( invisible() )
}     # END function .mseRsetupPerf


# .subPerf    (Processes guiPerf submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiView program flow:
#             - Attempts to check validity of the GUI parameters;
#             - Determines what plot is displayed;
#             - EXIT the GUI.
# Parameters: NONE
# Returns:    NULL (invisibly)
#
# Notes:      Any information read from a text box has a \n at the end of the 
#             input that needs to be removed, in most cases, before use.
# Source:     A.R. Kronlund
.subPerf <- function()
{
  win        <- .getWinName()                       # Get the current window name
  gvar       <- paste( ".", win, sep="" )           # Global variable name
  
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local scope
  
  act        <- getWinAct()[1]                      # Get last window action
  guiChanges <- guiInfo                             # Make a copy for changes.

  isExit    <- FALSE                                # Exiting the GUI?
  trackData <- .getTrackingData( .PRJFLD )
  nSims     <- .getNumSims( trackData )
  
  .setGuiPerfState( nSims )

  valid <- .validPerfPars()                         # Are guiPerf parameters valid?
  if ( valid )
  {
    # EXIT the Performance GUI (leave graphics on).
    if ( act=="pfExit" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )
      
      closeWin( win )
    }
    
    if ( act=="pfGroup" )
    {
      if ( pfGroup > 0 )
      {
        selectOld        <- pfSimList$Select
        pfSimList$Select <- FALSE
        pfSimList$Select[ pfGroup==pfSimList$group ] <- TRUE
        if ( sum( pfSimList$Select )== 0 )
        {
          pfSimList$Select <- selectOld
          cat( "\nMSG (.subPerf) No group identifiers match ", pfGroup,"\n" )
        }
        guiChanges$pfSimList <- pfSimList
        setWinVal( guiChanges )
      }
    }
    
    if ( act=="pfStats" )
    {
      cat( "\nMSG (guiPerf) Calculating performance statistics...\n" )
      
      # Steps in calculating and saving performance statistics:
      # 1. If default results exist, then query overwrite or new name, else go.
      # 2. Call performance statistics (calcPerfStats).
      # 3. Save results.
      # a. Save the parameters from blob (each row is a simulation).
      # b. Save the guiPerf parameters.
      # c. Save the performance statistics in a "spreadsheet" view.
      # d. Save the performance statistics in a "normalized" view.
      # e. Save the replicate statistics in a "spreadsheet" view.
      #    This is a worksheet of dimensions nSim*nReps X nStats + header.
      #    For example, with 10 simulations, 12 statistics, and 100 replicates
      #    we will have 10*100=10,000 rows and a little over 12 columns.
      
      # Step 1:
      
      tmpFile <- .DEFSTATFILE
     
      nSims <- .getNumSims( trackData )

      # Step 2: Load an Rdata working directory containing a list called blob.
      for ( i in 1:nSims )
      {
        simFile     <- trackData[ i, "simFile" ]
        simFolder   <- trackData[ i, "simFolder" ]
        simFilePath <- file.path( .PRJFLD, simFolder, simFile )

        cat( "\nMSG (.subPerf) Loading",simFilePath,"...\n" )    
        load( file=simFilePath )
        assign( "blob", blob, pos=1 )            
        
        ctlPars <- blob$ctlPars
        # Initialize storage objects.
        if ( i==1 )
        {
          parTable <- data.frame( parameter=character(nrow(ctlPars)),
                        matrix( "", nrow=nrow(ctlPars),ncol=nSims ) )

          names( parTable ) <- c( "parameter",paste( "Sim",c(1:nSims),sep="" ) )
          parTable$parameter <- ctlPars[ ,"parameter" ]
        }
  
        parTable[ , i+1 ] <- blob$ctlPars[ ,"value" ]

        gc()
      }
      
      # Step 3: Calculate Performance Statistics over all simulations.
      
      # These are from guiPerf, need to add multipliers, etc.
      perfPars <- guiInfo[ c( "pfShort1", "pfShort2", "pfMed1",
                              "pfMed2", "pfLong1", "pfLong2",
                              "pfDep1", "pfYear1", "pfProb1",
                              "pfDep2", "pfYear2", "pfProb2",
                              "pfDep3", "pfYear3", "pfProb3",                                                                
                              "pfQupper", "pfQlower",
                              "pfLimitMultBmsy", "pfUpperMultBmsy","pfTargetMultBmsy" ) ]
      perfPars <- c( runDate=date(), perfPars )

      perfPars <- data.frame( cbind(parameter=names(perfPars),value=perfPars) )
      
      perfStats <- .calcPerfStats()
      
      # Step 4: Save to Excel (Windows) or CSV (Mac) files.

      # Save to Excel.
      #if ( version$platform!="x86_64-pc-mingw32" )
      #{
      #  fName <- file.path( .PRJFLD, .DEFSTATFLD, basename(tmpFile) )
      #  saveToExcel( fName, list(
      #               trackTable=trackTable,
      #               parTable=parTable,
      #               perfParTable=perfPars,
      #               perfTable1=perfStats$simSummary1,
      #               perfTable2=perfStats$simSummary2 ) )
          
        # Open Excel file.             
      #  openFile( tmpFile )
      #}
      #else
      #{
        n <- nchar( tmpFile )
        fName <- substring( tmpFile,1, n-4 )
        fName <- file.path( .PRJFLD, .DEFSTATFLD, fName )

        write.csv( trackData, paste( fName,"_trackTable",".csv", sep="" ) )
        write.csv( parTable, paste( fName,"_parTable",".csv", sep="" ) )
        perfPars <- cbind( as.character(perfPars[,1]), as.character( perfPars[,2] ) )
        write.csv( perfPars, paste( fName,"_perfParTable",".csv", sep="" ) )
        #write.table( x=perfStats$summary1, file=paste( fName,"_perfTable1",".csv", sep="" ),sep="," )
        write.csv( perfStats$summary1, paste( fName,"_perfTable1",".csv", sep="" ), row.names=F) # K.Holt added this line & disabled line above for 2017 MSE
        write.csv( perfStats$summary2, paste( fName,"_perfTable2",".csv", sep="" ), row.names=F )
        
        # calcObjTable(fName) # Calls function from objectivesTable2017.r to summarize performance measures relative to 2017 MSE objectives 
      #}
    }
    
    # Clear the graphics windows.
    if ( act=="pfReset" )
    {
      graphics.off()

      closeWin()
      if ( exists( ".guiPerfPars", where=1 ) )
      {
        rm( .guiPerfPars, pos=1 )
        cat( "\nMSG (.subPerf) Removed .guiPerfPars...\n" )
      }
      assign( ".CTLFILE", .DEFCTLFILE, pos=1 )
      assign( ".PFJFLD",  .PRJFLD,     pos=1 )
      
      cat( "\nMSG (.subPerf) Reset Project Folder to ",.PRJFLD,"and Control File to ",
           .CTLFILE,"\n" )
                 
      on.exit( guiPerf() )          
    }   
    
    # Save plot(s) as an enhanced Windows metafile.
    if ( act=="pfSaveEMF" )
    {
      # Get simulation folder path.
      emfFile <- file.path( .PRJFLD, .DEFPLTFLD, pfPlotType )
      
      savePlot( filename=emfFile,
                type="emf", restoreConsole = .RESTORECONSOLE )
    }
              
    # Save plot(s) as a PDF file.
    if ( act=="pfSavePDF" )
    {
      # Get simulation folder path.
      pdfFile <- file.path( .PRJFLD, .DEFPLTFLD, pfPlotType )
      savePlot( filename=pdfFile,
                type="pdf", restoreConsole = .RESTORECONSOLE )
    }

    # BATCH generator.
    if ( act=="pfBatch" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiBatch() )
      isExit <- TRUE
    }  
    
    # OPTIONS.
    if ( act=="pfOpt" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiOpt() )
      isExit <- TRUE
    }  
  
    # VIEW results.
    if ( act=="pfView" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiView() )
      isExit <- TRUE
    }  

    # TRACKing GUI.
    if ( act=="pfTrack" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )
    
      closeWin( .getWinName() )       
      on.exit( guiTrack() )
      isExit <- TRUE
    }  
  
    # Simulation GUI.
    if ( act=="pfSim" )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )
      cat( "\nMSG (.subPerf) Exiting... saved GUI parameters to .guiPerfPars.\n" )

      closeWin( .getWinName() )       
      on.exit( guiSim() )
      isExit <- TRUE
    }                               
    
    # Save the Perf GUI parameters to a global in the working environment.
    if ( (!act=="pfExit") && (act!="pfReset") && nSims > 0 )
    {
      .doGuiPerfPlots( trackData )
    }
    
    if ( isExit )
    {
      guiInfo <- getWinVal( scope="L", winName=win )
      assign( ".guiPerfPars", guiInfo, pos=1 )    
    }
  }
  else
  {
    bringToTop( -1 )
    # EXIT the Performance GUI (leave graphics on).
    if ( act=="pfExit" )
    {
      closeWin( win )
    }
  }
  
  return( invisible() )  
}     # END function .subPerf

# .validPerfPars (valid parameters for performance statistics GUI):
# Purpose:       Check whether the parameters supplied in Perf GUI are valid.
#                If invalid, display an error message in the R console and
#                clear the invalid field.
#                If it is a correctable field, corrects it in the GUI and does
#                not flag it as invalid
# Parameters:    None
# GUI inputs:    Parameters taken directly from active mseRguiPerf. 
# Returns:       TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:   Clears invalid fields
#                Corrects correctable fields (if any)
.validPerfPars <- function()
{
  intVal <- function( x )
  {
    # Is the value of x an integer?  There must be a better way...
    result <- ifelse( (trunc(x)-x) == 0.0,TRUE,FALSE )
    result
  }
  
  # Get the GUI  values and make them local to this function.
  guiInfo    <- getWinVal( scope="L" )
  guiChanges <- list()                  
  isValid    <- TRUE
  
  tMP        <- pfTmp
  nT         <- pfNt
  
  if ( is.na(pfShort1) || (pfShort1 < tMP) || (pfShort1 >= pfShort2) || (pfShort1 > (nT-1)) || (!intVal(pfShort1)) )
  {
    cat( "\nMSG (.validPerfPars) Time 1 of Short-term period must be integer and tMP <= t1 < t2 < (nT-1).\n" )
    guiChanges$pfShort1 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfShort2) || (pfShort2 < (tMP+1)) || (pfShort2 <= pfShort1) || (pfShort2 > nT) || (!intVal(pfShort2)) )
  {
    cat( "\nMSG (.validPerfPars) Time 2 of Short-term period must be integer and tMP < t2 <= nT, and t1 < t2.\n" )
    guiChanges$pfShort2 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfShort1) || (pfMed1 < tMP) || (pfMed1 >= pfMed2) || (pfMed1 > (nT-1)) || (!intVal(pfMed1)) )
  {
    cat( "\nMSG (.validPerfPars) Time 1 of Medium-term period must be integer and tMP <= t1 < t2 < (nT-1).\n" )
    guiChanges$pfMed1 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMed2) || (pfMed2 < (tMP+1)) || (pfMed2 <= pfMed1) || (pfMed2 > nT) || (!intVal(pfMed2)) )
  {
    cat( "\nMSG (.validPerfPars) Time 2 of Medium-term period must be integer and tMP < t2 <= nT, and t1 < t2.\n" )
    guiChanges$pfMed2 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfLong1) || (pfLong1 < tMP) || (pfLong1 >= pfLong2) || (pfLong1 > (nT-1)) || (!intVal(pfLong1)) )
  {
    cat( "\nMSG (.validPerfPars) Time 1 of Long-term period must be integer and tMP <= t1 < t2 < (nT-1).\n" )
    guiChanges$pfLong1 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfLong2) || (pfLong2 < (tMP+1)) || (pfLong2 <= pfLong1) || (pfLong2 > nT) || (!intVal(pfLong2)) )
  {
    cat( "\nMSG (.validPerfPars) Time 2 of Long-term period must be integer and tMP < t2 <= nT, and t1 < t2.\n" )
    guiChanges$pfLong2 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfDep1) || (pfDep1<=0.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 1 depletion must be pfDep1 > 0.0.\n" )
    guiChanges$pfDep1 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfDep2) || (pfDep2<=0.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 2 depletion must be pfDep2 > 0.0.\n" )
    guiChanges$pfDep2 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfDep3) || (pfDep3<=0.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 3 depletion must be pfDep3 > 0.0.\n" )
    guiChanges$pfDep3 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfYear1) || (pfYear1<tMP) || (pfYear1>nT) || (!intVal(pfYear1)) )
  {
    cat( "\nMSG (.validPerfPars) Obj 1 year must be integer and tMP <= pfYear1 <= nT.\n" )
    guiChanges$pfYear1 <- nT
    isValid <- FALSE
  }
  
  if ( is.na(pfYear2) || (pfYear2<tMP) || (pfYear2>nT) || (!intVal(pfYear2)) )
  {
    cat( "\nMSG (.validPerfPars) Obj 2 year must be integer and tMP <= pfYear2 <= nT.\n" )
    guiChanges$pfYear2 <- nT
    isValid <- FALSE
  }
  
  if ( is.na(pfYear3) || (pfYear3<tMP) || (pfYear3>nT) || (!intVal(pfYear3)) )
  {
    cat( "\nMSG (.validPerfPars) Obj 3 year must be integer and tMP <= pfYear3 <= nT.\n" )
    guiChanges$pfYear3 <- nT
    isValid <- FALSE
  }
  
  if ( is.na(pfProb1) || (pfProb1<0.0) || (pfProb1>1.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 1 certainty must be 0.0 <= pfProb1 <= 1.0.\n" )
    guiChanges$pfProb1 <- NA
    isValid <- FALSE
  }

  if ( is.na(pfProb2) || (pfProb2<0.0) || (pfProb2>1.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 2 certainty must be 0.0 <= pfProb2 <= 1.0.\n" )
    guiChanges$pfProb2 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfProb3) || (pfProb3<0.0) || (pfProb3>1.0) )
  {
    cat( "\nMSG (.validPerfPars) Obj 1 certainty must be 0.0 <= pfProb3 <= 1.0.\n" )
    guiChanges$pfProb3 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfQlower) || (pfQlower<0.0) || (pfQlower>1.0) )
  {
    cat( "\nMSG (.validPerfPars) Lower quantiles must be 0.0 < pfQlower < 1.0.\n" )
    guiChanges$pfQlower <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfQupper) || (pfQupper<0.0) || (pfQupper>1.0) )
  {
    cat( "\nMSG (.validPerfPars) Upper quantiles must be 0.0 < pfQupper < 1.0.\n" )
    guiChanges$pfQupper <- NA
    isValid <- FALSE
  }

  if ( is.na(pfMinAAV) || (pfMinAAV < 0.0) || (pfMinAAV > pfMaxAAV ) )
  {
    cat( "\nMSG (.validPerfPars) Lower AAV plotting limit must be 0.0 <= pfMinAAV < pfMaxAAV.\n" )
    guiChanges$pfMinAAV <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMaxAAV) || (pfMaxAAV < pfMinAAV ) )
  {
    cat( "\nMSG (.validPerfPars) Upper AAV plotting limit must be pfMinAAV < pfMaxAAV.\n" )
    guiChanges$pfMaxAAV <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMinDep) || (pfMinDep < 0.0) || (pfMinDep > pfMaxDep ) )
  {
    cat( "\nMSG (.validPerfPars) Lower depletion plotting limit must be 0.0 <= pfMinDep < pfMaxDep.\n" )
    guiChanges$pfMinDep <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMaxDep) || (pfMaxDep <= pfMinDep ) )
  {
    cat( "\nMSG (.validPerfPars) Upper depletion plotting limit must be pfMaxDep < pfMinDep.\n" )
    guiChanges$pfMaxDep <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMinCat) || (pfMinCat < 0.0) || (pfMinCat >= pfMaxCat) )
  {
    cat( "\nMSG (.validPerfPars) Lower catch plotting limit must be 0.0 <= pfMinCat < pfMaxCat.\n" )
    guiChanges$pfMinCat <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMaxCat) || (pfMaxCat <= 0.0) || (pfMaxCat <= pfMinCat) )
  {
    cat( "\nMSG (.validPerfPars) Upper catch plotting limit must be 0.0 <= pfMinCat < pfMaxCat.\n" )
    guiChanges$pfMaxCat <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMinF) || (pfMinF < 0.0) || (pfMinF >= pfMaxF) )
  {
    cat( "\nMSG (.validPerfPars) Lower fishing mortality plotting limit must be 0.0 <= pfMinF < pfMaxF.\n" )
    guiChanges$pfMinF <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMaxF) || (pfMaxF <= 0.0) || (pfMaxF <= pfMinF) )
  {
    cat( "\nMSG (.validPerfPars) Upper fishing mortality plotting limit must be 0.0 <= pfMinF < pfMaxF.\n" )
    guiChanges$pfMaxF <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMinSSB) || (pfMinSSB < 0.0) || (pfMinSSB >= pfMaxSSB) )
  {
    cat( "\nMSG (.validPerfPars) Minimum spawning biomass plotting limit must be 0.0 <= pfMinSSB < pfMaxBio.\n" )
    guiChanges$pfMinSSB <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfMaxSSB) || (pfMaxSSB <= 0.0) || (pfMaxSSB <= pfMinSSB) )
  {
    cat( "\nMSG (.validPerfPars) Upper spawning biomass plotting limit must be 0.0 <= pfMinSSB < pfMaxSSB.\n" )
    guiChanges$pfMaxSSB <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfTraces) || (pfTraces < 0) || (!intVal(pfTraces)) )
  {
    cat( "\nMSG (.validPerfPars) Random traces shown on evelopes must be integer pfTraces >= 0.\n" )
    guiChanges$pfTraces <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfGroup) || (pfGroup < 0) || (!intVal(pfGroup)) )
  {
    cat( "\nMSG (.validPerfPars) Group identifier must be integer pfGroup >= 0.\n" )
    guiChanges$pfGroup <- NA
    isValid <- FALSE
  }  

  if ( is.na(pfLimitMultB0) || (pfLimitMultB0 <= 0.0) || (pfLimitMultB0 >= pfUpperMultB0) )
  {
    cat( "\nMSG (.validPerfPars) Limit bound multiplier for B0 must be 0 < pfLimitMultB0 < pfUpperMultB0.\n" )
    guiChanges$pfLimitMultB0 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfUpperMultB0) || (pfUpperMultB0 <= 0.0) || (pfUpperMultB0 <= pfLimitMultB0) )
  {
    cat( "\nMSG (.validPerfPars) Upper bound multplier for B0 must be 0 < pfLimitMultB0 < pfUpperMultB0.\n" )
    guiChanges$pfUpperMultB0 <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfLimitMultBmsy) || (pfLimitMultBmsy <= 0.0) || (pfLimitMultBmsy >= pfUpperMultBmsy) )
  {
    cat( "\nMSG (.validPerfPars) Limit bound multiplier for Bmsy must be 0 < pfLimitMultBmsy < pfUpperMultBmsy.\n" )
    guiChanges$pfLimitMultBmsy <- NA
    isValid <- FALSE
  }
  
  if ( is.na(pfUpperMultBmsy) || (pfUpperMultBmsy <= 0.0) || (pfUpperMultBmsy <= pfLimitMultBmsy) )
  {
    cat( "\nMSG (.validPerfPars) Upper bound multplier for Bmsy must be 0 < pfLimitMultBmsy < pfUpperMultBmsy.\n" )
    guiChanges$pfUpperMultBmsy <- NA
    isValid <- FALSE
  }
  
  # Check to ensure at least one simulation is selected.
  if ( all( pfSimList$Select==FALSE ) )
  {
    guiChanges$pfSimList <- pfSimList
    guiChanges$pfSimList$Select[1] <- TRUE
  }
  
  # Check to ensure that empty simulation rows in simList are NOT selected.
  if ( !all( pfSimList$Select==FALSE ) )
  {
    guiChanges$pfSimList <- pfSimList
    guiChanges$pfSimList$Select <- ifelse( (pfSimList$Select==TRUE) & (pfSimList$simFolder==""),
                                           FALSE, pfSimList$Select )
  }
  
  setWinVal( guiChanges )
  return( isValid )
}     # END function .validPerfPars


.initPfSimList <- function( trackObj,
                    guiVars=c( "simFolder","scenario","mp","nReps","rank","group" ) )
{
  result <- trackObj[ ,guiVars ]
  result <- cbind( result, Select=logical(nrow(result)) )
  if ( .getNumSims( trackObj ) > 0 )
    result$Select[1] <- TRUE
      
  assign( "pfSimList", result, pos=1 )
  result
}     # END function .savePfSimList


# .setGuiPerfState (ghosts/disables widgets for GUI):
# Purpose     : Ghosts (deactivates) fields depending on using input, tracker status.
# Parameters  : None
# GUI inputs  : Parameters taken directly from active guiSim.
# Returns     : status - a status indicator
# Side-effects: Widgets that don't apply to most recent input are deactivated
# Source      : A.R. Kronlund, modified from .ghostGuiSim by K.Holt.
.setGuiPerfState <- function( nSims=0 )
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Plots.
  if ( nSims > 0 )
  {
    setWidgetState( varname="pfPlotType", state="normal" )
    setWidgetState( varname="pfStats",    state="normal" )
  }
  else
  {
    setWidgetState( varname="pfPlotType", state="disabled" )
    setWidgetState( varname="pfStats",    state="disabled" )
  }

  # Objectives.
  if ( pfObj1 == "dep" )
  {
    setWidgetState( varname="pfDep1",  state="disabled" )
    setWidgetState( varname="pfYear1", state="normal" )
    setWidgetState( varname="pfProb1", state="normal" )
  }
  else if ( pfObj1 == "year" )
  {
    setWidgetState( varname="pfDep1",  state="normal" )
    setWidgetState( varname="pfYear1", state="disabled" )
    setWidgetState( varname="pfProb1", state="normal" )
  }
  else
  {
    setWidgetState( varname="pfDep1",  state="normal" )
    setWidgetState( varname="pfYear1", state="normal" )
    setWidgetState( varname="pfProb1", state="disabled" )
  }
    
  if ( pfObj2 == "dep" )
  {
    setWidgetState( varname="pfDep2",  state="disabled" )
    setWidgetState( varname="pfYear2", state="normal" )
    setWidgetState( varname="pfProb2", state="normal" )
  }
  else if ( pfObj2 == "year" )
  {
    setWidgetState( varname="pfDep2",  state="normal" )
    setWidgetState( varname="pfYear2", state="disabled" )
    setWidgetState( varname="pfProb2", state="normal" )
  }
  else
  {
    setWidgetState( varname="pfDep2",  state="normal" )
    setWidgetState( varname="pfYear2", state="normal" )
    setWidgetState( varname="pfProb2", state="disabled" )
  }
      
  if ( pfObj3 == "dep" )
  {
    setWidgetState( varname="pfDep3",  state="disabled" )
    setWidgetState( varname="pfYear3", state="normal" )
    setWidgetState( varname="pfProb3", state="normal" )
  }
  else if ( pfObj3 == "year" )
  {
    setWidgetState( varname="pfDep3",  state="normal" )
    setWidgetState( varname="pfYear3", state="disabled" )
    setWidgetState( varname="pfProb3", state="normal" )
  }
  else
  {
    setWidgetState( varname="pfDep3",  state="normal" )
    setWidgetState( varname="pfYear3", state="normal" )
    setWidgetState( varname="pfProb3", state="disabled" )
  }
  
  setWidgetColor( name="pfDep1",  noeditbg=.Obj1DepCOL,  entrybg="white" )
  setWidgetColor( name="pfYear1", noeditbg=.Obj1YrCOL,   entrybg="white" )
  setWidgetColor( name="pfProb1", noeditbg=.Obj1ProbCOL, entrybg="white" )
    
  setWidgetColor( name="pfDep2",  noeditbg=.Obj2DepCOL,  entrybg="white" )
  setWidgetColor( name="pfYear2", noeditbg=.Obj2YrCOL,   entrybg="white" )
  setWidgetColor( name="pfProb2", noeditbg=.Obj2ProbCOL, entrybg="white" )
    
  setWidgetColor( name="pfDep3",  noeditbg=.Obj3DepCOL,  entrybg="white" )
  setWidgetColor( name="pfYear3", noeditbg=.Obj3YrCOL,   entrybg="white" )
  setWidgetColor( name="pfProb3", noeditbg=.Obj3ProbCOL, entrybg="white" )            
  
  # Update the GUI.
  setWinVal( guiChanges, win )
  
  return( invisible() )
}    # END .setGuiPerfState function.


# .doGuiPerfPlots (do performance plots: envelopes (tulips), bars, quantile plots)
# Purpose:        Display the GUI requested performance plot.
#                 (a) Set up plot layout and margins;
#                 (b) Determine the type of plot and pass to .doTulipPlots,
#                     .doBarPlots, .doConvPlots, .doQuantBoxPlots, or .doOtherPlots
# Parameters:     None.
# Returns:        NULL (invisibly)
# Side Effects:   Produces plots.
# Source:         A.R. Kronlund
.doGuiPerfPlots <- function( trackObj )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )
  
  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )
  
  # Use the pfSimList to get the index values of the simulations in trackData
  # because we need all the information in trackData to find files, etc.  In
  # other words, pfSimList is just a visual aid to selection.

  #----------------------------------------------------------------------------#
  # WARNING: PBSmodelling changes characters to factors, bad, very bad!!!      #
  #          Therefore we need to coerce them back to character.  Watch out!   #
  #----------------------------------------------------------------------------#

  hdr    <- trackObj[ pfSimList$Select, ]
  idNum  <- c(1:length(pfSimList$Select))[pfSimList$Select]     
  nSim   <- nrow( hdr )
    
  hdr$simID     <- as.character( c(1:nrow(trackObj))[ pfSimList$Select ] )
  hdr$scenario  <- as.character( hdr$scenario )
  hdr$mp        <- as.character( hdr$mp )
  hdr$simLabel  <- paste( hdr$scenario,"-",hdr$mp, sep="" )

  # Make a period object with columns "pName","t1", and "t2".
  period <- data.frame( pName=c("Short","Medium","Long"),
            t1=c( pfShort1, pfMed1, pfLong1 ), t2=c( pfShort2, pfMed2, pfLong2),
            stringsAsFactors=FALSE )

  # Determine the type of performance plot requested.
  if ( substring( pfPlotType,3,7 )=="Tulip" )
    plotType <- "Tulip"
  else if ( substring( pfPlotType,3,5 )=="Bar" )
    plotType <- "Bar"
  else if ( substring( pfPlotType,3,5 )=="Box" )
    plotType <- "Box"
  else if ( substring( pfPlotType,3,6 )=="Conv" )
    plotType <- "Conv"
  else if ( substring( pfPlotType,3,4 )=="Eq" )
    plotType <- "Eq"    
  else
    plotType <- "Other"

  cat( "\nMSG (.doGuiPerfPlots) Calling function ", pfPlotType,"\n" )

  if ( plotType=="Tulip" )
    .doTulipPlots( hdr )
  else if ( plotType=="Bar" )
    .doBarPlots( hdr, period )
  else if ( plotType=="Box" )
    .doBoxPlots( hdr, period )
  else if ( plotType=="Conv" )
    .doConvPlots( hdr )
  else if ( plotType=="Eq" )
    .doEqPlots( hdr, idNum )
  else
    .doOtherPlots( hdr )

  if ( pfStamp )
  {
    oma <- par( "oma" )
    mtext( side=1, adj=1, col=.COLSTAMP, cex=.CEXSTAMP, line=oma[1]-1, outer=T, date() )
  }  

  # Restore par settings.
  par( oldpar )
  return( invisible() )
}     # END function .doGuiPerfPlots


# .doBarPlots  (Wrapper function for all bar plots in guiPerf window)
# Purpose:    Calls functions to calculate required statistics and then sets up
#                 inputs for .plotBarsByPeriod() or .plotBarsByStats() function 
# Parameters: NONE
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
.doBarPlots <- function( hdr, period )
{  
  # Get the guiPerf parameters so that plot controls available.
  guiInfo <- getWinVal(scope="L")
  iRow    <- 0
  nSim    <- nrow( hdr )

  # qLower and qUpper are user-specified from the guiPerf GUI.
  quantVals <- c( 0.05,pfQlower,0.5,pfQupper,0.95 )

  # Make an output object for performance statistics, there are nSim*3 rows
  # because of Short, Medium, and Long summary period.  The number of columns
  # is determined by the simID, simLabel, scenario, procedure, period, t1, t2,
  # plus the number of statistics calculated.

  nResults    <- nSim * 3
  headerNames <- c("simID","simLabel","scenario","mp","period","t1","t2")  
  statNames   <- c( "medAvgDep","Q1AvgDep","Q2AvgDep",
                    "medFinalDep","Q1finalDep","Q2finalDep",
                    "medAAV", "Q1AAV","Q2AAV",
                    "medAvgCatch","Q1AvgCatch","Q2AvgCatch" )
                    
  colNames    <- c( headerNames, statNames )
  result <- data.frame( matrix( NA, nrow=nResults,ncol=length(colNames) ) )  
  names( result ) <- colNames

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doBarPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )

    Bt <- blob$om$Bt
    Ct <- blob$om$Ct
    Dt <- blob$om$Dt
#    Ft <- blob$om$Ft

    nReps     <- blob$ctlList$gui$nReps
    #failList <- .getEstimationFails( blob$mp$assess$mpdPars,
    #               keepFailRefPts=pfFailRefPts, keepFailConv=pfFailConv )
    
    # Take away the failed reps (blob is a local copy from load).
    #if ( failList$nRepsFail > 0 )
    #{
    #  repsFail <- failList$repsFail
    #  Bt <- Bt[ !is.element( Bt[,iRep], repsFail ), ]
    #  Ct <- Ct[ !is.element( Ct[,iRep], repsFail ), ]
    #  Ft <- Ft[ !is.element( Ft[,iRep], repsFail ), ]
    #}    
    
    Bt <- Bt[ ,c(2:ncol(Bt)) ]
    Ct <- Ct[ ,c(2:ncol(Ct)) ]
    Dt <- Dt[ ,c(2:ncol(Dt)) ]
#    Ft <- Ft[ ,c(2:ncol(Ft)) ]        
    
    # Calculate the depletion values.
    Dept <- Bt / blob$ctlList$opMod$B0    
     
    # Calculate the number of selected replicates after adjusting for failed
    # reference points and failed convergence.
    #nSelected <- nReps - failList$nRepsFail
  
    # Accumulate statistics for each period - may need them later for barplots.
    for ( j in 1:nrow(period) )
    {
      # Get the header information for this simulation and period.
      iRow <- iRow + 1
      result[ iRow, "simID"    ] <- hdr$simID[i]
      result[ iRow, "simLabel" ] <- hdr$simLabel[i]
      result[ iRow, "scenario" ] <- hdr$scenario[i]
      result[ iRow, "mp"       ] <- hdr$mp[i]
      
      result[ iRow, "period"   ] <- period$pName[j]
      result[ iRow, "t1"       ] <- period$t1[j]
      result[ iRow, "t2"       ] <- period$t2[j]

      # Get the time index values that span this period.
      tdx <- c( period$t1[j]:period$t2[j] )

      #-- Aggregate depletion for period.
      tmp <- .calcStatsDepletion( Dept[,tdx], probs=quantVals )
      result[ iRow, "medAvgDep" ] <- tmp$medAvgDep
      result[ iRow, "Q1AvgDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgDep" ]  <- tmp$qVals[4]

      #-- Final depletion for period.
      tmp <- .calcStatsFinalDep( Dept[,tdx], quantVals )
      result[ iRow, "medFinalDep" ] <- tmp$medFinalDep
      result[ iRow, "Q1finalDep" ]  <- tmp$qVals[2]
      result[ iRow, "Q2finalDep" ]  <- tmp$qVals[4]
        
      #-- Catch Statistics
      tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
      result[ iRow, "medAvgCatch" ] <- tmp$medAvgCatch
      result[ iRow, "Q1AvgCatch" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgCatch" ]  <- tmp$qVals[4]
      
      #-- Discards Statistics
      tmp <- .calcStatsCatch( Dt[,tdx], quantVals )
      result[ iRow, "medAvgDiscards" ] <- tmp$medAvgCatch
      result[ iRow, "Q1AvgDiscards" ]  <- tmp$qVals[2]
      result[ iRow, "Q2AvgDiscards" ]  <- tmp$qVals[4]      
        
      #-- AAV Catch Statistics
      tmp <- .calcStatsAAV( Ct, tdx, quantVals )
      result[ iRow,"medAAV" ] <- tmp$medAAV
      result[ iRow,"Q1AAV" ]  <- tmp$qVals[2]
      result[ iRow,"Q2AAV" ]  <- tmp$qVals[4]

    }     # Loop over j periods.
  }     # Loop over i simulations.

  xLim <- NULL
  yLim <- NULL

  if ( pfPlotType=="pfBarDepPer" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(3,1) )
    }

    if ( pfSetYaxis )
      yLim <- c( pfMinDep, pfMaxDep )

     .plotBarsByPeriod( result, xvar="depletion", refPoints=NULL, 
                        gfx=list( xLim=xLim, yLim=yLim ) )
  }
  else if ( pfPlotType=="pfBarCatPer" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(3,1) )
    }
      
    if ( pfSetYaxis )
      yLim <- c( pfMinCat, pfMaxCat )
      
    .plotBarsByPeriod( result, xvar="catch", refPoints=NULL,
                       gfx=list( xLim=xLim, yLim=yLim ) )
  }
  else if ( pfPlotType=="pfBarDisPer" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(3,1) )
    }
      
    if ( pfSetYaxis )
      yLim <- c( pfMinDis, pfMaxDis )
    
    .plotBarsByPeriod( result, xvar="discards", refPoints=NULL,
                       gfx=list( xLim=xLim, yLim=yLim ) )
  }  
  else if ( pfPlotType=="pfBarAAVPer" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(3,1) )
    }
      
    if ( pfSetYaxis )
        yLim <- c( pfMinAAV, pfMaxAAV )
        
    .plotBarsByPeriod( result, xvar="AAV", refPoints=NULL,
                       gfx=list( xLim=xLim, yLim=yLim ) )
  }

  else if ( pfPlotType=="pfBarAllPer" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfcol=c(3,3) )
    }
      
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )
                     
    .plotBarsByPeriod( result, xvar=c("depletion","catch","AAV"), 
                       refPoints=NULL, gfx=c( xLim=xLim, yLim=yLim ) )  
  }
  else if ( pfPlotType=="pfBarShort" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(4,1) )
    }  
  
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )

    .plotBarsByStats( result, periodList="Short", refPoints=NULL,
                      gfx=c( xLim=xLim, yLim=yLim ) )
  }
  else if ( pfPlotType=="pfBarMedium" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(4,1) )
    }
     
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )
                     
    .plotBarsByStats( result, periodList="Medium", refPoints=NULL,
                      gfx=c( xLim=xLim, yLim=yLim ) )
  }
  
  else if ( pfPlotType=="pfBarLong" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(4,1) )
    }  
  
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )
                     
    .plotBarsByStats( result, periodList="Long", refPoints=NULL,
                      gfx=c( xLim=xLim, yLim=yLim ) )
  }
  
  else if ( pfPlotType=="pfBarAllStats" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(3,4) )
    }
      
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )

    .plotBarsByStats( result, periodList=c("Short","Medium","Long"), 
                      refPoints=NULL, gfx=c( xLim=xLim, yLim=yLim ) )
  }
  
  else if ( pfPlotType=="pfBarAllPerSL" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 8
    
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(2,4) )
    }
      
    if ( pfSetXaxis )
      xLim <- matrix( c( pfMinDep, pfMaxDep, pfMinCat, pfMaxCat,
                         pfMinAAV, pfMaxAAV), nrow=3, byrow=TRUE )

    .plotBarsByStats( result, periodList=c("Short","Long"), 
                      refPoints=NULL, gfx=c( xLim=xLim, yLim=yLim ) )
  }  
  
  else
    .plotStatus( paste( pfPlotType,"not implemented..." ) )
  
  return( invisible() )
}     # END function .doBarPlots.


# .doBoxPlots  (Wrapper function for all boxplots in guiPerf window)
# Purpose:   Sets up inputs for lower level barplot functions  
#             .plotQboxDep and .plotQboxSSB
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
.doBoxPlots <- function( hdr, period )
{
  # Get the guiPerf parameters so that plot controls available.
  guiInfo <- getWinVal(scope="L", winName=.getWinName() )

  iRow <- 0
  nSim <- nrow( hdr )
  simList <- as.list( nSim )

  # qLower and qUpper are user-specified from the guiPerf GUI.
  quantProbs <- c( 0.05,pfQlower,0.5,pfQupper,0.95 )

  # Make an output object for replicate performance statistics.  There are
  # nSim*3 rows and nRep columns because of Short, Medium, and Long summary
  # periods.  The number of columns is determined by the simID, simLabel,
  # scenario, mp, period, t1, t2, plus the nRep statistics.

  nResults    <- nSim * 3
  headerNames <- c( "simID","simLabel","scenario","mp","period","t1","t2",
                    "lrpB0","usrB0","trpB0","lrpBmsy","usrBmsy","trpBmsy","Fmsy" )
  #result      <- data.frame(matrix( NA,nrow=nResults,ncol=length(headerNames)))
  #names( result ) <- headerNames

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doBoxPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )
    
    result      <- data.frame(matrix( NA,nrow=nrow(period),ncol=length(headerNames)))
    names( result ) <- headerNames    
    
    nReps    <- blob$ctlList$gui$nReps
    failList <- .getEstimationFails( blob$mp$assess$mpdPars,
                   keepFailRefPts=pfFailRefPts, keepFailConv=pfFailConv )
    
    # Take away the failed reps (blob is a local copy from load).
    if ( failList$nRepsFail > 0 )
    {
      repsFail <- failList$repsFail
      Bt <- blob$om$Bt[ !is.element( Bt[,iRep], repsFail ), ]
      Ct <- blob$om$Ct[ !is.element( Ct[,iRep], repsFail ), ]
      Dt <- blob$om$Dt[ !is.element( Dt[,iRep], repsFail ), ]
      Ft <- blob$om$Ft[ !is.element( Ft[,iRep], repsFail ), ]
    }
    else
    {
      Bt <- blob$om$Bt
      Ct <- blob$om$Ct
      Dt <- blob$om$Dt
      Ft <- blob$om$Ft
    }    
    
    Bt <- Bt[ ,c(2:ncol(Bt)) ]
    Ct <- Ct[ ,c(2:ncol(Ct)) ]
    Dt <- Dt[ ,c(2:ncol(Dt)) ]
    
    # Calculate the number of selected replicates after adjusting for failed
    # reference points and failed convergence.
    nSelected <- nReps - failList$nRepsFail

    # Extract relevant reference point bases:
    B0   <- blob$ctlList$opMod$B0
    Bmsy <- blob$ctlList$refPts$ssbFmsy
    Umsy <- blob$ctlList$refPts$Umsy
    
    lrpB0 <- pfLimitMultB0  * B0
    usrB0 <- pfUpperMultB0  * B0
    trpB0 <- pfTargetMultB0 * B0

    lrpBmsy <- pfLimitMultBmsy  * Bmsy
    usrBmsy <- pfUpperMultBmsy  * Bmsy
    trpBmsy <- pfTargetMultBmsy * Bmsy

    repStats <- matrix( NA, nrow=nrow(result), ncol=nReps )
    dimnames( repStats ) <- list( NULL, paste( "Rep",c(1:nReps),sep="" ) )

    # Accumulate statistics for each period - may need them later for barplots.
    for ( j in 1:nrow(period) )
    {
      # Get the header information for this simulation and period.
      iRow <- j
      result[ iRow, "simID"    ] <- hdr$simID[i]
      result[ iRow, "simLabel" ] <- hdr$simLabel[i]
      result[ iRow, "scenario" ] <- hdr$scenario[i]
      result[ iRow, "mp"       ] <- hdr$mp[i]
      result[ iRow, "period"   ] <- period$pName[j]
      result[ iRow, "t1"       ] <- period$t1[j]
      result[ iRow, "t2"       ] <- period$t2[j]
      result[ iRow, "lrpB0"    ] <- lrpB0
      result[ iRow, "usrB0"    ] <- usrB0
      result[ iRow, "trpB0"    ] <- trpB0
      result[ iRow, "lrpBmsy"  ] <- lrpBmsy
      result[ iRow, "usrBmsy"  ] <- usrBmsy
      result[ iRow, "trpBmsy"  ] <- trpBmsy
      result[ iRow, "Umsy"     ] <- Umsy

      # Get the time index values that span this period.
      tdx <- c( period$t1[j]:period$t2[j] )
      
      if ( pfPlotType == "boxFt" )
        repStats[ iRow, ] <- apply( Ft[,tdx],1,mean )
      else
      {
        repStats[ iRow, ] <- apply( Bt[,tdx],1,mean )      
      }
    }     # Loop over j periods.
    
    simList[[i]] <- data.frame( result,repStats )
  }     # Loop over i simulations.

  if ( pfPlotType=="pfBoxDep" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 2
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(nrow(period),1) )
    }
    
    xLim <- NULL
    yLim <- NULL
    if ( pfSetYaxis )
      yLim <- c( pfMinDep,pfMaxDep )
   
    refPtNames <- c( "lrpB0","usrB0","trpB0","lrpBmsy","usrBmsy","trpBmsy" )
    for ( i in 1:length(simList) )
    {
      val <- simList[[i]]
      val[,refPtNames] <- val[,refPtNames] / B0
      idx <- c( (length(headerNames)+1):ncol(val) )
      val[ ,idx ] <- val[ ,idx ] / B0
      
      simList[[i]] <- val
    }

    .plotBxpStatus( simList, period, quantProbs, yLabel="Depletion (medians)",
      gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                xLim=xLim, yLim=yLim ) )
  }
  
  else if ( pfPlotType=="pfBoxSSB" )
  {
    if ( pfAuto )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 2
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(nrow(period),1) )
    }
      
    xLim <- NULL
    yLim <- NULL
    if ( pfSetYaxis )
      yLim <- c( pfMinSSB,pfMaxSSB )

    .plotBxpStatus( simList, period, quantProbs, yLabel="Biomass (medians)",
      gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid ) )
  }
  
  else if ( pfPlotType=="pfBoxUt" )
  {
    if ( pfAuto & i==1 )
    {
      myOma <- .OMA
      myOma[2] <- myOma[2] + 2
      myMar <- .MAR
      myMar[3] <- myMar[3] + 2
      par( oma=myOma, mar=myMar, mfrow=c(nrow(period),1) )
    }
      
    xLim <- NULL
    yLim <- NULL
    if ( pfSetYaxis )
      yLim <- c( pfMinF,pfMaxF )

    .plotBxpFmort( simList, period, quantProbs, yLabel="Fishing mortality",
      gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid ) )
  }
  else
  {
    .plotStatus( "Not Implemented" )
  }  
  
  return( invisible() )
}     # END function .doBoxPlots


# .doConvPlots  (Wrapper function for all convergence plots in guiPerf window)
# Purpose:   Conatins set-up for plots summarizing estimation model convergence 
#             Sets up inputs for lower level plotting functions .plotMaxGrad,   
#             .plotExitCodes, .plotFunCalls, and .plotConvTime.
# Parameters: NONE
# Returns:    NULL (invisibly)
# Source:     K.Holt (21-Aug-09)
.doConvPlots <- function( hdr )
{
  # Get the guiPerf parameters so that plot controls available.
  guiInfo <- getWinVal( scope="L", winName=.getWinName() )

  iRow <- 0
  nSim <- nrow( hdr )

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doConvPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )

    nReps     <- blob$ctlList$gui$nReps
    
    mpdPars <- blob$mp$assess$mpdPars
    failList <- .getEstimationFails( mpdPars,
                   keepFailRefPts=pfFailRefPts, keepFailConv=pfFailConv )
    
    # Take away the failed reps (blob is a local copy from load).
    repsFail <- failList$failConv
    if ( failList$nFailConv > 0 )
      mpdPars <- mpdPars[ !is.element( mpdPars[,iRep], repsFail ), ]

    # Plot maximum gradients by rep or by year
    if ( pfPlotType=="convGradRep" | pfPlotType=="convGradYr"  )
    {
      if ( i==1 )
      {
        myOma <- .OMA
        myOma[2] <- myOma[2] + 1
        par( oma=.OMA, mar=.MAR, mfrow=.getRowCol( nSim ) )
      }
        
      .plotMaxGrad( mpdPars, typ=pfPlotType,
         gfx=list( annotate=pfAnnotate, xLim=NULL, yLim=NULL ) )
          
      if ( i==nSim )
      {
        if ( pfPlotType=="convGradRep" )
        {
          mtext( side=1, line=.OUTLINE,  cex=.CEXAXIS4, outer=TRUE, "Replicate" )
          mtext( side=2, line=.OUTLINE2, cex=.CEXAXIS4, outer=TRUE, "Maximum Gradient" )
        }
          
        if ( pfPlotType=="convGradYr" )
        {
          mtext( side=1, line=.OUTLINE,  cex=.CEXAXIS4, outer=TRUE, "Year" )
          mtext( side=2, line=.OUTLINE2, cex=.CEXAXIS4, outer=TRUE, "Maximum Gradient" )
        }
      }
    }
    
    # Plot convergence time by rep or by year
    else if ( pfPlotType=="convTimeRep" | pfPlotType=="convTimeYr"  )
    {
      if ( i==1 )
      {
        myOma <- .OMA
        myOma[2] <- myOma[2] + 1
        par( oma=.OMA, mar=.MAR, mfrow=.getRowCol( nSim ) )
      }
              
      xLim <- NULL
      yLim <- c( 0,max( na.omit( blob$mp$assess$mpdPars[,"convT"] ) ) ) 
        
      .plotConvTime( mpdPars, typ=pfPlotType,
         gfx=list( annotate=pfAnnotate, doLegend=pfLegend, xLim=xLim, yLim=yLim ) )

      if ( i==nSim )
      {
        if ( pfPlotType=="convTimeRep" )
        {
          mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Replicate" )
          mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, 
                 "Convergence time (seconds)" )
        }
          
        if ( pfPlotType=="convTimeYr" )
        {
          mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Year" )
          mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE,
                 "Convergence time (seconds)" )
        }
      }
    
      if ( pfAnnotate )
      {
      }
 
      if ( pfStamp )
      {     
        .addStamp( iSim=i, iRep=0, nReps, simFolder=simFolder,
                   scenario=blob$ctlList$gui$scenarioLabel,
                   procedure=blob$ctlList$gui$mpLabel,
                   showFile=TRUE, outLine=NULL,
                   side=1, col=.COLSTAMP, cex=.CEXSTAMP )
      }
    }
    else
    {
      .plotStatus( paste( pfPlotType,"not implemented..." ) )
    }
  
  }     # Loop over selected simulations
  
  return( invisible() )
} # END function .doConvPlots

.doEqPlots <- function( hdr, idNum )
{
  # Get the guiPerf parameters so that plot controls available.
  guiInfo <- getWinVal(scope="L")
    
  checked          <- c( chkFmsy, chkF0, chkF01, chkF40, chkFmax, chkFcra )
  names( checked ) <- c( "Umsy",  "U0",  "U01", "U40",   "Umax",  "Ucra" )

  nSim <- nrow( hdr )
  obj <- as.list( c(1:nSim) )

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doEqPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )
    opMod <- blob$refPtList
  
    # Calculate life history schedules and OM equilibrium reference points.  
    cat( "\nMSG (.createMP) Calculating reference points...\n ")
    tmp <- calcRefPoints( as.ref(opMod) )
    refPts <- deref( tmp )

    obj[[i]] <- refPts
  }
  
  if ( pfAuto )
  {
    myOma <- c(1,1,1,1)
    myMar <- c(3,4,1,1)
    par( oma=myOma, mar=myMar, mfrow=c(1,1) )
  }
    
  if ( pfPlotType=="pfEqYprU" )
  {
    .plotEqYprU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
  }
    
  else if ( pfPlotType=="pfEqYieldU" )
  {
    .plotEqYieldU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
    }
    
  else if ( pfPlotType=="pfEqSsbRU" )
  {
    .plotEqSsbPerRecU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
      }
      
  else if ( pfPlotType=="pfEqSsbU" )
  {
    .plotEqSsbU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
    }
      
  else if ( pfPlotType=="pfEqRecSSB" )
  {
    .plotEqRecSSBU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
  }
      
  else if ( pfPlotType=="pfEqYieldSSB" )
  {
    .plotEqYieldSSBU( obj, idNum,
       gfx=list( annotate=pfAnnotate,checked=checked,doLegend=pfLegend,
       setXaxis=pfSetXaxis, setYaxis=pfSetYaxis,
       xLim=NULL,yLim=NULL ) )
  }
  else
  {
    .plotStatus( paste( pfPlotType, "not implemented..." ) )
  }
}     # END function .doEqPlots


# .doOtherPlots  (Wrapper function for all "other" plots not set up in 
#                      previous .doPlot functions)
# Purpose:   Sets up inputs for lower level plot function .plotFvsSSB 
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
.doOtherPlots <- function( hdr )
{
  # Get the guiPerf parameters so that plot controls available.
  guiInfo <- getWinVal(scope="L", winName=.getWinName() )

  iRow <- 0
  nSim <- nrow( hdr )

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doOtherPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )

    nReps     <- blob$ctlList$gui$nReps
    nReps     <- blob$ctlList$gui$nReps
    failList <- .getEstimationFails( blob$mp$assess$mpdPars,
                   keepFailRefPts=pfFailRefPts, keepFailConv=pfFailConv )
    
    Bt <- blob$om$Bt
    Ct <- blob$om$Ct
    Ft <- blob$om$Ft
    
    # Take away the failed reps (blob is a local copy from load).
    if ( failList$nRepsFail > 0 )
    {
      repsFail <- failList$repsFail
      Bt <- Bt[ !is.element( Bt[,iRep], repsFail ), ]
      Dt <- Dt[ !is.element( Dt[,iRep], repsFail ), ]
      Ft <- Ft[ !is.element( Ft[,iRep], repsFail ), ]
    }    
    
    # Calculate the depletion values.
    Dept <- Bt / blob$ctlList$opMod$B0    
    
    # Calculate the number of selected replicates after adjusting for failed
    # reference points and failed convergence.
    nSelected <- nReps - failList$nRepsFail

    # Set the x-axis limits.
    nT <- blob$ctlList$opMod$nT
    tMP <- blob$ctlList$opMod$tMP
    
    if ( pfProj )
      xLim <- c( tMP - 1, nT )
    else
      xLim <- c( 1,nT )

    if ( pfPlotType=="stratFB" )
    {
      if ( pfAuto & i==1 )
      {
        myOma <- .OMA
        myOma[2] <- myOma[2] + 1
        par ( oma=myOma, mar=.MAR, mfrow=.getRowCol( nSim ) )    
      }
      
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinSSB, pfMaxSSB )
          
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinF, pfMaxF )
         
      .plotFtBt( blob, qProbs=quantVals, refPts=pfRefPts, allQuants=TRUE,
         gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                   image=pfImage, xLim=xLim, yLim=yLim ) )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Spawning Biomass" )
        mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, "Legal HR" )
      }
    }
    
    if ( pfPlotType == "pfDesign" )
    {
      if ( pfAuto )
      {
        myMar <- c(2,6,1,3)
        myOma <- c(2,1,2,1)
        par( oma=myOma, mar=myMar, mfrow=c(1,1) )
      }
    
      xLim <- NULL
      yLim <- NULL
  
      .plotDesign( blob$ctlList, iObj=i, nObj=nSim,
        gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
        xLim=xLim, yLim=yLim, years=pfYrs ) ) 
    }
    
    else if ( pfPlotType == "stratPhase" )
    {
      if ( pfAuto & i==1 )
      {
        par ( oma=.OMA, mar=.MAR, mfrow=.getRowCol( nSim ) )    
      }
    
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinSSB, pfMaxSSB )
          
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinF, pfMaxF )
         
      .plotFtBt( blob, qProbs=quantVals, refPts=pfRefPts, allQuants=TRUE, phase=TRUE,
         gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                   image=pfImage, xLim=xLim, yLim=yLim ) )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE,  cex=.CEXAXIS4, outer=TRUE, "B / Bmsy" )
        mtext( side=2, line=.OUTLINE2, cex=.CEXAXIS4, outer=TRUE, "U / Umsy" )
      }
    }
    
    if ( pfPlotType == "stratBmsy" )
    {
      if ( pfAuto & i==1 )
      {
        myOma <- .OMA
        myOma[2] <- 4
        par ( oma=myOma, mar=.MAR, mfrow=.getRowCol( nSim ) )    
      }
          
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinSSB, pfMaxSSB )
          
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinF, pfMaxF )
      
      mults <- list( pfLimitMultBmsy=pfLimitMultBmsy, pfUpperMultBmsy=pfUpperMultBmsy,
                     pfTargetMultBmsy=pfTargetMultBmsy, pfLimitMultB0=pfLimitMultB0,
                     pfUpperMultB0=pfUpperMultB0, pfTargetMultB0=pfTargetMultB0 )
      .plotStrategy( blob, base="Bmsy", phase=FALSE, mults=mults,
                     gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                               image=pfImage, xLim=xLim, yLim=yLim ) )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Spawning Biomass" )
        mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, "Fishing Mortality" )
      }
    }
 
    if ( pfPlotType == "stratB0" )
    {
      if ( pfAuto & i==1 )
      {
        myOma <- .OMA
        myOma[2] <- 4
        par ( oma=myOma, mar=.MAR, mfrow=.getRowCol(nSim) )          
      }
    
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinSSB, pfMaxSSB )
          
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinF, pfMaxF )
      
      mults <- list( pfLimitMultBmsy=pfLimitMultBmsy, pfUpperMultBmsy=pfUpperMultBmsy,
                     pfTargetMultBmsy=pfTargetMultBmsy, pfLimitMultB0=pfLimitMultB0,
                     pfUpperMultB0=pfUpperMultB0, pfTargetMultB0=pfTargetMultB0 )
      .plotStrategy( blob, base="B0", phase=FALSE, mults=mults,
                     gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                               image=pfImage, xLim=xLim, yLim=yLim ) )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Spawning Biomass" )
        mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, "Fishing Mortality" )
      }
    }

    if ( pfPlotType == "simDiags" )
    {
      if ( pfAuto & i==1 )
      {
        myOma <- .OMA
        myOma[2] <- 3
        myOma[4] <- 1
        par ( oma=myOma, mar=.MAR, mfrow=.getRowCol(nSim) )    
      }    
    
      .plotDiagSim( blob, gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                                    xLim=NULL, yLim=NULL, useYears=pfYrs ) ) 
    }
    
    if ( pfPlotType=="pfTrend" )
    {
      if ( pfAuto )
      {
        par( oma=.OMA, mar=.MAR, mfrow=c(2,1) )
      }
      
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinSSB,pfMaxSSB )

      .plotTrend( blob, delta=10,
         multLrp=pfLimitMultBmsy, multUsr=pfUpperMultBmsy,
         label="LABEL", traces=c(1,2,3), failedReps=failList$repsFail,
         gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
         doGrid=pfGrid, showProj=pfProj, xLim=xLim, yLim=yLim,
         useYears=pfYrs ) )
           
      if ( i==nSim )
      {
        mtext( side=2, line=0.5, cex=.TULCEX, outer=TRUE, "Spawning Biomass (000s t)" )
      }
    }     # if pfTrend          
    
    if ( pfStamp & pfPlotType!="pfDesign" )
    {
      .addStamp( iSim=i, iRep=0, nReps, simFolder="",
                 scenario=blob$ctlList$gui$scenarioLabel,
                 procedure=blob$ctlList$gui$mpLabel,
                 showFile=TRUE, outLine=NULL,
                 side=1, col=.COLSTAMP, cex=.CEXSTAMP )
    }    
    
  }     # For i in 1:nSim loop.
  return( invisible() )
}     # END function .doOtherPlots


# .doTulipPlots  (Wrapper function for all tulip plots in guiPerf window)
# Purpose:   Sets up inputs for lower level tulip functions  
#             .plotTulipCatch, .plotTulipDepletion, .plotTulipF
# Returns:    NULL (invisibly)
# Source:     A.R. Kronlund
.doTulipPlots <- function( hdr )
{
  # Get the guiPerf parameters so that plot controls are available.
  guiInfo <- getWinVal(scope="L", winName=.getWinName() )

  iRow <- 0
  nSim <- nrow( hdr )

  # qLower and qUpper are user-specified from the guiPerf GUI.
  quantVals <- c( 0.05,pfQlower,0.5,pfQupper,0.95 )

  # Loop over the selected simulations.
  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile     <- hdr[ i, "simFile" ]
    simFolder   <- hdr[ i, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.doTulipPlots) Loading",simFilePath,"...\n" )
    load( file=simFilePath )

    nReps     <- blob$ctlList$gui$nReps
    failList <- .getEstimationFails( blob$mp$assess$mpdPars,
                   keepFailRefPts=pfFailRefPts, keepFailConv=pfFailConv )
    
    # Take away the failed reps (blob is a local copy from load).
    if ( failList$nRepsFail > 0 )
    {
      repsFail <- failList$repsFail
      blob$om$Bt <- blob$om$Bt[ !is.element( blob$om$Bt[,iRep], repsFail ), ]
      blob$om$Ct <- blob$om$Ct[ !is.element( blob$om$Ct[,iRep], repsFail ), ]
      blob$om$Dt <- blob$om$Dt[ !is.element( blob$om$Dt[,iRep], repsFails), ]
#      blob$Ft <- Ft[ !is.element( Ft[,iRep], repsFail ), ]
    }
    
    # Calculate the depletion values.
    
    if( .DeptIsBiomass )
      Dept <- blob$om$Bt[ ,c(2:ncol(blob$om$Bt)) ]
    else
      Dept <- blob$om$Bt[ ,c(2:ncol(blob$om$Bt)) ] / blob$ctlList$opMod$B0    

    # Calculate the number of selected replicates after adjusting for failed
    # reference points and failed convergence.
    nSelected <- nReps - failList$nRepsFail
    
    # Get pfTraces random replicates and save indices to working directory.
    # Note that if the simulations contain unequal numbers of replicates then
    # the trace indices can be out of range.  This is caught by plot functions.

    if ( pfTraces > 0 )
      traces <- sample( nSelected, min( pfTraces,nSelected ) )
    else
      traces <- 0
    assign( "traces",traces, pos=1 )

    #--------------------------------------------------------------------------#
    #-- Display the requested plot(s)                                        --#
    #--------------------------------------------------------------------------#
    
    # if saving plots to pdf, create pdf using mp label
    #browser()
    #if( .MAKEPDF )
    #{
      #if( nSim==1 )
      #{
      #  fname <- blob$ctlList$gui$mpLabel
      #  fname <- gsub(x=fname,pattern="%",replacement="")
      #  pdf(file=paste(fname,".pdf",sep=""))              
      #}

    #}
    if ( pfPlotType == "pfTulipCat" )
    {
      if ( pfAuto & i==1 )
      {
        par( oma=.OMA, mar=.MAR, mfrow=.getGearRowCol( nSim ) )        
      }
          
      # Simulation envelope for catch.
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinCat, pfMaxCat )
      
      .plotTulipCatch( blob, qProbs=quantVals,
                       traces=traces, refPts=pfRefPts, allQuants=TRUE,
                       gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                                 showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs ) )
      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, .CtLAB )
      }
    }    
    
    else if ( pfPlotType=="pfTulipDep" )
    {
      if ( pfAuto & i==1 )
      {
        myMar <- .MAR
        myMar[4] <- 1.5      
        par( oma=.OMA, mar=myMar, mfrow=.getRowCol( nSim ) )        
      }
      
      # Simulation envelope for depletion.
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinDep, pfMaxDep )
      
      .plotTulipDepletion( blob, qProbs=quantVals,
                      traces=traces, refPts=pfRefPts, allQuants=TRUE,
                      gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                      showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs ) )

      # If requested, plot Objectives 1-3 from GUI.
      if ( pfObj )
        #.calcObjectives( Dept, add=TRUE )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, .DeptLAB )
      }
    }     # end if pfPlotType="tulipDep"
      
    else if ( pfPlotType=="pfTulipDepCat" )
    {
      if ( pfAuto & i==1 )
      {
        myMar <- .MAR
        myMar[4] <- 1.5
        par( oma=.OMA, mar=myMar, mfcol=c( 2,nSim ) )        
      }
          
      # Simulation envelope for depletion.
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinDep, pfMaxDep )
              
      .plotTulipDepletion( blob, qProbs=quantVals,
                           traces=traces, refPts=pfRefPts, allQuants=TRUE,
                           gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                                     showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs ) )

      mfg <- par( "mfg" )
      if ( mfg[1]==1 & mfg[2]==1 )
        mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .DeptLAB )

      #if ( pfObj )
        #.calcObjectives( Dept, add=TRUE )
      if ( pfStamp )
      {
        .addStamp( iSim=i, iRep=0, nReps, simFolder="",
             scenario=blob$ctlList$gui$scenarioLabel,
             procedure=blob$ctlList$gui$mpLabel,
             showFile=TRUE, outLine=NULL,
             side=1, col=.COLSTAMP, cex=.CEXSTAMP, xPos=0.025, yPos=0.95 )
      }        

      # Simulation envelope for catch.
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinCat, pfMaxCat )
      
      .plotTulipCatch( blob, qProbs=quantVals, xLim=c(pfMinYr, pfMaxYr), 
                       yLim=c(pfMinCat,pfMaxCat),
                       traces=traces, refPts=pfRefPts, allQuants=TRUE,
                       gfx=list( annotate=pfAnnotate, doLegend=pfLegend, grids=pfGrid,
                                 showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs ) )

      mfg <- par( "mfg" )
      if ( mfg[1]==2 & mfg[2]==1 )
        mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .CtLAB )
        
      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
      }
    }
    
    else if ( pfPlotType=="pfTulipDis" )
    {
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinDis,pfMaxDis )
        
      .plotTulipDis( blob, traces=traces, qProbs=quantVals, allQuants=TRUE,
                       failedReps=failList$repsFail,
                       gfx=list( annotate=pfAnnotate, doLegend=pfLegend,
                       doGrid=pfGrid, showProj=pfProj, xLim=xLim,yLim=yLim,
                       useYears=pfYrs ) )
                         
      if ( i==nSim )
      {
        mtext( side=1, line=0.5, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=0.5, cex=.CEXLAB4, outer=TRUE, "Discards (t)" )
      }                                    
    }                
    
    else if ( pfPlotType=="pfTulipF" )
    {
      if ( pfAuto & i==1 )
      {
        myMar <- .MAR
        myMar[4] <- 2
        par( oma=.OMA, mar=myMar, mfrow=.getRowCol( nSim ) )        
      }
          
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
      
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinF, pfMaxF )      

      # Simulation envelope for fishing mortality.
      if( .USEMt )
      {
          .plotTulipF(  blob, qProbs=quantVals, traces=traces,
                        allQuants=TRUE,
                        gfx=list( annotate=pfAnnotate, grids=pfGrid, doLegend=pfLegend,
                                  showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs)               
                    )
          .UtLAB <- "Natural Mortality" 
      } else {
        .plotTulipHR( blob, qProbs=quantVals, traces=traces,
                   allQuants=TRUE,
                   gfx=list( annotate=pfAnnotate, grids=pfGrid, doLegend=pfLegend,
                             showProj=pfProj, xLim=xLim, yLim=yLim, useYears=pfYrs)               
                   ) 
      }
      


      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, .UtLAB )
      }
    }
    
    else if ( pfPlotType=="pfTulipBmsy" )
    {
      if ( pfAuto & i==1 )
      {
        par( oma=.OMA, mar=.MAR, mfrow=.getRowCol( nSim ) )        
      }
          
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
         
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinBmsy, pfMaxBmsy ) 
      
       # Simulation envelope for Bmsy.
      .plotTulipBmsy( blob, multLrp=pfLimitMultBmsy, multUsr=pfUpperMultBmsy,
         qProbs=quantVals, traces=traces,
         refPts=pfRefPts, allQuants=TRUE,
         gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
         showProj=TRUE, xLim=xLim, yLim=yLim, useYears=pfYrs ) )

      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, .BmsyEstLAB )
      }
    }
    
    else if ( pfPlotType=="pfTulipFmsy" )
    {
      if ( pfAuto & i==1 )
      {
        par( oma=.OMA, mar=.MAR, mfrow=.getRowCol( nSim ) )        
      }
          
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
         
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinFmsy, pfMaxFmsy )
               
      # Simulation envelope for Fmsy.
      .plotTulipFmsy( blob, qProbs=quantVals, traces=traces,
                      refPts=pfRefPts, allQuants=TRUE, elim=guiInfo$elimReps,
                      gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                      showProj=TRUE, xLim=NULL, yLim=NULL ) )
      if ( i==nSim )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, .FmsyEstLAB )
      }
    }
      
    else if ( pfPlotType=="pfTulipBmsyFmsy" )
    {
      if ( pfAuto & i==1 )
      {
        myMar <- .MAR
        myMar[2] <- myMar[2] + 1
        par( oma=.OMA, mar=myMar, mfcol=c( 2, nSim ) )        
      }
          
      xLim <- NULL
      if ( pfSetXaxis )
        xLim <- c( pfMinYr,pfMaxYr )
         
      yLim <- NULL
      if ( pfSetYaxis )
        yLim <- c( pfMinBmsy, pfMaxBmsy ) 
      
      # Simulation envelope for Bmsy.
      .plotTulipBmsy( blob, qProbs=quantVals, traces=traces,
                      refPts=pfRefPts, allQuants=TRUE,
                      gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                      showProj=TRUE, xLim=NULL, yLim=NULL ) )

      mfg <- par( "mfg" )
      if ( (mfg[1]==1) && (mfg[2]==1) )
        mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Estimated Bmsy" )
         
        # Simulation envelope for Fmsy.
       .plotTulipFmsy( blob, qProbs=quantVals, traces=traces,
                       refPts=pfRefPts, allQuants=TRUE,
                       gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                       showProj=TRUE, xLim=NULL, yLim=NULL ) )

      mfg <- par ( "mfg" )
      if ( (mfg[1]==2) && (mfg[2]==1) )
      {
        mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
        mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Estimated Fmsy" )
      }
    }
    
    if ( pfStamp )
    {
      .addStamp( iSim=i, iRep=0, nReps, simFolder="",
           scenario=blob$ctlList$gui$scenarioLabel,
           procedure=blob$ctlList$gui$mpLabel,
           showFile=TRUE, outLine=NULL,
           side=1, col=.COLSTAMP, cex=.CEXSTAMP, xPos=0.025, yPos=0.95 )
    }  
    #if( nSim==1 ) dev.off()      
  }     # for i in nSim loop.

  return( invisible() )
}     # END function .doTulipPlots

#------------------------------------------------------------------------------#
#-- Helper Functions                                                         --#
#------------------------------------------------------------------------------#
