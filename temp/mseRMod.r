#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 4.x                   #
#                                                                              #
#     Copyright 2008, 2009, 2010 by A.R. Kronlund, Jaclyn Cleary               #
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
#-------------------- mseRmod.r: mseR Modelling GUI Functions -----------------#
#--                                                                          --#
#-- mseRmod.r: A mseR GUI template to configure, fit and view the results of --#
#              a fisheries model using the AD Model Builder package.         --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          J.S. Cleary (Pacific Biological Station, Nanaimo, B.C.)         --#
#--                                                                          --#
#-- First Implementation: 06-Dec-09 from mseR V2.0 (mseRgui_funs.r)          --#
#-- 14-Jan-10 Revised GUI and started implementing widget functions.         --#
#-- 20-Feb-10 Implemented notebook widgets to combine Mod and Fit.           --#
#--                                                                          --#
# TO DO:                                                                       #
#                                                                              #
# 1. Put a legend/help info at the top of guiMod to remind user of gear/survey #
#    order in the vectors.                                                     #
# 3. Consider sub-directories for .Rdata files containing fits.                #
# 6. When you remove a fit, also remove all the saved date/stamp files.        #
# 9. The modpars list should include pars, ctl, data elements to be sent to    #
#    the pin, ctl, and data files respectively.  Thus, you never load into the #
#    program a pin, ctl, or data file.  You load the modPars file and write    #
#    these things just before the fit.  If you want to change their names, then#
#    we'll have to alter the args entry widget for ADMB to include the -ind.   #
#    e.g., -ind catage57.dat and -aind catage57.pin.                           #
#    So, the droplists need to be changed to entry character strings for pin,  #
#    dat, and ctl.                                                             #
#    Then the options in the respective boxes alter the modPars dataframe, so  #
#    that these can be changed in the modPars object OR the radio or droplists.#
#    This makes things more generic if the USERCODE does not supply anything.  #
#    The write function (there is only one now) simply has a branch point for  #
#    each first-level list element desired.  Will work great!                  #
# 17. Must modfiy View and Import to read stamped files, not defaults as the   #
#     code currently assumes the default.                                      #
# 19. When new TPL is loaded, fit must be set to false.  Similarly data, ctl,  #
#     and pin.                                                                 #
#                                                                              #
# HOW DOES mseRmod.r CODE WORK?                                                #
#                                                                              #
# 1. We assume you have an ADMB program that can be compiled, linked and run   #
#    successfully.  Furthermore, the ADMB program generates a .rep file.       #
# 2. The mseRmod parameter input file is a dataframe with fields called        #
#    "parameter" and "value".  The "value" field can be any valid R expression #
#    such as a number (e.g., 0.7), or a vector (e.g., c(0.7,0.8,0.9), or a     #
#    function that creates a vector (e.g., rep(1,100), or more complex types   #
#    such as matrices (e.g., matrix(c(1:12),nrow=3,ncol=4).                    #
# 3. The mseRmod parameter file is assumed to contain parameter needed to      #
#    create and ADMB pin (paramter input) file.                                #
# 4. The R code is sprinkled with the key word "USERCODE" to locate places in  #
#    the code where the User (you) has to modify this template.  Typically,    #
#    these include functions that write data and pin files needed by your ADMB #
#    excecutable program.  They also include places where the output from your #
#    ADMB executable program is read into R using mseRmod.r.  Another place    #
#    where USERCODE appears to is to control GUI behavior for special needs in #
#    the Data Options, Parameter Options and Control Options GUI boxes.  You   #
#    may not need these for you application, but they are handy.               #
# 5. Each time you "Run" a model with guiMod(), you can add the configuration  #
#    and the resulting fit to a list called fitTracker.  This list is saved by #
#    default in an Rdata file called mseRfits.Rdata.                           #
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
# 6. Oddly, notebooks appears to cause the symptoms of an event on windows     #
#    creation.  Thus, any function linked to notebook gets called on creation. #
#    This can mean that parameters are not set correctly.                      #
# 7. Grandin helps me with:                                                    #
#    strsplit( x="C:\\Working\\MyDir\\foo.dat", split="\\\\", perl=TRUE )      #
#                                                                              #
# REQUIRES: PBSmodelling, RODBC                                                #
#                                                                              #
# NON-R PACKAGES: ADMB PROJECT 2009                                            #
#                                                                              #
# References:                                                                  #
#                                                                              #
# ADMB Project. 2009 AD Model Builder: automatic differentiation model builder.#
#     Developed by David Fournier and freely available from admb-project.org.  #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., Egeli, A. and Kronlund, A.R. 2009#
#   PBS Modelling 2.50: user’s guide revised from Canadian Technical Report of #
#   Fisheries and Aquatic Science 2674: v+146 p. Last updated October 23, 2008 #
#                                                                              #
# GUI Functions:                                                               #
#                                                                              #
#   Flow: guiMod -> .setupGuiMod -> .setupModDir -> createWin -> .subGuiModFit #
#                                                                              #
# guiMod        : Run the mseR Modelling GUI.                                  #
# .setupGuiMod  : Initialize GUI settings.                                     #
# .setupModDir  : Setup the working directory and sub-directories as required. #
# createWin     : Create the GUI window.                                       #
# .subGuiModFit : Processes GUI submit actions, e.g., buttons, entry fields... #
# .subGuiModel  : Process Model notebook page actions.                         #
# .subGuiFit    : Process Fit notebook page actions.                           #
#                                                                              #
# GUI Hidden Functions (to show hidden function ls(all.names=TRUE)):           #
#                                                                              #
# .getWinName    : Get the current GUI window name.                            #
# .viewFile      : View a file saved in the mseRtemp directory.                #
# .viewHelp      : View a file saved in the mseRhelp directory.                #
#                                                                              #
# Some useful tricks for later:                                                #
#  tmpPars[ parType!=types, ] <- ""                                            #

# USERCODE Requirements                                                        #
#                                                                              #
# ADMB TPL Requirements                                                        #
#                                                                              #
# (1) REPORT_SECTION                                                           #
#                                                                              #
# The REPORT_SECTION must output a file namesd admbApp.rep where admbApp is    #
# the name of the ADMB executable.  The admbApp.rep file must be in "lisread"  #
# format that precedes variable names with a single # sign, and comments with  #
# a double ## pound sign, e.g.,                                                #
#                                                                              #
#      ## File begins.                                                         #
#      ## This is the variable alpha followed by its value.                    #
#      # alpha                                                                 #
#      0.049                                                                   #
#      ## This is the vector beta followeed by its values.                     #
#      # beta                                                                  #
#      12 32 43 12                                                             #
#      ## File ends.                                                           #
#                                                                              #
# The REPORT_SECTION must output the following variables.                      #
#      maxGrad   - the maximum gradient at the end of execution.               #
#      objFun    - the value of the objective function.                        #
#      exitCode  - the exit code from ADMB's minimizer.                        #
#      funEvals  - the number of function evaluations (put an integer variable #
#                  in your objective function call like:   funEvals+=1;        #
#                                                                              #
# The code required follows below:                                             #
#
#  report << "## Objective function value " << endl;
#  report << "# objFun" << endl;
#  report << objective_function_value::pobjfun << endl;
#  report << "## Maximum gradient" << endl;
#  report << "# maxGrad" << endl;
#  report << objective_function_value::gmax << endl;
#  report << "## Exit Code " << endl;
#  report << "# exitCode" << endl;
#  report << iexit << endl;
#  report << "## Number of function calls " << endl;
#  report << "# funEvals" << endl;
#  report << funEvals << endl;#
#------------------------------------------------------------------------------#

source( "mseRadmb.r" )
source( "mseRglobals.r" )
source( "mseRtools.r" )

#-----------------------------------------------------------------------------##
#-- GUI Functions                                                           --##
#-----------------------------------------------------------------------------##

# guiMod      (Run GUI)
# Purpose:    Run the GUI.
# Parameters: None
# Returns:    NULL (invisibly)
guiMod <- function()
{
  return( .setupGuiMod("mseRguiMod") )
}

.ghostGuiMod <- function()
{
  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  guiChanges <- list()                              # List to track GUI changes.

  # Model Page: Is there a fit for the current model?
  if ( fitTracker[[iFit]]$status$fitStamp=="NO_FIT" )
  {
    setWidgetState( "toView",   state="disabled" )
    setWidgetState( "mdView",   state="disabled" )
    setWidgetState( "mdImport", state="disabled" )
  }
  else
  {
    setWidgetState( "toView",   state="normal" )
    setWidgetState( "mdView",   state="normal" )
    setWidgetState( "mdImport", state="normal" )    
  }
  
  # The Model and Fit are synchronized.
  if ( fitTracker[[iFit]]$status$modStamp==fitTracker[[iFit]]$status$fitStamp )
  {
    # Lock the current page parameter widget.
    setWidgetState( "modPars", state="disabled" )
    
    # Lock the Load button.
    setWidgetState( "mdLoadPars", state="disabled" )
    
    # Lock the File IO widget.
    setWidgetState( "mdFileIO", state="disabled" )
    
    # Lock elements of the the ADMB that allow TPL to change.
    setWidgetState( "prefix",       state="disabled" )
    setWidgetState( "mdEditTpl",    state="disabled" )
    setWidgetState( "mdOpenTpl",    state="disabled" )
    setWidgetState( "mdResetNames", state="disabled" )
    
    # Lock the Run button.
    setWidgetState( "mdRun", state="disabled" )
    
    # Lock the ADMB arguments radio button.
    setWidgetState( "admbArg", state="disabled" )
    setWidgetState( "nsims",   state="disabled" )
    setWidgetState( "nthin",   state="disabled" )
    
    # Lock the ADMB argument string.
    setWidgetState( "argvec",  state="disabled" )
    
    # Lock the "File", "Delete" and "Write" buttons.
    setWidgetState( "mdSetFile", state="disabled" )
    setWidgetState( "mdDelFile", state="disabled" )
    setWidgetState( "mdWrtFile", state="disabled" )
    
    for ( i in 1:nrow(mdFileIO) )
    {
      if ( mdFileIO$FileName[i]!="" )
        mdFileIO$Select[i] <- TRUE
    }
    guiChanges$mdFileIO <- mdFileIO
    
    # USERCODE - Lock the user's widgets.
    setWidgetState( "mdDataOpt", state="disabled" )
    setWidgetState( "mdDataOpt", state="disabled" )
    setWidgetState( "mdM", state="disabled" )
    setWidgetState( "mdMdevs", state="disabled" )
    setWidgetState( "mdStockrec", state="disabled" )
    setWidgetState( "mdq" , state="disabled" )          
  }
  else
  {
    # The Model and Fit are not synchronized.
    
    if ( mdParsToADMB )
      setWidgetState( "modPars", state="normal" )
    else
      setWidgetState( "modPars", state="disabled" )
      
    setWidgetState( "mdLoadPars", state="normal" )
    setWidgetState( "mdFileIO",   state="normal" )
      
    setWidgetState( "prefix",       state="normal" )
    setWidgetState( "mdEditTpl",    state="normal" )
    setWidgetState( "mdOpenTpl",    state="normal" )
    setWidgetState( "mdResetNames", state="normal" )  
  
    setWidgetState( "mdRun",   state="normal" )
    setWidgetState( "admbArg", state="normal" )
    setWidgetState( "nsims",   state="normal" )
    setWidgetState( "nthin",   state="normal" )    
    setWidgetState( "argvec",  state="normal" )
    
    setWidgetState( "mdSetFile", state="normal" )
    setWidgetState( "mdDelFile", state="normal" )
    setWidgetState( "mdWrtFile", state="normal" )
    
    # USERCODE - Unlock the user's widgets?
    if ( mdParsToADMB )
    {
      setWidgetState( "mdDataOpt", state="normal" )
      setWidgetState( "mdM", state="normal" )
      setWidgetState( "mdMdevs", state="normal" )
      setWidgetState( "mdStockrec", state="normal" )
      setWidgetState( "mdq" , state="normal" )
    }
    else
    {
      setWidgetState( "mdDataOpt", state="disabled" )
      setWidgetState( "mdM", state="disabled" )
      setWidgetState( "mdMdevs", state="disabled" )
      setWidgetState( "mdStockrec", state="disabled" )
      setWidgetState( "mdq" , state="disabled" )      
    }
  }
  
  # Fit Page: If at least one fit is selected, enable the plottings.
  if ( any(fits$Select)==TRUE )
    setWidgetState( "ftPlotType", state="normal" )
  else
    setWidgetState( "ftPlotType", state="disabled" )
  
  # Disable the Orphans button because it is just dangerous right now.
  setWidgetState( "orphans", state="disabled" )
  
  # Options Page.
  setWidgetState( "opCompiler", radiovalue="Borland", state="disabled" )
  setWidgetState( "opCompiler", radiovalue="VisualC", state="disabled" )

  setWinVal( guiChanges, winName=win )
  
  return( invisible() )
}

# .setupGuiMod (Setup for guiMod creation)
# Purpose:    Set up and run guiMod
# Parameters: win is a character containing the name of the window to setup
# Returns:    NULL (invisibly)
# Source:     PBSref (modified)
.setupGuiMod <- function(win)
{
  # Get the required libraries.
  require( PBSmodelling )            # For the GUIs
  require( RODBC )                   # For the Excel and database SQL
  
  # Close all windows that are currently open to prevent crashes.
  closeWin()
  
  # Close all graphics windows.
  graphics.off()  
  
  gvar <- paste( ".", win, sep="" )  # Global variable name for GUI control file
  assign( gvar, list(), pos=1 )      # Hidden global list
  
  # Copy the GUI window description files and any .exe files to temp directory.
  dir <- .setupModDir()
 
  # Can a menu be created?
  goMenu   <- TRUE
  
  # Get the ADMB compiler/linker options.
  if ( file.exists( "ADMBcmd.txt" ) )
  {
    ADMBcmd <- read.table( "ADMBcmd.txt", as.is=TRUE, header=TRUE, sep="," )
    cat( "\nMSG (.setupGuiMod) ADMBcmd.txt found and loaded.\n" )
    assign( "ADMBcmd",ADMBcmd,pos=1 )
  }
  else
  {
    cat( "\nERROR (.setupGuiMod) ADMBcmd.txt not found, aborting.\n" )
    goMenu <- FALSE
  }
  
  fitFileExists <- file.exists( .FFITS ) 
  # If .FFITS exists then load the model fit tracking list.
  if ( fitFileExists )
  {
    load( .FFITS )                           # Load fitTracker from .FFITS.
    cat( "MSG (.setupGuiMod) Found fit tracking file: ",.FFITS,"\n\n" )
    
    # If any fitName values are "", then assign the fitTracker node name.
    # *** Need to add a check for duplicate names here.
    for ( i in 1:length(fitTracker) )
    {
      if ( fitTracker[[i]]$guiInfo$fitName=="" )
        fitTracker[[i]]$guiInfo$fitName <- names( fitTracker )[i]
        
      fitTracker[[i]]$guiInfo$nFit <- length( fitTracker )
    }
    
    # Create objects required for PBSmodelling menus in the working directory.
    # The entire GUI list is saved with each fitTracker element, so simply
    # restore the GUI using fitTracker[[1]]$guiInfo.
    
    # *** Need to add checks to see if things exist, valid ... if not...
    valid <- TRUE
    if ( valid )
    {
      iFit <- fitTracker[[1]]$guiInfo$iFit
      
      # Assign modPars to global for GUI "Model" page creation.
      assign( "modPars", fitTracker[[iFit]]$guiInfo$modPars, pos=1 )
    
      # Assign a copy of modPars to "remember" initial sort order.
      assign( ".initModPars", modPars, pos=1 )
      
      # Assign mdFileIO to global for GUI "Model" page creation.
      assign( "mdFileIO", fitTracker[[iFit]]$guiInfo$mdFileIO, pos=1 )

      # fits is an object containing a list of the fits in the fitTracker.
      # Although fits is saved, initialize and update it just in case...
      #fits <- .updateFits( fitTracker, fits )
    
      # Assign a copy of fits to global for GUI "Fit" notebook page creation.
      assign( "fits", fitTracker[[iFit]]$guiInfo$fits, pos=1 )        

      assign( "fitTracker",fitTracker,pos=1 )  # Make fitTracker global.

      # Update the GUI to reflect the saved model fit.
      guiChanges      <- fitTracker[[iFit]]$guiInfo
      #guiChanges$fits <- fits
   }
   else
     goMenu <- FALSE
  }
  else
  {
    # Create a new fit tracking list and save to the working directory.
    fitTracker <- list()
    fitTracker[[1]] <- .createFitNode()
    names(fitTracker) <- "Model1"
    
    # Save the fitTracker to global in local working directory.
    assign( "fitTracker", fitTracker, pos=1 )
    
    # Assign modPars to global for GUI "Model" page creation.
    modPars <- data.frame( parameter="NoParametersLoaded",
                           value=0 )
    assign( "modPars", modPars, pos=1 )   
    
    # Assign a copy of modPars to "remember" initial sort order.
    assign( ".initModPars", modPars, pos=1 )
    
    # Create the ADMB argument list for file I/O.
    mdFileIO <- .resetFileIO()
    assign( "mdFileIO", mdFileIO, pos=1 )
    
    # fits is an object containing a list of the fits in the fitTracker.    
    # There are no fits, so fits is simply initialized for GUI creation.
    fits <- .updateFits( fitTracker, fits )
    assign( "fits", fits, pos=1 )

    guiChanges <- list()
    guiChanges$fits       <- fits
    guiChanges$iFit       <- 1
    guiChanges$nFit       <- 1
    guiChanges$fitName    <- names(fitTracker)[1]
    guiChanges$fitDesc    <- "Description1"
    guiChanges$opAdmbPath <- ""
    guiChanges$opCppPath  <- ""
    guiChanges$opProjDir  <- ""

    #goMenu <- TRUE     # Everything required for GUI creation should be defined.    
  }

  # Create the GUI.
  if ( goMenu )
  {
    # Initialize the GUI from the description file using PBSmodelling createWin.
    createWin( paste( dir, "/", win, "Win.txt", sep="" ) )
    guiDefault <- getWinVal( winName=win )

    # ARK *** Revise this to grab from the options page.
    #.admbReadOptions()  # ADMB option list to global .admbOptions

    if ( guiChanges$opAdmbPath=="" | is.null(guiChanges$opAdmbPath) )
      guiChanges$opAdmbPath <- guiDefault$opAdmbPath

    if ( guiChanges$opCppPath=="" | is.null(guiChanges$opCppPath) )
      guiChanges$opCppPath <- guiDefault$opCppPath
    
    #if ( guiChanges$opProjDir=="" | is.null(guiChanges$opProjDir) )
    #  guiChanges$opProjDir <- getwd()

    # Apply any changes to the GUI.
    setWinVal( guiChanges, winName=win )
    guiInfo <- getWinVal( scope="L", winName=win )        

    # Reset the changes list.
    guiChanges <- list()

    # If there was no previously saved model fits.
    if ( !fitFileExists )
    {
      # Prompt for a model parameter file.
      modPars <- .readModParFile( .FMODPARS )
        
      # Get a tpl file name.
      tmpFile      <- .openAdmbFile( "", ext=".tpl" )
      tmpFile      <- basename( tmpFile )
      guiChanges$prefix  <- .getFileName( tmpFile )$fileName      
      
      # Successful load of model parameters and tpl?
      if ( !is.null( modPars ) && !is.null(guiChanges$prefix) )
      {
        assign( "modPars",    modPars,pos=1 )   # Can be sorted in GUI.
        assign( ".initModPars",modPars,pos=1 )  # Remembers initial order.
      
        # Validation checks?
        #valid <- .validGuiModPars()

        # Reset Sorting.
        guiChanges$mdSortOrder <- "DEFAULT"
        guiChanges$mdSortName  <- FALSE
        guiChanges$mdSortType  <- FALSE
        
        # Close the window and re-open 
        closeWin()
      
        # Initialize the GUI from the description file using PBSmodelling createWin.
        # Interesting, this appears to work, but suspect it for later problems.
        # Also, when we move to a project approach, the guiInfo parameters should
        # be loaded.
        createWin( paste( win, "Win.txt", sep="" ) )
              
        fitTracker[[1]]$guiInfo <- guiInfo
        fitTracker[[1]]$status$modStamp <- .getStamp()
      
        guiChanges$modPars   <- modPars
      
        guiChanges$iFit      <- 1
        guiChanges$nFit      <- length(fitTracker)
        guiChanges$fitName   <- names(fitTracker)[1]
        guiChanges$fitDesc   <- "Description1"
         
        setWinVal( guiChanges, winName=win )
        guiInfo <- getWinVal( scope="L", winName=win )
      }
      else
        cat( "\nERROR (.setupGuiMod): Invalid model parameter file or no tpl.\n" )
    }     # endif No fit tracking file exists.

    # Write the ADMB files.
    if ( mdParsToADMB )
      .writeADMBinputFiles()        

    .ghostGuiMod()      
      
    .updateStatus( fitTracker[[iFit]]$status$modStamp,
                      fitTracker[[iFit]]$status$fitStamp,
                      winName=win )

    guiInfo <- getWinVal( scope="L", winName=win )
                      
    # Assign the GUI settings to the Global environment.
    assign( ".guiModPars",  guiInfo,     pos=1 )
    
    fitTracker[[iFit]]$guiInfo <- guiInfo
    
    # Make sure all fitTracker changes are committed.
    assign( "fitTracker", fitTracker, pos=1 )                          
    
    act     <- getWinAct()[1]              # Get last menu window action.
    guiInfo <- getWinVal( scope="L", winName=win )
    
    # Set the ADMB options.
    assign( ".admbOptions", .setAdmbOptions( guiInfo ), pos=1 )
    
    # Do the plotting.
    if ( act=="notebook" )
    {
       if ( modTab==2 )
         if ( any( fits$Select==TRUE ) )
           .doFitPlots( prefix, fitTracker ) 
         else
         {
           cat( "\nWARNING (.setupGuiMod) At least one model must be selected for plotting.\n" )
           graphics.off()
         }
    }
  }
  else
    cat( "\nERROR (.setupGuiMod): GUI creation not possible:\n ")

  return( invisible() )
}     # .setupGuiMod

# .subGuiModFit (Processes guiMod submit actions, e.g., buttons)
# Purpose:    This is the function that directs the guiMod program flow:
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
.subGuiModFit <- function()
{
# The use of "notebook" tends to generate very busy .subGuiX functions.  The
# strategy used here is to have a .subGuiX function for each page of notebook.
# e.g., .subGuiModel, .subGuiFit, .subGuiOptions.

  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  act        <- getWinAct()[1]                      # Last GUI action.
  guiChanges <- list()                              # List to track GUI changes.

  valid <- .validGuiModPars()                       # GUI parameters valid?

  if ( valid )
  {
    # Assign the GUI settings to the Global environment.
    assign( ".guiModPars", guiInfo, pos=1 )
   
    # This call sets MCMC, -ind, etc by updated guiInfo$argvec.
    #.admbSetArgs( win )
   
    #----------------------------------------------------------------#
    #--                   HEADER Menu Actions                      --#
    #----------------------------------------------------------------#

    #----------------------------------------------------------------#
    #--                   Notebook Page Actions                    --#
    #----------------------------------------------------------------#    
 
    if ( modTab==1 )                     # "Model" notebook page.
      .subGuiModel()
    else if ( modTab==2 )                # "Fit" notebook page.
      .subGuiFit()         
    else if ( modTab==3 )                # "Options" notebook page.
      .subGuiOptions()
    
    # Update for changes made in .subGuiModel, .subGuiFit, and .subGuiOptions.  
    guiInfo <- getWinVal( scope="L", winName=win )
    
    # LOAD the model parameters, set the GUI.
    if ( act=="mdLoadPars" )
    {
      # Read a model parameter file.
      tmpPars <- .readModParFile( .FMODPARS )
      if ( nrow(tmpPars)!=nrow(modPars) )
      {
        tmpPars <- NULL
        cat( "\nERROR (.subGuiModFit) Parameter list dimensions do not match, no change.\n" )
      }
    
      # Successful load of model parameters?
      if ( !is.null( modPars ) )
      {
        assign( "modPars",     modPars,pos=1 )  # Can be sorted in GUI.
        assign( ".initModPars",modPars,pos=1 )  # Remembers initial order.
  
        # Validation checks?
        #valid <- .validGuiModPars()

        guiChanges$modPars <- tmpPars
     
        # Reset Sorting.
        guiChanges$mdSortOrder <- "DEFAULT"
        guiChanges$mdSortName  <- FALSE
        guiChanges$mdSortType  <- FALSE
        
        guiChanges$prefix      <- prefix
        guiChanges$argvec      <- argvec
        
        guiChanges$mdObjFun   <- 0
        guiChanges$mdFunEvals <- 0
        guiChanges$mdMaxGrad  <- 0
        guiChanges$mdExitCode <- 0
        
        guiChanges$fitName <- fitName
        guiChanges$fitDesc <- "NO_NAME"
        guiChanges$iFit    <- iFit
        guiChanges$nFit    <- length(fitTracker)
     
        # Initialize the GUI from the description file using PBSmodelling createWin.
        # Interesting, this appears to work, but suspect it for later problems.
        # Also, when we move to a project approach, the guiInfo parameters should
        # be loaded.
        # closeWin, createWin was here. BAD, causes memory issues. Don't do it.
        
        setWinVal( guiChanges, winName=win )

        fitTracker[[iFit]]       <- .resetFitNode( fitTracker[[iFit]] )
        fitTracker[[iFit]]$files <- .resetFitTrackerFiles()
        fitTracker[[iFit]]$status$modStamp <- .getStamp()
        fitTracker[[iFit]]$status$fitStamp <- "NO_FIT"
      }
    }     # if mdLoadPars.
    
    # SAVE the model parameters to an ASCII file in current sort order.
    if ( act=="mdSavePars" )
    {
       .saveModParFile( parFile=paste( "mseR",prefix,".par", sep="" ) )
    }
   
    #----------------------------------------------------------------#
    #--                       Footer Actions                       --#
    #----------------------------------------------------------------#    
    
    # EXIT the GUI (leave graphics on).
    if ( act=="mdExit" )
    {
      # Save fitTracker only if at least one fit has been added.
      if ( length(fitTracker) > 0 )
      {
        guiInfo <- getWinVal( scope="L", winName=win )
        fitTracker[[iFit]]$guiInfo <- guiInfo
        
        for ( i in 1:length(fitTracker) )
          fitTracker[[i]]$guiInfo$iFit <-  guiInfo$iFit
          
        assign( "fitTracker", fitTracker, pos=1 )
          
        .saveFitToRdata( fitFile=.FFITS, overWrite=TRUE )
      }
      else
      {
        cat( "\nMSG (.subGuiModFit) No fits available, fitTracker not saved...\n" )
        if ( file.exists(.FFITS) )
          file.remove( .FFITS )
      }
      on.exit( closeWin() )     # Don't close until .subGuiModFit has terminated.
    }
    
    # TABLES requested.
    if ( act=="mdTables" )
    {
      saveStatus <- .saveFitToExcel( .FFXLS )
      if ( saveStatus$fileSaved )
        openFile( .FFXLS )
    }

    # Update the fit list.
    if ( length(fitTracker) > 0 )
    {
      setWinVal( guiChanges, winName=win )
      guiInfo <- getWinVal( scope="L", winName=win )

      fits <- .updateFits(fitTracker, fits )
      
      for ( i in 1:length(fitTracker) )
        fitTracker[[i]]$guiInfo$fits <- fits
        
      fitTracker[[iFit]]$guiInfo <- guiInfo
      assign( "fitTracker",fitTracker,pos=1 )
    }
    # Make a default fits list.
    else
      fits <- .initFits(maxFits=100)
    
    guiChanges$fits <- fits
    
    # Sets status box colors (red, green).
    .updateStatus( fitTracker[[iFit]]$status$modStamp,
                     fitTracker[[iFit]]$status$fitStamp,
                     winName=win )  
    
    .ghostGuiMod()
      
    setWinVal( guiChanges, winName=win )
    
    # Fit plotting.
    if ( modTab==2 )
      if ( any( fits$Select==TRUE ) )
        .doFitPlots( prefix, fitTracker )
      else
      {
        cat( "\nWARNING (.subGuiModFit): At least one model must be selected for plotting.\n" )    
        graphics.off()
      }
  }     # if valid.
  else
  {
    # Invalid GUI parameters so bring R console to top.
    bringToTop(-1)
  }     # if valid GUI parameters.

  return( invisible() )  
}     # END function .subGuiModFit

.subGuiModel <- function()
{
  #----------------------------------------------------------------#
  #--              Notebook Model Page Actions                   --#
  #----------------------------------------------------------------#
  win        <- .getWinName()                       # Get the current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  act        <- getWinAct()[1]                      # Last GUI action.
  guiChanges <- list()                              # List to track GUI changes.    

  # Edit the model parameters (this breaks fit synchronization).
  if ( act=="modPars" )
  {
    # Set all fit results (e.g., cor, mcmc, par, rep std) for node to NULL.
    fitTracker[[iFit]] <- .resetFitNode( fitTracker[[iFit]] )
      
    # Reset the model stamp, and set fit stamp to "NO_FIT".
    fitTracker[[iFit]]$status$modStamp <- .getStamp()
    fitTracker[[iFit]]$status$fitStamp <- "NO_FIT"

    # Update the ADMB results which were just reset using .resetFitNode.
    guiChanges$mdObjFun   <- fitTracker[[iFit]]$admb$objFun
    guiChanges$mdMaxGrad  <- fitTracker[[iFit]]$admb$maxGrad
    guiChanges$mdExitCode <- fitTracker[[iFit]]$admb$exitCode
    guiChanges$mdFunEvals <- fitTracker[[iFit]]$admb$funEvals
  }
  
  if ( act=="mdParsToADMB" )
  {
    .ghostGuiMod()
  }
    
  # Sort the parameter display.
  if ( act=="mdSortOrder" || act=="mdSortName" || act=="mdSortType" )
  {
    # Establish current ordering of types.
    if ( mdSortOrder=="DEFAULT" )
    {
      typeOrder <- mdSortOrder.values
      typeOrder <- typeOrder[ typeOrder!="DEFAULT" ]
      typeOrder <- sort( typeOrder )
    }
    else
    {
      typeOrder <- mdSortOrder.values
      typeOrder <- typeOrder[ typeOrder!="DEFAULT" ]
      typeOrder <- typeOrder[ typeOrder!=parType ]
      typeOrder <- c( mdSortOrder,typeOrder )
    }

    # Sort by Parameter within Type using current typeOrder.
    if ( guiInfo$mdSortType && guiInfo$mdSortName )
    {
      # Find the position of the first "$" in parameter vector.      
      posDollar <- unlist( gregexpr( "\\$",modPars$parameter ) )
      # Extract the parameter & type using the "$" position.
      types     <- substring( modPars$parameter,1,posDollar-1 )
      parameter <- substring( modPars$parameter,(posDollar+1), )
      
      guiChanges$modPars <- modPars[order(ordered(types,levels=typeOrder),
                              parameter),]
    }

    # Sort by Type using current typeOrder, leaving Parameter as input.      
    if ( guiInfo$mdSortType && !guiInfo$mdSortName )
    {
      # Restore input sort order.
      modPars <- modPars[ dimnames(.initModPars)[[1]], ]
      
      # Find the position of the first "$" in parameter vector.      
      posDollar <- unlist( gregexpr( "\\$",modPars$parameter ) )
      # Extract the type using the "$" position.
      types     <- substring( modPars$parameter,1,posDollar-1 )
       
      guiChanges$modPars <- modPars[ order(ordered(types,levels=typeOrder)), ]
    }

    # Leave Parameter as input, sort by Type using current typeOrder.
    if ( !guiInfo$mdSortType && guiInfo$mdSortName )
    {
      # Restore input sort order.
      modPars            <- modPars[ dimnames(.initModPars)[[1]], ]      
 
      # Find the position of the first "$" in parameter vector.      
      posDollar <- unlist( gregexpr( "\\$",modPars$parameter ) )
      # Extract the parameter using the "$" position.
      parameter <- substring( modPars$parameter,(posDollar+1), )

      guiChanges$modPars <- modPars[ order(parameter), ]
    }

    # No sorting, restore input sort order.      
    if ( !guiInfo$mdSortType && !guiInfo$mdSortName )
    {
      # Restore input sort order.
      guiChanges$modPars <- modPars[ dimnames(.initModPars)[[1]], ]
    }        
  }     # sortType or sortName.
    
  #----------------------------------------------------------------#
  # File IO Arguments                                              #
  #----------------------------------------------------------------#
  
  # ADMB I/O file dataframe *** here is where file.exists types of
  # checks should be done for input files.
  if ( act=="mdFileIO" )
  {
    guiChanges$mdFileIO <- mdFileIO
  }
    
  # Edit the ADMB I/O files.
  if ( act=="mdEditFile" )
  {
    for ( i in 1:nrow(mdFileIO) )
    {
      if ( mdFileIO$Select[i] )
      {
        if ( mdFileIO$FileName[i]!="" )
          .editTextFile( mdFileIO$FileName[i] )
      }
    }
  }
    
  # Set the ADMB I/O files.
  if ( act=="mdSetFile" )
  {
    for ( i in 1:nrow(mdFileIO) )
    {
      if ( mdFileIO$Select[i] )
      {
        val <- .setFile( mdFileIO$FileName[i],
                  list( c(".dat","dat files"),c(".ctl","ctl files") ) )
        if ( !is.null(val) )
        {
          # Use the existing argument, or set to -ind if NULL or empty string.
          if ( mdFileIO$Arg[i] == "" | is.null(mdFileIO$Arg[i]) )
            mdFileIO$Arg[i]      <- "-ind"
            
          # If the file path is the same as the working directory, just save
          # the file name, not the entire path.
          wkDir   <- getwd()
          fileDir <- dirname( val )
          if ( wkDir==fileDir )
          {
            # File directory string does not have the final separator, so add 2.
            val <- substring( val, nchar( fileDir )+2,nchar(val) )
          }
          mdFileIO$FileName[i] <- val
        }
      }
    }
    guiChanges$mdFileIO <- mdFileIO
  }
    
  # Delete the ADMB I/O files.
  if ( act=="mdDelFile" )
  {
    for ( i in 1:nrow(mdFileIO) )
    {
      if ( mdFileIO$Select[i] )
      {
        mdFileIO$Arg[i]      <- ""
        mdFileIO$FileName[i] <- ""
        mdFileIO$Select[i]   <- FALSE
      }
    }
    guiChanges$mdFileIO <- mdFileIO
  }
  
  # Write the ADMB I/O files.
  if ( act=="mdWrtFile" )
  {
    if ( mdParsToADMB )
      .writeADMBinputFiles()
    else
      cat( "\nMSG (.subGuiModel) ADMB files not written - user inputs are off.\n" )
  }
    
  #----------------------------------------------------------------#
  #-- ADMB Control and Status Widget (Don't touch these please)  --#
  #----------------------------------------------------------------#
    
  # Edit the ADMB pin file.
  if ( act=="admbEditPin" )
  {
    cat( "\nMSG (.subGuiModel) .admbEditFiles called for *.pin ...\n" )
    .admbEditFiles( guiInfo$prefix,c(".pin"))
  }
    
  # Set the ADMB pin file name.
  if ( act=="mdPinOpen" )
  {
    tmpFile <- .openAdmbFile( admbFile=guiInfo$prefix, ext=".pin" )
    guiChanges$mdPinFile <- .getFileName( basename( tmpFile ) )$fileName
    if ( guiChanges$mdPinFile!=guiInfo$prefix )
      guiChanges$useAppName <- FALSE      
  }    
    
  # Edit the ADMB ctl file.
  if ( act=="admbEditCtl" )
  {
    cat( "\n(.subGuiModel) .admbEditFiles called for *.ctl ...\n" )
    .admbEditFiles( guiInfo$prefix,c(".ctl") )   
  }
    
  # Set the ADMB ctl file name.
  if ( act=="mdCtlOpen" )
  {
    tmpFile <- .openAdmbFile( admbFile=guiInfo$prefix, ext=".ctl" )
    guiChanges$mdCtlFile <- .getFileName( basename( tmpFile ) )$fileName
    if ( guiChanges$mdCtlFile!=guiInfo$prefix )
      guiChanges$useAppName <- FALSE
  }   
    
  # Edit the ADMB tpl file.
  if ( act=="mdEditTpl" )
  {
    cat( "\n(.subGuiModel) .admbEditFiles called for *.tpl ...\n" )
    .admbEditFiles( guiInfo$prefix,c(".tpl") )   
  }

  # Set the ADMB tpl name.
  if ( act=="mdOpenTpl" )
  {
    tmpFile <- .openAdmbFile( admbFile=guiInfo$prefix, ext=".tpl" )
    guiChanges$prefix <- .getFileName( basename( tmpFile ) )$fileName
  }

  if ( act=="TPL2CPP" )
  {
    tpl2cpp( prefix, raneff=FALSE )
  }
  
  if ( act=="compile" )
  {
    compile( prefix, raneff=FALSE, safe=!safe, logfile=TRUE,
             verbose=verbose, comp=.admbOptions$compiler )
  }
  
  if ( act=="link" )
  {
    link( prefix, raneff=FALSE, safe=!safe, logfile=TRUE,
          add=TRUE, verbose=TRUE, comp=.admbOptions$compiler )
  }
  
  if ( act=="make" )
  {
    tpl2cpp( prefix, raneff=FALSE )
    compile( prefix, raneff=FALSE, safe=!safe, logfile=TRUE,
             verbose=verbose, comp=.admbOptions$compiler )
    link( prefix, raneff=FALSE, safe=!safe, logfile=TRUE,
          add=TRUE, verbose=TRUE, comp=.admbOptions$compiler )
  }
    
  if ( act=="run" )
  {
    # Bring the R console forward.
    graphics.off()
    bringToTop( -1 )
    cat( "\n#-------------------------------------------------------------------#\n" )
    cat(   "# MSG (.subGuiModel) Running program: ",prefix," please wait...\n" )
    cat(   "#-------------------------------------------------------------------#\n" )    
    
    if ( mdParsToADMB )
      .writeADMBinputFiles()

      #------------------------------------------------------------#
      # Add to the fitTracker list.  Each fit gets the following:  #
      # (1) guiInfo with control settings at the time of the fit.  #
      # (2) ADMB cor, std, par files attached to the list.         #
      # (3) USERCODE ADMB rep file attached to the list.           #
      # (4) ADMB status (objfun, maxgrad, convergeCode, funEvals.  #
      # (5) Date/time stamps for model and fit synchronization.    #
      # (6) Date/time stamped copies of cor, std, par, mcmc files. #
      #     e.g., If the tpl is called catage.tpl, then the file   #
      #     catage.par is copied to catageDateTime.par, where the  #
      #     "DateTime" is the date/time stamp.                     #
      #     This is particularly important for an mcmc output file #
      #     as long chains may be too large for fitTracker.        #
      #------------------------------------------------------------#
    
    # ADMB command-line file I/O qualifiers, added to argvec.
    #.admbSetArgs( winName=win )
    #guiInfo <- getWinVal( scope="L", winName=win )
    
    tmpArgs <- ""

    # Add the file I/O switches.          
    for ( i in 1:nrow(mdFileIO) )
    {
      if ( mdFileIO$FileName[i]!="" && mdFileIO$Arg[i]!="" )
      {
        tmpArgs <- paste( tmpArgs, mdFileIO$Arg[i],mdFileIO$FileName[i],sep=" " )
      }
    }
    
    # Add the normal, -mcmc, or -lprof to the argument.
    #if ( admbArg=="normal" )
    #  Do nothing.
    if ( admbArg=="mcmc" )
      tmpArgs <- paste(tmpArgs,"-mcmc",.asIs(nsims),"-mcsave",.asIs(nthin),sep=" ")
    if ( admbArg=="profile" )
      tmpArgs <- paste( tmpArgs,"-lprof", sep=" " )
      
    # Add the user specified switches.
    tmpArgs <- paste( tmpArgs,argvec,sep=" " )
    cat( "\nMSG (.subGuiModel) ADMB argument list is:\n\n", tmpArgs,"\n" )
    
    #setWinVal( guiChanges, winName=win )
    #getWinVal( scope="L", winName=win )
    
    # ADMB output files.
    corFile  <- paste( prefix,".cor", sep="" )
    mcmcFile <- paste( prefix,".mcmc",sep="" )
    parFile  <- paste( prefix,".par", sep="" )
    repFile  <- paste( prefix,".rep", sep="" )
    stdFile  <- paste( prefix,".std", sep="" )          
    
    # First delete any existing prefix cor, mcmc, par, std, rep files.
    if ( file.exists( corFile ) )
      file.remove( corFile )
    if ( file.exists( mcmcFile ) )
      file.remove( mcmcFile )
    if ( file.exists( parFile ) )
      file.remove( parFile )
    if ( file.exists( repFile ) )
      file.remove( repFile )
    if ( file.exists( stdFile ) )
      file.remove( stdFile )

    # Update the date/time stamps.    
    fitStamp <- .getStamp()
    modStamp <- fitStamp
    
    # Run the executable.
    #.admbRun()
    run( prefix, tmpArgs, logfile=TRUE, add=TRUE, verbose=verbose )
     
    # *** Do we allow non-converged fits to be added? Yes at thie
    #     point since you might want to view why non-convergence.
            
    # IF they exist, read in the results from the cor, par, rep, std files.
    # Make copies of each using the date/time stamp.

    tmpPathFname <- ""
    if ( opProjDir!="" )
    {
      wkDir <- getwd()
      
      if ( opProjDir!=wkDir )
      {
        if ( substring( opProjDir,1,nchar(wkDir) )==wkDir )
        {
          # This is "+2" because you have to allow 1 character for a "/".
          projDir <- substring( opProjDir,nchar(wkDir)+2,nchar(opProjDir) )
          tmpPathFname <- paste( projDir,"/",sep="" )
        }
      }
    }
     
    # *** Do we need to copy the date/time stamped pin, dat, ctl, tpl files?
    # *** We could loop through mdFileIO and copy each file with the
    # *** date/time stamp appended.

    if ( file.exists( corFile ) )
    {
      fitTracker[[iFit]]$cor <-.admbImportRep( prefix, suffix=c(".cor") )
      fname <- paste( tmpPathFname,prefix,fitStamp,".cor",sep="" )     
      cat( "\nMSG (.subGuiModel) Writing ",corFile," to ",fname,"\n" )      
      file.copy( corFile,fname,overwrite=TRUE)
    }
    else
      cat( "\nWARNING (.subGuiModel) File does not exist: ",corFile,"\n" )

    if ( file.exists( mcmcFile ) )
    {
      # *** Eventually we need to load the thinned chain.
      fitTracker[[iFit]]$mcmc <- paste( mcmcFile,".mcmc", sep="" )
      fname <- paste( tmpPathFname,prefix,fitStamp,".mcmc",sep="" )     
      cat( "\nMSG (.subGuiModel) Writing ",mcmcFile," to ",fname,"\n" )      
      file.copy( mcmcFile,fname,overwrite=TRUE)
    }
    else
      cat( "\nWARNING (.subGuiModel) File does not exist: ",mcmcFile,"\n" )    
        
    if ( file.exists( parFile ) )
    {
      fitTracker[[iFit]]$par <-.admbImportRep( prefix, suffix=c(".par") )
      fname <- paste( tmpPathFname,prefix,fitStamp,".par",sep="" )     
      cat( "\nMSG (.subGuiModel) Writing ",parFile," to ",fname,"\n" )
      file.copy( parFile,fname,overwrite=TRUE)
    }
    else
      cat( "\nWARNING (.subGuiModel) File does not exist: ",parFile,"\n" )    
        
    if ( file.exists( repFile ) )
    {
      #fitTracker[[iFit]]$rep <-.admbImportRep( prefix, suffix=c(".rep") )
      fitTracker[[iFit]]$rep <- lisread( repFile )
      fname <- paste( tmpPathFname,prefix,fitStamp,".rep",sep="" )     
      cat( "\nMSG (.subGuiModel) Writing ",repFile," to ",fname,"\n" )
      file.copy( repFile,fname,overwrite=TRUE)
    }
    else
      cat( "\nWARNING (.subGuiModel) File does not exist: ",repFile,"\n" )    
        
    if ( file.exists( stdFile ) )
    {
      fitTracker[[iFit]]$std <-.admbImportRep( prefix, suffix=c(".std") )
      fname <- paste( tmpPathFname,prefix,fitStamp,".std",sep="" )     
      cat( "\nMSG (.subGuiModel) Writing ",stdFile," to ",fname,"\n" )      
      file.copy( stdFile,fname,overwrite=TRUE)        
    }
    else
      cat( "\nWARNING (.subGuiModel) File does not exist: ",stdFile,"\n" )    
      
    # Assign the ADMB status node.
    admb <- list( maxGrad=NA, objFun=NA, exitCode=NA, funEvals=NA )
    if ( any(names(fitTracker[[iFit]]$rep)=="maxGrad") )
     admb$maxGrad <- fitTracker[[iFit]]$rep$maxGrad
    if ( any(names(fitTracker[[iFit]]$rep)=="objFun") )
      admb$objFun <- fitTracker[[iFit]]$rep$objFun
    if ( any(names(fitTracker[[iFit]]$rep)=="exitCode") )
      admb$exitCode <- fitTracker[[iFit]]$rep$exitCode
    if ( any(names(fitTracker[[iFit]]$rep)=="funEvals") )
      admb$funEvals <- fitTracker[[iFit]]$rep$funEvals
    fitTracker[[iFit]]$admb <- admb
      
    # Update GUI changes.
    guiChanges$mdObjFun   <- admb$objFun
    guiChanges$mdMaxGrad  <- admb$maxGrad
    guiChanges$mdExitCode <- admb$exitCode
    guiChanges$mdFunEvals <- admb$funEvals
      
    # Assign the "files" node.
    files <- list( ctl=NULL, pin=NULL, dat=NULL, cor=NULL, mcmc=NULL,
                   par=NULL, rep=NULL, std=NULL, fitTracker=NULL, xls=NULL )
    
    # ADMB output files.
    if ( file.exists( corFile ) )
      files$cor <- corFile
    if ( file.exists( mcmcFile ) )
      files$mcmc <- mcmcFile
    if ( file.exists( parFile ) )
      files$par <- parFile
    if ( file.exists( repFile ) )
    files$rep <- repFile
    if ( file.exists( stdFile ) )
      files$std <- stdFile
        
    # EXCEL and Rdata files.
    files$fitTracker <- .FFITS
    files$xls <- .FXLS
    fitTracker[[iFit]]$files   <- files

    fitTracker[[iFit]]$status  <- list( modStamp=modStamp, fitStamp=fitStamp )
    
    # Update fits for Fit page.
    fits <- .updateFits( fitTracker, fits )
    
    fits$Select <- rep( FALSE, length(fits$Select) )
    fits$Select[iFit] <- TRUE
    guiChanges$fits <- fits
  }     # admbRun
    
  # Import ADMB files into R objects.
  if ( act=="admbImport" )
  {
    .admbImportRep( prefix, global=TRUE )
  }
 
  #----------------------------------------------------------------# 
  # Fit tracker widget.                                            #
  #----------------------------------------------------------------#

  # Edit the fit name.
  if ( act=="fitName" )
  {
    fitTracker[[iFit]]$guiInfo$fitName <- fitName
    guiChange$fitName <- fitName
    # Update the fits widget.
    fits <- .updateFits(fitTracker, fits)
    guiChanges$fits <- fits    
  }

  # Edit the fit description.
  if ( act=="fitDesc" )
  {
    fitTracker[[iFit]]$guiInfo$fitDesc <- fitDesc
    guiChanges$fitDesc <- fitDesc
    # Update the fits widget.
    fits <- .updateFits(fitTracker, fits)
    guiChanges$fits <- fits    
  }
    
  # Add a Fit to the tracking list.
  if ( act=="addFit" )
  {
    # Create the next slot for a fit, update GUI counters.
    nFit    <- length( fitTracker ) + 1
    iFit    <- nFit
    fitTracker[[nFit]] <- .createFitNode()
      
    fitName <- paste( "Model",      nFit,sep="" )
    fitDesc <- paste( "Description",nFit,sep="" )
        
    # Ensure that the fitName doesn't already exist.
    i <- 0
    while( any(fitName==names(fitTracker) ) )
    {
      i <- i + 1
      fitName <- paste( fitName,"v",i,sep="" )
    }
    
    i <- 1
    j <- 0
    while( any(fitDesc==fitTracker[[i]]$guiInfo$fitDesc ) )
    {
      j <- j + 1
      fitDesc <- paste( fitDesc,"v",j,sep="" )
    }    
       
    #------------------------------------------------------------#
    # Add to the fitTracker list.  Each fit gets the following:  #
    # (1) guiInfo with control settings at the time of the fit.  #
    # (2) ADMB cor, std, par files attached to the list.         #
    # (3) USERCODE ADMB rep file attached to the list.           #
    # (4) ADMB status (objfun, maxgrad, convergeCode, funEvals.  #
    # (5) Date/time stamps for model and fit synchronization.    #
    # (6) Date/time stamped copies of cor, std, par, mcmc files. #
    #     e.g., If the tpl is called catage.tpl, then the file   #
    #     catage.par is copied to catageDateTime.par, where the  #
    #     "DateTime" is the date/time stamp.                     #
    #     This is particularly important for an mcmc output file #
    #     as long chains may be too large for fitTracker.        #
    #------------------------------------------------------------#

    # Add the guiInfo from the currently displayed fit.
    fitTracker[[nFit]]$guiInfo <- guiInfo

    # Update the name of the fitTracker node.
    fitTracker[[nFit]]$guiInfo$fitName <- fitName
    names( fitTracker )[[nFit]] <- fitName
      
    # Update iFit and nFit in all fits and save the fitTracker.
    for ( i in 1:length(fitTracker) )
    {
      fitTracker[[i]]$guiInfo$iFit <- i
      fitTracker[[i]]$guiInfo$nFit <- nFit
    }

    # Reset the admb reporting variables.
    fitTracker[[nFit]] <- .resetFitNode(fitTracker[[nFit]])

    # Update the date/time stamps.
    fitTracker[[nFit]]$status$modStamp <- .getStamp()
    fitTracker[[nFit]]$status$fitStamp <- "NO_FIT"
    
    # Update the fits object for the Fit page.
    fits <- .updateFits( fitTracker, fits )
    guiChanges$fits       <- fits
    guiChanges$iFit       <- iFit
    guiChanges$nFit       <- nFit
    guiChanges$fitName    <- fitName
    guiChanges$fitDesc    <- fitDesc
    guiChanges$fits       <- fits
    guiChanges$mdObjFun   <- 0
    guiChanges$mdMaxGrad  <- 0
    guiChanges$mdExitCode <- 0
    guiChanges$mdFunEvals <- 0
  }
  
  # Next Fit.
  if ( act=="nextFit" )
  {
    if ( length(fitTracker) > 0 )
    {
      # Do not permit iFit > nFit.
      idxFit <- min( (iFit+1), nFit )
 
      # Update the Fit on the GUI.
      guiChanges <- fitTracker[[idxFit]]$guiInfo    
      guiChanges$iFit <- idxFit
    }
    else
      cat( "\nMSG (.subGuiModel) No models available.\n" )
  }
    
  # Previous Fit.
  if ( act=="prevFit" )
  {
    if ( length(fitTracker) > 0 )
    {
      # Get the index of the previous fit.  Do not permit iFit < 1.
      idxFit <- max( 1, (iFit-1) )
    
      # Update the parameter object widget.
      guiChanges <- fitTracker[[idxFit]]$guiInfo
      guiChanges$iFit <- idxFit
    }
    else
      cat( "\nMSG (.subGuiModel) No models available.\n ")
  }
    
  # Remove a Fit from the tracking list.
  if ( act=="remFit" )
  {
    # Rules:
    # (b) If Fit (1,...,n-1) is removed, show the next Fit.
    # (c) If Fit n is removed, show Fit n-1.
    
    # Only take action if there is at least one Fit.
    if ( length(fitTracker)> 0 )
    { 
      # Find the index of the Fit to be removed.
        
      n    <- length(fitTracker)
      iRem <- c(1:n)[ guiInfo$fitName==names(fitTracker) ]

      # Problem here is to show either next node, or the previous node if
      # node n is removed.  But, removing the node changes the number of 
      # nodes and the index of the one to be displayed.
      #
      # Case 1: Remove node n.  Update GUI with node n-1. New index after
      #         removal of node n is still node n-1.
      # Case 2: Remove one of nodes i=2,...,n-1.  Update GUI with node i-1.
      #         New index after removal is still node i-1.
      # Case 1: Remove node 1.  Update GUI with node 2.  New index after
      #         removal of node 1 is node 1.

      if ( n==1 )
      {
        # Can't remove last fit, but maybe break synchronization?
        iShow <- 1
        cat( "\nWARNING (.subGuiModel) Removal denied, you must have at least model.\n" )            
      }
      else
      {
        if ( iRem==1 )
        {
          iShow <- 2
          iNew  <- 1
        }
        else
        {
          iShow <- iRem - 1    # Removing one of nodes 1 through n-1.
          iNew  <- iShow
        }
       
        # Get the results for the new node to show.
        guiChanges <- fitTracker[[iShow]]$guiInfo
        
        # Remove the node.
        fitTracker[[iRem]] <- NULL

        # Renumber, and re-assign revised fitTracker to global.
        for ( i in 1:length(fitTracker) )
        {
          fitTracker[[i]]$guiInfo$iFit <- i
          fitTracker[[i]]$guiInfo$nFit <- length(fitTracker)
        }
          
        # Now update the iFit and nFit.
        guiChanges$iFit <- iNew
        guiChanges$nFit <- length(fitTracker)
        
        # Update the fits widget.
        fits <- .updateFits(fitTracker, fits)
        guiChanges$fits <- fits
      }
    }   # If fitTracker > 0.
    
  }  # remFit
        
  # Export Fit list to an R directory file.
  if ( act=="expFit" )
  {
    # Save fitTracker only if at least one fit has been added.
    if ( length(fitTracker) > 0 )
      .saveFitToRdata()
    else
      cat( "\nMSG (.subGuiModel) No fits available, fitTracker not saved.\n" )
  }
    
  # Import Fit list from an R directory file.
  if (act=="impFit" )
  {
    if ( !is.null(.loadFitFromRdata()) )
    {
      # Update the trial information.
      guiChanges         <- fitTracker[[1]]$guiInfo
      guiChanges$iFit    <- 1
      guiChanges$nFit    <- length(fitTracker)
      guiChanges$fitName <- names(fitTracker)[1]
    }
    else
      cat( "\nMSG (.subGuiModel) No file selected, tracking data not loaded.\n" )
  }

  if ( act=="orphans" )
  {
    cat( "\n(.subGuiModel) Removing all orphan files not in fitTracker...\n" )
      
    stampList <- character( length(fitTracker) )
    for ( i in 1:length(fitTracker) )
      stampList[i] <- fitTracker[[i]]$status$modStamp
        
    # Null string used to ensure prefix.cor, prefix.par, etc. NOT removed.
    stampList <- c( "",stampList )
        
    fileExt <- c( ".cor",".ctl",".mcmc",".pin",".rep",".std" )
    
    for ( j in 1:length(fileExt) )
    {
      allFiles <- list.files( pattern=fileExt[j] )
        
      # Check the list to ensure that the filename contains a valid date stamp.
      # Use .gotStamp.
        
      # Somehow we have to restrict the removals to those files containing
      # a date/time stamp.  So this is a hack ***.  There are 14 characters
      # in the date/time stamp.  

      keep     <- paste( prefix,stampList,fileExt[j], sep="" )
      remFiles <- setdiff( allFiles, keep )
        
      # Now check to ensure data/time stamp.
      result <- unlist(strsplit( remFiles, ".", fixed = TRUE))
        
      cat( "\nMSG (.subGuiModel) Removing ",fileExt[j]," orphans...\n" )
      #file.remove( remFiles )
    }
  }
  
  # Set the GUI changes.
  setWinVal( guiChanges, winName=win )

  # Write the ADMB files.
  if ( mdParsToADMB )
    .writeADMBinputFiles()
                    
  assign( "fitTracker", fitTracker, pos=1 )
  
  return( invisible() )
}     # END .subGuiModel function.

.subGuiFit <- function()
{
  #----------------------------------------------------------------#
  #--                Notebook Fit Page Actions                   --#
  #----------------------------------------------------------------#

  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  act        <- getWinAct()[1]                      # Last GUI action.
  guiChanges <- list()                              # List to track GUI changes.
  
  for ( i in 1:length(fitTracker) )
  {
    if ( fits$Select[i]==TRUE )
    {
      tmpName <- paste( prefix,fits$Stamp[i],sep="" )

      # Can the model be selected, does it have a fit?
      if ( act=="updateFits" )
      {
        if ( fits$Stamp[i]=="NO_FIT" )
        {
          fits$Select[i] <- FALSE
          guiChanges$fits <- fits
          cat( "\nMSG (.subGuiFit): No model fit available, no results to plot.\n" )
        }
      }
        
      # Edit the ADMB par file.
      if ( act=="ftPar" )
      {
        cat( "\nMSG (.subGuiFit) .admbEditFiles called for *.par ...\n" )
        .admbEditFiles( tmpName,c(".par"))
      }        
        
      # Edit the ADMB cor file.
      if ( act=="ftCor" )
      {
        cat( "\nMSG (.subGuiFit) .admbEditFiles called for *.cor ...\n" )
        .admbEditFiles( tmpName,c(".cor"))
      }
        
      # Edit the ADMB mcmc file.
      if ( act=="ftMCMC" )
      {
        cat( "\nMSG (.subGuiFit) .admbEditFiles called for *.mcmc ...\n" )
        .admbEditFiles( tmpName,c(".mcmc"))
      }
        
      # Edit the ADMB rep file.
      if ( act=="ftRep" )
      {
        cat( "\nMSG (.subGuiFit) .admbEditFiles called for *.rep ...\n" )
        .admbEditFiles( tmpName,c(".rep"))
      }
        
      # Edit the ADMB std file.
      if ( act=="ftStd" )
      {
        cat( "\nMSG (.subGuiFit) .admbEditFiles called for *.std ...\n" )
        .admbEditFiles( tmpName,c(".std"))
      }                       
    }        
  }
    
  # Select All and Clear All.
  if ( act=="ftSelectAll" )
  {
    for ( i in 1:length(fitTracker) )
      if ( fits$Stamp[i]!="NO_FIT" )
        fits$Select[i] <- TRUE
    guiChanges$fits <- fits
  }
    
  if ( act=="ftClearAll" )
  {
    for ( i in 1:length(fitTracker) )
      fits$Select[i] <- FALSE
    guiChanges$fits <- fits
    graphics.off()
  }
  
  # *** Can EMF, PDF and Plots Off calls go here?
  if ( act=="ftPlotOff" )
    graphics.off()
 
  setWinVal( guiChanges, winName=win )
  #assign( "fits", fits, pos=1 )
  
}   # END .subGuiFit function.

.subGuiOptions <- function()
{
  #----------------------------------------------------------------#
  #--             Notebook Options Page Actions                  --#
  #----------------------------------------------------------------#

  win        <- .getWinName()                       # Current window name.
  guiInfo    <- getWinVal( scope="L", winName=win ) # GUI information local.
  act        <- getWinAct()[1]                      # Last GUI action.
  guiChanges <- list()                              # List to track GUI changes.
  
  if ( act=="opAdmbPath" )
  {
    if ( file.exists( opAdmbPath ) )
      guiChanges$opAdmbPath <- opAdmbPath
    else
    {
      cat( "\nMSG (.subGuiOptions) Requested directory does not exist, not set.\n" )
      guiChanges$opAdmbPath <- ""
    }
  }
  
  if ( act=="opSetAdmb" )
  {
    tmpDir <- selectDir( initialdir=opAdmbPath, title="Set ADMB Library Path",
                usewidget=NULL )
    if ( tmpDir!="" )
      guiChanges$opAdmbPath <- tmpDir
    else
    {
      cat( "\nMSG (.subGuiOptions) ADMB path not selected, not set.\n" )
      guiChanges$opAmdbPath <- ""
    }
  }
  
  if ( act=="opCppPath" )
  {
    if ( file.exists( opCppPath ) )
      guiChanges$opCppPath <- opCppPath
    else
    {
      cat( "\nMSG (.subGuiOptions) Requested directory does not exist, not set.\n" )
      guiChanges$opCppPath <- ""
    }
  }
  
  if ( act=="opSetCpp" )
  {
    tmpDir <- selectDir( initialdir=opCppPath, title="Set C++ Compiler Path",
                usewidget=NULL )
    if ( tmpDir!="" )
      guiChanges$opCppPath <- tmpDir
    else
    {
      cat( "\nMSG (.subGuiOptions) Compiler path not selected, setting unchanged.\n" )
      guiChanges$opCppPath <- ""
    }
  }
  
  if ( act=="opCompOpt" )
  {
    guiChanges$opCompOpt <- opCompOpt
  }
  
  if ( act=="opLinkOpt" )
  {
    guiChanges$opLinkOpt <- opLinkOpt
  }
  
  if ( act=="opSetProj" )
  {
    tmpDir <- selectDir( initialdir=opProjDir, title="Set Project Directory",
                usewidget=NULL )
    if ( tmpDir!="" )
      guiChanges$opProjDir <- tmpDir
    else
    {
      cat( "\nMSG (.subGuiOptions) Project directory not set, using R directory.\n" )
      guiChanges$opProjDir <- ""
    }
  }
  
  if ( act=="opProjDir" )
  {
    if ( file.exists( opProjDir ) )
      guiChanges$opProjDir <- opProjDir
    else
    {
      cat( "\nERROR (.subGuiOptions) Requested directory does not exist, using R directory.\n" )
      guiChanges$opProjDir <- ""
    }
  }
  
  if ( act=="opEditor" )
  {
    if ( file.exists( opEditor ) )
      guiChanges$opEditor <- opEditor
    else
    {
      cat( "\nERROR (.subGuiOptions) Requested editor does not exist, not set.\n" )
      guiChanges$opEditor <- ""
    }
  }
  
  if ( act=="opSetEdit" )
  {
    tmpFile <- selectFile( initialfile=opEditor, mode="open",
                           title="Select Editor Application" )
    if ( !is.null(tmpFile) )
      guiChanges$opEditor <- tmpFile
    else
      cat( "\nMSG (.subGuiOptions) No editor selected, setting unchanged.\n" )
  }
  
  setWinVal( guiChanges, winName=win )
  guiInfo <- getWinVal( winName=win )
  
  assign( ".admbOptions",.setAdmbOptions( guiInfo ), pos=1 )
  
  return( invisible() )
}     # END .subGuiOptions function.
    

# .validGuiModPars (valid parameters for myGUI):
# Purpose:      Check whether the parameters supplied are valid.
#               If invalid, display an error message in the R console and
#               clear the invalid field.
#               If it is a correctable field, corrects it in the GUI and does
#               not flag it as invalid.
# Parameters:   None
# GUI inputs:   Parameters taken directly from active myGui. 
# Returns:      TRUE if the parameters were valid, FALSE otherwise.
# GUI outputs:  Clears invalid fields, corrects correctable fields (if any)
.validGuiModPars <- function()
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

  #------------------------------------------------------------------#
  # USERCODE: Insert parameter validity checking here.  This logic   #
  # is unique for each model.                                        #
  #------------------------------------------------------------------#

  #---------------- END USERCODE VALIDITY CHECKING-------------------#

  # If there are invalid parameters, bring the console to the top.  
  if ( !isValid )
    bringToTop( -1 )
  
  # This takes the accumulated list of changes and applies them to the GUI.
  setWinVal( changes )
  return( isValid )
}     # .validGuiModPars

#------------------------------------------------------------------------------#
# Plotting: USERCODE                                                           #
#------------------------------------------------------------------------------#

.doFitPlots <- function( prefix, obj )
{
  # USERCODE - Fournier's catch-at-age example.
  if ( prefix=="catage" )
  {
    .doGuiFitPlotsCatAge( obj )
  }
      
  # USERCODE - JSC Herring HCAM model.
  if ( prefix=="HCAMv2initpop" )
  {
    .doGuiFitPlotsHCAM( obj )
  }
}

#------------------------------------------------------------------------------#
#-- GUI Helper Functions                                                     --#
#------------------------------------------------------------------------------#

# .editTextFile (Wrapper to call specified text editor):
# Purpose:      Starts a text editor application to edit/view a text file.
# Parameters:   fName is a file name.
#               editApp is a text editor application such as notepad.exe.
# Returns:      TRUE if the file existed for editing, else FALSE.
# Source:       A.R. Kronlund (modified from PBSadmb function ....
.editTextFile <- function( fName, editApp="notepad.exe" )
{
  for ( i in 1:length(fName) )
  {
    # convSlashes is a PBSmodelling function that handles backslashes in
    # file paths.  dQuote adds double quotes, but options must be set to
    # options( "useFancyQuotes"=FALSE )
    
    options( "useFancyQuotes"=FALSE )
    
    fEdit <- paste( "start \"\"", dQuote(convSlashes(editApp)),
                    dQuote(convSlashes(fName[i])), sep=" " )
    fErr  <- paste( "WARNING (.editTextFile): File", fName[i],
               "does not exist.\n", sep=" " )
             
    if (file.exists(fName[i]))
    {
      shell( fEdit, intern = TRUE )
      cat( "\nMSG (.editTextFile):", fEdit, "\n" )
      fOut <- TRUE
    }
    else
    {
      cat( fErr )
      fOut <- FALSE
    }
  }
  return( fOut )
}

.getFileName <- function( fname )
{
  # Returns path, file name and file extension.
  # strsplit( x="C:\\Working\\MyDir\\foo.dat", split="\\\\", perl=TRUE )
  # strsplit( x="C:/Working/MyDir/foo.dat,     split="/",    perl=TRUE )
  
  dirName <- dirname( fname )
 
  # ARK: I don't actually understand how this works, but it does.
  #      Obtained from CRAN mail archives.
  #fileName <- sub( "[.][^.]*$","", fname, perl=TRUE )

  tmp <- strsplit( x=fname, split="/", perl=TRUE )
  
  fileName <- tmp[[1]][length( tmp[[1]] ) ]
  
  # If the fileName is "test.dat", then a list is returned [[1]] [1] "test" "dat".
  tmp <- strsplit( fileName, ".", fixed=TRUE )
  if ( length(tmp[[1]]) > 1 )
  {
    fileExt  <- paste( tmp[[1]][2:length(tmp[[1]])], collapse="" )
    fileName <- tmp[[1]][1]
  }
  else
  {
    fileExt  <- ""
    fileName <- tmp[[1]]
  }
    
  result <- list( dirName=dirName, fileName=fileName, fileExt=fileExt )
  result
}

.setAdmbOptions <- function( obj )
{
  result <- list( admpath=obj$opAdmbPath,
                  gccpath=obj$opCppPath,
                  compiler=obj$opCompiler,
                  editor=obj$opEditor,
                  ver="NONE" )
  result
}

# .setFile    (Uses the PBSmodelling function selectFile to set a filename)
# Purpose:    Allows the user to set a filename via the GUI.
# Parameters: fName is a file name character string.
#             fileType is a file type as a character vector.
# Returns:    val the new file name.
# Source:          A.R. Kronlund
.setFile <- function( fName, fileType )
# fName is a file name.
# fileType is a list of character vectors of file types.
# e.g., c( ".par","par files" )
{
  val <- NULL     # Returns NULL if no file name selected.
  tmpFile <- selectFile( initialfile=fName,
               filetype=fileType, mode="open" )
               
  if ( !is.null(tmpFile) )
    val <- tmpFile   # Set new file name.
  return( val )
}

#------------------------------------------------------------------------------#
# Model-Specific Plotting Control.                                             #
#------------------------------------------------------------------------------#

# USERCODE - Fournier's Catch-at-age example.
.doGuiFitPlotsCatAge <- function( obj )
{
  win     <- .getWinName()
  guiInfo <- getWinVal(scope="L", winName=win ) # GUI information local scope
                                                
  # Save par settings that can be restored (some are readonly).
  oldpar <- par( no.readonly=TRUE )
  
#  if ( ftPlotType=="???" )
  
  # Save plot(s) as an enhanced Windows metafile.
  if ( act=="ftSaveEMF" )
  {
    fName <- paste( fitName,ftPlotType,sep="" )
    savePlot( filename=fName, type="emf", restoreConsole = TRUE )
    cat( "\nMSG (.subGuiFit): Plot saved to ",fName,".\n" )
  }
  
  # Save plot(s) as a PDF file.
  if ( act=="ftSavePDF" )
  {
    fName <- paste( fitName,ftPlotType,sep="" )
    savePlot( filename=fName, type="pdf", restoreConsole = TRUE )                
    cat( "\nMSG (.subGuiFit): Plot saved to ",fName,".\n" )
  }
    
  # Restore the graphics settings.
  par( oldpar )  
  return( invisible() )  
}     # .doGuiFitPlotsCatAge

# USERCODE - JSC Herring HCAM model.
.doGuiFitPlotsHCAM <- function( obj )
{
  win     <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )
  act     <- getWinAct()[1]                      # Last GUI action.
    
  cat( "\nMSG (.doGuiFitPlotsHCAM) Fit plot type = ", ftPlotType,".\n" )

  guiChanges <- list()          # List for accumulating GUI changes.
  hdr <- fits[ fits$Select, ]   # Extract header info for selected fits.
  
  iRow <- 0                     
  nRuns <- nrow( hdr )          # Number of model runs to plot.
  
  if ( nRuns==0 )
  {
    cat( "WARNING (.doGuiFitPlotsCatAge) At least one fit must be selected.\n" )
    return( invisible() )
  }

  # Are any posterior density plots requested?
  if ( ftPlotType=="traces" | ftPlotType=="density" | ftPlotType=="pairs" )
    mcmcPlots <- TRUE
  else
    mcmcPlots <- FALSE
  
  # Set windows pars here.
  if ( ftAuto )
  {
    # Non-mcmc plots.
    if ( nRuns < 4 )
    {
      winCols <- 1
      winRows <- nRuns
    }
    else if ( (nRuns>3) && (nRuns<=6) )
    {
      winCols <- 2
      winRows <- 3
    }
    
    # Combo plots -where each Run has more than one plot panel.
    # For example, .plotSpBioIdxRes creates two plots per Run.
    if ( ftPlotType=="catchgear" || ftPlotType=="predcatch" )
    {
      winCols <- nRuns
      winRows <- 3
    }

    if ( ftPlotType=="ftSSBres" )
    {
      winCols <- nRuns
      winRows <- 2
    }
    
    if ( ftPlotType=="ftCatGr" | ftPlotType=="ftObsPredCat" )
    {
      winRows <- 3
      winCols <- 1
    }
    
    # MCMC plots.
    if ( mcmcPlots )
    {
      winCols <- 3
      winRows <- 3
    }
  }
  else
  {
    winCols <- ftNcols
    winRows <- ftNrows
  }
    
  # Update the GUI parameters.
  guiChanges$ftNcols <- winCols
  guiChanges$ftNrows <- winRows
  
  setWinVal( guiChanges, winName=win )
  
  if ( ftByRow )
    par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(winRows,winCols) )
  else
    par( oma=.VIEWOMA, mar=.VIEWMAR, mfcol=c(winRows,winCols) )
  
  # Loop over the selected run results.
  newPlot <- FALSE
  for ( i in 1:nRuns )
  {
    # ARK (06-May-10): All this is now in fitTracker.
    # Remove whitespace from field read from GUI text box.
    #fileName <- gsub( " ", "", hdr$RepFile[i] )
    # Load an HCAM rep file.
    #cat( "\nMSG (.doViewPlots) Loading",fileName,"...\n" )     
    #repObj <- lisread( fname=fileName, quiet=TRUE )

    repObj  <- obj[[ hdr$Name[i] ]]$rep
    fitDesc <- obj[[ hdr$Name[i] ]]$guiInfo$fitDesc
    fitName <- obj[[ hdr$Name[i] ]]$guiInfo$fitName
    
    # Load an HCAM mcmc file, apparently these are tab delimited.
    #if ( hdr$mcmcFile[i]!="NA" )
    #{
    #  mcmcName <- gsub( " ", "", hdr$mcmcFile[i] )    
    #  if ( file.exists( mcmcName ) )
    #  {
    #    cat( "\nMSG (.doViewPlots) Loading",mcmcName,"...\n" )    
    #    mcmcObj <- read.table( file=mcmcName, as.is=TRUE, header=TRUE, sep="\t" )
    #  }
    #  else
    #    cat( "\nMSG (.doViewPlots ) Warning",mcmcName," not found...\n" )
    #}
    #else
      mcmcObj <- NULL
   
    # GUI set to output a new graphics window for each run, and new plot required.
    if ( !ftOverWrite && newPlot )
    {
      windows()
      if ( ftByRow )
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(winRows,winCols) )
      else
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfcol=c(winRows,winCols) )
      newPlot <- FALSE
    }
    
    if ( ftOverWrite && newPlot )
    {
      if ( ftByRow )
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(winRows,winCols) )
      else
        par( oma=.VIEWOMA, mar=.VIEWMAR, mfcol=c(winRows,winCols) )
      newPlot <- FALSE    
    }    
    # ARK: Why is this here?
    oldpar <- par( no.readonly=TRUE )
  
    if ( ftPlotType=="ftAgeRes" )
    {
      .plotAgeResids( repObj, label=fitDesc,ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftAvail" )
    {
  	  .plotAvail( repObj, label=paste( fitDesc,": ", fitName, sep="" ), 
	    	ftAnnotate, ftSetXaxis, ftSetYaxis )
    }
    
    if ( ftPlotType=="ftBiomass" )
    {
      .plotBiomass( repObj, label=fitDesc,
        ftAnnotate,ftLegend, ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftCatch" )
    {
      .plotCatch( repObj, label=fitDesc,ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftCatGr" )
    {
      .plotCatchGear( repObj, label=fitDesc,ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftObsPredCat" )
    {
      .plotObsPredCt( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
   
    if ( ftPlotType=="ftCutOff" )
    {
      .plotCutoffs( repObj, label=paste( fitDesc, fitName ),
        ftAnnotate, ftSetXaxis, ftSetYaxis )
    }
    
    if ( ftPlotType=="ftF" )
    {
      .plotF( repObj, label=fitDesc, ftAnnotate, ftSetXaxis, ftSetYaxis )
    }
    
    if ( ftPlotType=="ftForecast" )
    {
      .plotForecast( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftM" )
    {
      .plotM( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftPAA" )
    {
      .plotObsPropAge( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftRec" )
    {
      .plotRecruits( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftRetro" )
    {
      .plotRetroPrefishB ( obj$guiInfo$fits, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftSel" )
    {
      .plotS( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftSpawn" )
    {
      .plotSpawn( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftSSBres" )
    {
      .plotSpBioIdxRes( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftWgt" )
    {
      .plotWeightAgeXbyYear( repObj, label=fitDesc,ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
    
    if ( ftPlotType=="ftWAA" )
    {
      .plotWtAge( repObj, label=fitDesc, ftAnnotate,ftSetXaxis,ftSetYaxis )
    }
  
     # MCMC Plots
    if ( !is.null(mcmcObj) )
    {
      if ( ftPlotType=="density" )
      {
        .mcmcDensity( mcmcObj, label= hdr$Stock[i], annotate )
      }
      
      if ( ftPlotType=="pairs" )
      {
        .mcmcPairs( mcmcObj, label= hdr$Stock[i], annotate )
      }
      
      if ( ftPlotType=="traces" )
      {
        .mcmcTraces( mcmcObj, label= hdr$Stock[i], annotate )
      }
    }
    
    # Set newPlot to TRUE if current panel is the last row and column of layout.
    mfg <- par( "mfg" )
    if ( (mfg[1]==mfg[3]) && (mfg[2]==mfg[4]) )
      newPlot <- TRUE
    
    if ( mcmcPlots )
      newPlot <- TRUE

    if ( newPlot || i==nRuns )     # New plot or last model fit.
    {
      # Save plots(s) as an Enhanced Windows Metafile.
      if ( act=="ftSaveEMF" )
      {
        fName <- paste( fitName,ftPlotType,sep="" )
        savePlot( filename=fName, type="emf", restoreConsole = TRUE )
        cat( "\nMSG (.subGuiFit): Plot saved to ",fName,".EMF.\n",sep="" )
      }
          
      # Save plot(s) as a PDF file.
      if ( act=="ftSavePDF" )
      {
        fName <- paste( fitName,ftPlotType,sep="" )
        savePlot( filename=fName, type="pdf", restoreConsole = TRUE )
        cat( "\nMSG (.subGuiFit): Plot saved to ",fName,".PDF.\n",sep="" )
      }
    }
    
    # Identify the run by stamp.
    if ( ftShowStamp )
      mtext( side=1, adj=1, cex=0.75, font=3, line=2, hdr$Stamp[i] )

  }   # Loop over i run results.  
    
  # Restore the graphics settings.
  par( oldpar )  
  return( invisible() )    
}     # .doGuiFitPlotsHCAM

#------------------------------------------------------------------------------#
# FILE I/O FUNCTIONS                                                           #
#------------------------------------------------------------------------------#

.gotStamp <- function( stamp )
{
  result <- TRUE
        
  day  <- as.numeric( substring( stamp, 1, 2 ) )
  mon  <- as.numeric( substring( stamp, 3, 4 ) )
  year <- as.numeric( substring( stamp, 5, 8 ) )
  hrs  <- as.numeric( substring( stamp, 9,10 ) )
  mins <- as.numeric( substring( stamp,11,12 ) )
        
  if ( (day < 1 ) || ( day > 31 ) )
    result <- FALSE
  if ( (mon < 1 ) || ( mon > 12 ) )
    result <- FALSE
  if ( (year < 2000 ) || (year > 2017 ) )
    result <- FALSE
  if ( (hrs < 0 ) || (hrs > 23 ) )
    result <- FALSE
  if ( (mins < 0) || (mins > 59) )
    result <- FALSE
        
  result
}     # .gotStamp   

.resetFileIO <- function( nArgs=5 )
{
  result <- data.frame( Arg=character(nArgs), FileName=character(nArgs),
                        Select=logical(nArgs), stringsAsFactors=FALSE )
  result
}

.resetFitNode <- function( fitNode )
{
  fitNode$admb <- list( objFun=0, maxGrad=0, exitCode=0,
                        funEvals=0 )
  fitNode$cor  <- NULL
  fitNode$mcmc <- NULL
  fitNode$par  <- NULL
  fitNode$rep  <- NULL
  fitNode$std  <- NULL
  fitNode
}     # .resetFitNode

.resetFitTrackerFiles <- function()
{
  result <- list( ctl=NULL, pin=NULL, dat=NULL, cor=NULL, mcmc=NULL,
                  par=NULL, rep=NULL, std=NULL, fitTracker=NULL, xls=NULL )
  result
}

.updateFits <- function( fitTracker, fits=NULL )
{
  result <- .initFits( maxFits=100 )
  
  nFits <- length(fitTracker)
  for ( i in 1:min(nFits,100) )
  {
    # Fit name.
    result$Name[i]   <- names( fitTracker )[i]
          
    # Fit description.
    if ( !is.null(fitTracker[[i]]$guiInfo) )
      result$Description[i] <- fitTracker[[i]]$guiInfo$fitDesc

    # Fit select.
    result$Select[i] <- FALSE
    
    # Fit stamp.
    result$Stamp[i]  <- fitTracker[[i]]$status$fitStamp    
  }
  
  for ( i in 1:nFits )
  {
    if ( result$Stamp[i] != "NO_FIT" )
    {
      # Found a valid stamp, see if it was in fits$Stamp.
      if ( is.element( result$Stamp[i], fits$Stamp ) )
      {
        # The stamp was in fits$Stamp, so find the valud of fits$Select.
        idx <- c(1:length(fits$Stamp))[ result$Stamp[i]==fits$Stamp  ]
        result$Select[i] <- fits$Select[idx]
      }
    }
  }
  result
}     # .updateFits

.updateStatus <- function( modStamp, fitStamp, winName )
{
  if ( modStamp==fitStamp )
  {
    setWidgetColor( "mdModStatus", noeditbg="green", winName=winName )
    setWidgetColor( "mdFitStatus", noeditbg="green", winName=winName )
  }
  else
    setWidgetColor( "mdFitStatus", noeditbg="red" )      
}     # .updateStatus

.createFitNode <- function()
{
  result <- list( admb=NULL, cor=NULL, par=NULL, mcmc=NULL, std=NULL, rep=NULL,
                  std=NULL, files=NULL, guiInfo=NULL, status=NULL )
  
  # Oddly, setting stamp to NULL seems to delete the first sub-node of the tree.
  # So, set it to an arbitrary string that can also be used to determine if the
  # node has been updated at all from initial conditions.
  result$admb$objFun   <- 0
  result$admb$maxGrad  <- 0
  result$admb$funEvals <- 0
  result$admb$exitCode <- 0
  
  result$status$modStamp <- "NO_MODEL"
  result$status$fitStamp <- "NO_FIT"
  result
}     # .createFitNode

.loadFitFromRdata <- function( fitFile=.FFITS )
{
  tmpFile <- selectFile( initialfile=fitFile,
                filetype=list( c(".Rdata","Rdata files") ), mode="open" )
  if ( is.null(tmpFile) )
    return( NULL )          # No such file exists, bale returning NULL.
  else
    fitFile <- tmpFile      # Return the selected file name.
    
  load( fitFile, .GlobalEnv )
  
  return( invisible() ) 
}     # .loadFitFromRdata

.openAdmbFile <- function( admbFile, ext=".dat" )
{
  tmpFile <- selectFile( initialfile=admbFile,
                filetype=list( ext ), mode="open" )
  if ( is.null(tmpFile) )
    return( NULL )          # No such file exists, bale returning NULL.
  else
    admbFile <- tmpFile     # Return the selected file name.
    
  return( admbFile )
}     # openAdmbFile

# .readModParFile (reads guiRef GUI parameters from file written by saveRefPars)
# Purpose:      Reads an ASCII file with reference points parameters.
# Parameters:   GUI parameters for guiRef. Output GUI parameter list.
# Returns:      NULL (invisibly)
# Side effects: Reads an ASCII control file written by saveRefPars and resets
#               the guiRef GUI parameters.
# Source:       A.R. Kronlund
.readModParFile <- function( parFile=.FMODPARS )
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
}     # .readModParFile

# .saveFitToExcel (Save fits in Excel format)
# Purpose:    Saves data in obj to a Microsoft Excel .xls file.
# Parameters: fname - a character string file name.
#             statObj - the result of a call to .calcPerfStats.
# Returns:    NULL
# Source:     Modified from saveExcel by T.K. Deering (PopSim.r)
.saveFitToExcel <- function( fname=.FFXLS )
{
  fileSaved <- TRUE
  
	# No name provided, or "Cancel" selected.
	if ( fname == "" )
	{
	  fileSaved <- FALSE
    msg       <- "Fits not saved, no file name specified"	
    return( saveStatus <- list( fileSaved=fileSaved, msg=msg ) )
  }
    
	# If fname already exists, then remove it.
	fileGone <- TRUE
	if ( file.exists( fname) )
    fileGone <- file.remove( fname )
    
  if ( fileGone )
  {
  	conn <- RODBC::odbcConnectExcel( fname, readOnly=FALSE )

    # Write the parameters by Fit.
    # NOTE: The par file output by ADMB does not seem to follow any rules.
    #       Therefore, use the std file, which at least is formatted consistently.
    #       But, these cannot be combined on the same Excel page because different
    #       model configurations have different numbers of active parameters, and
    #       therefore different number of rows in the std file.

    modelNames <- names(fitTracker)
    for ( i in 1:length(fitTracker) )    
    {
      if ( !is.null(fitTracker[[i]]$std) )     # Is there a stored std file?
      {
        std <- fitTracker[[i]]$std
        excelTabName <- paste( modelNames[i],"_EstPars",sep="" )
        .excelTable( conn, std, excelTabName, dimnames(std)[[2]], dimnames(std)[[1]] )
      }
    }
    
    # ARK: JC, I don't know what you want to save in the Excel sheet, but fill
    #      your boots here.
    
    odbcClose(conn)
    
    fileSaved <- TRUE
    msg <- paste( "mseRmod (.saveFitToExcel): Fits saved to Excel ",fname )
  }
  else
  {
    fileSaved <- FALSE
    msg <- c( "(.saveFitToExcel): Fits NOT saved to Excel, permission denied" )
    msg <- c( msg, paste( "(.saveFitToExcel): Close or rename file ",fname) )
  }
	return( list( fileSaved=fileSaved, msg=msg ) )
}     # .saveFitToExcel

.saveFitToRdata <- function( fitFile=.FFITS, overWrite=FALSE )
{
  # Need to check if there are trials to save before calling this...
  if ( !overWrite )
  {
    # If not overwriting the file, then use a prompt menu, bale if no selection.
    #tmpFile <- promptSaveFile( initialfile=fitFile,
    #             filetype=list( c(".Rdata","Fit files") ) )
    tmpFile <- selectFile( initialfile=fitFile,
                  filetype=list( c(".Rdata","Fit files")), mode="save" )
    
    if ( is.null(tmpFile) )
      return( invisible() )
    else
      fitFile <- tmpFile
  }
  else
  {
    # Force an overwrite of .FTRLS.
    cat( "\n(.saveFitToRdata) Overwriting :",fitFile,"\n" )
  }

  save( fitTracker, file=fitFile )
 
  return( invisible() )  
}     # .saveFitToRdata

# .saveModParFile (saves the mod GUI parameters to a file).
# Purpose:      Saves an ASCII file with model parameters.
# Parameters:   Output file name for ASCII file containing mod GUI parameters.
# Returns:      NULL
# Side effects: Writes an ASCII control file configured for read usng .readModParFile.
# Source:       A.R. Kronlund
.saveModParFile <- function( parFile=.FMODPARS, overWrite=FALSE )
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
    # Force an overwrite of .FMODPARS.
    cat( "\n(.saveModParFile) Overwriting :",parFile,"\n" )
  }

  win <- .getWinName()                           # Get the current window name
  guiInfo <- getWinVal( scope="L", winName=win ) # GUI information local scope

  cat( file=parFile, paste( "# mseRmod GUI parameters written",date(),"\n" ) )
  cat( file=parFile, "parameter value\n", append=TRUE )

  for ( i in 1:nrow(guiInfo$modPars) )
  {
    cat( file=parFile, guiInfo$modPars[i,1]," ",guiInfo$modPars[i,2],"\n", sep="",
         append=TRUE )
  }
  return( invisible() )  
}    # .saveModParFile

.initFits <- function( maxFits=100 )
{
  result <- data.frame( Name=rep("",maxFits), Description=rep("",maxFits),
              Select=rep(F,maxFits), Stamp=rep("",maxFits),
              stringsAsFactors=FALSE )
  result
}

# .setupModDir (Working directory set-up):
# Purpose:    Creates three (3) sub-directories if they do not exist:
#             (a) .DTEMP - contains R code and PBSModelling GUI control files
#                 copied from working directory.
#             (b) .DHELP - contains help files copied from working directory.
#             (c) .DDOCS - contains code and model documentation.
# Notes:      In "shell", translate=TRUE turns forward slash / to backslash \.
# Parameters: None
# Returns:    The full path to the new directory
# Source:     mseR V2.0, modified from PBSref
.setupModDir <- function()
{
  pckg  <- .PACKAGE                      # Current package name.
  ddocs <- .DDOCS                        # Directory for documentation files.
  dhelp <- .DHELP                        # Directory for help files.
  ftemp <- .DTEMP                        # Directory for temporary files.
  wkDir <- getwd()                       # Current R working directory.
  
  # These two will be used when mseR is a proper R library.
	#rdir <- system.file(package = pckg)          # Path to the R directory
  #fnam <- paste(rdir, dname, fils, sep = "/")  # the files you want to copy
  	
	# Create .DDOC directory if needed, then copy any required files.
	docDir <- paste( wkDir, .DDOCS, sep="/" )
	if ( !file.exists(docDir) )
	  shell( paste( "mkdir", docDir ), translate=TRUE )
	
	# Create. DHELP directory if needed, then copy any required files.
	helpDir <- paste( wkDir, .DHELP, sep="/" )
	if ( !file.exists(helpDir) )
	  shell( paste( "mkdir", helpDir ), translate=TRUE )
	
	helpFiles <- c( "mseRabout.txt" )
	
	srcFiles <- paste( wkDir,   helpFiles, sep="/" )    # Source files.
	tarFiles <- paste( helpDir, helpFiles, sep="/" )    # Target files.
	file.copy( srcFiles, tarFiles, overwrite = TRUE )
	
	# Create .DTEMP directory if needed, then copy R and PBSmodelling files.
  tempDir   <- paste( wkDir, .DTEMP, sep="/" )
  if ( !file.exists(tempDir) )
    shell( paste("mkdir", tempDir), translate=TRUE )  # Create target directory.

  tempFiles <- c(
                   "mseRmod.r",
                   "mseRguiModWin.txt",
                   "mseRabout.txt"
                )

  srcFiles <- paste( wkDir,   tempFiles, sep="/" )    # Source files.
  tarFiles <- paste( tempDir, tempFiles, sep="/" )    # Target files.
  file.copy( srcFiles, tarFiles, overwrite = TRUE)
  
  return(wkDir)
}

#--------------------------------------------------------------------#
# catage USERCODE to support ADMB example.                           #
#--------------------------------------------------------------------#

.writeADMBinputFiles <- function()
{
  win        <- .getWinName()
  guiInfo    <- getWinVal( scope="L", winName=win )
  guiChanges <- list()
  
  modParsList <- .createList( modPars )
  
  #----------------------------------------------------------------#
  # Begin USERCODE: Write the pin, ctl, and data files here.       #
  #                                                                #
  # Usually at least one of a pin, ctl, or data file is required.  #
  # However, may not be the case as these files may not change.    #
  # prefix is passed because there may be more than one compiled   #
  # version of the model.  In that case the USERCODE can have a    #
  # branch point within the writePrefixPin, writePrefixData or     #
  # writePrefixCtl functions.                                      #
  #----------------------------------------------------------------#
  
  #--------------------------------------------------------------#
  # catage.tpl: Example catage.tpl from the ADMB documentation.  #
  #--------------------------------------------------------------#

  if ( prefix=="catage" )
  {
    writePinCatAge( prefix, modParsList )
  }
       
  #--------------------------------------------------------------#
  # HCAMv2initpop.tpl: Herring stock assessment (JSC).           #
  #--------------------------------------------------------------#
    
  if ( prefix=="HCAMv2initpop" )
  {
    majorMinor <- "Major"
    if ( mdDataOpt=="A27" )
      majorMinor <- "Minor"
    if ( mdDataOpt=="A2W" )
      majorMinor <- "Minor"
    if ( mdDataOpt=="Other" )
      majorMinor <- "Other"
      
    # Update the mdFileIO widget.
    tmpSelect <- mdFileIO$Select
    guiChanges$mdFileIO           <- .resetFileIO()
    guiChanges$mdFileIO[1,c(1,2)] <- c( "-ind",paste( prefix,majorMinor,".dat",sep="" ))
    guiChanges$mdFileIO[2,c(1,2)] <- c( "-stock",paste(mdDataOpt,".dat",sep="" ))
    guiChanges$mdFileIO[3,c(1,2)] <- c( "-ctlfile",paste(mdDataOpt,".ctl",sep="") )
    guiChanges$mdFileIO$Select <- tmpSelect
    
    # ARK: Function is not stock dependent according to the code.
    writeCtlHCAM( paste( mdDataOpt,".ctl",sep=""  ), modParsList )
    
    # The writeDataHCAM function is stock dependent.
    writeDataHCAM( prefix,majorMinor )
  }
  
  #-----------------------------------------------------------------#    
  # End USERCODE: Write pin, data, ctl files.                       #
  #-----------------------------------------------------------------#
  
  setWinVal( guiChanges, winName=win )
  
  return( invisible() )
}     # End .writeADMBinputFiles function.

# Fournier's Catch-at-age model.
writePinCatAge <- function( prefix, modParsList )
{
  # Write the pin file for ADMB example catage.tpl.
        
  pinFile <- paste( prefix,".pin",sep="" )
  cat( file=pinFile, paste( "# mseRmod(",prefix,") parameters written",
       date(),"\n" ) )
  cat( file=pinFile, "# log_q.\n",          append=TRUE )
  cat( file=pinFile, modParsList$pars$logq,"\n",        append=TRUE )
  cat( file=pinFile, "# log_popscale\n",    append=TRUE )
  cat( file=pinFile, modParsList$pars$logPopScale,"\n", append=TRUE )
  cat( file=pinFile, "# log_sel_coff\n",    append=TRUE )
  cat( file=pinFile, modParsList$pars$logSelCoff,"\n",  append=TRUE )
  cat( file=pinFile, "# log_relpop\n",      append=TRUE )
  cat( file=pinFile, modParsList$pars$logRelPop,"\n",   append=TRUE )
  cat( file=pinFile, "# effort_devs\n",     append=TRUE )
  cat( file=pinFile, modParsList$pars$effortDevs,"\n",  append=TRUE )
  
  cat( "\nMSG( writePinCatAge): Catch-Age pin file",pinFile," written.\n" )
  return( invisible() )
}

#-------------------------------------------------------------------#
# HCAM USERCODE Code from JSC                                       #
#-------------------------------------------------------------------#

writeCtlHCAM <- function( ctlFile, modParsList )
{
  # Write the ctl file for each stock for ADMB.
        
  cat( file=ctlFile, paste( "# HCAMv2 Control file(",ctlFile,") parameters written",
       date(),"\n" ) )
  cat( file=ctlFile, paste( "# ival" ),      paste("		"),	paste( "phz" ),	
     	 paste("		"),paste( "lower_bound" ), paste("		"), paste( "upper_bound" ),
       paste("		"),paste( "parname" ), "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$available,   sep="     ", 
	     paste("# available_params, bounds are dummies"), "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$selectivity, sep="     ", 
	     paste("# selectivity_params, bounds are dummies"), "\n", append=TRUE )
  cat( file=ctlFile, modParsList$pars$M,           sep="     ", paste("# M_params"),     "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$Mextra,      sep="     ", paste("# M_extra"),      "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$steep,       sep="     ", paste("# steep"),        "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$qs,          sep="     ", paste("# qs"),           "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$log_Bo_init, sep="     ", paste("# log_Bo_init"),  "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$sigma,       sep="     ", paste("# sigma"),        "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$initial_F,   sep="     ", paste("# initial_F"),    "\n", append=TRUE )

	cat( file=ctlFile, paste("#"), "\n", append=TRUE )
	cat( file=ctlFile, paste("##################################"), "\n", append=TRUE )
	cat( file=ctlFile, paste("#"), "\n", append=TRUE )
	cat( file=ctlFile, paste("#....stdev of normal-log like for spawn data (for two periods)"), 
	     "\n", append=TRUE ) 
	cat( file=ctlFile, modParsList$pars$stdev[1], "\n", append=TRUE )
	cat( file=ctlFile, modParsList$pars$stdev[2], "\n", append=TRUE )
	cat( file=ctlFile, paste("##################################"), "\n", append=TRUE )
	cat( file=ctlFile, paste("#"), "\n", append=TRUE )
	cat( file=ctlFile, paste("#process error in N_samp? This is actually an observation error"), 
			"\n", append=TRUE ) 
	cat( file=ctlFile, modParsList$pars$process, "\n", append=TRUE )
	
	cat( "\nMSG (writeCtlHCAM): HCAM Control file ",ctlFile," written.\n" )
	
	return( invisible() )
}

writeDataHCAM <- function( prefix,majorMinor )
{
  # Write the dat file specifying Major or Minor stock.
  win     <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )
  
  modParsList <- .createList( modPars )

  datFile <-         paste( prefix,majorMinor,".dat",sep="" )
  cat( file=datFile, paste( "# HCAMv2 Control file(",majorMinor,") parameters written",
       date(),"\n" ) )
  cat( file=datFile, paste( "#....First year, last data year "), "\n", append=TRUE )
  cat( file=datFile, paste( "1951 2009\n" ), append=TRUE )
  #cat( file=datFile, .guiModPars$firstYr, "   ", .guiModPars$lastYr, "\n", append=TRUE )
	cat( file=datFile, paste( "#....periods (in a year)"), "\n", append=TRUE )
	cat( file=datFile, 3, "\n", append=TRUE )
	cat( file=datFile, paste( "#....sexes (1 or 2)"), "\n", append=TRUE )
	cat( file=datFile, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....regions ( 1:7 )"), "\n", append=TRUE )
	cat( file=datFile, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....first_age last_age" ), "\n", append=TRUE )
	cat( file=datFile, 2, 10, "\n", append=TRUE )
	cat( file=datFile, paste( "#....age plus group"), "\n", append=TRUE )
	cat( file=datFile, 9, "\n", append=TRUE )
	cat( file=datFile, paste( "#....lag (years) before spawning"), "\n", append=TRUE )
	cat( file=datFile, 2, "\n", append=TRUE )
	cat( file=datFile, paste( "#....minimum fraction to survive each fishery"), "\n", append=TRUE )
	cat( file=datFile, 0.05, "\n", append=TRUE )

	cat( file=datFile, paste( "#....number of q's to estimate, i.e., p(spawn) observed" ), "\n", append=TRUE )
	cat( file=datFile, paste( "#....-1 = est. no q's, fix q==1" ), "\n", append=TRUE )
	cat( file=datFile, paste( "#....0 = est. 1 q for the entire time period (eg, minor stocks)" ), "\n", append=TRUE )
	cat( file=datFile, paste( "#....1 = est. 1 q for the 1st period, fix q==1 for 2nd period" ), "\n", append=TRUE )
	cat( file=datFile, paste( "#....2 = est. 2qs, 1 for each period " ), "\n", append=TRUE )

	if ( guiInfo$mdq=="q==1" )
	  val <- -1
	if ( guiInfo$mdq=="est1q" )
	  val <- 0
	if ( guiInfo$mdq=="est1q/q==1" )
	  val <- 1
	if ( guiInfo$mdq=="est2q" )
	  val <- 2	      	        
	    
  cat( file=datFile, val, "\n", append=TRUE )
  cat( file=datFile, paste( "#....year where q switch occurs" ), "\n", append=TRUE )
  cat( file=datFile, 1988, 1988, "\n", append=TRUE )
  cat( file=datFile, paste( "#....stdev of normal-log like for spawn data (for two periods)" ),
       "\n", append=TRUE )
  cat( file=datFile, paste( "#0.65 0.4 " ), "\n", append=TRUE )
  cat( file=datFile, paste( "#0.4 0.35" ), "\n", append=TRUE )
  cat( file=datFile, paste( "#....prop( M ) by period (sums to 1)" ), "\n", append=TRUE )
	cat( file=datFile, 0.9, 0.05, 0.05, "\n", append=TRUE )
        
  cat( file=datFile, paste( "#....M switch: 0 is fixed, 1 is estimated, 2 is density dependent" ),
       "\n", append=TRUE )
         	
  if ( guiInfo$mdM=="fixed" )
    val <- 0
  if ( guiInfo$mdM=="annual" )
    val <- 1
  if ( guiInfo$mdM=="ddAnnual" )
    val <- 2
	        
  cat( file=datFile, val, "\n", append=TRUE )  
 	cat( file=datFile, paste( "#....M_devs switch : 0 is off, 1 is annual deviations, 2 is random walk" ),
     	 "\n", append=TRUE )

  if ( guiInfo$mdMdevs=="off" )
	  val <- 0
	if ( guiInfo$mdMdevs=="annualDevs" )
	  val <- 1
	if ( guiInfo$mdMdevs=="randomWalk" )
	  val <- 2
	        
  cat( file=datFile, val, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....stdev in M_devs (only when estimating M_devs)" ), "\n", append=TRUE )  
	cat( file=datFile, modParsList$pars$sdM, "\n", append=TRUE )
	cat( file=datFile, paste( "#....M by region. Est. M pars same for each region (1=yes)" ), 
       "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....Is M sex specific? 1 = no, 2= yes" ), "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....availability switch: 0=fixed at 1 (2nd and 3rd pars ignored)" ), 
	     "\n", append=TRUE ) 
	cat( file=datFile, paste( "#.... 1=logistic	(2nd and 3rd pars determine if pars fixed (0) or est (1))" ),
       "\n", append=TRUE )			
	cat( file=datFile, paste( "#.... 2=age-specific (2nd and 3rd params are first and last age to estimate)" ),
	     "\n", append=TRUE )
	cat( file=datFile, 2, 2, 4, "\n", append=TRUE )
	cat( file=datFile, paste( "#....switch for availability devs" ), "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "#....stdev in availablity devs" ), "\n", append=TRUE )
	cat( file=datFile, 0.1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Is availability common by region? 1=yes" ), "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....Is availability by sex? 1 = no, 2 = yes" ), "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....selectivity by period" ),
	     "\n", append=TRUE )
 	cat( file=datFile, 1, 2, 3, "\n", append=TRUE )
 	cat( file=datFile, paste( "#....selectivity switch: 0 = fixed at 1" ), "\n", append=TRUE )
	cat( file=datFile, paste( "#.... 1= logistic (2nd par==1 to estimate a50; 3rd par==1 to estimate a95)" ), 
	     "\n", append=TRUE )
	cat( file=datFile, paste( "#.... 2=age-specific (2nd and 3rd pars are first and last age to estimate)" ),
	     "\n", append=TRUE )
	cat( file=datFile, paste( "#.... 3=Gillnet (2nd and 3rd values to fix or estimate)" ), "\n", append=TRUE )
	cat( file=datFile, 1, 1, 1, "\n", append=TRUE )
	cat( file=datFile, 1, 1, 1, "\n", append=TRUE )
	cat( file=datFile, 3, 1, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....switch for selectivity devs, 0=no est" ), "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "#...stdev in selectivity devs (only when selectivity_devs estimated)" ), 
       "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Are selectivities common by region? (1==yes)" ), "\n", append=TRUE )
	cat( file=datFile, 1, 1, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Is selectivity by sex? 1=no, 2=yes " ), "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....Selectivity devs (Jake devs), 1=additive, else multiplicative" ), 
	     "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Fecundity: 1 is fecundity==wt (combined with avail), else fecundity = wt*maturity" ), 
	     "\n", append=TRUE )
  cat( file=datFile, 1, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....Catch units, 0=catch given in numbers, else catch is in biomass" ), 
       "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Estimating fishing intensity (sw_estFfull)" ), "\n", append=TRUE )
 	cat( file=datFile, 1, "\n", append=TRUE )
	cat( file=datFile, paste( "#....Use instantaneous fishing mortality eqn? (0==no)" ), "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "#....spawner-recruit rel: 0=none, 1=BH" ), "\n", append=TRUE )
		
	if ( guiInfo$mdStockrec=="none" )
	  val <- 0
	if ( guiInfo$mdStockrec=="B-H" )
	  val <- 1

 	cat( file=datFile, val, "\n", append=TRUE )  
	cat( file=datFile, paste( "#....Ageing error: 0=none, 1=vectors, 2=per otolith approach" ), "\n", append=TRUE )
	cat( file=datFile, 0, "\n", append=TRUE )
	cat( file=datFile, paste( "# check value" ), "\n", append=TRUE )
	cat( file=datFile, -999, "\n", append=TRUE )
	#JSC: stopped here (at some point...)
	
	cat( "\nMSG (writeDataHCAM): HCAM data file",datFile," written.\n" )
	return( invisible() )
}