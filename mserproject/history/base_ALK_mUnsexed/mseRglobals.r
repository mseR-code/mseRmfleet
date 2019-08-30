#------------------------------------------------------------------------------#
# (c) mseR-FinFish: Management Strategy Evaluation in R, Finfish Version 1.0   #
#                                                                              #
#     Copyright 2012-2013 by A.R. Kronlund and S.P. Cox.                       #
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
#                                                                              #
# mseRglobals.r: An mseR module that provides global variables, but not for    #
#                graphics options.  These globals intended for developers.     #
#                Changing these globals can cause mayhem - be careful.         #
                                                                               #
# Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)           #
#          S.P. Cox (Simon Fraser University, Burnaby, B.C.)                   #
#                                                                              #
# First Implementation: 07-Aug-12 from mseR V2, mseR-Prawn and mseR2011.       #
#------------------------------------------------------------------------------#

options(useFancyQuotes = FALSE)        # Required for parameter eval

# General global variables.
.GUIMSG         <- TRUE    # Control console whining re: a GUI message.
.PACKAGE        <- ""      # When mseR is an R package this will be needed.
.PLTMSG         <- TRUE    # Controls console whining re: a plot call.
.PLATFORM       <- .Platform$OS.type
.RESTORECONSOLE <- ifelse( .PLATFORM=="unix", FALSE, TRUE )
.WHINE          <- 3       # 1=Essential, 2=Info, 3=Debugging.
.MAKEPDF        <- TRUE

# mseR folder settings - do not alter unless you are a developer.
.FCODE <- "mseRcode"                  # Copy of *.r and *.txt GUI files for safety.
.FDEFS <- "mseRdefaults"              # Folder containing default options.
.FDOCS <- "mseRdocs"                  # Folder containing documents.
.FHELP <- "mseRhelp"                  # Directory for help files.
.FTEMP <- "mseRtemp"                  # Folder containing temporary files.

# mseR project folder definitions - do not alter unless you are a developer.
.DEFBATFLD  <- "batch"                # For batch files.
.DEFHSTFLD  <- "history"              # For stock initialization (conditioning).
.DEFOPTFLD  <- "options"              # Folder for general and project options.
.DEFPLTFLD  <- "plots"                # Folder for guiSim and guiPerf plots.
.DEFPRJFLD  <- "mseRproject"          # Default project folder.
.PRJFLD     <- .DEFPRJFLD
.DEFSTATFLD <- "statistics"           # Performance statistics folder.

# mseR default file names.
.DEFBASEFILE  <- "simCtlFile.txt"      # Default batch simulation control file.
.DEFCTLFILE   <- "simCtlFile.txt"      # Default simulation control file.
.DEFHSTFILE   <- "simHistoryFile.csv"  # History file for conditioning.
.DEFSTATFILE  <- "mseRstatistics.xls"  # Performance statistics file (Excel .xls).

# Batch file names, don't use simCtlFile.txt 'cause it gets overwritten.
.FBATDES  <- "mseRbatch.design"        # Design file (output)

# ADMB related globals.
.FADMBOPT            <- "mseRadmbOpt.txt"     # ADMB and C++ compiler options.
.ADMBOPTS            <- "-nox -iprint 1000"   # Default ADMB command line arguments.
.INVISIBLE           <- TRUE                  # Is ADMB reporting invisible?
.NOHESS              <- FALSE                 # Is hessian calculated?
.SHOWOUTPUTONCONSOLE <- FALSE                 # Windows: Show ADMB running on console?
.INTERN              <- FALSE                 # Mac: Show ADMB running on console? 

# Parallel processing globals.
.ISCLUSTER <- FALSE                           # Is snow cluster active?

# Constants to ID assessment method for management procedure.
.MOVAVG  <- 1           # Moving average.
.KALMAN  <- 2           # Kalman filter.
.PMOD    <- 3           # Surplus production model.
.DDMOD   <- 4           # Delay-Difference model.
.CAAMOD  <- 5           # Catch-At-Age model.
.VPA     <- 6           # Virtual Population Analysis.
.PERFECT <- 7           # Perfect information.

# Flag to indicate a fishery collapse
.DEADFLAG  <- FALSE       # All future catch==0 if TRUE

# Lables for each harvest control rule.
.HCRNAMES <- c( "Constant F", "Variable F", "Decline Risk", "User Rule" )

# Labels for each method.
.METHODLAB <- c( "Moving Average", "Kalman Filter", "Surplus Production",
                 "Delay-Diff",     "Catch-at-Age",  "VPA",              "Perfect" )
                 
# Global Options from 2010.

# Following directory names usually prefixed by .PACKAGE so that, for example,
# the directory name for .FDOCS becomes "mseRdocs".
.FOMDAT   <- "mseRomDAT.dat"    # File for operating model data for ADMB.
.FOMPIN   <- "mseRomPIN.pin"    # File for operating model pin file for ADMB.
                                # for the conditioning step.
.FFXLS    <- "mseRfits.xls"     # Excel output file for fits.

.FOPMODAGES <- "opModAges.dat"
.FOPMODCAT  <- "opModCatch.dat"
.FOPMODCTL  <- "opModControl.ctl"
.FOPMODIDX  <- "opModIndex.dat"
.FOPMODLIFE <- "opModLifeScheds.dat"
.FOPMODPIN  <- "sableOpMod.pin"

#------------------------------------------------------------------------------#
# Globals for mseRSystem, mseRrefPoints and general SPC usage - nowhere else.  #
#------------------------------------------------------------------------------#

.INITCATCHKNOTS <- 5
.LASTSOLUTION <- NULL
.INITMSYMULT <- c(0,0.30,1.0,0.75)
.INITTIMES   <- c(0,0.25,0.75,1.0)
.CATCHSERIESINPUT <- FALSE
.FIRSTCATCHYEAR   <- 17
.INDEXSERIESINPUT <- FALSE
.AGESERIESINPUT   <- FALSE
.RECSERIESINPUT   <- FALSE

# Reference point grid - used by calcRefCurves in mseRrefPoints.r.
.nFVALS <- 100
.MAXF   <- 4                    # Multiplier of M in generating yield curves
.MAXIT  <- 50                   # Max iterations for solving F multipliers fg in refpts
.FGINIT <- rep(-2,5)            # Initial Fgs given to optim to solve allocation prob

#------------------------------------------------------------------------------#

# General mseR globals.
.INITYEAR <- 1965     # initial year.
.MAXREP <- 200
.GUIMSG <- TRUE
.PLTMSG <- TRUE
.WINNERS <- c( "StrsHiTuneHCR46" )
.XAXISTICKBY <- 1

# Globals for reference point plots.
.REFCOLF0   <- "green"           # Colour for F0.
.REFCOLF01  <- "pink"            # Colour for F0.1.
.REFCOLFCRA <- "black"           # Colour for Fcrash.
.REFCOLFMSY <- "cyan"            # Colour for Fmsy.
.REFCOLFMAX <- "red"             # Colour for Fmax.
.REFCOLF40  <- "gray"            # Colour for F40%.

# Line types:
# Line types can either be specified as an integer (0=blank, 1=solid (default),
# 2=dashed, 3=dotted, 4=dotdash, 5=longdash, 6=twodash) or as one of the
# character strings "blank", "solid", "dashed", "dotted", "dotdash", "longdash",
# or "twodash", where "blank" uses invisible lines, (i.e., does not draw them).

.REFLTYBMSY <- 4                # Line type for Bmsy.
.REFLTYFCRA <- 5                # Line type for Fcrash.
.REFLTYFMSY <- 4                # Line type for Fmsy.
.REFLTYLRP  <- 2                # Line type for limit reference point.
.REFLTYMSY  <- 2                # Line type for MSY.
.REFLTYUSR  <- 2                # Line type for upper stock reference.

# Globals for Simulation plots (guiSim).
.REFCAX       <- 1.2            # Axis character expansion factor.
.REFCEX       <- 1.2            # mtext character expansion factor.
.REFCEXOUT    <- 1.4            # Outer mtext character expansion factor.
.REFPEX       <- 1.8            # Plotting character expansion factor.
.TRUECOL      <- "red3"         # Color of true values.
.TRUELWD      <- 2              # Line width of true values.

.INLINE1      <- 2
.INLINE2      <- 3
.INLINE3      <- 4                 
.INLINE4      <- 5                 
