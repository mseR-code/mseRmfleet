# Global Options.

options( useFancyQuotes=FALSE )

# General global variables (this will eventually be set to mseR).
.PACKAGE <- ""

# Following directory names usually prefixed by .PACKAGE so that, for example,
# the directory name for .FDOCS becomes "mseRdocs".
.DDOCS    <- "docs"              # Directory containing documents.
.DHELP    <- "help"              # Directory containing help files.
.DTEMP    <- "temp"              # Directory containing temporary files.

.FMSE     <- "runMSE.par"        # File for feedback loop.
.FPAR     <- "mseRsim.par"       # File containing operating model parameters.

.FSIM     <- "mseRsimTracker.Rdata" # Simulation file - an R binary file.
.FXLS     <- "mseRsimTracker.xls"   # Excel output file for simulations.

.FMODPARS <- "mseRmod.par"       # Parameter file for mseRmod.
.FFITS    <- "mseRfits.Rdata"    # R working directory (a file) containing a list
                                 # of fits to track.
.FOMDAT   <- "mseRomDAT.dat"    # File for operating model data for ADMB.
.FOMPIN   <- "mseRomPIN.pin"    # File for operating model pin file for ADMB.
                                 # for the conditioning step.
.FFXLS    <- "mseRfits.xls"      # Excel output file for fits.

.FADMBOPT <- "mseRadmbOpt.txt"   # ADMB and C++ compiler options.

# ADMB command line options.
.ADMBOPTS <- "-nox -iprint 1000"

# system command GLOBALS for ADMB calls.
.INVISIBLE           <- TRUE
.SHOWOUTPUTONCONSOLE <- FALSE

#------------------------------------------------------------------------------#
# Globals for mseRSystem, mseRrefPoints and general SPC usage - nowhere else.  #
#------------------------------------------------------------------------------#

.INITCATCHKNOTS <- 5
.LASTSOLUTION <- NULL
.INITMSYMULT <- c(0,0.30,1.0,0.75)
.INITTIMES   <- c(0,0.25,0.75,1.0)
.CATCHSERIESINPUT <- FALSE
.INDEXSERIESINPUT <- FALSE
.AGESERIESINPUT   <- FALSE
.RECSERIESINPUT   <- FALSE

# Reference point grid - used by calcRefCurves in mseRrefPoints.r.
.nFVALS <- 50
.MAXF   <- 4.0                  # Multiplier of M in generating yield curves
.MAXIT  <- 50                  # Max iterations for solving F multipliers fg in refpts
.FGINIT <- rep(-2,4)            # Initial Fgs given to optim to solve allocation prob

#------------------------------------------------------------------------------#

# General mseR globals.
.INITYEAR <- 1965     # Sablefish analysis intial year.
.MAXREP <- 200
.GUIMSG <- TRUE
.PLTMSG <- TRUE
.WINNERS <- c( "StrsHiTuneHCR46" )

# ADMB related globals.
.BMSYCRIT    <- 10.0
.FMSYCRIT    <- 0.001
.MAXFUNCALLS <- 500      # Does not affect what ADMB does, just an arbitrary cut-off.
.MAXGRADCRIT <- 1.0E-02
.COLEXIT1 <- "black"
.COLEXIT2 <- "blue"
.COLEXIT3 <- "red"

# Fishery objectives defaults.
.LRPZONEMULT <- 0.4
.USRZONEMULT <- 0.8

# True reference points - depleltion at MSY.
.COLDepMsy      <- "black"
.LTYDepMsy      <- 4
.LWDDepMsy      <- 1
.SYMDepMsy      <- 16

.COLDepLrp      <- "black"
.LTYDepLrp      <- 2
.LWDDepLrp      <- 1
.SYMDepLrp      <- 16

.COLDepUsr      <- "black"
.LTYDepUsr      <- 2
.LWDDepUsr      <- 1
.SYMDepUsr      <- 16

# General global variables.
.CEXANNO      <- 1.0            # Annotation character expansion.
.CEXANNO2     <- 1.2
.CEXAXIS      <- 1.0            # Axis character expansion.
.CEXAXIS2     <- 1.2
.CEXLAB       <- 1.2            # Label character expansion.
.CEXLEG       <- 1.0            # Legend character expansion.
.CEXLEG2      <- 1.2
.CEXSYM       <- 1.0
.CEXSYM2      <- 1.4
.ESTCOL       <- "black"        # Color of estimated values.
.ESTLWD       <- 1              # Line width of estimated values.
.INLINE1      <- 2
.INLINE2      <- 3
.INLINE3      <- 3
.MAR          <- c(2,2,1,1)
.OMA          <- c(2,3,1,1)
.OUTLINE1     <- 0.5
.OUTLINE2     <- 1.5
.OUTLINE3     <- 0.5
.OUTLINE4     <- 1.5
.TRUECOL      <- "red3"         # Color of true values.
.TRUELWD      <- 2              # Line width of true values.

.YAXISLAS     <- 2              # y-Axis label orientation (1=vertical, 2=horiz)

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

.YAXISLAS     <- 2              # y-Axis label orientation (1=vertical, 2=horiz)

# Globals for fits.
.COLFIT       <- "red"
.LTYFIT       <- 1
.LWDFIT       <- 1
.SYMFIT       <- 1

# Globals for legal and sub-legal harvest rate.
.COLLEGALHR <- "black"
.LTYLEGALHR <- 1
.LWDLEGALHR <- 2
.SYMLEGALHR <- 16

.COLSUBLEGHR <- "darkgreen"
.LTYSUBLEGHR <- 2
.LWDSUBLEGHR <- 2
.SYMSUBLEGHR <- 1

# Globals for size limit.
.COLSIZE      <- "black"
.LTYSIZE      <- 2
.LWDSIZE      <- 1

# Globals for gear lines, colors and symbols.

# tMP globals.
.COLTMP       <- "black"
.LTYTMP       <- 2
.LWDTMP       <- 1

.COLGEAR      <- c( "black","red","green","blue","magenta" )

.COLIDX       <- "black"
.LTYIDX       <- 1
.LWDIDX       <- 1
.SYMIDX       <- 21

.COLBTOT      <- "black"
.LTYBTOT      <- 1
.LWDBTOT      <- 1

.COLBmsy      <- "black"
.LTYBmsy      <- 4
.LWDBmsy      <- 2
.SYMBmsy      <- 16

# Spawning biomass.
.COLBt        <- "black"
.LTYBt        <- 1
.LWDBt        <- 3

# Legal biomass.
.COLlegalB    <- "blue"
.LTYlegalB    <- 5
.LWDlegalB    <- 2

# Sub-legal biomass.
.COLslegalB    <- "blue"
.LTYslegalB    <- 3
.LWDslegalB    <- 2

.COLCt        <- "black"
.LTYCt        <- 1
.LWDCt        <- 2
.SYMCt        <- 21

.COLCtg       <- .COLGEAR
.LTYCtg       <- c( 1, 1, 1, 1, 1)
.LWDCtg       <- c( 3, 3, 3, 3, 3)
.SYMCtg       <- c(21,21,21,21,21)

.COLDt        <- "black"
.LTYDt        <- 1
.LWDDt        <- 2
.SYMDt        <- 16

.COLDtg       <- .COLGEAR
.LTYDtg       <- c( 1, 1, 1, 1, 1)
.LWDDtg       <- c( 1, 1, 1, 1, 1)
.SYMDtg       <- c(21,21,21,21,21)

.COLFtg       <- .COLGEAR
.LTYFtg       <- c( 1, 1, 1, 1, 1 )
.LWDFtg       <- c( 1, 1, 1, 1, 1 )

.COLItg       <- .COLGEAR
.LTYItg       <- c( 1, 1, 1, 1, 1)
.LWDItg       <- c( 3, 3, 3, 3, 3)
.SYMItg       <- c(21,21,21,21,21)

.COLNTOT      <- "black"
.LTYNTOT      <- 1
.LWDNTOT      <- 1

.COLNt        <- "black"
.LTYNt        <- 1
.LWDNt        <- 3

.COLPlg       <- .COLGEAR
.LTYPlg       <- rep( 1,length(.COLGEAR) )
.LWDPlg       <- rep( 2,length(.COLGEAR) )

.COLRt        <- "black"
.LTYRt        <- 1
.LWDRt        <- 2
.SYMRt        <- 21

.COLSlg       <- .COLGEAR
.LTYSlg       <- rep( 1,length(.COLGEAR) )
.LWDSlg       <- rep( 2,length(.COLGEAR) )

# Globals for View plots (guiView).
.SIMCAX      <- 1.2            # Axis character expansion factor.
.SIMCEX      <- 1.2            # Axis label character expansion factor.
.SIMCEXOUT   <- 1.2            # Axis character expansion factor outer margin.

# Globals for View plots (guiView).
.VIEWCAX      <- 1.2            # Axis character expansion factor.
.VIEWCEX      <- 1.2            # Axis label character expansion factor.
.VIEWYLAS     <- 2              # Axis label orientation for Y-axis.
.VIEWLEGCEX   <- 0.8            # Legend character expansion factor.
.VIEWPANCEX   <- 1              # Default cex for panLab.
.VIEWTEX      <- 1.4            # Title label character expansion factor.

.VIEWMAR      <- c(3, 3, 1, 1)  # 1-Panel plots: plot margin sizes: c(b,l,t,r).
.VIEWMMAR     <- c(2, 4, 1, 1)  # 3-Panel plots: plot margin sizes: c(b,l,t,r).
.VIEWMOMA     <- c(3, 2, 2, 2)  # 3-Panel plots: outer margin sizes: c(b,l,t,r).
.VIEWOMA      <- c(3, 2, 2, 2)  # 1-Panel plots: outer margin sizes: c(b,l,t,r).
.VIEWTMPLTY   <- 3              # Line type for tMP vertical line.
.VIEWXLIM     <- c(1,100)       # X-axis range.
.VIEWYMULT    <- 3.0            # Plot scaling multipler * average of y-values.

# Globals for Harvest Control Rule plots.
.CRITCOL      <- "pink"
.CAUTCOL      <- "yellow"
.HEALCOL      <- "lightgreen"
.HCRLRPLTY    <- 2
.HCRLRPLWD    <- 1
.HCRUSRLTY    <- 5
.HCRUSRLWD    <- 1
.ZONELIMLTY   <- 2
.ZONELIMLWD   <- 2
.ZONEUPPLTY   <- 5
.ZONEUPPLWD   <- 2

# Color for background grid on plots.
.COLGRID      <- "lightblue"

# Globals for Performance statistics plots.
# Globals for Other plots (guiPerf).
.OTHMAR       <- c(2, 2,0.5,1)  # Lines of panel margins: c(b, l, t, r).
.OTHOMA       <- c(2, 3, 2,2.5) # Lines of outer margins: c(b, l, t, r).

# Globals for Performance plots (guiPerf).
.PERFCAX      <- 1.2            # Axis character expansion factor.
.PERFCEX      <- 1.2            # mtext character expansion factor.
.PERFCEX2     <- 1.4            # mtext character expansion factor 2
.PERFLEX      <- 1.0            # Simulation label character expansion factor.
.PERFSEX      <- 1.4            # Symbol expansion factor.
.PERFSYM      <- 16             # Symbol type for performance barplots.
.PERFTEX      <- 1.4            # Title character expansion factor.

.PERFMAR      <- c(2,0.5,1,0.5) # Barplots: plot margin sizes: c(b,l,t,r).
.PERFOMA      <- c(3, 12, 3, 2)  # Barplots: outer margin sizes: c(b,l,t,r).

# Globals for target objectives.
.TARGCOL      <- "red"
.TARGPROBCOL  <- "royalblue"
.TARGYEARCOL  <- "black"

# Used for .plotTulipXXX.
.TULCAX       <- 1.2            # Axis character expansion factor.
.TULCEX       <- 1.2            # Character expansion factor for annotation.
.TULCOL       <- "lightgray"    # Color of simulation envelope.
.TULIDX       <- c( 1, 2, 3 )   # Replicate index for random traces.
.TULMAR       <- c(2, 2, 0.5, 1)  # Lines of panel margins: c(b, l, t, r).
.TULMFROW     <- c(2, 2)        # Default number of panels in plot: c(nRows,nCols).
.TULQCOL      <- "red"          # Color of user supplied quantile lines.
.TULOMA       <- c(2, 3, 2, 1)  # Lines of outer margins: c(b, l, t, r).
.TULXLIMB     <- c( 0, 100 )    # x-axis limits for spawning biomass.
.TULXLIM      <- c( 1,100 )     # x-axis limits for time.
.TULYLIMC     <- c( 0,7000 )    # y-axis limits for catch.
.TULYLIMD     <- c( 0.0, 0.6 )  # y-axis limits for depletion.
.TULYLIMF     <- c( 0.0, 1.0 )  # y-axis limits for fishing mortality.

# Used for .plotPerfBars.

.BARSMFROW    <- c(2,3)         # Number of rows and columns for bar plots.
