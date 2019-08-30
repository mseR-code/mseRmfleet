# Global Options.
# General globals.
.INITYEAR <- 1965
.LASTYEAR <- 2016
.USEYEAR <- TRUE

# General global variables.
.CEXANNO      <- 1.0            # Annotation character expansion.
.CEXANNO2     <- 1.2
.CEXAXIS      <- 1.0            # Axis character expansion.
.CEXAXIS2     <- 1.2
.CEXLAB       <- 1.            # Label character expansion.
.CEXLAB2      <- 1.4
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
.OMA          <- c(4,7,1,1)
.OUTLINE1     <- 1
.OUTLINE2     <- 1
.OUTLINE3     <- 0.5
.OUTLINE4     <- 1.5
.TRUECOL      <- "red3"         # Color of true values.
.TRUELWD      <- 2              # Line width of true values.
.YAXISLAS     <- 2              # y-Axis label orientation (1=vertical, 2=horiz)
.LTYGROUP1    <- 1
.LTYGROUP2    <- 2

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

# Globals for size limit.
.COLSIZE      <- "black"
.LTYSIZE      <- 2
.LWDSIZE      <- 1

# Globals for gear lines, colors and symbols.

.COLGEAR      <- c( "black","red","green","blue","magenta" )
#.LWDGEAR      <- c( 1, 1, 1, 1, 1 )
.LWDGEAR      <- c(  2, 2, 2, 2, 2)
.LTYGEAR      <- c( 1, 1, 1, 1, 1 )
.SYMGEAR      <- c( 21, 21, 21, 21, 21 )

.COLBTOT      <- "black"
.LTYBTOT      <- 1
.LWDBTOT      <- 1

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

# Globals for legal and sub-legal harvest rate.
.COLLEGALHR <- "black"
.LTYLEGALHR <- 1
.LWDLEGALHR <- 2
.SYMLEGALHR <- 16

.COLSUBLEGHR <- "darkgreen"
.LTYSUBLEGHR <- 2
.LWDSUBLEGHR <- 2
.SYMSUBLEGHR <- 1

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

