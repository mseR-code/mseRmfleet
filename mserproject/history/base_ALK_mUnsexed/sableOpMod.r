#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Version 4.x                   #
#                                                                              #
#     Copyright 2008, 2009, 2010 by A.R. Kronlund and S.P. Cox.                #
#                                K. Holt.                                      #
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
#----------------- mseRuserCode.r: mseRmod Plotting Functions -----------------#

#------------------------------------------------------------------------------#
#-- General plotting functions (not specified to sableOpmod)                 --#
#------------------------------------------------------------------------------#

library(dplyr)
library(plotrix)


.makePriorCptsTable <- function( obj, saveLoc = "./" )
{
  # Outputs a csv version of the standard errors
  # table - this will be a little more involved than makeEstimates
  tmp <- obj[ c("mPrior","ssbPrior","selPrior","recPrior","hPrior")]
  tmp$releaseLikelihood <- tmp$releaseLikelihood[1:3]

  df <- data.frame( matrix(nrow = 1, ncol = 5) )
  names(df) <- names(tmp)
  df[1,] <- round(unlist(tmp),3)

  saveFile <- file.path(saveLoc,"priorCpts.csv")

  write.csv(df, file = saveFile )
}

.makeLikelihoodCptsTable <- function( obj, saveLoc = "./" )
{
  # Outputs a csv version of the standard errors
  # table - this will be a little more involved than makeEstimates
  tmp <- obj[ c("indexLikelihood","ageLikelihood_m","ageLikelihood_f","releaseLikelihood","fLike","catLike")]
  tmp$releaseLikelihood <- tmp$releaseLikelihood[1:3]

  idxNames   <- paste("idxLike_",c(1,4,5),sep = "")
  ageMNames  <- paste("ageLike_m",c(1,4,5),sep = "")
  ageFNames  <- paste("ageLike_f",c(1,4,5),sep = "")
  relNames   <- paste("relLike_",1:3,sep = "")
  lastNames  <- c("fLike","catLike")

  df <- data.frame( matrix(nrow = 1, ncol = 14) )
  names(df) <- c( idxNames, ageMNames, ageFNames, relNames, lastNames )
  df[1,] <- round(unlist(tmp),3)

  saveFile <- file.path(saveLoc,"likelihoodCpts.csv")

  write.csv(df, file = saveFile )
}

.makeStdErrTable <- function( obj, saveLoc = "./" )
{
  # Outputs a csv version of the standard errors
  # table - this will be a little more involved than makeEstimates
  tmp <- obj[ c("tauIndex","tauAge_m","tauAge_f","tauRel")]
  tmp$tauRel <- tmp$tauRel[1:3]

  tauIdxNames   <- paste("tauIndex_",c(1,4,5),sep = "")
  tauAgeMNames  <- paste("tauAge_m",c(1,4,5),sep = "")
  tauAgeFNames  <- paste("tauAge_f",c(1,4,5),sep = "")
  tauRelNames   <- paste("tauRel_",1:3,sep = "")

  df <- data.frame( matrix(nrow = 1, ncol = 12) )
  names(df) <- c( tauIdxNames, tauAgeMNames, tauAgeFNames, tauRelNames )
  df[1,] <- round(unlist(tmp),3)

  saveFile <- file.path(saveLoc,"MLEstdErrs.csv")

  write.csv(df, file = saveFile )
}

.makeEstimatesTable <- function( obj, saveLoc = "./" )
{
  # Plots a text table in the graphics window.
  # gridExtra package required.
  # obj is a list, probably from opmodControl.ctl or REPORT file.
  tmp <- obj[ c("objFun","maxGrad","h","SSB0",
                "R0","M_m","M_f","sigma_R","bh_a","bh_b" ) ]

  SSB_T <- obj$SSBt[length(obj$SSBt)]
  tmp$SSB_T <- SSB_T

  # Gonna have to add Ref Pts for the output table
  refPts <- .calcRefPointsFromRep(obj,"simCtlFile.txt")
  
  refPts <- unlist(refPts)
  refPts <- round(refPts,3)

  df <- data.frame( matrix(NA,nrow = 1, ncol = length(tmp) + length(refPts) ) )
  names(df) <- c(names(tmp),names(refPts))
  df[1,] <- c(round(unlist(tmp),3),refPts)

  df <- df %>% mutate( Dmsy_T = round(SSB_T/Bmsy,2) )

  saveFile <- file.path(saveLoc,"MLEparEsts.csv")

  write.csv(df, file = saveFile )

}

.plotAgeErrorMatrix <- function( obj )
{
  # Access ageing error matrix
  Q <- as.matrix(obj$Q)

  Q <- apply( X=Q, MARGIN=2, FUN=function(x) x<-x/sum(x) )

  # Example output
  ages     <- 2:35
  M        <- obj$M_f
  trueAges <- exp( -M*(ages-1) )
  trueAges[34] <- trueAges[34-1]/(1.-exp(-M))
  trueAges <- trueAges/sum(trueAges)
  obsAges  <- Q%*%trueAges
  obsAges  <- obsAges/sum(obsAges)
  plot( 2:35, trueAges, type="b", las=1, pch=19,
          xlab="Age", ylab="Proportion-at-age",ylim=c(0,max(obsAges)) )

  #plot( 3:35, obsAges[-c(1:2)], type="b", las=1, pch=19,
  #        xlab="Age", ylab="Proportion-at-age",ylim=c(0,max(obsAges)) )
  lines( 2:35, obsAges )
  legend( x=35/2,y=0.07, legend="Obs age",
          lty="solid", bty="n" )
  legend( x=35/2,y=0.06, legend="True age",
          lty="solid", pch=19, bty="n" )
}

.plotEstimatesTable <- function( obj )
{
  # Plots a text table in the graphics window.
  # gridExtra package required.
  # obj is a list, probably from opmodControl.ctl or REPORT file.
  tmp <- obj[ c("objFun","maxGrad","exitCode","funEvals","avgR","h","SSB0",
                "R0","M_m","M_f","sigma_R","bh_a","bh_b" ) ]
  df <- data.frame( Parameter=names(tmp), Value=unlist(tmp) )

  mytheme <- gridExtra::ttheme_default( core=list( fg_params=list(cex=1.5),
               colhead=list( fg_params=list(cex=2), bg_params=list(cex=2) ),
               rowhead=list( fg_params=list(cex=2) ) ) ) 
  grid.table( df, rows=NULL, theme=mytheme )
}

# Plot reference curves
.plotRefCurves <- function( obj = repList )
{
  # Use calcRefPoints function
  refPts <- .calcRefCurvesFromRep( obj, "simCtlFile.txt"  ) 

  # plot ref curves
  .plotRefPoints(obj = refPts )

}

# Plot reference curves
.plotRefCurvesF <- function( obj = repList )
{
  # Use calcRefPoints function
  refPts <- .calcRefCurvesFromRep( obj, "simCtlFile.txt"  ) 

  # plot ref curves
  .plotRefPoints(obj = refPts )

}

# Plot reference curves
.plotRefCurvesU <- function( obj = repList )
{
  # Use calcRefPoints function
  refPts <- .calcRefCurvesFromRep( obj, "simCtlFile.txt"  ) 

  # plot ref curves
  .plotRefPointsU(obj = refPts )

}


.plotBubbles <- function (z, xval = FALSE, yval = FALSE, dnam = FALSE, rpro = FALSE, 
    cpro = FALSE, rres = FALSE, cres = FALSE, powr = 0.5, size = 0.2, 
    lwd = 1, clrs = c("black", "red", "blue"), hide0 = FALSE, 
    frange = 0.1, ...) 
{
    if (is.data.frame(z)) {
        use = !sapply(z, is.factor) & sapply(z, is.numeric)
        z = z[, use]
        if (ncol(z) == 0) {
            showAlert("data frame not useable")
            return()
        }
        z = as.matrix(z)
    }
    dz <- dim(z)
    ny = ny1 = dz[1]
    nx = nx1 = dz[2]
    if (length(dz) > 2) {
        showAlert("Input matrix must have only 2 dimensions")
        return()
    }
    xval1 <- 1:nx
    yval1 <- 1:ny
    if (mode(xval) == "logical") {
        if (xval[1]) {
            xval1 <- z[1, ]
            ny1 <- ny - 1
        }
    }
    if (mode(yval) == "logical") {
        if (yval[1]) {
            yval1 <- z[, 1]
            nx1 <- nx - 1
        }
    }
    xind <- (nx - nx1 + 1):nx
    x2 = xlabel = xval1[xind]
    yind <- (ny - ny1 + 1):ny
    y2 = ylabel = yval1[yind]
    if ((mode(xval) != "logical") & (length(xval) == nx1)) {
        if (mode(xval) == "numeric") 
            x2 = xval
        xlabel = xval
    }
    if ((mode(yval) != "logical") & (length(yval) == ny1)) {
        if (mode(yval) == "numeric") 
            y2 = yval
        ylabel = yval
    }
    zz <- array(z[yind, xind], dim = c(length(yind), length(xind)), 
        dimnames = dimnames(z))
    dots = list(...)
    xlab = dots$xlab
    if (is.null(xlab)) 
        xlab = ""
    ylab = dots$ylab
    if (is.null(ylab)) 
        ylab = ""
    if (dnam & !is.null(dimnames(zz))) {
        warn = options()$warn
        options(warn = -1)
        if (!is.null(dimnames(zz)[[2]])) {
            xpos = try(as.numeric(dimnames(zz)[[2]]), silent = TRUE)
            if (all(is.na(xpos))) 
                xlabel = dimnames(zz)[[2]]
            else if (!any(is.na(xpos)) && all(diff(xpos) > 0 | 
                all(diff(xpos) < 0))) {
                xlabel = as.character(xpos)
                x2 = xpos
            }
        }
        if (!is.null(dimnames(zz)[[1]])) {
            ypos = try(as.numeric(dimnames(zz)[[1]]), silent = TRUE)
            if (all(is.na(ypos))) 
                ylabel = dimnames(zz)[[2]]
            else if (!any(is.na(ypos)) && all(diff(ypos) > 0 | 
                all(diff(ypos) < 0))) {
                ylabel = as.character(ypos)
                y2 = ypos
            }
        }
        options(warn = warn)
    }
    xx <- rep(x2, each = length(y2))
    yy <- rep(y2, length(x2))
    minz <- min(zz, na.rm = TRUE)
    maxz <- max(zz, na.rm = TRUE)
    if (rpro | cpro) {
        if (minz < 0) {
            zz <- zz - minz
            minz <- 0
            maxz <- max(zz, na.rm = TRUE)
        }
    }
    if (rpro) {
        zs <- apply(zz, 1, sum, na.rm = TRUE)
        zz <- sweep(zz, 1, zs, "/")
    }
    if (cpro) {
        zs <- apply(zz, 2, sum, na.rm = TRUE)
        zz <- sweep(zz, 2, zs, "/")
    }
    if (rres) {
        zm <- apply(zz, 1, mean, na.rm = TRUE)
        zz <- sweep(zz, 1, zm, "-")
    }
    if (cres) {
        zm <- apply(zz, 2, mean, na.rm = TRUE)
        zz <- sweep(zz, 2, zm, "-")
    }
    zNA <- is.na(zz) | is.nan(zz) | is.infinite(zz)
    zz[zNA] <- 0
    z0 <- sign(zz) * abs(zz)^abs(powr)
    z1 <- z3 <- z0
    z1[z0 <= 0] <- NA
    z3[z0 < 0 | z0 > 0] <- NA
    z2 <- -z0
    z2[z0 >= 0] <- NA
    za <- max(z0, na.rm = TRUE)
    zb <- min(z0, na.rm = TRUE)
    zM <- max(abs(z0))
    sz1 <- max(za * size/zM, 0.001)
    sz2 <- max(-zb * size/zM, 0.001)
    
    # ARK (11-Jul-10) Added axes=FALSE to remove y-axis labels.
    evalCall(plot, argu = list(x = 0, y = 0, xlim = extendrange(x2, 
        f = frange), ylim = extendrange(y2, f = frange), axes=FALSE, type = "n", 
        xaxt = "n", xlab = xlab, ylab = ylab ), ..., checkdef = TRUE, 
        checkpar = TRUE)
        
    evalCall(axis, argu = list(side = 1, at = x2, labels = xlabel), 
        ..., checkpar = TRUE)
    
    # ARK (11-Jul-10) Added to implement yval.    
    evalCall(axis, argu = list(side = 2, at = y2, labels = ylabel, las=2), 
        ..., checkpar = TRUE)

    evalCall(axis, argu = list(side = 3, at = x2, labels = FALSE ), 
        ..., checkpar = TRUE)

    evalCall(axis, argu = list(side = 4, at = y2, labels = FALSE), 
        ..., checkpar = TRUE)

    box()        
        
    if (!hide0 && !all(is.na(z3))) {
        evalCall(symbols, argu = list(x = xx, y = yy, circles = as.vector(z3), 
            inches = 0.001, fg = clrs[3], lwd = lwd, add = TRUE), 
            ..., checkpar = TRUE)
    }
    if (!all(is.na(z2))) {
        evalCall(symbols, argu = list(x = xx, y = yy, circles = as.vector(z2), 
            inches = sz2, fg = clrs[2], lwd = lwd, add = TRUE), 
            ..., checkpar = TRUE)
    }
    if (!all(is.na(z1))) {
        evalCall(symbols, argu = list(x = xx, y = yy, circles = as.vector(z1), 
            inches = sz1, fg = clrs[1], lwd = lwd, add = TRUE), 
            ..., checkpar = TRUE)
    }
    
    # ARK (18-Jul-10) Changed from z0 to zz.
    invisible(zz)
}

#------------------------------------------------------------------------------#
#-- sableOpMod Plotting Functions                                            --#
#------------------------------------------------------------------------------#

.plotLenAtAge <- function( obj, label=NULL,
                   gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )
    yLim <- c( 0,max( c(obj$lenAge_m,obj$lenAge_f ) ) )
  
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  #for( l in 1:ncol(obj$Lal) )  
  lines( obj$ages, obj$lenAge_m, col="blue",  lty=1, lwd=2 )
  lines( obj$ages, obj$lenAge_f, col="black", lty=1, lwd=2 )
  
  if ( gfx$annotate )
  {
    abline( h=obj$sizeLim,lty=2, lwd=2 )
    if ( !is.null(label) )
      panLab( 0.75, 0.05, adj=0, cex=.CEXLAB, label )
  }
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS, label=FALSE )
  box()
    
  mtext( side=1, line=2,   cex=.CEXLAB, "Age" )
  mtext( side=2, line=2.5, cex=.CEXLAB, "Length-at-age (cm)" )
}     # .plotLenAtAge function


.plotMatAtAge <- function( obj, label=NULL,
                   gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(c( obj$matAge_m, obj$matAge_f) ) )
    
  # Maturity at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  
  if ( !is.null( obj$matAge_m ) )
    lines( obj$ages, obj$matAge_m, col="blue",  lty=1, lwd=2 )
  lines( obj$ages, obj$matAge_f, col="black", lty=1, lwd=2 )

  if ( gfx$annotate )
  {  
    A50 <- max( obj$ages[ obj$matAge_f <= 0.5001 ] )
    A95 <- max( obj$ages[ obj$matAge_f <= 0.9501 ] )
    segments( A50, 0.0,  A50, 0.5,  lty=2, lwd=2 )
    segments( 0.0, 0.5,  A50, 0.5,  lty=2, lwd=2 )
    segments( A95, 0.0,  A95, 0.95, lty=2, lwd=2 )
    segments( 0.0, 0.95, A95, 0.95, lty=2, lwd=2 )
      
    if ( !is.null(label) )
      panLab( 0.75, 0.05, adj=0, cex=.CEXLAB, label )      
  }
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2,   cex=.CEXLAB, "Age" )
  mtext( side=2, line=2.5, cex=.CEXLAB, "Proportion Mature-at-Age" )
}     # .plotMatAtAge function


.plotWgtAtAge <- function( obj, label=NULL,
                   gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( 0,max(obj$ages) )
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max( c(obj$wtAge_m,obj$wtAge_f) ) )
  
  # Weight at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  #for( l in 1:ncol(obj$Wal) )  
  lines( obj$ages, obj$wtAge_m, col="blue",  lty=1, lwd=2 )
  lines( obj$ages, obj$wtAge_f, col="black", lty=1, lwd=2 )
    
  if ( gfx$annotate )
    if ( !is.null(label) )
      panLab( 0.75, 0.05, adj=0, cex=.CEXLAB, label )        
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  box()
    
  mtext( side=1, line=2,   cex=.CEXLAB, "Age" )
  mtext( side=2, line=2.5, cex=.CEXLAB, "Weight-at-age (kg)" )
}     # .plotWgtAtAge function


.plotWgtLen <- function( obj, label=NULL, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    #xLim <- c( min(rowMeans(obj$Lal)),max(rowMeans(obj$Lal)) )
    xLim <- c( 0, 110 )

  if ( is.null(yLim) )
    #yLim <- c( min(rowMeans(obj$Wal)),max(rowMeans(obj$Wal)) )
    yLim <- c( 0, 10 )
  
  # Weight against length.
  #plot( rowMeans(obj$Lal), rowMeans(obj$Wal), type="n", axes=FALSE,
  #      xlab="",xlim=xLim,ylab="",ylim=yLim )
  #lines( rowMeans(obj$Lal), rowMeans(obj$Wal), lty=1 )
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  len <- seq( 1,110,1 )
  wgt_m <- obj$wt_a[1] * len^obj$wt_b[1]
  wgt_f <- obj$wt_a[2] * len^obj$wt_b[2]

  lines( len, wgt_m, col="blue",  lty=1, lwd=2 )
  lines( len, wgt_f, col="black", lty=1, lwd=2 )

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  box()
  
  if ( gfx$annotate )
    if ( !is.null(label) )
      panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, label )        
    
  mtext( side=1, line=2,   cex=.CEXLAB, "Length (cm)" )
  mtext( side=2, line=2.5, cex=.CEXLAB, "Weight (kg)" )    
}     # .plotWgtLen function

#------------------------------------------------------------------------------#
#-- Plotting Functions: Selectivity and Discards                             --#
#------------------------------------------------------------------------------#

.plotSalg <- function(  obj, seriesName=NULL, gfx=list( autolayout=TRUE, annotate=TRUE,
                        doLegend=TRUE, xLim=NULL, yLim=NULL))
{
  # ARK (13-Oct-10) former format of Salg was a large array, now a list of nSeries
  # matrix of dimension nGrps by nAges.
  #Salg    <- obj$Salg
  #dimSalg <- dim( Salg )
  #nAges   <- dimSalg[1]
  #nGear   <- dimSalg[3]
  #nGrps   <- dimSalg[2]

  # ARK (04-Nov-15).  The new format is individual matrices of nT by nAges.
  # Each gear will be passed in separately.  Groups are gone...
  
  #nAges <- dim(obj[[1]])[2]
  #nGear <- length( obj )
  #nGrps <- dim(obj[[1]])[1]
  # trap

  nAges <- ncol( obj )
  nT    <- nrow( obj )
  
  if ( is.null(gfx$xLim) )
    xRange <- c(0,nAges)
  else
    xRange <- gfx$xLim
    
  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim

  # detect selectivity blocks
  
  plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  
  # Colours
  yearCols <- gray.colors(n = nT, start = 0.7, end =0)
  
  # Plot yearly lines
  for( tIdx in 1:nT )
    lines( c(1:nAges), obj[tIdx,], col=yearCols[tIdx], lty=1, lwd=1 )

  # Now plot first year's line as a solid black line
  lines( 1:nAges, obj[1,], col = "black", lty = 1, lwd = 2 )

  panLab( 0.7,0.8, adj=0, cex=1.2, seriesName )  

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
  box()

  if ( gfx$annotate )
  {
    mtext( side=1, line=.OUTLINE3, cex=1.2, outer=TRUE, "Age" )
    mtext( side=2, line=2, cex=1.2, outer=TRUE, "Selectivity" )
  }


  if( gfx$doLegend)
  {
    pnts <- cbind( x = c( 32, 33, 33, 32), y = c(0.2, 0.2, 0.8, 0.8 ) )
    legend.gradient(  pnts, cols = yearCols, 
                      limits = c(.INITYEAR,.INITYEAR + nT - 1),
                      title = "Year")
  }
}     # .plotSalg selectivity function

plotSelFuncs <- function()
{
  s50 <- 60
  s95 <- 80

  s95d <- 100
  s50d <- 300

  l <- seq(0,300,length=100)
  sel1 <- 1./(1.+exp(-log(19)*(l-s50)/(s95-s50)))
  sel2 <- 1./(1.+exp(-log(19)*(l-s50d)/(s95d-s50d)))
  sel <- sel1*sel2
  sel <- sel/max(sel)
  plot( l, sel, type="l", ylim=c(0,1) )

  # output scal parameters:
  # log_S50_g_a[1]:
  log_S50_g_a <- log(s50)
  # dev_S95_g_a[1]:
  dev_S95_g_a <- log( s95-s50 )
  # dev_S95_g_d[1]:
  dev_S95_g_d <- log(s95d-s95)
  # dev_S50_g_d[1]:
  dev_S50_g_d <- log(s50d-s95d)
  selPars <- c(log_S50_g_a,dev_S95_g_a,
                dev_S95_g_d,dev_S50_g_d)
  selNames <- c("log_S50_g_a","dev_S95_g_a",
                "dev_S95_g_d","dev_S50_g_d")
  write(x=selNames, file="selPars.pin", ncolumns=4,append=F,sep=" ")
  write(x=selPars, file="selPars.pin",ncolumns=4,append=T, sep=" ")

} 


.plotSlg <- function( obj, label=NULL, gearNum = 1, sexNum = 1,
                      gfx=list( autolayout=TRUE, annotate=TRUE, bygear=TRUE,
                      doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  if ( is.null(gfx$xLim) )
    xRange <- range( obj$lenAge_m, obj$lenAge_f )
  else
    xRange <- gfx$xLim
    
  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim
  
  nGear   <- length(obj$alpha_g1)

  
  # Initial values.
  alpha_g1  <- obj$alpha_g1[gearNum]
  beta_g1   <- obj$beta_g1[gearNum]
  
  # Time varying values
  alpha_gt_m  <- obj$alpha_gt_m[gearNum,]
  beta_gt_m  <- obj$beta_gt_m[gearNum,]
  alpha_gt_f  <- obj$alpha_gt_f[gearNum,]
  beta_gt_f  <- obj$beta_gt_f[gearNum,]

  nT <- length(alpha_gt_m)

  # Get size limit and sel type
  sizeLim <- obj$sizeLim
  selType <- obj$selType

  # Calculate curves from parameters
  len <- seq(from = 32, to = 75, length = 100)
  Sel_tlx <- array(0,dim = c(nT,100,2))
  for( t in 1:nT)
  {
    alpha_t_m <- alpha_gt_m[t]
    beta_t_m  <- beta_gt_m[t]
    alpha_t_f <- alpha_gt_f[t]
    beta_t_f  <- beta_gt_f[t]

    if(selType[gearNum] == 1)
    {
      Sel_tlx[t,,1] <- 1 / (1 + exp(-log(19) * (len - beta_t_m) / (alpha_t_m - beta_t_m)))
      Sel_tlx[t,,2] <- 1 / (1 + exp(-log(19) * (len - beta_t_f) / (alpha_t_f - beta_t_f)))
    }
    if(selType[gearNum] == 2)
    {
      Sel_tlx[t,,1] <- exp(-(0.5 * (len - alpha_t_m)/beta_t_m)^2 )
      Sel_tlx[t,,2] <- exp(-(0.5 * (len - alpha_t_f)/beta_t_f)^2 )
    }

    if(selType[gearNum] == 3)
    {
      Sel_tlx[t,,1] <- len ^(alpha_t_m - 1) * exp(-len/beta_t_m)
      Sel_tlx[t,,1] <- Sel_tlx[t,,1] / max(Sel_tlx[t,,1])
      Sel_tlx[t,,2] <- len ^(alpha_t_f - 1) * exp(-len/beta_t_f)
      Sel_tlx[t,,2] <- Sel_tlx[t,,2] / max(Sel_tlx[t,,2])
    }
  }

  plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )

  yearCols <- gray.colors(n = nT, start = .7, end = 0)

  # Plot sel-at-length by year
  for( t in 1:nT )
    lines( len, Sel_tlx[t,,sexNum], col=yearCols[t], lty = 1, lwd=1 )

  lines( len, Sel_tlx[1,,sexNum], col = "black", lty = 1, lwd = 2 )
  
  abline(v = sizeLim, lty = 2, col = "salmon", lwd = 2)

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
  box()

  if ( gfx$annotate )
  {
    mtext( side=1, line=.OUTLINE3, cex=1.2, outer=TRUE, "Age" )
    mtext( side=2, line=2, cex=1.2, outer=TRUE, "Selectivity" )
  }

  if( gfx$doLegend )
  {
    pnts <- cbind( x = c( 32, 35, 35, 32), y = c(0.2, 0.2, 0.8, 0.8 ) )
    legend.gradient( pnts, cols = yearCols, limits = c(.INITYEAR,.INITYEAR + nT - 1),
                      title = "Year")
  }

  
}     # .plotSlg function


.plotPlg <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE, bygear=TRUE,
                           doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # obj: input as a list of parameters.
  
  if ( is.null(gfx$xLim) )
    xRange <- c( obj$L1,trunc(max(obj$Lal)) )
  else
    xRange <- gfx$xLim

  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim
  
  dg      <- obj$dg
  nGear   <- obj$nGear
  L50Dg   <- obj$L50Dg
  L95Dg   <- obj$L95Dg
  sizeLim <- obj$sizeLim
  
  seriesNames <- paste( "Series",c(1:nGear) )
  
  # Vector for plotting range of lengths.  
  len <- seq( xRange[1],xRange[2],0.25 )

  Plg <- matrix( NA, nrow=length(len), ncol=nGear )
  for( g in 1:nGear )
  {
    tmp <- exp( (-1.)*log(19.0)*(len-L50Dg[g])/(L95Dg[g] - L50Dg[g]) )
    tmpP <- (1./(1.+ tmp))
    tmpP <- ifelse( len < sizeLim[g], 1.0, tmpP )
    Plg[,g] <- tmpP
  }
 
  # Separate plot for each gear type.
  if ( gfx$bygear )
  {
    for ( g in 1:nGear  )
    {
      plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
      lines( len,Plg[,g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )
      
      #if ( gfx$annotate )
        abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )
    
      if ( gfx$annotate )
        panLab( 0.025,0.80, adj=0, cex=1.5,
                paste( seriesNames[g],"(dg =",round( obj$dg[g],digits=3 ),")"  ) )
 
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
      
      if ( gfx$doLegend )
      {
        rdig <- 5
        parVals <- paste( "L50Dg  ",format(L50Dg[g],digits=rdig,width=6),
                          "  L95Dg  ", format(L95Dg[g],digits=rdig,width=6),
                          sep="" ) 
        panLab( 0.025, 0.65, adj=0, cex=1.1, parVals )
      }
    }
  }
  # Single plot with gear overlay.
  else
  {
    plot( xRange, yRange, type="n",axes=F,xlab="",ylab="" )
    for ( g in 1:nGear  )
      lines( len,Plg[,g], col=.COLPlg[g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )

    if ( gfx$annotate )
      abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )
    
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
      
    if ( gfx$doLegend )
    {
      tmp <- paste( seriesNames, "(dg =",round( obj$dg, digits=3 ),")" )
      panLegend( 0.6,0.95, legTxt=tmp, bg="white",
                 col=.COLPlg, lty=.LTYPlg, lwd=.LWDPlg, pt.cex=1.2 )    
    }
  }

  mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Length" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Proportion Discarded at Length" )
}


.plotPalg <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL) )
{
  # ARK (13-Oct-10) former format of Salg was a large array, now a list of nSeries
  # matrix of dimension nGrps by nAges.
  #Palg    <- obj$Palg
  #dimPalg <- dim( Palg )
  #nAges   <- dimPalg[1]
  #nGear   <- dimPalg[3]
  #nGrps   <- dimPalg[2]
  
  # obj is now a list "Palg" of length nFish with matrices (nGrps,nAges)
  
  nAges <- dim(obj[[1]])[2]
  nGear <- length( obj )
  nGrps <- dim(obj[[1]])[1]

  seriesNames <- paste( "Series",c(1:nGear) )

  if ( is.null(gfx$xLim) )
    xRange <- c(0,nAges)
  else
    xRange <- gfx$xLim
    
  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim

  for ( g in 1:nGear )
  {  
    plot( xRange,yRange,type="n",axes=FALSE,xlab="",ylab="" )
  
    for( i in 1:nGrps )  
      #lines( c(1:nAges),Palg[,i,g], lty=1 )
      lines( c(1:nAges),obj[[g]][i,], lty=1 )
      
    panLab( 0.025,0.90, adj=0, cex=1.3, seriesNames[g] )  
        
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
  }
  
  mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Age" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Proportion Discarded at Age" )
}      # .plotPalg function

#------------------------------------------------------------------------------#
#-- Plotting Functions: Data inputs                                          --#
#------------------------------------------------------------------------------#

.plotIndices <- function( obj, seriesNames=NULL,
                  gfx=list( annotate=TRUE, bygears=FALSE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # idxSeries (nSeries by nT)
  idxSeries <- obj$idxSeries
  nSeries  <- nrow( idxSeries )
  
  # These are the indices of the possible stock indices, BUT NOT the row index
  # values to pull out of idxSeries which consist only of the active indices.
  
  idxIndex <- obj$idxIndex
  if ( is.null(seriesNames) )
    idxNames <- paste( "Series",idxIndex,sep="" )
  else
    idxNames <- seriesNames

  nT       <- obj$nT

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # years: These will become xval.
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- range( xVal )

  if ( gfx$bygears )
  {
    #dev.new()
    #par( oma=.OMA, mar=.MAR, mfrow=c(nSeries,1) )
  
    for ( g in 1:nSeries )
    {
      # Y-axis limits.
      if ( is.null(yLim) )
        yLimit <- c( 0,max(idxSeries[g,],na.rm=TRUE ) )

      plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

      It <- idxSeries[g,]
      It[ It==-1 ] <- NA
      
      tmpCol <- .COLItg[ idxIndex[g] ]
      tmpLty <- .LTYItg[ idxIndex[g] ]
      tmpLwd <- .LWDItg[ idxIndex[g] ]
      tmpSym <- .SYMItg[ idxIndex[g] ]
      
      lines( xVal, It, col=tmpCol, lty=tmpLty, lwd=tmpLwd )
      points( xVal, It, bg="white", cex=1.4, pch=tmpSym )
      
      panLab( 0.025,0.90, adj=0, cex=1.4, idxNames[g] ) 
      
      axis( side=1, cex.axis=1.4 )
      axis( side=2, cex.axis=1.4, las=.YAXISLAS )
      axis( side=4, cex.axis=1.4, labels=FALSE )
      box()    
    }
    mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE1, cex=1.2, outer=TRUE, "Relative Index (kg/trap)" )
  }
  else
  {
    # Y-axis limits.
    yLimit <- yLim
    if ( is.null(yLim) )
      yLim <- c(0,max(obj$idxSeries,na.rm=TRUE ) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nSeries )
    {
      It <- idxSeries[g,]
      It[ It==-1 ] <- NA
       
      tmpCol <- .COLItg[ idxIndex[g] ]
      tmpLty <- .LTYItg[ idxIndex[g] ]
      tmpLwd <- .LWDItg[ idxIndex[g] ]
      tmpSym <- .SYMItg[ idxIndex[g] ]       
       
      lines( c(1:nT), It, col=tmpCol,lty=tmpLty,lwd=tmpLwd )
      points( c(1:nT), It, bg="white", cex=1.2, pch=tmpSym )
    }

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Stock Index" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=idxNames, bg="white",
                 pt.bg="white", col=.COLItg[idxIndex], lty=.LTYItg[idxIndex],
                 lwd=.LWDItg[idxIndex], pt.cex=1.2, pch=.SYMItg )
    }
  }
  return( invisible() )
}     # .plotIndices function


.plotRetCatch <- function( obj, gfx=list( annotate=TRUE, bygears=FALSE,
                   doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # landCatchMatrix  = Catch in year t for gear g (columns 3+ since 1 is "year"
  # and column 2 is tStep.

  Ctg    <- obj$landCatchMatrix
  nGear  <- ncol( Ctg ) - 2
  nT     <- nrow( Ctg )
  
  seriesNames <- paste( "Series",c(1:nGear) )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLimit <- range( c(0,Ctg[,c(3:ncol(Ctg))],na.rm=TRUE ) )

  if ( gfx$bygears )
  {
    dev.new()
    par( oma=.OMA, mar=.MAR, mfrow=c(nGear,1) )
    
    for ( g in 1:nGear )
    {
      if ( is.null(yLim) )
        yLimit <- c( 0,max(Ctg[,2+g], na.rm=TRUE) )
        
      plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

      Ct <- Ctg[ ,g+2 ]
      Ct[ Ct==-1 ] <- NA
      Ct[ Ct==0.0 ] <- NA
      
      lines( c(1:nT), Ct, col=.COLCtg[g], lty=.LTYCtg[g], lwd=.LWDCtg[g] )
      points( c(1:nT), Ct, bg="white", cex=1.2, pch=.SYMCtg[g] )
      
      panLab( 0.025,0.90, adj=0, cex=1.2, seriesNames[g] ) 
      
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()    
    }
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Retained Catch (t)" )
  }
  else
  {
    plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nGear )
    {
       Ct <- Ctg[ ,g+2 ]
       Ct[ Ct==-1 ] <- NA
       Ct[ Ct==0.0 ] <- NA
       lines( c(1:nT), Ct, col=.COLCtg[g], lty=.LTYCtg[g], lwd=.LWDCtg[g] )
       points( c(1:nT), Ct, bg="white", cex=1.2, pch=.SYMCtg[g] )
    }

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Retained Catch (t)" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.75,0.95, legTxt=seriesNames, bg="white", cex=0.8,
                 pt.bg="white", col=.COLCtg, lty=.LTYCtg, lwd=.LWDCtg-1,
                 pt.cex=1.2, pch=.SYMCtg )
    }
  }
  return( invisible() )
}     # .plotRetCatch function

#------------------------------------------------------------------------------#
#-- Age Proportions                                                          --#
#------------------------------------------------------------------------------#

.plotCatAgeBubbles <- function( obj, seriesNames=NULL, minAge=2,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # obj is an array with dimensions nT by (plusGrpAge-minAge+1) so tranpose.
  
  nT    <- nrow( obj )
  nAges <- ncol( obj )

  # Remove -1s.
  obj[ obj==-1 ] <- NA

  # years: These will become xval.
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  # ageclasses: These will become yval.
  yVal <- c( 1:nAges ) + (minAge-1)

  # These are years.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( xVal  )

  # These are ages.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    #yLim <- c( 1,nrow(resids)+1 )
    yLim <- range( yVal )
  
  obj   <- t( obj )

  #nGear <- dim( obj )[3]
  if ( gfx$bygears )
  {
    #for ( i in 1:nGear )
    {
      #dev.new()
      #browser()
      .plotBubbles( obj, xval=xVal, yval=yVal, xlim=xLim, ylim=yLim, lwd=2 )
      
      mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=1.2, outer=TRUE, "Age Class" )
      if ( !is.null(seriesNames) )
        #panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, seriesNames )
        mtext( side=3, line=1, cex=1.2, seriesNames )
      #else
       # panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, paste( "Series",i ) )
    }
  }
  else
  {
    par( mfrow=c( nGear,1 ) )
    # Object is a 3-dimensional array by age, time, gear for this replicate.
    for ( i in 1:nGear )
    {
      .plotBubbles( obj[,,i], xlim=xLim, ylim=yLim, lwd=2 )
       
      mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Age Class" )
     
      if ( !is.null(seriesNames) )
        panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, seriesNames[i] )
      else
        panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, paste( "Series",i ) )
    }
  }
  return( invisible() )
}     # .plotCatAgeBubbles function


.plotCatAgeFreq <- function( obj, minAge=3, seriesNames=NULL,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, showProj=FALSE,
            xLim=NULL, yLim=NULL ) )
{
  # obj is an array with dimensions nT by (plusGrpAge-minAge+1).
  
  # Remove -1s.
  obj[ obj==-1 ] <- NA

  # years: These will become xval.
  nT <- nrow(obj)
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  # ageclasses: These will become yval.
  nAges <- ncol(obj)
  yVal <- c( 1:nAges ) + (minAge-1)

  xLim <- gfx$xLim
  yLim <- gfx$yLim
    
  pLim <- c( 0, 0.25 )
  
  yearClasses <- c( 1:dim(obj)[2] )
  
  #nGear <- dim( obj )[3]
  if ( gfx$bygears )
  {
    if ( is.null(xLim) )
      xLim <- c( 1,dim(obj)[2]+minAge-1 )
    
    if ( is.null(yLim) )
     yLim <- c( 1,dim(obj)[1] )  
  
    #for ( i in 1:nGear )
    {
      years  <- c( yLim[1]:yLim[2] )
      idx <- apply( obj,1, FUN=function(x){ !all(is.na(x) ) } )
      nYears <- length( years[idx] )
      #dev.new()
      
      myMar <- c(0.75,0.75,0,0)
      myOma <- c(4,4,4,4)

      #browser()
      
      if ( nYears <= 12 )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/4),4 ) )
      if ( nYears > 12 )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/4),4 ) )
      if ( (nYears > 20 ) && (nYears <= 50) )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/5),5 ) )
      if ( (nYears > 50 ) && (nYears <= 80 ) )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/8), 8) )
      if ( (nYears > 80 ) )
        par( oma=myOma, mar=myMar, mfcol=c (ceiling(nYears/10),10) )
 
      for ( t in years )
      {
        if ( !all(is.na(obj[t,]) ) )
        {        
          plot( xLim, pLim, type="n", axes=FALSE, xlab="", ylab="" )
          lines( yearClasses+minAge-1, y=obj[t,], type="h", lwd=2 )
        
          panLab( 0.9, 0.9, cex=1, .INITYEAR+t-1 )
        
          mfg <- par( "mfg" )
        
          # Row one.
          if ( mfg[1]==1 && mfg[2]%%2==0 )
            axis( side=3, labels=FALSE )
        
          if ( mfg[1]==1 && mfg[2]%%2!=0 )
            axis( side=3 )
          
          # Column one.
          if ( mfg[2]==1 && mfg[1]%%2==0 )
            axis( side=2, las=2 )
        
          if ( mfg[2]==1 && mfg[1]%%2!=0 )
            axis( side=2, labels=FALSE )
          
          # Last row.
          if ( mfg[1]==mfg[3] && mfg[2]%%2==0 )
            axis( side=1 )
          
          if ( mfg[1]==mfg[3] && mfg[2]%%2!=0 )
            axis( side=1, labels=FALSE )
          
          # Last column.
          if ( mfg[2]==mfg[4] && mfg[1]%%2==0 )  
            axis( side=4, labels=FALSE )
          
          if ( mfg[2]==mfg[4] && mfg[1]%%2!=0 )
            axis( side=4, las=2 )

          box()
        }     # if not missing age props.
      }

      mtext( side=1, line=.OUTLINE4, cex=.CEXLAB, outer=TRUE, "Age Class" )
      mtext( side=2, line=2.5, cex=.CEXLAB, outer=TRUE,
             "Proportion-At-Age Class" )
      if ( !is.null(seriesNames) )
        mtext( side=3, line=2, cex=.CEXLAB, outer=TRUE, seriesNames )
      #else
      #  panLab( 0.5, 0.95, cex=.CEXLAB, paste( "Series",i ) )
    }
  }
  else
  {
    if ( is.null(xLim) )
      xLim <- c( 1,dim(obj)[1] )
      
    if ( is.null(yLim) )
      yLim <- c( 1,dim(obj)[2] )
      
    #par( oma=c(3,3,2,2), mar=c(2,2,1,1), mfrow=c( 1, nGear ) )
    #  NOT Object is a 3-dimensional array by age, time, gear for this replicate.
   
   # E.g., if year range is 1:40, then years=c(1,2,...,40).
   # Then make the years negative to plot from -1 to -40.
    years  <- c( yLim[1]:yLim[2] ) * -1
    yLim <- range( c(0, years,min(years)-1 ) )
    
    #for ( i in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="",
            yaxs="i" )
      axis( side=1 )
      axis( side=2, at=pretty(years), labels=-1*pretty(years), cex.axis=1.2, las=2 )
      box()
      
      for ( t in years )
      {
        segments( x=yearClasses, y0=t, x1=yearClasses, y1=t+obj[,-t]*2.5, lwd=3 )     
      }   # Over years.
      
      if ( !is.null(seriesNames) )
        panLab( 0.5, 0.95, cex=.CEXLAB, seriesNames )
      #else                
      #  panLab( 0.5, 0.95, cex=.CEXLAB, paste( "Series",i ) )
      
      mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE,
             "Age Class" )
      mtext( side=2, line=2, cex=.CEXLAB, outer=TRUE,
             "Proportion-at-Age Class" )
     }   # Over gears.
  }
  return( invisible() )
}     # .plotCatAgeFreq function


.plotMLEageProps <- function( obj, seriesNames=NULL,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # uCatg is a list of arrays with dimensions( nT,nAges ).
  
  # SPC 10-Nov-2015: uCgta is an array with
  # dim = (nAgeSeries,nT,maxAge)
  nGear <- length( obj )
  
  # These are the years.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 1,nrow(obj[[1]]) )
    
  # These are the age classes.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 1,ncol(obj[[1]]) )
  
  if ( gfx$bygears )
  {
    for ( g in 1:nGear )
    {
      dev.new()
      .plotBubbles( t( obj[[g]] ), xlim=xLim, ylim=yLim )
      
      mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Age Class" )
      
      if ( gfx$annotate & !is.null(seriesNames) )
        panLab( 0.05,0.95, cex=.CEXLAB, seriesNames[g] )
    }
  }
  else
  {
    for ( g in 1:nGear )
    {
      plotBubbles( t( obj[[g]] ), xlim=xLim, ylim=yLim )
       
      mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Age Class" )
     
      if ( gfx$annotate & !is.null(seriesNames) )
        panLab( 0.05,0.95, cex=.CEXLAB, seriesNames[g] )
    }
  }
  return( invisible() )
}     # .plotMLEageProps function


.plotMLEageFreq <- function( obsAges, predAges, minAge=2, seriesNames=NULL,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL,
            average = FALSE ) )
{
  # uCatg is a list of arrays with dimensions( nT,nAges ).
  #uCatg <- obj$uCatg
  #nGear <- length( uCatg )

  # SPC 10-Nov-2015: uCgta is an array with
  # dim = (nAgeSeries,nT,maxAge)
  obsAges[ obsAges < 0 ] <- NA
  predAges[ predAges < 0 ] <- NA

  if( gfx$average )
  {

    obsAges   <- apply( X = obsAges, FUN = sum, MARGIN = 2, na.rm = T )
    obsAges   <- matrix(obsAges, nrow = 1, ncol = length(obsAges))/sum(obsAges)
    predAges  <- apply( X = predAges, FUN = sum, MARGIN = 2, na.rm = T )
    predAges  <- matrix(predAges, nrow = 1, ncol = length(predAges))/sum(predAges)
  }
  
  #dimPS <- dim( paaSeries )
  #obsCatg <- as.list( 1:dimPS[3] )
  #names( obsCatg ) <- paste( "obsCatg",obj$ageIndex,sep="" )
  
  #for ( g in 1:length(obsCatg) )
  #  obsCatg[[g]] <- t( paaSeries[,,g] )
 
  nT    <- nrow( predAges )
  nAges <- ncol( predAges )
  
  ageClasses <- c( 1:nAges ) + (minAge-1)
  
  # These are the ages.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( ageClasses )
    
  # These are the fitted age proportions.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 0,1 )

  #for ( g in 1:nGear )
  {
    yLim <- c( 0,max( c( obsAges, predAges), na.rm=TRUE ) )


    sumProp <- apply( predAges,1,sum, na.rm=TRUE )
    
    # How many years have ages?
    nPanels <- sum( sumProp!=0 )
    
    myMar <- c( 1.5,1.5,0.5,0.5 )

    # Set up par for multipanels if not averaging
    if(!gfx$average)
    {
      if ( nPanels <= 9 )
        par( oma=.OMA, mar=myMar, mfcol=c(3,3) )
      else if ( nPanels > 9 & nPanels <= 12 )
        par( oma=.OMA, mar=myMar, mfcol=c(4,3) )
      else if ( nPanels > 12 & nPanels <= 16 )
        par( oma=.OMA, mar=myMar, mfcol=c(4,4) )
      else if ( nPanels > 16 & nPanels <= 20 )
        par( oma=.OMA, mar=myMar, mfcol=c(5,4) )
      else if ( nPanels > 20 & nPanels <=24 )
        par( oma=.OMA, mar=myMar, mfcol=c(6,4) )
      else
        par( oma=.OMA, mar=myMar, mfcol=c(6,5) )
    }
    
    # Years where the sum is not 0, i.e., there are fitted ages.
    idxPlot <- c(1:nT)[sumProp!=0.0]
    
    delta <- 0.3
    
    for ( i in idxPlot )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
      abline( h=0 )

      # The observed age proportions.
      #lines( ageClasses, obsCatg[[g]][i,], type="h", col="gray", lwd=3 )
      rect( ageClasses-delta, 0, ageClasses+delta, obsAges[i,], col="gray88" )

      # The fitted age proportions.      
      lines( ageClasses, predAges[i,], lty=1, lwd=2 )
      points( ageClasses, predAges[i,], bg="white", col="black", cex=1.6,
              lwd=2, pch=21 )
    
      if(!gfx$average)
      {
        if ( .USEYEAR )
        { 
          xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
          panLab( 0.5, 0.9, cex=1.2, paste( i+.INITYEAR-1) )
        }
        else      
          panLab( 0.5, 0.9, cex=1.2, paste( "Year",i ) )
      }
      
      mfg <- par( "mfg" )
      
      axis( side=1, cex.axis=.CEXAXIS, pos=0 )
      
      if ( mfg[2]==1 )
        axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
      else
        axis (side=2, labels=FALSE )
        
    }     # i=1,..,nT

    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE,
           "Age Class" )
    mtext( side=2, line=2, cex=.CEXLAB, outer=TRUE,
           "Proportion-at-Age Class" )
    
    if(gfx$average)
      titleOuterMar <- FALSE
    else titleOuterMar <- TRUE

    if ( gfx$annotate & !is.null(seriesNames) )
      mtext( side=3, line=-0.5, cex=0.9, outer=titleOuterMar, seriesNames )    
  }
  return( invisible() )
}     # .plotMLEageFreqs function

.plotMLEageResids <- function( obsAges, predAges, minAge=3, seriesNames=NULL,
    gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  obsAges[ obsAges < 0 ] <- NA
  predAges[ predAges < 0 ] <- NA

  nT    <- nrow( predAges )
  nAges <- ncol( predAges )
  
  # years: These will become xval.
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  # ageclasses: These will become yval.
  yVal <- c( 1:nAges ) + (minAge-1)

  resids <- obsAges - predAges

  resids <- t( resids )

  # These are years.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( xVal  )

  # These are ages.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    #yLim <- c( 1,nrow(resids)+1 )
    yLim <- range( yVal )
  
  .plotBubbles( resids, xval=xVal, yval=yVal, xlim=xLim, ylim=yLim, lwd=2 )
    
  mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE2, cex=1.2, outer=TRUE, "Age Class" )
     
  if ( !is.null(seriesNames) )
    #panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, seriesNames )
    mtext( side=3, line=0, cex=.CEXLAB, outer=TRUE, seriesNames )

  return( invisible() )
}     # FUNCTION .plotMLEageResids

.plotMLEageResids_Ave <- function( obsAges, predAges, minAge=2, seriesNames=NULL,
    gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  obsAges[ obsAges < 0 ] <- NA
  predAges[ predAges < 0 ] <- NA

  nT    <- nrow( predAges )
  nAges <- ncol( predAges )
  
  # ages: These will become xval.
  xVal <- minAge:(nAges+minAge-1)

  resids    <- predAges - obsAges
  aveResids <- apply(X = resids, FUN = mean, MARGIN = 2, na.rm = T)
  sdResids  <- apply(X = resids, FUN = sd, MARGIN = 2, na.rm = T)


  # These are years.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( xVal  )

  # These are ages.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    #yLim <- c( 1,nrow(resids)+1 )
    yLim <- range( aveResids + sdResids, aveResids - sdResids )
  
  plot( xLim, yLim, xlab = "", ylab = "", type = "n", las =1 )
    abline( h = 0, lty = 3, lwd = .8 )
    segments( x0 = xVal, x1 = xVal, 
              y0 = aveResids - sdResids, 
              y1 = aveResids + sdResids,
              lwd = 2 )
    
    points( x = xVal, y = aveResids, pch = 16, col = "grey60" )
    lines(loess.smooth( x = xVal, y = aveResids, span = 2/3, degree = 2), col = "salmon", lwd = 2 )
    
  mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE2, cex=1.2, outer=TRUE, "Age Proportion Residual" )
     
  if ( !is.null(seriesNames) )
    #panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, seriesNames )
    mtext( side=3, line=0, cex=.CEXLAB, outer=FALSE, seriesNames )

  return( invisible() )
}     # FUNCTION .plotMLEageResids



#------------------------------------------------------------------------------#
#-- MLE Plotting Functions                                                   --#
#------------------------------------------------------------------------------#

.plotMLEbiomass <- function( obj, label=NULL,
                gfx=list( annotate=TRUE, bygears=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  expBit     <- rbind( obj$expBit1, obj$expBit2, obj$expBit3 )
  legalBt    <- obj$legalBt
  SSBt       <- obj$SSBt
  sublegalBt <- obj$sublegalBt
  nT   <- length( SSBt )

  ColGear <- brewer.pal(n = 3, "Dark2")

  # years: These will become xval.
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- range( xVal )

  # Y-axis limits.
  if ( is.null( yLim ) )
    yLim <- range( c( 0,sublegalBt,legalBt,SSBt, as.vector(expBit)  ) )

  if ( gfx$bygears )
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( xVal, SSBt, col=.COLBt, lty=.LTYBt, lwd=.LWDBt )
    lines( xVal, legalBt, col=.COLlegalB, lty=.LTYlegalB, lwd=.LWDlegalB )  
    lines( xVal, sublegalBt, col=.COLslegalB, lty=.LTYslegalB, lwd=.LWDslegalB )

  
    for ( g in 1:nrow(expBit) )
      lines( xVal, expBit[ g, ], col=ColGear[g], lty=.LTYGEAR[g], lwd=2 )
    
    #abline( h=obj$pars$ssbFmsy, lty=.REFLTYFMSY )

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    box()

    if ( gfx$annotate & !is.null(label) )
      panLab( 0.05, 0.1, adj=0, cex=1.0, label )

    mtext( side=1, line=2, cex=1.2, "Year" )
    mtext( side=2, line=2.5, cex=1.2, "Biomass (000s t)" )

    if ( gfx$doLegend )
    {
      legNames <- c( c("Spawning biomass","Legal Biomass","Sublegal Biomass"),
                    c("Trap","Hook","Trawl") )

      panLegend( 0.65,0.95,
                 legTxt=legNames,
                 col=c(.COLBt,.COLlegalB,.COLslegalB,ColGear),
                 lty=c(.LTYBt,.LTYlegalB,.LTYslegalB,.LTYGEAR),
                 lwd=c(.LWDBt,.LWDlegalB,.LWDslegalB,.LWDGEAR),
                 bg="white" )
    }
  }     # if bygears
  else
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    lines( xVal, SSBt, col=.COLBt, lty=.LTYBt, lwd=.LWDBt )
    lines( xVal, legalBt, col="black", lty=.LTYlegalB, lwd=.LWDlegalB )  
    lines( xVal, sublegalBt, col="black", lty=.LTYslegalB, lwd=.LWDslegalB )

    #abline( h=obj$refPoints$ssbFmsy, lty=3, lwd=1 )
    box()    
    
    if ( .USEYEAR )
    { 
      xPos <- seq( xVal[1],xVal[length(xVal)], 5 )
      actPos <- xPos - .INITYEAR + 1
      xLabs <- paste( xPos )
    
      axis( side=1, cex.axis=.CEXAXIS2, at=actPos, labels=xLabs )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      axis( side=3, cex.axis=.CEXAXIS2, at=actPos, labels=FALSE )
      axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )    
    }
    else
    {
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )
      axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
    }
    mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE1, cex=1.2, outer=TRUE, "Biomass (000s t)" )
  }
  
  if ( gfx$annotate )
    panLab( 0.8,0.9, cex=.CEXLAB, label )
  
}    # .plotMLEbiomass


.plotMLEdiscards <- function( obj, seriesNames=c("Trap","Hook","Trawl","Std","StRS"), 
                              scalar=1000.0, label=NULL, fit = TRUE,
                              gfx=list( annotate=TRUE, bygears=TRUE, doLegend=TRUE,
                                        xLim=NULL, yLim=NULL ) )
{
  #legalDtg    <- obj$legalDtg
  #sublegalDtg <- obj$sublegalDtg
  #totalDtg    <- obj$totalDtg
  #totalDtgObs <- obj$totalDtgObs
  
  #nSeries     <- length( legalDtg )
  nT          <- obj$nT
  obsRel      <- rbind( obj$obs_relCtg1, obj$obs_relCtg2, obj$obs_relCtg3 )
  predRel     <- rbind( obj$relCtg1, obj$relCtg2, obj$relCtg3 )

  # Now populate
  # trap
  selBlockStart <- vector(mode = "list",length = nrow(predRel))
  selBlockEnd <- vector(mode = "list",length = nrow(predRel))
  selBlockStart[[1]] <- obj$selBlockStart_g1
  selBlockEnd[[1]]   <- obj$selBlockEnd_g1
  # hook
  selBlockStart[[2]] <- obj$selBlockStart_g2
  selBlockEnd[[2]]   <- obj$selBlockEnd_g2
  # trawl
  selBlockStart[[3]] <- obj$selBlockStart_g3
  selBlockEnd[[3]]   <- obj$selBlockEnd_g3

  obsRel[ obsRel < 0.0 ] <- NA
  predRel[ predRel < 0.0 ] <- NA
 
  #seriesNames <- paste( "Series", c(1:nSeries) )
  #seriesNames <- obj$gNames

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  if ( gfx$bygears )
  {
    # ARK (09-Dec-10) HACK to omit surveys from plots.  Should be 1:nSeries.
    for ( g in 1:nrow(predRel) )
    {
      #legalDt    <- legalDtg[[g]]    * 1000.0
      #sublegalDt <- sublegalDtg[[g]] * 1000.0
      #totalDt    <- totalDtg[[g]]    * 1000.0
      #totalDtObs <- totalDtgObs[[g]] * 1000.0
      #totalDtObs[totalDtObs < 0.0 ] <- NA
          
      # Y-axis limits.
      if ( is.null(yLim) )
        yLimit <- c( 0,max(c(obsRel[g,],predRel[g,]),na.rm=TRUE) )
      else
        yLimit <- yLim * scalar
        
      plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )
       
      tmpCol <- .COLItg[ g ]
      tmpCol <- "black"
      tmpLty <- .LTYItg[ g ]
      tmpLwd <- .LWDItg[ g ]
      tmpSym <- .SYMItg[ g ]

      nSelBlocks <- length(selBlockStart[[g]])
      if( selBlockStart[[g]][1] > 1)
      {
        selBlockEnd[[g]]    <- c( nT, selBlockEnd[[g]] )
        selBlockStart[[g]]  <- c( 1, selBlockStart[[g]] )
        nSelBlocks <- nSelBlocks + 1
      }

      selBlockCols <- "white"
      if(nSelBlocks > 1)
      selBlockCols <- c( selBlockCols, brewer.pal(n = nSelBlocks-1, "Dark2" ))

      selBlockCols <- alpha(selBlockCols, alpha = 0.3 )
      
      rect( xleft = selBlockStart[[g]] - .5,
            xright = selBlockEnd[[g]] + .5,
            ybottom = 0, ytop = yLimit[2],
            col = selBlockCols, border = NA )

      if( fit )
      {
        lines( c(1:nT), predRel[g,], col=tmpCol, lty=tmpLty, lwd=tmpLwd )
        points( c(1:nT), obsRel[g,], bg="white", cex=1.4, pch=tmpSym )
      }

      if( !fit )
        lines( x = 1:nT, y = obsRel[g,], col=tmpCol, lty=tmpLty, lwd=tmpLwd  )

      #lines( c(1:nT), legalDt, col=tmpCol, lty=tmpLty, lwd=tmpLwd )
      #points( c(1:nT), legalDt, bg="white", cex=1.2, pch=tmpSym )
      
      #lines( c(1:nT), totalDt, col=tmpCol, lty=tmpLty, lwd=tmpLwd )
      
      #points( c(1:nT), totalDtObs, bg="white", cex=1.6, pch=tmpSym )
      
      #lines( c(1:nT), legalDt, col="black", lty=1, lwd=1 )
      
      #lines( c(1:nT), sublegalDt, col="black", lty=2, lwd=1 )
      #points( c(1:nT), sublegalDt, bg="black", cex=1, pch=tmpSym )      
      
      panLab( 0.025,0.90, adj=0, cex=1.4, seriesNames[g] ) 
    
      if ( .USEYEAR )
      { 
        xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
        actPos <- xPos - .INITYEAR + 1
        xLabs <- paste( xPos )
    
        axis( side=1, cex.axis=.CEXAXIS2, at=actPos, labels=xLabs )
        axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
        axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
      }
      else
      {
        axis( side=1, cex.axis=.CEXAXIS2 )
        axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
        axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
      }      
      box()    
    }
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Releases (000s t)" )
  }
  else
  {
    # NOT by gears.
    
    # Y-axis limits.
    yLimit <- yLim
    if ( is.null(yLim) )
      yLimit <- c(0,max(c(unlist(totalDtg),unlist(totalDtgObs)),na.rm=TRUE )*1000.0 )
    else
      yLimit <- yLim
    
    plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nSeries )
    {
      legalDt    <- legalDtg[[g]]    * 1000.0
      sublegalDt <- sublegalDtg[[g]] * 1000.0
      totalDt    <- totalDtg[[g]]    * 1000.0
      totalDtObs <- totalDtgObs[[g]] * 1000.0
      totalDtObs[totalDtObs < 0.0 ] <- NA      
       
      tmpCol <- .COLItg[ idxIndex[g] ]
      tmpLty <- .LTYItg[ idxIndex[g] ]
      tmpLwd <- .LWDItg[ idxIndex[g] ]
      tmpSym <- .SYMItg[ idxIndex[g] ]       
      
      lines( c(1:nT), totalDt, col=tmpCol, lty=tmpLty, lwd=tmpLwd )
      
      points( c(1:nT), totalDtObs, bg=tmpCol, cex=1.2, pch=tmpSym )      
       
      #lines( c(1:nT), legalDt, col=tmpCol,lty=tmpLty,lwd=tmpLwd )
      #points( c(1:nT), legalDt, bg="white", cex=1.2, pch=tmpSym )
    }

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Total Releases (t)" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=seriesNames,
                 pt.bg=.COLItg, col="black", lty=.LTYItg,
                 lwd=1, pt.cex=1.2, pch=.SYMItg, bg="white" )
    }
  }
  return( invisible() )
}     # .plotMLEdiscards function


.plotMLEfMort <- function( obj, label=NULL, gfx=list( annotate=TRUE, doLegend=TRUE,
                    bygears=FALSE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # Ftg  = fishing mortality in year t for gear g.

  # Pull all Ftg
  Ftg <- obj$Ftg

  nGear  <- length( Ftg )
  nT     <- length( Ftg[[1]] )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) ) 
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c( 0,unlist(Ftg) ) )

  if ( gfx$bygears )
  {
    # HACK: ARK 25-Nov-09
    if ( nGear==2 )
      mfRow <- c(2,1)
    else if ( nGear==3 )
      mfRow <- c(3,1)
    else if ( nGear==4 )
      mfRow <- c(2,2)
    else if ( nGear<=6 )
      mfRow <- c(3,2)
  
    par( oma=.OMA, mar=.MAR, mfrow=mfRow )
      
    for ( g in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      Ft <- Ftg[[g]]
      lines( c(1:nT), Ft, col=.COLFtg[g], lty=.LTYFtg[g], lwd=.LWDFtg[g] )
      points( c(1:nT), Ft, bg="white", col=.COLFtg[g], pch=21 )

      if ( gfx$annotate )
      {
        abline( h=obj$pars$Fmsy, lty=.REFLTYFMSY )
        abline( h=obj$pars$Fcra, lty=.REFLTYFCRA )
      }
      abline( v=obj$pars$tMP,  lty=.VIEWTMPLTY )

      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
      
      panLab( 0.8, 0.9, adj=0, cex=1.0, obj$gNames[g] )

      #if ( gfx$doLegend )
      #  panLegend( 0.05,0.95, legTxt=c("Fmsy","Fcrash"),
      #             bg="white", lty=c(.REFLTYFMSY,.REFLTYFCRA) )                
      
      mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Fishing Mortality" )
    }

  }
  else
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    for ( g in 1:nGear )
    {
      Ft <- Ftg[[g]]
      lines( c(1:nT), Ft, col=.COLFtg[g], lty=.LTYFtg[g], lwd=.LWDFtg[g] )
      points( c(1:nT), Ft, bg="white", col=.COLFtg[g], pch=21 )
    }
    
    if ( gfx$annotate )
    {
      abline( h=obj$pars$Fmsy, lty=.REFLTYFMSY )
      abline( h=obj$pars$Fcra, lty=.REFLTYFCRA )
    }
    abline( v=obj$pars$tMP,  lty=.VIEWTMPLTY )

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.7,0.95, legTxt=obj$gNames, col=.COLFtg, lty=.LTYFtg, lwd=.LWDFtg )
    }
    
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Fishing Mortality" )
  }
}     # .plotMLEfmort function

.plotMLEindices <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                     doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # idxSeries (nSeries by nT) - the data.
  # Itg: Itg1,.... Itg2 - scaled indices from fit

  gNames <- c( "Trap","Std.","StRS" )
  nSeries  <- nrow( obj$idxSeries )
  
  expBit <- rbind( obj$expBit1, obj$expBit4, obj$expBit5 )
  Itg <- rbind( obj$Itg1, obj$Itg4, obj$Itg5 )

  q_it <- rbind( obj$q_1t, obj$q4t, obj$q_5t)

  # ARK (29-Oct-15) The above makes idxIndex irrelevant for indexing series (but is relevant
  # for naming series) as there are no longer 5 gears in a matrix or list.
  
  idxIndex <- obj$idxIndex
  idxNames <- paste( "Series",idxIndex,sep="" )
  nT       <- obj$nT

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    # X-axis limits drawn from global default settings.
    xLim <- c( 1,nT )

  if ( gfx$bygears )
  {
    #par( mfrow=c(nSeries,1) )
    
    for ( g in 1:nSeries )
    {
      #ItObs <- obj$idxSeries[g,]
      It <- Itg[g,]
    
      # Y-axis limits.
      yLimit <- yLim
      if ( is.null(yLim) )
        #yLimit <- c( 0,max(expBit[ idxIndex[g], ],na.rm=TRUE ) )
        yLimit <- c( 0, max(expBit[ g, ], na.rm=TRUE ) )
      
      plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

      #ItObs[ ItObs==-1 ] <- NA
      It[ It < 0 ]     <- NA
      # lines( c(1:nT), It, col=.COLItg[g], lty=.LTYItg[g], lwd=.LWDItg[g] )
      #points( c(1:nT), It, bg="white", cex=1.2, pch=.SYMItg[g] )
      
      #lines( c(1:nT), expBit[[idxIndex[g]]], col=.COLItg[idxIndex[g]],
      #  lty=.LTYItg[idxIndex[g]], lwd=.LWDItg[idxIndex[g]] )
        
      #lines( c(1:nT), expBit[ idxIndex[g], ], col="black",
      #  lty=.LTYItg[ idxIndex[g] ], lwd=.LWDItg[ idxIndex[g] ] )       

      lines( c(1:nT), expBit[ g, ], col="black",
        lty=.LTYItg[ idxIndex[g] ], lwd=.LWDItg[ idxIndex[g] ] )       
             
      # Show the fitted index.
      #lines( c(1:nT), Itg, col=.COLItg[g], lwd=.LWDItg[2] )
      points( c(1:nT), It, bg="white", cex=1.8, pch=.SYMItg[idxIndex[g]] )      
      
      # Show the data.
      #points( c(1:nT), ItObs, bg=.COLItg[g], cex=1.2, pch=.SYMItg[g] )
      #panLab( 0.025,0.90, adj=0, cex=1.2, paste( "idxSeries",obj$idxIndex[g], sep=" " ) )
      panLab( 0.025,0.8, adj=0, cex=1.4, gNames[ g ] )      

      if ( .USEYEAR )
      { 
        xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
        actPos <- xPos - .INITYEAR + 1
        xLabs <- paste( xPos )
    
        axis( side=1, cex.axis=1.4, at=actPos, labels=xLabs )
        axis( side=2, cex.axis=1.4, las=.YAXISLAS )
        axis( side=3, cex.axis=.CEXAXIS2, at=actPos, labels=FALSE )
        axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )    
      }
      else
      {
        axis( side=1, cex.axis=.CEXAXIS2 )
        axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
        axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )
        axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
      }
      
      #axis( side=1, cex.axis=.CEXAXIS2 )
      #axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      #axis( side=3, labels=FALSE )
      box()
      
      if ( g==1 )
        mtext( side=3, line=1, cex=.CEXLAB, label )          
    }
    mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=1.2, outer=TRUE, "(Scaled) Observed and Fitted Indices (000s t)" )
  }
  else
  {
    # Y-axis limits.
    if ( is.null(yLim) )
      yLim <- c(0,max(Itg,na.rm=TRUE ) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nSeries )
    {
      #ItObs <- obj$idxSeries[g,]
      #ItObs[ ItObs==-1 ] <- NA
       
      It <- Itg[g]
      It[ Itg < 0.0 ] <- NA
       
      #lines( c(1:nT), Itg, col=.COLItg[g], lty=.LTYItg[g], lwd=.LWDItg[g] )
      #points( c(1:nT), ItObs, bg=.COLItg[g], cex=1.2, pch=.SYMItg[g] )
      points( c(1:nT), It, bg=.COLItg[idxIndex[g]], cex=1.8,
              pch=.SYMItg[idxIndex[g]] )
      
      # Remember there is an exploitable biomass for each fishery, but only
      # specific indices may be fitted in the model as per idxIndex. 
      lines( c(1:nT), expBit[ g, ], col=.COLItg[idxIndex[g]],
             lty=.LTYItg[idxIndex[g]], lwd=.LWDItg[idxIndex[g]] )              
    }

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    box()
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Stock Index" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=idxNames, bg="white",
                 col=.COLItg[idxIndex], lty=.LTYItg[idxIndex],
                 lwd=.LWDItg[idxIndex]-1, pt.cex=1.2,
                 pch=.SYMItg[idxIndex] )
    }
  }
  return( invisible() )
}     # .plotMLEindices function

.plotMLEharvestRate <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                         doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # legalHR, and sublegalHR.

  legalHR    <- obj$legalHR
  sublegalHR <- obj$subLegalHR
  nT         <- length( legalHR )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c(0,max(c(legalHR,sublegalHR),na.rm=TRUE ) )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
  lines( c(1:nT), legalHR, col=.COLLEGALHR,  lty=.LTYLEGALHR,  lwd=.LWDLEGALHR )
  points( c(1:nT), legalHR, cex=1.5, bg=.COLLEGALHR, pch=.SYMLEGALHR )
  lines( c(1:nT), sublegalHR, col=.COLSUBLEGHR, lty=.LTYSUBLEGHR, lwd=.LWDSUBLEGHR )
  
  # ARK (09-Dec-10) Changed to legalHRFmsy.
  #Umsy <- obj$refPoints$Umsy
  #Umsy <- obj$refPoints$legalHRFmsy
  
  #( h=Umsy, col="black", lty=3, lwd=2 ) 
  
  #abline( v=c(1977-1965+1,2000-1965+1),col="red" )
  
  if ( gfx$annotate )
    panLab( 0.1,0.9, cex=.CEXLAB, label )
  
  if ( .USEYEAR )
  {
    xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
    actPos <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )
    
    axis( side=1, cex.axis=.CEXAXIS, at=actPos, labels=xLabs )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, at=actPos, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )    
  }
  else
  {
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  }
  box()
  
  mtext( side=1, line=2.5, cex=1.2, "Year" )
  mtext( side=2, line=3, cex=1.2, "Harvest Rate" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.65,0.15, legTxt=c("Legal Harvest Rate","Sublegal Harvest Rate"),
      col=c(.COLLEGALHR,.COLSUBLEGHR), lty=c(.LTYLEGALHR,.LTYSUBLEGHR),
      lwd=c(.LWDLEGALHR,.LWDSUBLEGHR), bg="white" )
  }
  return( invisible() )
}     # .plotMLEharvestRate


.plotMLErecAge1 <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                             doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Recruitment deviations.

  Rt <- obj$Rt[-1]
  nT <- length( Rt )
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( Rt,na.rm=TRUE )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( c(1:nT), Rt, col="black", lty=1, lwd=1 )
  points( c(1:nT), Rt, bg="black", cex=1.2, pch=21 )
  abline( h=mean(Rt[1:(length(Rt)-3)]), lty=3 )

  if ( gfx$annotate )
  {
    panLab( 0.1,0.9, cex=.CEXLAB, label )
  }

  if ( .USEYEAR )
  { 
    xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
    actPos <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )
    
    axis( side=1, cex.axis=.CEXAXIS, at=actPos, labels=xLabs )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, at=actPos, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )    
  }
  else
  {
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  }
  
  box()
  
  mtext( side=1, line=2.5, cex=1.2, "Year" )
  mtext( side=2, line=2.5, cex=1.2, "Recruitment at Age-1 (millions)" )
  
  if ( gfx$doLegend )
  {
    abline( v=c(1977-1965+1,2000-1965+1,2008-1965+1),col="red",
            lty=c(2,3,4), lwd=c(2,2,2) )  
    panLegend( 0.85,0.95, legTxt=c("1977","2000","2008"),
      col=c("red"), lty=c(2,3,4), lwd=c(2,2,2), bg="white" )
  }
  return( invisible() )
}     # .plotMLErecAge1 function

.plotMLErecDevs <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Recruitment deviations.

  recDevs <- obj$recDevs[-1]
  nT      <- length( recDevs ) + 4
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( recDevs,na.rm=TRUE )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( c(2:(nT-3)), recDevs, col="black", lty=1, lwd=1 )
  points( c(2:(nT-3)), recDevs, bg="black", cex=1.2, pch=21 )
  
  if ( .USEYEAR )
  { 
    xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
    actPos <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )
    
    axis( side=1, cex.axis=.CEXAXIS, at=actPos, labels=xLabs )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, at=actPos, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )    
  }
  else
  {
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, cex.axis=.CEXAXIS, labels=FALSE )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  }
  
  box()
  
  if ( gfx$doLegend )
  {
    abline( v=c(1977-1965+1,2000-1965+1,2008-1965+1),
            col="red", lty=c(2,3,4), lwd=c(2,2,2) )  
    panLegend( 0.85,0.95, legTxt=c( "1977","2000","2008" ),
      col=c("red","red"), lty=c(2,3,4), lwd=c(2,2,2), bg="white" )
  }  
  mtext( side=1, line=2.5, cex=1.2, "Year" )
  mtext( side=2, line=2.5, cex=1.2, "Recruitment Deviation" )
  
  if ( gfx$doLegend )
  {
  #  panLegend( 0.15,0.95, legTxt=c("Legal Harvest Rate","Sublegal Harvest Rate"),
  #    col=c(.COLLEGALHR,.COLSUBLEGHR), lty=c(.LTYLEGALHR,.LTYSUBLEGHR),
  #    lwd=c(.LWDLEGALHR,.LWDSUBLEGHR), bg="white" )
  }
  return( invisible() )
}     # .plotMLErecDevs

.plotMLEqDevs <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                           doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Catchability deviations.

  qDevs <- obj$qDevs
  nT      <- length( qDevs )
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( qDevs,na.rm=TRUE )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( c(1:nT), qDevs, col="black", lty=1, lwd=1 )
  points( c(1:nT), qDevs, bg="black", cex=1.2, pch=21 )

  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Catchability Deviations" )
  
  if ( gfx$doLegend )
  {
  #  panLegend( 0.15,0.95, legTxt=c("Legal Harvest Rate","Sublegal Harvest Rate"),
  #    col=c(.COLLEGALHR,.COLSUBLEGHR), lty=c(.LTYLEGALHR,.LTYSUBLEGHR),
  #    lwd=c(.LWDLEGALHR,.LWDSUBLEGHR), bg="white" )
  }
  return( invisible() )
}     # .plotMLEqDevs


.plotLikelihoods <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                              doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Likelihood values.

  indexLikelihood <- obj$indexLikelihood
  idxIndex        <- obj$idxIndex
  
  ageLikelihood   <- obj$ageLikelihood
  ageIndex        <- obj$ageIndex
  
  releaseLikelihood <- obj$releaseLikelihood
  
  objFun          <- obj$objFun
  maxGrad         <- obj$maxGrad
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # Plot the index likelihoods.
    
  xPos <- c(1:length(indexLikelihood))
  plot( range(xPos), range(indexLikelihood),
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, indexLikelihood, type="h", col="black", lty=1, lwd=1 )
  points( xPos, indexLikelihood, bg="white", cex=1.2, pch=21 )
  points( idxIndex, indexLikelihood[idxIndex], bg="black", cex=1.2, pch=21 )
  
  panLab( 0.05,0.95, adj=0, cex=.CEXLAB, label )
  
  axis( side=1, at=xPos, labels=idxIndex, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Index Series" )

  # Plot the release likelihoods.
    
  xPos <- c(1:length(releaseLikelihood))
  plot( range(xPos), range(releaseLikelihood),
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, releaseLikelihood, type="h", col="black", lty=1, lwd=1 )
  points( xPos, releaseLikelihood, bg="white", cex=1.2, pch=21 )
  
  axis( side=1, at=xPos, labels=xPos, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Release Series" )
    
  # Plot the age likelihoods.
    
  xPos <- c( 1:length(ageLikelihood) )
  plot( range(xPos), range(ageLikelihood),
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, ageLikelihood, type="h", col="black", lty=1, lwd=1 )
  points( xPos, ageLikelihood, bg="black", cex=1.2, pch=21 )
  
  axis( side=1, at=xPos, labels=ageIndex, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Age Series" )
  
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "log(Likelihood)" )  
  
  if ( gfx$doLegend )
  {
  #  panLegend( 0.15,0.95, legTxt=c("Legal Harvest Rate","Sublegal Harvest Rate"),
  #    col=c(.COLLEGALHR,.COLSUBLEGHR), lty=c(.LTYLEGALHR,.LTYSUBLEGHR),
  #    lwd=c(.LWDLEGALHR,.LWDSUBLEGHR), bg="white" )
  }
  return( invisible() )
}     # .plotLikelihoods function


.plotTau <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                      doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Tau estimates.

  # ARK (13-Oct-10) This was tauSquareIndex but SPC changed it to tauIndex.
  tauIndex <- obj$tauIndex
  idxIndex <- obj$idxIndex
  
  # ARK (13-Oct-10) This was tauSquareAges but SPC changed it to tauAges.
  tauAges   <- obj$tauAges
  ageIndex  <- obj$ageIndex
  
  # ARK (06-Nov-10) SPC adds release fitting, assume just nGear.
  tauReleases <- obj$tauReleases
  
  xLim <- gfx$xLim
  yLimit <- gfx$yLim
  if ( is.null(yLimit) )
    yLimit <- c( 0,max(tauIndex) )

  # Plot the index tau estimates.
    
  xPos <- c(1:length(tauIndex))
  plot( range(xPos), yLimit,
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, tauIndex, type="h", col="black", lty=1, lwd=1 )
  points( xPos, tauIndex, bg="white", cex=1.2, pch=21 )
  points( idxIndex, tauIndex[idxIndex], bg="black", cex=1.2, pch=21 )
 
  panLab( 0.05,0.95, adj=0, cex=.CEXLAB, label )
  
  axis( side=1, at=xPos, labels=idxIndex, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
   
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Index Series" )
    
  xLim <- gfx$xLim
  yLimit <- gfx$yLim
  if ( is.null(yLimit) )
    yLimit <- c( 0,max(tauReleases) )

  # Plot the release tau estimates.
    
  xPos <- c(1:length(tauReleases))
  plot( range(xPos), yLimit,
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, tauReleases, type="h", col="black", lty=1, lwd=1 )
  points( xPos, tauReleases, bg="white", cex=1.2, pch=21 )
 
  axis( side=1, at=xPos, labels=xPos, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
   
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Release Series" )    
    
  # Plot the age tau estimates.

  yLimit <- gfx$yLim
  if ( is.null(yLimit) )
    yLimit <- c( 0,max(tauAges) )
    
  xPos <- c( 1:length(tauAges) )
  plot( range(xPos), yLimit,
        type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( xPos, tauAges, type="h", col="black", lty=1, lwd=1 )
  points( xPos, tauAges, bg="black", cex=1.2, pch=21 )
  
  axis( side=1, at=xPos, labels=ageIndex, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mfg <- par( "mfg" )
  
  if ( mfg[1]==mfg[3] )
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Age Series" )
  
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Estimated tau" )  
  
  if ( gfx$doLegend )
  {
  #  panLegend( 0.15,0.95, legTxt=c("Legal Harvest Rate","Sublegal Harvest Rate"),
  #    col=c(.COLLEGALHR,.COLSUBLEGHR), lty=c(.LTYLEGALHR,.LTYSUBLEGHR),
  #    lwd=c(.LWDLEGALHR,.LWDSUBLEGHR), bg="white" )
  }
  return( invisible() )
}     # .plotLikelihoods function


#------------------------------------------------------------------------------#
# Status Vs. Reference Point Plotting Functions                                #
#------------------------------------------------------------------------------#

.plotLegalHRssb <- function( obj, label=NULL, base="Bmsy",
   gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, xLim=NULL,yLim=NULL ) )
{
  SSBt    <- obj$SSBt
  legalHR <- obj$legalHR
  
  if ( base=="Bmsy" )
    refBiomass <- obj$refPoints$ssbFmsy
  else
    refBiomass <- obj$refPoints$B0
  
  # ARK (09-Dec-10) Change to legalHRFmsy  
  #Umsy <- obj$refPoints$Umsy
  # <- obj$refPoints$legalHRFmsy
    
  # Convert instantaneous rate to annual rate.
  #Fmsy <- obj$refPoints$Fmsy
  # ARK (21-Oct-10) This is how Cox interpolates legal harvest rate at MSY.
  #hrspline <- splinefun( x=obj$refPoints$F, y=obj$refPoints$legalHR )
  #Umsy <- hrspline( Fmsy )
  
  nT   <- length( SSBt )
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  xVal <- SSBt / refBiomass
  yVal <- legalHR / Umsy
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- range( c(0,1,xVal) )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c(0,1,yVal) )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=1, lty=2 )
  abline( v=1, lty=2 )
  
  if ( base=="Bmsy" )
  {
    abline( v=0.8, lty=3 )
    abline( v=0.4, lty=3 )
  }
  else
    abline( v=0.2, lty=3 )
  
  colVec <- rev( heat.colors( nT ) )
  points( xVal[length(xVal)], yVal[length(yVal)], pch=3, cex=4, lwd=3, col="black" )      
  lines( xVal, yVal, col="black", lty=1, lwd=1 )
  points( xVal, yVal, bg=colVec, cex=1.4, pch=21 )
  #points( xVal[length(xVal)], yVal[length(yVal)], pch=3, cex=4, lwd=3, col=colVec[length(xVal)] )

  if ( gfx$annotate )
  {
    panLab( 0.8,0.9, cex=.CEXLAB, label )
  }
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  if ( base=="Bmsy" )
  {
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "B / B at MSY" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE,
           "Legal Harvest Rate / Legal Harvest Rate at MSY" )
  }
  
  if ( base=="B0" )
  {
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "B / B0" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE,
           "Legal Harvest Rate / Legal Harvest Rate at MSY" )
  }
  
  
  if ( gfx$doLegend )
  {
    #panLegend( 0.5,0.95, legTxt=c("1977","2000"),
    #  col=c("red","red"), lty=c(2,3), lwd=c(1,1), bg="white" )
  }
  return( invisible() )
}     # .plotLegalHRssb function

#------------------------------------------------------------------------------#
# Tables                                                                       #
#------------------------------------------------------------------------------#

.plotModEstsTable <- function( trackerObj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c(0,1)
  
  nRows <- 20  
  if ( is.null(yLim) )
    yLim <- c( 0,max( length(trackerObj), nRows+1 ) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  varNames <- c( "Model","Name","objFun","B0","h","M","MSY","Bmsy","Umsy","legUmsy","Dmsy",
                 "projSSB","projDep","projLegalB","legHR.T","slegHR.T" )
  result <- matrix( NA, nrow=length(trackerObj), ncol=length(varNames) )
  result <- data.frame( result )
  names( result ) <- varNames
  
  nFits <- length(trackerObj)
  for ( i in 1:nFits )
  {
    result$Model[i] <- i
    result$Name[i]   <- trackerObj[[i]]$guiInfo$fitName
    if ( trackerObj[[i]]$status$fitStamp != "NO_FIT" )
    {
      result$objFun[i] <- trackerObj[[i]]$rep$objFun
      result$B0[i]     <- trackerObj[[i]]$rep$B0
      result$h[i]      <- trackerObj[[i]]$rep$rSteepness
      result$M[i]      <- trackerObj[[i]]$rep$M
      result$MSY[i]    <- trackerObj[[i]]$rep$refPoints$yieldFmsy
      result$Bmsy[i]   <- trackerObj[[i]]$rep$refPoints$ssbFmsy
      #result$Fmsy[i]   <- trackerObj[[i]]$rep$refPoints$Fmsy
      # ARK (09-Dec-10) Changed to legalHRFmsy
      result$Umsy[i]   <- trackerObj[[i]]$rep$refPoints$Umsy
      result$legUmsy[i]   <- trackerObj[[i]]$rep$refPoints$legalHRFmsy      
      result$Dmsy[i]   <- result$Bmsy[i] / result$B0[i]
    
      nT <- length( trackerObj[[i]]$rep$SSBt )
      result$projSSB[i] <- trackerObj[[i]]$rep$projSSB
      result$projDep[i] <- trackerObj[[i]]$rep$projDep
      result$projLegalB[i] <- trackerObj[[i]]$rep$projLegalB 
      result$legHR.T[i] <- trackerObj[[i]]$rep$legalHR[nT]
      result$slegHR.T[i] <- trackerObj[[i]]$rep$sublegalHR[nT]
    }
  }
  
  # Make a copy for formatting.
  tmp <- result
  digits <- c( 0, 0, 2, 2, 3, 3, 3, 2, 3, 3, 3, 2, 3, 3, 3, 3, 3 )
  for ( j in 1:ncol(result) )
    tmp[,j] <- formatC( result[,j], format="f", digits=digits[j], width=8 )
  
  xDelta <- 1 / ncol(tmp)
  for ( j in 1:ncol(tmp) )
    text( j*xDelta, nRows+1, adj=1, cex=0.7, names(tmp)[j] )
    
  for ( i in 1:nrow(tmp) )
    for ( j in 1:ncol(tmp) )
       text( j*xDelta, y=nRows-i+1, adj=1, cex=0.7, tmp[i,j] )
    
  box()
  
  mtext( side=3, line=0.5, cex=.CEXLAB, paste( "MLE Model Summary" ) )
  
  cat( "\n\nMSG (.plotModEsts) MLE summary of model fits:\n\n" )
  print( result )
  result
}     # .plotModEstsTable function

.plotModEstsMCMCTable <- function( trackerObj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c(0,1)
  
  nRows <- 20  
  if ( is.null(yLim) )
    yLim <- c( 0,max( length(trackerObj), nRows+1 ) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  #varNames <- c( "Model","Name","B0","h","MSY","Bmsy","Fmsy","Dmsy","Bterm","Dterm" )
  varNames <- c( "Model","Name","B0","h","M","MSY","Bmsy","Umsy","legUmsy","Dmsy",
                 "projSSB","projDep","projLegalB","legHR.T","slegHR.T","Uadj","legHarv" )  
  result <- matrix( NA, nrow=length(trackerObj), ncol=length(varNames) )
  result <- data.frame( result )
  names( result ) <- varNames
  
  nFits <- length(trackerObj)
  for ( i in 1:nFits )
  {
    result$Model[i] <- i
    result$Name[i]  <- trackerObj[[i]]$guiInfo$fitName  
    
    # Compute the statistics.
    if ( !is.null(trackerObj[[i]]$mcmcStats) )
    {
      val <- apply( trackerObj[[i]]$mcmcStats, 2, mean, na.rm=TRUE )
      result[ i, varNames[3:length(varNames)] ] <- val[ varNames[3:length(varNames)] ]
    }
  }
  
  # Make a copy for formatting.
  missVal <- -999
  tmp <- result
  
  for ( j in 3:ncol(result) )
    tmp[ is.na(tmp[,j]),j ] <- missVal

  digits <- c( 0, 2, 2, 3, 3, 3, 2, 3, 3, 3, 2, 3, 3, 3, 3, 3, 3 )
  for ( j in 1:ncol(result) )
  {
    tmp[,j] <- formatC( tmp[,j], format="f", digits=digits[j], width=8 )
    if ( j > 2 )
    {
      idx <- as.numeric(tmp[,j])==missVal
      tmp[idx,j] <- "NA"
    }
  }
  
  xDelta <- 1 / ncol(tmp)
  for ( j in 1:ncol(tmp) )
    text( j*xDelta, nRows+1, adj=1, cex=0.7, names(tmp)[j] )
    
  for ( i in 1:nrow(tmp) )
    for ( j in 1:ncol(tmp) )
       text( j*xDelta, y=nRows-i+1, adj=1, cex=0.7, tmp[i,j] )
    
  box()
  
  mtext( side=3, line=0.5, cex=.CEXLAB, paste( "MCMC Model Summary of Posterior Means" ) )
  
  cat( "\n\nMSG (.plotModEsts) MCMC summary of model fits:\n\n" )
  print( result )
  result
}    # .plotModEstsMCMCTable function.

#------------------------------------------------------------------------------#
# MCMC Plotting Functions                                                      #
#------------------------------------------------------------------------------#

.mcmcDensity <- function( mcmcObj, label=NULL, annotate=TRUE )
{
  # Find the active parameters.  If the chain is all equal, then the parameter
  # was fixed in the model configuration.  This gets a Boolean vector that
  # indicates which columns have fixed values.

  iPars <- apply( mcmcObj,2,function(x) { sum(diff(x))!=0.0 } )
  nPars <- sum( iPars )     # Number of active parameters in mcmc output.
  
  if ( nPars==0 )
  {
    cat( "\nMSG (.mcmcDensity) No active parameters or all values equal.\n" )
    return()
  }

  tmp <- mcmcObj[ ,iPars ]
  tmpNames <- names( tmp )

  for ( i in 1:ncol(tmp) )
  {
    plot( density( tmp[,i] ), axes=FALSE, main="", xlab="", ylab="" )
    abline( v = quantile( tmp[,i],probs = c(0.05,0.95) ), lty=1, lwd=2, col="gray" )
    abline( v = mean(tmp[,i]), lty=2, lwd=3, col="black" )

    # This is the MLE estimate.
    abline( v = tmp[1,i], lwd=2, col="green" )
    panLab( 0.05,0.95, adj=0, cex=.CEXLAB, tmpNames[i] )
    
    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
  }
  if ( !is.null(label) )
    mtext( side=3, line=-0.5, cex=1.0, outer=T, label )
    
  cat( "\nMSG (.mcmcDensity) MLE Estimate:\n" )
  print( tmp[1,] )
}

.mcmcPairs <- function( mcmcObj, label=NULL, annotate=TRUE )
{
  panel.mcmc <- function( x,y,z=modes,... )
  {
	  xMean <- mean( x,na.rm=T )
    yMean <- mean( y,na.rm=T )
		points( x,y,pch=16,cex=0.6,col="darkgray" )
		abline( h=yMean,v=xMean,col="blue",lty=3 )
		points( xMean,yMean, bg="cyan", pch=21,cex=1.8 )
		if ( !is.null(modes) )
    {
      # This is logic to figure out what "pair" is being plotted.
      # The modal estimates are the first row of the mcmcObj.
      # The par()$mfg calls finds the current row and column indices of
      # the panel being plotted.

	    xMode <- z[ par()$mfg[2] ]
      yMode <- z[ par()$mfg[1] ]
		  points( xMode,yMode, bg="red", pch=22, cex=1.8 )
    }
  }

  panel.hist <- function( x,... )
  {
    # Histograms for diagonal of pairs plot (from PBS Modelling CCA).
	  usr <- par("usr")
    on.exit( par(usr) )
	  h <- hist( x, breaks="Sturges", plot=FALSE )
	  breaks <- h$breaks
    nB <- length(breaks)
	  y <- h$counts
    y <- y / sum(y)
	  par( usr = c(usr[1:2], 0, max(y)*1.5) )
	  rect( breaks[-nB], 0, breaks[-1], y, col="#FFD18F" )
    box()
  }

  # Find the active parameters.  If the chain is all equal, then the parameter
  # was fixed in the model configuration.  This gets a Boolean vector that
  # indicates which columns have fixed values.

  iPars <- apply( mcmcObj,2,function(x) { sum(diff(x))!=0.0 } )
  nPars <- sum( iPars )     # Number of active parameters in mcmc output.

  if ( nPars==0 )
  {
    cat( "\nMSG (.mcmcDensity) No active parameters or all values equal.\n" )
    return()
  }

  tmp <- mcmcObj[ ,iPars ]
  tmpNames <- names( tmp )

  modes <- mcmcObj[1,]
  pairs( tmp, panel=panel.mcmc, diag.panel=panel.hist, gap=0, cex.axis=1.6 )

  if ( !is.null(label) )
    mtext( side=3, adj=0, line=-0.5, cex=.CEXLAB, outer=T, label )
}

.mcmcTraces <- function( mcmcObj, label=NULL, annotate=TRUE )
{
  plotTrace <- function( obj )
  {
    # Input "obj" is a VECTOR of MCMC samples.
    # Produces one panel trace plot.

    nSample <- length( obj )
    plot( c(1:nSample), obj, type="n", axes=FALSE, xlab="", ylab="" )
    points( c(1:nSample),obj, cex=0.2, pch=16, col="darkgray" )

    lines( lowess( c(1:nSample),obj,f=1/4), lty=1, lwd=1 )
    abline( h=mean(obj), lty=2 )

    # Plot MPD point (1st element).
    points( 1,obj[1], cex=2.0, pch=16, col="green" )
    points( 1,obj[1], cex=2.0, pch=1 )

    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
  }

  # Find the active parameters.  If the chain is all equal, then the parameter
  # was fixed in the model configuration.  This gets a Boolean vector that
  # indicates which columns have fixed values.

  iPars <- apply( mcmcObj,2,function(x) { sum(diff(x))!=0.0 } )
  nPars <- sum( iPars )     # Number of active parameters in mcmc output.

  if ( nPars==0 )
  {
    cat( "\nMSG (.mcmcDensity) No active parameters or all values equal.\n" )
    return()
  }

  tmp <- mcmcObj[ ,iPars ]
  tmpNames <- names( tmp )

  for ( i in 1:ncol(tmp) )
  {
    plotTrace( tmp[,i] )
    panLab( 0.5, 0.9, cex=.CEXLAB, tmpNames[i] )
  }

  if ( !is.null(label) )
    mtext( side=3, line=-0.5, cex=1.0, outer=T, label )
  mtext( side=1, line=0.5, cex=1.0, outer=T, "Sample" )
}     # .mcmcTraces function.


.plotPriors <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                xLim=NULL, yLim=NULL ) )
{
  # obj is a dataframe with fields "B0","M" from the posterior
  # This function is a hardwired hack for sableOpMod.
  
  # Prior on B0.  
  B0    <- seq( 1,200,length=100 )
  dens  <- 1.0 / B0
  pDens <- density( obj$B0 )
    
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c(75,175)
    
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( c(0,0.06 ) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  #tmp <- hist( obj$B0, breaks=50, plot=FALSE )
  
  lines( B0, dens, lwd=1 )
  lines( density(obj$B0, adjust=1.5), lwd=3 )
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "B0" )
  #mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Prior and Posterior Density" )
  
  # Prior on M.
  muPriorM <- 0.08
  sdPriorM <- 0.005
    
  M <- seq( 0.03, 0.11, length=100 )
    
  # SPC: (24-Nov-2010) switch to normal prior
  #dens <- dlnorm( msy,meanlog=log(muPriorMSY),sdlog=sdPriorMSY )
  dens <- dnorm( M,mean=muPriorM,sd=sdPriorM )
  
  pDens <- density( obj$M )
    
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( M )
    
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( c(dens,pDens$y) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  lines( M, dens, lwd=1 )
  lines( density( obj$M ), lwd=3 )  
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Natural Mortality" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Prior and Posterior Densities" )

}     # .plotPriors                


.plotMCMCssb <- function( obj, label=NULL, qProbs=c(0.025,0.05,0.25,0.5,0.75,0.95,0.975),
                  gfx=list( xLim=NULL, yLim=NULL ) )
{
  # Assume and object with dimensions nDraws by c(1:nYears), say 1:46.
  
  if ( is.null( gfx$xLim ) )
    xLim <- c( 1,ncol(obj) )
    
  if ( is.null( gfx$yLim ) )
    yLim <- c( 0,max(obj) )
  
  xvals <- c( 1:ncol(obj) )
 
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  # Plot the individual traces.
  #for ( i in 1:nrow(obj) )
  #  lines( xvals,obj[i,], lty=1, col="gray" )

  qVals <- apply( obj,2,quantile,probs=qProbs )
  
  nCols <- trunc( length( qProbs ) / 2.0 )
    
  colVec <- heat.colors( nCols )
  colVec <- c( rev(colVec),colVec )

  for ( i in c(1:(length(qProbs)-1)) )
  {
  #  polygon( c(xvals,rev(xvals)), c( qVals[i,],rev(qVals[i+1,])), col=colVec[i] )
  }
  
  # Plot the mean of the marginal posteriors.
  meanSSB <- apply( obj,2,mean )
  #lines( xvals, meanSSB, col="white", lty=1, lwd=3 )  
  
  #lines( xvals, qVals[1,], col="blue", lwd=2 )
  #lines( xvals, qVals[3,], col="blue", lwd=2 )
  
  qVals <- apply( obj,2,quantile,probs=c(0.05,0.1,0.5,0.9,0.95) )
  delta <- 0.2
  for ( i in 1:ncol(qVals) )
  {
    segments( i, qVals[1,i], i, qVals[5,i] )
    rect( i-delta,qVals[2,i], i+delta,qVals[4,i], col="white" )
  }
  
  points(xvals,meanSSB,pch=21, bg="black", cex=1 )
  
  panLab( 0.8,0.9, cex=.CEXLAB, label )
  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Spawning Stock Biomass" )

  #boxplot(obj, ylim=yLim )
  

}     # .plotMCMCssb function

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
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=pex, cex=cax,
             bty = "n" )
}

.addRefPointsLegendU <- function( x=0.5, y=0.5, checked )
{
  # Is there anything in the legend, or is nothing checked?
  if ( sum(checked)==0 )
  {
    cat( "\nmseRrefPoints (.addRefPointsLegend): Legend vector of length 0.\n" )
    return()
  }
  
  cax <- .REFCAX
  pex <- .REFPEX
  
  labels <- c( "U0","U0.1","Umsy","Uspr40","Umax","Ucrash" )
  names(labels) <- c("U0","U01","Umsy","U40","Umax","Ucra" )
  
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
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=pex, cex=cax,
              bty = "n" )
}

.plotRecSSB <- function( obj, gfx )
{
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

  yRange <- range( c(0,max(obj$recruits)/1e6) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot recruits against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$recruits/1e6), lwd=2 )
    
  # Adding steepness lines at B20=0.2*B0, R20, 
  # and steepness R20/B0.
  lines( c(obj$B20,obj$B20), c(0,      obj$R20/1e6), lty=2 )
  lines( c(0,      obj$B20), c(obj$R20/1e6,obj$R20/1e6), lty=2 )
  
  lines( c(obj$B0,obj$B0), c(0,     obj$R0/1e6), lty=2 )
  lines( c(0,     obj$B0), c(obj$R0/1e6,obj$R0/1e6), lty=2 )
  
  # Adding steepness label
  h <- round( obj$R20/obj$R0, digits=2 )
  xPos <- obj$B20
  yPos <- obj$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",obj$rSteepness,sep="") )
  
  if ( checked["F0"] )  
    points( obj$ssbF0,   obj$recruitsF0/1e6,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$ssbF01,  obj$recruitsF01/1e6,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$ssbFcra, obj$recruitsFcra/1e6, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$ssbFmax, obj$recruitsFmax/1e6, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$ssbFmsy, obj$recruitsFmsy/1e6, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$ssbF40,  obj$recruitsF40/1e6,  cex=pex, bg=.REFCOLF40,  pch=21 )      
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Recruits" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.5, checked=checked )
}

.plotRecSSBU <- function( obj, gfx )
{
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

  yRange <- range( c(0,max(obj$recruits/1e6)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot recruits against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$recruits)/1e6, lwd=2 )
    
  # Adding steepness lines at B20=0.2*B0, R20, 
  # and steepness R20/B0.
  lines( c(obj$B20,obj$B20), c(0,      obj$R20/1e6), lty=2 )
  lines( c(0,      obj$B20), c(obj$R20/1e6,obj$R20/1e6), lty=2 )
  
  lines( c(obj$B0,obj$B0), c(0,     obj$R0/1e6), lty=2 )
  lines( c(0,     obj$B0), c(obj$R0/1e6,obj$R0/1e6), lty=2 )
  
  # Adding steepness label
  h <- round( obj$R20/obj$R0, digits=2 )
  xPos <- obj$B20
  yPos <- obj$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",obj$rSteepness,sep="") )
  
  if ( checked["U0"] )  
    points( obj$ssbF0,   obj$recruitsF0/1e6,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$ssbF01,  obj$recruitsF01/1e6,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$ssbFcra, obj$recruitsFcra/1e6, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$ssbFmax, obj$recruitsFmax/1e6, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    points( obj$ssbFmsy, obj$recruitsFmsy/1e6, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["U40"] )
    points( obj$ssbF40,  obj$recruitsF40/1e6,  cex=pex, bg=.REFCOLF40,  pch=21 )      
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Recruits" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked=checked )
}

.plotEqRecSSBU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 )
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoints$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$recruits)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$ssb), .posVal(refPts$recruits), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )

    # Adding steepness lines at B20=0.2*B0, R20, 
    # and steepness R20/B0.
    lines( c(refPts$B20, refPts$B20), c(         0, refPts$R20), lty=2 )
    lines( c(         0, refPts$B20), c(refPts$R20, refPts$R20), lty=2 )
  
    lines( c(refPts$B0, refPts$B0), c(        0, refPts$R0), lty=2 )
    lines( c(        0, refPts$B0), c(refPts$R0, refPts$R0), lty=2 )
    
    # Adding steepness label
    h <- round( refPts$R20/refPts$R0, digits=2 )
    xPos <- refPts$B20
    yPos <- refPts$R20 * 0.8
    text( xPos, yPos, cex=1.0, pos=4, paste( "h=",refPts$rSteepness,sep="") )
    
    if ( checked["U0"] )  
      points( refPts$ssbF0, refPts$recruitsF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$ssbF01, refPts$recruitsF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$ssbFcra, refPts$recruitsFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$ssbFmax, refPts$recruitsFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$ssbF40,  refPts$recruitsF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      points( refPts$ssbFmsy, refPts$recruitsFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( refPts$ssbFmsy, refPts$recruitsFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Recruits" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.6, y=0.4, checked )
}    

.plotSsbF <- function( obj, gfx )
{
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
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotSsbU <- function( obj, gfx )
{
  pex <- .REFPEX
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$legalHR)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ssb)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot spawning stock biomass against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$legalHR), .posVal(obj$ssb), lwd=2 )
  
  if ( checked["U0"] )  
    points( obj$U0,   obj$ssbF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$ssbF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$ssbFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$ssbFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (09-Dec-10) Changed to legalHRFmsy
    #points( obj$Umsy, obj$ssbFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$ssbFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$ssbF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.9, checked )
}

.plotEqSsbU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 )
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoints$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$ssb)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$ssb), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$ssbF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01, refPts$ssbF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$ssbFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$ssbFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40, refPts$ssbF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$ssbFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )      
      points( refPts$legalHRFmsy, refPts$ssbFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( refPts$legalHRFmsy, refPts$ssbFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
}    

.plotSsbPerRecF <- function( obj, gfx )
{
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
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}


.plotSsbPerRecU <- function( obj, gfx )
{
  pex <- .REFPEX
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$legalHR)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ssbpr)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot spawning stock biomass per recruit against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$legalHR), .posVal(obj$ssbpr), lwd=2 )

  if ( checked["U0"] )  
    points( obj$U0,   obj$ssbprF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$ssbprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$ssbprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$ssbprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (09-Dec-10) Changed Umsy to legalHRFmsy
    #points( obj$Umsy, obj$ssbprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$ssbprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$ssbprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )       
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.9, checked )
}

.plotEqSsbPerRecU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 )
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoints$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$ssbpr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$ssbpr), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$ssbprF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$ssbprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$ssbprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$ssbprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$ssbprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (09-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$ssbprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$ssbprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        #text( refPts$Umsy, refPts$ssbprFmsy, idNum[i], cex=0.8 )
        text( refPts$legalHRFmsy, refPts$ssbprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "SSB per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
}    


.plotYieldF <- function( obj, gfx )
{
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
  lines( .posVal(obj$F), .posVal(obj$landed), lwd=2 )
  lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
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

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotYieldU <- function( obj, gfx )
{
  pex <- .REFPEX

  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$Ucra)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$yield)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$legalHR), .posVal(obj$landed), lwd=2 )
  lines( .posVal(obj$legalHR), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
  if ( checked["U0"] )  
    points( obj$U0,   obj$yieldF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (09-Dec-10) Changed Umsy to legalHRFmsy
    #points( obj$Umsy, obj$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )    

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.9, checked )
}

.plotEqYieldU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(idNum) )
    idNum <- c(1:n)
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 )
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoint$Ucra)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$yield)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$landed), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$yieldF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (09-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        #text( refPts$Umsy, refPts$yieldFmsy, idNum[i], cex=0.8 )
        text( refPts$legalHRFmsy, refPts$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked )
}    

.plotYieldSSB <- function( obj, gfx )
{
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
  lines( .posVal(obj$ssb), .posVal(obj$landed), lwd=2 )
  lines( .posVal(obj$ssb), .posVal(obj$discarded), lwd=2, lty="dashed" )
  
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
   
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Landed Yield" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotYieldSSBU <- function( obj, gfx )
{
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
  lines( .posVal(obj$ssb), .posVal(obj$landed), lwd=2 )
  lines( .posVal(obj$ssb), .posVal(obj$discarded), lwd=2, lty="dashed" )
  
  if ( checked["U0"] )  
    points( obj$ssbF0,   obj$yieldF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$ssbF01,  obj$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$ssbFcra, obj$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$ssbFmax, obj$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    points( obj$ssbFmsy, obj$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
  if ( checked["U40"] )
    points( obj$ssbF40,  obj$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )
   
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Landed Yield" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.9, checked )
}

.plotEqYieldSSBU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 ) 
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoints$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$landed)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$ssb), .posVal(refPts$landed), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$ssbF0, refPts$yieldF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$ssbF01,  refPts$yieldF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$ssbFcra, refPts$yieldFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$ssbFmax, refPts$yieldFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$ssbF40,  refPts$yieldF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      points( refPts$ssbFmsy, refPts$yieldFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( refPts$ssbFmsy, refPts$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Landed Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
}      

.plotYprF <- function( obj, gfx )
{
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
  lines( .posVal(obj$F), .posVal(obj$yprL), lwd=2 )
  lines( .posVal(obj$F), .posVal(obj$yprD), lwd=2, lty="dashed" )

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
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.4, checked )
}


.plotYprU <- function( obj, gfx )
{
  pex <- .REFPEX

  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- range( c(0,max(obj$legalHR)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$ypr)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot yield per recruit against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$legalHR), .posVal(obj$yprL), lwd=2 )
  lines( .posVal(obj$legalHR), .posVal(obj$yprD), lwd=2, lty="dashed" )

  if ( checked["U0"] )  
    points( obj$U0,   obj$yprF0,   cex=pex, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$yprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$yprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$yprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (09-Dec-10) Changed Umsy to legalHRFmsy
    #points( obj$Umsy, obj$yprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$yprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$yprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )    
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked )
}


.plotEqYprU <- function( obj, idNum=NULL, gfx )
{
  pex <- 2.5
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  xRange <- c( 0,0 )
  yRange <- c( 0,0 )
  for ( i in 1:n )
  {
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$rep$refPoints$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$rep$refPoints$ypr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]$rep$refPoints
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$yprL), lwd=2 )

    if ( checked["U0"] )  
      points( refPts$U0, refPts$yprF0, cex=pex, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$yprF01,  cex=pex, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$yprFcra, cex=pex, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$yprFmax, cex=pex, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$yprF40,  cex=pex, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (09-Dec-10) Changed Umsy to legalHRFmsy
      #points( refPts$Umsy, refPts$yprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$yprFmsy, cex=pex, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        #text( refPts$Umsy, refPts$yprFmsy, idNum[i], cex=0.8 )
        text( refPts$legalHRFmsy, refPts$yprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB, "Yield Per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked )
}    


# .plotRefPts    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPoints <- function( obj, gfx=list( annotate=TRUE, 
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  # win <- .getWinName()
  # guiInfo <- getWinVal( scope="L",winName=win )

  checked <- rep(TRUE,6)
  names(checked) <- c("F0","F01","Fcra","Fmax","Fmsy","F40")

  
  annotate <- gfx$annotate
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis

  par(mfrow = c(3,2), oma = c(3,5,2,3), mar = c(2,3,2,3) )
   
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,.15),yLim=NULL ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate, checked = checked,
                 doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,.15),yLim=NULL ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,.15),yLim=c(0,2.9) ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,.15),yLim=c(0,120) ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,120),yLim=c(0,2.5) ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(0,120),yLim=c(0,2.9) ) )
                 
  tmpNames <- c( "B0", "rSteepness",
                 "F0","F01","Fmsy","F40","Fmax","Fcra",
                 "ssbF0","ssbF01","ssbFmsy","ssbF40","ssbFmax","ssbFcra",
                 "landedF0","landedF01","landedFmsy","landedF40","landedFmax","landedFcra",
                 "yprF0","yprF01","yprFmsy","yprF40","yprFmax","yprFcra",
                 "ssbprF0","ssbprF01","ssbprFmsy","ssbprF40","ssbprFmax","ssbprFcra"
               )

  mtext( side = 3, outer = TRUE, text = "F-based Reference Curves", font = 2)
  
  result <- matrix( unlist( obj[ tmpNames ] ), ncol=1 )
  dimnames( result ) <- list( tmpNames, "Value" )
  
  result
}

# .plotRefPtsU  (Plot fishery reference points for the operating model, but on
#               the scale of harvest rates (for sablefOpMod legal HR).
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPointsU <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  checked <- rep(TRUE,6)
  names(checked) <- c("U0","U01","Ucra","Umax","Umsy","U40")

  
  annotate <- gfx$annotate
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis

  par(mfrow = c(3,2), oma = c(3,5,2,3), mar = c(2,3,2,3) )
   
  .plotYprU( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbPerRecU( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )  
  .plotRecSSBU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldSSBU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
                 
  tmpNames <- c( "B0", "rSteepness",
                 "U0","U01","Umsy","U40","Umax","Ucra",
                 "legalHRF0","legalHRFmsy",
                 "ssbF0","ssbF01","ssbFmsy","ssbF40","ssbFmax","ssbFcra",
                 "landedF0","landedF01","landedFmsy","landedF40","landedFmax","landedFcra",
                 "yprF0","yprF01","yprFmsy","yprF40","yprFmax","yprFcra",
                 "ssbprF0","ssbprF01","ssbprFmsy","ssbprF40","ssbprFmax","ssbprFcra"
               )

  mtext( side = 3, outer = TRUE, text = "U-based reference points", font = 2)
  
  result <- matrix( unlist( obj[ tmpNames ] ), ncol=1 )
  dimnames( result ) <- list( tmpNames, "Value" )
  
  result
}

#------------------------------------------------------------------------------#
# Harvest Control Rule Plots                                                   #
#------------------------------------------------------------------------------#

.plotHCR1 <- function( obj, label=NULL,
                gfx=list( annotate=ftAnnotate, doLegend=ftLegend,
                xLim=NULL, yLim=NULL ) )
{
  # obj is a rep file object read by lisread.
  
  # ARK (13-Dec-10) Changed Umsy to legalHRFmsy.
  Umsy <- obj$refPoints$legalHRFmsy
  
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 0,obj$B0 )
  
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 0,Umsy*1.2 )
  
  #
  
  val <- list()
  val$B0                <- obj$B0
  val$SSB               <- obj$projSSB
  val$legalBt           <- obj$projLegalB
  val$refPoints$ssbFmsy <- obj$refPoints$ssbFmsy
  val$refPoints$Umsy    <- Umsy
  projDep               <- obj$projDep  
  
  ruleList <- .calcDfoRule( val )  

  B0       <- ruleList$B0
  targetB  <- ruleList$targetB
  LRP      <- ruleList$LRP
  USR      <- ruleList$USR
  SSB      <- ruleList$SSB
  remRate  <- ruleList$refRemRate
  precRate <- ruleList$precautionaryRemRate
  
  legalB     <- ruleList$legalB
  legalCatch <- ruleList$legalCatch
  projSSB    <- ruleList$SSB
  
  precautionaryRemRate <- ruleList$precautionaryRemRate
  remRate              <- ruleList$remRate
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  lines( ruleList$stockStatus, ruleList$precRemRateVec, lwd=2 )
  
  abline( v=LRP,     lty="dotted" )
  abline( v=USR,     lty="dotted" )
  abline( v=targetB, lty="dashed" )
  abline( v=B0,      lty="dashed" )
  
  abline( h=remRate, lty="dashed" )
  
  points( SSB, precautionaryRemRate, pch=21, bg="black", cex=1.5 )
  
  axis( side=1, cex=.CEXAXIS )
  axis( side=2, cex=.CEXAXIS, las=.YAXISLAS )
  
  mtext( side=1, line=.OUTLINE1, outer=TRUE, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.OUTLINE2, outer=TRUE, cex=.CEXLAB, "Legal Harvest Rate" )
  box()
 
  if ( gfx$annotate )
  {
    if ( !is.null(label) )
      panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, label )
  }
  
  panLab( 0.5, 0.4, adj=0, cex=1.0,
    paste( "Ref.  Rem. Rate = ",round(remRate,digits=3),"\n",
           "Prec. Rem. Rate = ",round(precRate,digits=3),"\n",
           "Legal Biomass   = ",round(legalB,digits=1),"\n",
           "Legal Harvest   = ",round(legalCatch,digits=3),"\n",
           "Proj. SSB       = ",round(projSSB,digits=3),"\n",
           "Proj. Depletion = ",round(projDep,digits=3),sep=""  ) ) 
  
  return( invisible() )
}

.plotHCR2 <- function( obj, label=NULL,
                gfx=list( annotate=ftAnnotate, doLegend=ftLegend,
                xLim=NULL, yLim=NULL ) )
{
  # HCR based on MCMC, obj passed in is mcmcStats.
  
  # Need MCMC estimates of Bmsy, LRP, USR, Umsy, and SSB from object mcmcStats.
  
  meanVals <- apply( obj,2,mean, na.rm=TRUE )

  # These quantities were used for "DFO rule" but are no longer needed?
  val <- list()
  val$B0                <- as.numeric( exp( meanVals["logB0"] ) )
  val$projSSB           <- as.numeric( meanVals["projSSB"] )
  val$legalBt           <- as.numeric( meanVals["projLegalB"] )
  val$sublegalBt        <- as.numeric( meanVals["projSublegalB"] )
  val$refPoints$ssbFmsy <- as.numeric( meanVals["Bmsy"] )
  #val$refPoints$Umsy    <- as.numeric( meanVals["Umsy"] )
  val$refPoints$legalHRFmsy <- as.numeric( meanVals["legUmsy" ] )
  
  # These are means from the MCMC posterior.
  B0         <- as.numeric( exp( meanVals["logB0"] ) )
  projSSB    <- as.numeric( meanVals["projSSB"] )
  projDep    <- as.numeric( meanVals["projDep"] )
  legalB     <- as.numeric( meanVals["projLegalB"] )
  sublegalB  <- as.numeric( meanVals["projSublegalB"] )
  targetB    <- as.numeric( meanVals["Bmsy"] )
  #remRate    <- as.numeric( meanVals["Umsy"] )
  remRate    <- as.numeric( meanVals[ "legUmsy" ] )
  LRP        <- 0.4 * targetB
  USR        <- 0.8 * targetB
  precautionaryRemRate <- as.numeric( meanVals["Uadj"] )
  legalCatch           <- as.numeric( meanVals["legHarv"] )  
  
  #ruleList <- .calcDfoRule( val )  

  #B0       <- ruleList$B0
  #targetB  <- ruleList$targetB
  #LRP      <- ruleList$LRP
  #USR      <- ruleList$USR
  #SSB      <- ruleList$SSB
  #remRate  <- ruleList$refRemRate
  #precRate <- ruleList$precautionaryRemRate
  
  #legalB     <- ruleList$legalB
  #legalCatch <- ruleList$legalCatch
  
  #precautionaryRemRate <- ruleList$precautionaryRemRate
  #remRate              <- ruleList$remRate

  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$B0) )
  
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 0,max(obj$Uadj) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  #lines( ruleList$stockStatus, ruleList$precRemRateVec, lwd=2 )
  
  # Loop over all the draws from the posterior.
  for ( i in 1:nrow(obj) )
  {
     B0i         <- obj$B0[i]
     SSBi        <- obj$projSSB[i]
     #legalB     <- as.numeric( meanVals["legalB.T"] )
     #sublegalB  <- as.numeric( meanVals["sublegalB.T"] )
     targetBi    <- obj$Bmsy[i]
     #remRatei    <- obj$Umsy[i]
     remRatei    <- obj$legUmsy[i]
     LRPi        <- 0.4 * targetB
     USRi        <- 0.8 * targetB
     precRatei   <- obj$Uadj[i]
     #legalCatch <- as.numeric( meanVals["legHarv"] )
     
     #points( SSBi,precRatei, pch=21, cex=0.8, col="blue" )  
  }
  
  # Plot mean rule from posterior.
  segments( 0,         0, LRP, 0,       col="black", lty=1, lwd=2 )
  segments( LRP,       0, USR, remRate, col="black", lty=1, lwd=2 )
  segments( USR, remRate,  B0, remRate, col="black", lty=1, lwd=2 )   
  
  # Plot points from posterior.
  points( obj$projSSB, obj$Uadj, pch=21, cex=0.8, col="gray" )  
  
  # Put 5-th and 95-th percentiles of MCMC distribution on SSB and Uadj. 
  confInt <- quantile( obj$Uadj, probs=c(0.05,0.95) )
  segments( projSSB, confInt[1],projSSB,confInt[2], lty=1, col="red", lwd=3 )
  confInt <- quantile( obj$projSSB, probs=c(0.05,0.95) )
  segments( confInt[1], precautionaryRemRate, confInt[2], precautionaryRemRate,
            lty=1, col="red", lwd=3 )
  
  abline( v=LRP,     lty="dotted" )
  abline( v=USR,     lty="dotted" )
  abline( v=targetB, lty="dashed" )
  abline( v=B0,      lty="dashed" )
  
  abline( h=remRate, lty="dashed" )
  
  # Plot the mean projSSB and precautionaryRemRate.
  points( projSSB, precautionaryRemRate, pch=21, bg="black", cex=1.5 )
  
  axis( side=1, cex=.CEXAXIS )
  axis( side=2, cex=.CEXAXIS, las=.YAXISLAS )
  
  mtext( side=1, line=.OUTLINE1, outer=TRUE, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.OUTLINE2, outer=TRUE, cex=.CEXLAB, "Legal Harvest Rate" )
  box()
 
  if ( gfx$annotate )
  {
    if ( !is.null(label) )
      panLab( 0.05, 0.9, adj=0, cex=.CEXLAB, label )
  }
  
  panLab( 0.5, 0.4, adj=0, cex=1.0,
    paste( "Ref.  Rem. Rate = ",round(remRate,digits=3),"\n",
           "Prec. Rem. Rate = ",round(precautionaryRemRate,digits=3),"\n",
           "Legal Biomass   = ",round(legalB,digits=1),"\n",
           "Legal Harvest   = ",round(legalCatch,digits=3),"\n",
           "Proj. SSB       = ",round(projSSB,digits=3),"\n",
           "Proj. Depletion = ",round(projDep,digits=3), sep=""  ) )
  
  return( invisible() )
}

.plotModCat <- function( repObj,
   seriesNames=c("Trap","Hook","Trawl","Std.","StRS"),
   label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
   doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Input is a rep list, compares input retained catch to model retained catch.
  Ctg    <- repObj$landCatchMatrix

  Ctg[,3:ncol(Ctg)] <- Ctg[,3:ncol(Ctg)] / 1000.0

  # First two columns of landCatchMatrix are year and timestep.
  nGear  <- ncol( Ctg ) - 2
  nT     <- nrow( Ctg )

  # Sum the catch by gear.
  totalCt <- apply( Ctg[,c(3:ncol(Ctg))], 1, sum )
  
  # Select out the landedCtgx and make an array out of the matrix.
  idx <- grep( "landedCtg",names(repObj) )
  landedCtg <- repObj[idx]
  #landedCtg <- sapply( landedCtg, t ) * 1000.0
  landedCtg <- sapply( landedCtg, t )

  landedCt  <- apply( landedCtg,1,sum )

  idx <- grep( "undirLandCtg",names(repObj) )
  undirLandCtg <- repObj[idx]
  #undirLandCtg <- sapply( undirLandCtg, t ) * 1000.0
  undirLandCtg <- sapply( undirLandCtg, t )
  undirLandCtg[undirLandCtg <= 0] <- NA

  undirLandCt  <- apply( undirLandCtg,1,sum, na.rm = T )
  undirLandCt[undirLandCt <= 0] <- NA
  
  if ( is.null( seriesNames) )
    seriesNames <- paste( "Series",c(1:nGear) )

  # years: These will become xval.
  xVal <- 1:nT
  if ( .USEYEAR )
    xVal <- c( .INITYEAR:(.INITYEAR+nT-1) )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    xLim <- range( xVal )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLimit <- range( c(0,Ctg[,c(3:ncol(Ctg))],na.rm=TRUE ) )

  if ( gfx$bygears )
  {
    #dev.new()
    #par( oma=.OMA, mar=.MAR, mfrow=c(nGear,1) )
    
    for ( g in 1:nGear )
    {
      if ( is.null(yLim) )
        yLimit <- c( 0,max(Ctg[,2+g], na.rm=TRUE) )
        
      plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )

      Ct <- Ctg[ ,g+2 ]
      Ct[ Ct <= 0.0 ] <- NA
      
      lines( xVal, Ct, col=.COLCtg[g], lty=.LTYCtg[g], lwd=.LWDCtg[g] )
      points( xVal, Ct, bg="white", cex=2, pch=.SYMCtg[g] )
      
      # Now plot the landedCtg.
      points( xVal, ifelse(landedCtg[,g]==0,NA,landedCtg[,g]), bg="green", cex=1.2, pch=21 )
      # Plot undirLandected Ctg

      lines( x = xVal, y = undirLandCtg[,g], lty = 2, col = "red", lwd = 2 )


      
      panLab( 0.025,0.90, adj=0, cex=1.4, seriesNames[g] ) 
      
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )
      axis( side=4, cex.axis=.CEXAXIS2, labels=FALSE )
      box()    
    }
    mtext( side=1, line=.OUTLINE1, cex=1.2, outer=TRUE, "Year" )
    mtext( side=2, line=2, cex=1.2, outer=TRUE, "Retained Catch (000s t)" )
  }
  else
  {
  # Y-axis limits.
    if ( is.null(yLim) )
      yLimit <- range( c(0,totalCt) )  
  
    plot( xLim, yLimit, type="n", axes=FALSE, xlab="", ylab="" )
  
    lines( xVal, totalCt, col="black", lty=1, lwd=1 )
    points( xVal, totalCt, bg="white", cex=2, pch=21 )

    points( xVal, landedCt, bg="green", cex=1.2, pch=21 )
    lines(  xVal, undirLandCt, lty = 2, col = "grey40" )

    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
    box()
    mtext( side=1, line=0.5, cex=1.2, outer=TRUE, "Year" )
    mtext( side=2, line=0.5, cex=1.2, outer=TRUE, "Retained Catch (000s t)" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.75,0.95, legTxt=c("Observed","Model"),
                 cex=0.8, pt.bg=c("white","green"), col=c("black","black"),
                 lty=c(1,NA), lwd=c(1,NA), pt.cex=c(2,1.2), pch=c(21,21) )
    }
  }
    
  if ( gfx$annotate )
  {
    if ( !is.null(label) )
      panLab( 0.6, 0.05, adj=0, cex=.CEXLAB, label )
  }
  return( invisible() )  
}

.plotSelAge <- function( pars, shape, lenAtAge, gfx=list( doAnnotate=TRUE,
   doBW=TRUE, doFacet=TRUE, doLegend=TRUE, doVerbose=TRUE, xLim=NULL, yLim=NULL ) )
{
  # This takes the admbPin object, and lenAge dataframe from .calcLenAge.
  # It outputs the back transformed parameters to the R-console, and changes
  # selectivity at length to selectivity at age.
  # Note that this is for t=1 only by sex, if time-varying selectivity is
  # desired then need to plot as function of years.

  parNames <- names( pars )

  key      <- "log_S50_g_a"
  idx      <- grep( key, parNames )
  nSeries  <- length( idx )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp    <- data.frame( Idx=c(1:nSeries), Series=paste( "Series",c(1:nSeries),sep="" ) )
  result <- tmp

  tmp$logS50gAsc <- unlist( pars[idx] )

  key      <- "dev_S95_g_a"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS95gAsc <- unlist( pars[idx] )

  key      <- "dev_S95_g_d"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS95gDes <- unlist( pars[idx] )

  key      <- "dev_S50_g_d"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS50gDes <- unlist( pars[idx] )

  # Now find L50a, L95a, L95d, L50d.
  result$L50a <- exp( tmp$logS50gAsc )

  # devS95Asc = log( L95ga-L50ga )
  result$L95a <- exp( tmp$devS95gAsc ) + result$L50a

  # devS95Des = log( L95gd-L95ga )
  result$L95d <- exp( tmp$devS95gDes ) + result$L95a

  # devS50Des = log( L50gd-L95gd )
  result$L50d <- exp( tmp$devS50gDes ) + result$L95d

  result$Dome <- shape

  cat( "\nMSG (.plotSelLen) Selectivity at length parameters:\n\n" )
  print( result )
  cat( "\n" )

  nAge <- length( unique( lenAtAge$Age ) )

  # Make vectors that have replicated indices and parameters for each series.
  Series <- rep( result$Series, rep( nAge, nSeries ) )
  Domed  <- rep( shape, rep( nAge, nSeries ) )
  L50a   <- rep( result$L50a, rep( nAge, nSeries ) )
  L95a   <- rep( result$L95a, rep( nAge, nSeries ) )
  L95d   <- rep( result$L95d, rep( nAge, nSeries ) )
  L50d   <- rep( result$L50d, rep( nAge, nSeries ) )

  # Set paramaters for dome-shape vs. asymptotic selectivity.
  L95d   <- ifelse( Domed==1, L95d, L50a )
  L50d   <- ifelse( Domed==1, L50d, L95a )

  # Females length at age.
  Age       <- lenAtAge[ lenAtAge$Sex=="Female", "Age" ]
  lenAtAgeF <- lenAtAge[ lenAtAge$Sex=="Female", "Length" ]
  lenAtAgeF <- rep( lenAtAgeF, nSeries )

  # Female selectivity.
  selAsc <- 1.0 / ( 1.0 + exp( -log(19.0) * ( lenAtAgeF-L50a ) / ( L95a-L50a ) ) )
  selDes <- 1.0 / ( 1.0 + exp( -log(19.0) * ( lenAtAgeF-L50d ) / ( L95d-L50d ) ) )
  selAtAgeF <- ifelse( Domed==1, selAsc*selDes, selAsc )

  # Careful here, have to divide through by series max, not grand max.
  maxSeries <- sapply( split( selAtAgeF, Series ), max )
  maxSel    <- rep( maxSeries, rep( nAge, nSeries ) )
  selAtAgeF <- selAtAgeF / maxSel

  tmpF     <- data.frame( Series=Series, Age=Age, Length=lenAtAgeF, Selectivity=selAtAgeF )
  tmpF$Sex <- rep( "Females", nrow(tmpF) )

  # Male selectivity.
  Age       <- lenAtAge[ lenAtAge$Sex=="Male", "Age" ]
  lenAtAgeM <- lenAtAge[ lenAtAge$Sex=="Male", "Length" ]
  lenAtAgeM <- rep( lenAtAgeM, nSeries )

  selAsc <- 1.0 / ( 1.0 + exp( -log(19.0) * ( lenAtAgeM-L50a ) / ( L95a-L50a ) ) )
  selDes <- 1.0 / ( 1.0 + exp( -log(19.0) * ( lenAtAgeM-L50d ) / ( L95d-L50d ) ) )
  selAtAgeM <- ifelse( Domed==1, selAsc*selDes, selAsc )

  # Careful here, have to divide through by series max, not grand max.
  maxSeries <- sapply( split( selAtAgeM, Series ), max )
  maxSel    <- rep( maxSeries, rep( nAge, nSeries ) )
  selAtAgeM <- selAtAgeM / maxSel

  tmpM     <- data.frame( Series=Series, Age=Age, Length=lenAtAgeM, Selectivity=selAtAgeM )
  tmpM$Sex <- rep( "Males", nrow(tmpM) )

  tmp <- rbind( tmpF, tmpM )

  p <- ggplot( tmp, aes( x=Age, y=Selectivity, group=interaction( Sex, Series ),
               colour=Series ) )
  p <- p + geom_line( aes( linetype=Sex ), size=1 )
  p <- p + labs( x="Age", y="Selectivity" )

  if ( gfx$doFacet )
    p <- p + facet_wrap( ~Series )

  if ( gfx$doVerbose )
    print( p$data )
  else
    print( head( p$data ) )

  p
}     # END function .plotSelAge

.plotSelAgeGearYear <- function( obj, sexType="Female", gfx=list( doAnnotate=TRUE,
   doBW=TRUE, doFacet=TRUE, doLegend=TRUE, doVerbose=TRUE, xLim=NULL, yLim=NULL ) )
{
  # This takes the report$estimates object.

  sex <- ifelse( sexType=="Female", "f","m" )

  repNames <- names( obj )

  key      <- paste( "sel_gta_", sex, sep="" )
  idx      <- grep( key, repNames )
  nSeries  <- length( idx )

  tmpNames <- paste( key,c(1:nSeries), sep="" )
  tmp      <- obj[ idx ]
  names( tmp ) <- paste( "Series",c(1:nSeries),sep="" )

  cat( "\nMSG (.plotSelAgeGearYear) Selectivity at age by gear and year:\n\n" )

  tmpMelt <- melt( tmp )
  names( tmpMelt ) <- c( "tStep","Age","Selectivity","Series" )

  # HACK .INITYEAR should really be passed in scal.rep.
  tmpMelt$Year   <- tmpMelt$tStep + .INITYEAR - 1
  tmpMelt$offSet <- tmpMelt$Year + tmpMelt$Age - 1

  tmpMelt <- tmpMelt[ tmpMelt$Year %% 5==0, ]

  p <- ggplot( tmpMelt, aes( x=offSet, y=Selectivity, colour=interaction( Series,Year ) ) )

  p <- p + geom_line( size=1 )
  p <- p + scale_color_grey()

  p <- p + theme( legend.position="none" )

  if ( gfx$doFacet )
    p <- p + facet_wrap( ~Series )

  if ( gfx$doVerbose )
    print( p$data )
  else
    print( head( p$data ) )

  p
}     # END function .plotSelAgeGearYear


.plotSelLen <- function( pars, shape, gfx=list( doAnnotate=TRUE, doBW=TRUE,
                 doFacet=TRUE, doLegend=TRUE, doVerbose=TRUE, xLim=NULL, yLim=NULL ) )
{
  # This takes the admbPin object, and admbDat$ctlPars object for the std. devs.
  # of the ascending and descending limbs.
  # It outputs the back transformed data to the R-console, and plots the curves.

  parNames <- names( pars )

  key      <- "log_S50_g_a"
  idx      <- grep( key, parNames )
  nSeries  <- length( idx )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp    <- data.frame( Idx=c(1:nSeries), Series=paste( "Series",c(1:nSeries),sep="" ) )
  result <- tmp

  tmp$logS50gAsc <- unlist( pars[idx] )

  key      <- "dev_S95_g_a"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS95gAsc <- unlist( pars[idx] )

  key      <- "dev_S95_g_d"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS95gDes <- unlist( pars[idx] )

  key      <- "dev_S50_g_d"
  idx      <- grep( key,parNames )
  tmpNames <- paste( key,c(1:nSeries), sep="" )

  tmp$devS50gDes <- unlist( pars[idx] )

  # Now find L50a, L95a, L95d, L50d.
  result$L50a <- exp( tmp$logS50gAsc )

  # devS95Asc = log( L95ga-L50ga )
  result$L95a <- exp( tmp$devS95gAsc ) + result$L50a

  # devS95Des = log( L95gd-L95ga )
  result$L95d <- exp( tmp$devS95gDes ) + result$L95a

  # devS50Des = log( L50gd-L95gd )
  result$L50d <- exp( tmp$devS50gDes ) + result$L95d

  result$Dome <- shape

  cat( "\nMSG (.plotSelLen) Selectivity at length parameters:\n\n" )
  print( result )
  cat( "\n" )

  len  <- seq( 0, .MAXLEN, 1 )
  nLen <- length( len )

  Series <- rep( result$Series, rep( nLen, nSeries ) )
  Domed  <- rep( shape, rep( nLen, nSeries ) )
  L50a   <- rep( result$L50a, rep( nLen, nSeries ) )
  L95a   <- rep( result$L95a, rep( nLen, nSeries ) )
  L95d   <- rep( result$L95d, rep( nLen, nSeries ) )
  L50d   <- rep( result$L50d, rep( nLen, nSeries ) )

  L95d   <- ifelse( Domed==1, L95d, L50a )
  L50d   <- ifelse( Domed==1, L50d, L95a )

  selAsc <- 1.0 / ( 1.0 + exp( -log(19.0) * ( len-L50a ) / ( L95a-L50a ) ) )
  selDes <- 1.0 / ( 1.0 + exp( -log(19.0) * ( len-L50d ) / ( L95d-L50d ) ) )

  selAtLen <- ifelse( Domed==1, selAsc*selDes, selAsc )

  # Careful here, have to divide through by series max, not grand max.
  maxSeries <- sapply( split( selAtLen, Series ), max )
  maxSel    <- rep( maxSeries, rep( nLen, nSeries ) )
  selAtLen <- selAtLen / maxSel

  tmp <- data.frame( Series=Series, Length=len, SelAtLen=selAtLen )

  p <- ggplot( tmp, aes( x=Length, y=SelAtLen, colour=Series ) )
  p <- p + geom_line( size=1 )
  p <- p + labs( x="Length (mm)", y="Selectivity" )

  if ( gfx$doFacet )
    p <- p + facet_wrap( ~Series )

  if ( gfx$doVerbose )
    print( p$data )
  else
    print( head( p$data ) )

  p
}     # END function .plotSelLen

.calcRefPointsFromRep <- function( obj, parList = "simCtlFile.txt" )
{
  
  # # Can we continue this and produce the base simCtlFile rapidly?

  tmp <- .makeParListFromRep( obj = obj, parFile = parList)
  refPoints <- deref( calcRefPoints( as.ref(tmp) ) )
        
  result <- list()  

  result$MSY      <- refPoints$yieldFmsy
  result$Bmsy     <- refPoints$ssbFmsy
  result$Fmsy     <- refPoints$Fmsy
  result$Umsy     <- refPoints$Umsy
  result$legUmsy  <- refPoints$legalHRFmsy
  result$Dmsy     <- result$Bmsy / obj$SSB0
    
  return(result)
}     # .calcRefPointsFromRep function


.makeParListFromRep <- function( obj, parFile = "simCtlFile.txt" )
{
  library(ref)
  source("mseRrefPoints.r")
  source("mseRglobals.r")
  source("mseRtools.r")

  ctlPars <- .readParFile( parFile )
  parList <- .createList( ctlPars )$opMod

  # ref pts matrix
  resultNames <- c (  "Bmsy", "Fmsy", "Umsy", "legUmsy", "Dmsy" )
  result <- as.data.frame( matrix( NA, nrow=1,ncol=length(resultNames) ) )
  names(result) <- resultNames
 
  cat( "\nMSG (.calcRefPointsFromRep) Calculating reference points from rep file.\n" )

  objNames <- names(obj)

  # Unfished biomass.
  if ( any(objNames=="SSB0" ) )
    parList$B0 <- obj$SSB0
  
  # Steepness.          
  if ( any(objNames=="h" ) )
    parList$rSteepness <- obj$h

  # Natural mortality.  
  if ( any(objNames=="M_f") )
    parList$M[2] <- obj$M_f

  if ( any(objNames=="M_m") )
    parList$M[1] <- obj$M_m
    
  # Generation time.
  if ( any(objNames=="genTime") )
    parList$genTime <- obj$genTime

  selType <- obj$selType

  for(g in 1:length(selType))
  {
    # L50 (or mean if asymptotic)
    parList$L50Cg1[g] <- obj$alpha_g1[g]

    # L95 - L50 (or sd if normal model)
    parList$L95Cg1[g] <- obj$beta_g1[g]
    # Add L50 if asymptotic
    if(selType[g] == 1)
      parList$L95Cg1[g] <- parList$L95Cg1[g] + parList$L50Cg1[g]    
  }

  # Can we continue this and produce the base simCtlFile rapidly??

  tmp <- parList
  
  return(tmp) 
}

# plotRefPts    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
plotRefPts <- function( obj )
{
  par( oma=c(2,1.5,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )

  cax     <- .REFCAX
  pex     <- .REFPEX

  cols <- brewer.pal(n = 7, "Dark2")

  colF0   <- cols[1]
  colF01  <- cols[2]
  colFcra <- cols[3]
  colFmsy <- cols[4]
  colFmax <- cols[5]
  colF40  <- cols[6]

  rpRef <- calcRefPoints( as.ref( obj ) )
  rp <- deref( rpRef )

  # Plot yield per recruit against fishing mortality.
  plot( c(0,max(rp$F)), c(0,max(rp$ypr)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$F, rp$ypr, lwd=2 )

  points( rp$F0,   rp$yprF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$F01,  rp$yprF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$Fcra, rp$yprFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$Fmax, rp$yprFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$Fmsy, rp$yprFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$F40,  rp$yprF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "F" )
  mtext( side=2, line=3.5, cex=1.0, "Yield per Recruit" )
  box()

  # Plot spawning stock biomass per recruit against fishing mortality.
  plot( c(0,max(rp$F)), c(0,max(rp$ssbpr)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$F, rp$ssbpr, lwd=2 )

  points( rp$F0,   rp$ssbprF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$F01,  rp$ssbprF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$Fcra, rp$ssbprFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$Fmax, rp$ssbprFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$Fmsy, rp$ssbprFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$F40,  rp$ssbprF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "F" )
  mtext( side=2, line=3.5, cex=1.0, "SSB per Recruit" )
  box()

  # Add a legend.
  panLegend( 0.6, 0.95, legTxt=c( "F0","F0.1","Fmsy","Fspr40","Fmax","Fcrash" ),
    pch=c(21,21,21,21,21,21),
    pt.bg=c(colF0,colF01,colFmsy,colF40,colFmax,colFcra),
    pt.cex=.CEXSYM8, cex=cax )

  # Plot yield against fishing mortality.
  plot( c(0,rp$Fcra), c(0,max(rp$yield)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$F, rp$yield, lwd=2 )

  points( rp$F0,   rp$yieldF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$F01,  rp$yieldF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$Fcra, rp$yieldFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$Fmax, rp$yieldFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$Fmsy, rp$yieldFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$F40,  rp$yieldF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "F" )
  mtext( side=2, line=3.5, cex=1.0, "Yield" )
  box()

  # Plot spawning stock biomass against fishing mortality.
  plot( c(0,rp$Fcra), c(0,max(rp$ssb)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$F, rp$ssb, lwd=2 )

  points( rp$F0,   rp$ssbF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$F01,  rp$ssbF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$Fcra, rp$ssbFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$Fmax, rp$ssbFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$Fmsy, rp$ssbFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$F40,  rp$ssbF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "F" )
  mtext( side=2, line=3.5, cex=1.0, "SSB" )
  box()

  # Plot recruits against spawning stock biomass.
  plot( c(0,max(rp$ssb)), c(0,max(rp$recruits)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$ssb, rp$recruits, lwd=2 )

  # SPC 03Feb08: Adding steepness lines at B20=0.2*B0, R20,
  # and steepness R20/B0.
  lines( c(rp$B20,rp$B20), c(0,rp$R20), lty=2 )
  lines( c(0,rp$B20),  c(rp$R20,rp$R20), lty=2 )

  lines( c(rp$B0,rp$B0), c(0,rp$R0), lty=2 )
  lines( c(0,rp$B0),  c(rp$R0,rp$R0), lty=2 )

  # Adding steepness label
  h <- round( rp$R20/rp$R0, digits=2 )
  # xPos <- 0.15
  # yPos <- (1+h)/2.
  # panLab( xPos, yPos, cex=1.2, txt=paste("h=",h,sep="") )

  xPos <- rp$B20
  yPos <- rp$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",rp$rSteepness,sep="") )

  points( rp$ssbF0,   rp$recruitsF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$ssbF01,  rp$recruitsF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$ssbFcra, rp$recruitsFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$ssbFmax, rp$recruitsFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$ssbFmsy, rp$recruitsFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$ssbF40,  rp$recruitsF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "SSB" )
  mtext( side=2, line=3.5, cex=1.0, "Recruits" )
  box()

  # Plot yield against spawning stock biomass.
  plot( c(0,max(rp$ssb)), c(0,max(rp$yield)), type="n", axes=FALSE,
        xlab="", ylab="" )
  lines( rp$ssb, rp$yield, lwd=2 )

  points( rp$ssbF0,   rp$yieldF0,   cex=.CEXSYM8, bg=colF0,   pch=21 )
  points( rp$ssbF01,  rp$yieldF01,  cex=.CEXSYM8, bg=colF01,  pch=21 )
  points( rp$ssbFcra, rp$yieldFcra, cex=.CEXSYM8, bg=colFcra, pch=21 )
  points( rp$ssbFmax, rp$yieldFmax, cex=.CEXSYM8, bg=colFmax, pch=21 )
  points( rp$ssbFmsy, rp$yieldFmsy, cex=.CEXSYM8, bg=colFmsy, pch=21 )
  points( rp$ssbF40,  rp$yieldF40,  cex=.CEXSYM8, bg=colF40,  pch=21 )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=2.5, cex=1.0, "SSB" )
  mtext( side=2, line=3.5, cex=1.0, "Yield" )
  box()
  mtext( side=3, line=0, cex=1.2, outer=TRUE, "Reference Points" )
}

.calcRefCurvesFromRep <- function( obj, parList = "simCtlFile.txt" )
{
  
  # # Can we continue this and produce the base simCtlFile rapidly??

  tmp <- .makeParListFromRep(obj = obj, parFile = parList)
  refPoints <- deref( calcRefPoints( as.ref(tmp) ) )
          
  return(refPoints)
}     # .calcRefCurvesFromRep function


# Selectivity!

#    // Length-at-50% selectivity: fishery
#    mcoutSel_F  << row(S50_gt_a,1) <<
#    mcoutSel_F  << row(S95_gt_a,1) <<
#    mcoutSel_F  << row(S50_gt_d,1) <<
#    mcoutSel_F  << row(S95_gt_d,1) << endl;
#    // Length-at-50% selectivity: RV survey
#    mcoutSel_RV  << row(S50_gt_a,5) <<
#    mcoutSel_RV  << row(S95_gt_a,5) <<
#    mcoutSel_RV  << row(S50_gt_d,5) <<
#    mcoutSel_RV  << row(S95_gt_d,5) << endl;
#    // Length-at-50% selectivity: Halibut survey
#    mcoutSel_HS  << row(S50_gt_a,6) <<
#    mcoutSel_HS  << row(S95_gt_a,6) << endl;

#FUNCTION calc_sel_gta
#  // This function creates selectivity curves by fishery while
#  // allowing for changes over time in the parameters.
#  // Time-varying selectivity is modelled using a random-walk
#  // on S50_a and each of the 3 possible offsets. If dome-shaped
#  // selectivity is assumed, then there are 3 offsets, and
#  // only 1 for asymptotic.
#  // IMPORTANT: if using constant selectivity over time, make sure
#  // that all the deviation parameters involving "t" are == 0.
#  rw_S50_a.initialize(); // random-walk deviations on ascending S50
#  rw_S95_a.initialize(); // random-walk deviations on ascending S95
#  rw_S95_d.initialize(); // random-walk deviations on descending S95
#  rw_S50_d.initialize(); // random-walk deviations on descending S50
#  // Construct rw selectivity parameters for each fishery
#  for( int g=1; g<=nFisheries; g++ )
#  {
#    // parameters at t==1 are constant offsets from S50_a
#    rw_S50_a(g,1) = dev_S50_gt_a(g,1);
#    rw_S95_a(g,1) = dev_S95_g_a(g); // offset to S50_a
#    rw_S95_d(g,1) = dev_S95_g_d(g); // offset to S95_a
#    rw_S50_d(g,1) = dev_S50_g_d(g); // offset to S95_d
#    // for each year, build parameter rw from previous year
#    for( int t=2; t<=nT; t++ )
#    {
#      rw_S50_a(g,t) = rw_S50_a(g,t-1) + dev_S50_gt_a(g,t);
#      rw_S95_a(g,t) = rw_S95_a(g,t-1) + dev_S95_gt_a(g,t);
#      rw_S95_d(g,t) = rw_S95_d(g,t-1) + dev_S95_gt_d(g,t);
#      rw_S50_d(g,t) = rw_S50_d(g,t-1) + dev_S50_gt_d(g,t);
#    }
#    // If selectivity is assumed constant over time, then
#    // all these rw matrices should equal 0
#  }
#  // Initialize selectivity function derived parameters
#  S50_gt_a.initialize(); S95_gt_a.initialize();
#  S50_gt_d.initialize(); S95_gt_d.initialize();
#
#  // Initialize selectivity-at-age/sex arrays
#  sel_gta_m.initialize();sel_gta_f.initialize();
#
#  // Build selectivity-at-age for each fishery
#  {
#    for( int t=1; t<=nT; t++ )
#    {
#      // time-varying selectivity combines ascending (a) and
#      // descending (d) sigmoid functions
#      // Start with ascending limb (a)
#      if( t==1 )
#      {
#        // S50
#        S50_gt_a(g,t) = mfexp( log_S50_g_a(g) );
#        // Ascending limb S95 is offset from S50
#        S95_gt_a(g,t) = S50_gt_a(g,t) + mfexp( dev_S95_g_a(g) );
#      }
#      if( t>1 )
#      {
#        // Multiply by the random-walk deviation for this year
#        S50_gt_a(g,t) = S50_gt_a(g,t-1)*mfexp(rw_S50_a(g,t) );
#        // Ascending limb S95 is offset from S50, but additive
#        // to ensure S95>S50 always
#        S95_gt_a(g,t) = S50_gt_a(g,t) + mfexp( rw_S95_a(g,t)) - 1.;
#      } // substracting 1 to make sure S95 is constant at initial
#        // value if time-varying selectivity is off.
#
#      // If domed selectivity, then descending limb (d) is further
#      // offset from S95a. Not strictly necessary to have S95d>S95a,
#      // but this way avoids a spike-shaped selectivity, which would
#      // cause other problems.
#      if( domeSelectivity(g) )
#      {
#        // S95d offset from S95a
#        S95_gt_d(g,t) = S95_gt_a(g,t) + mfexp( rw_S95_d(g,t) ) - 1.;
#        // S50d offset from S95d
#        S50_gt_d(g,t) = S95_gt_d(g,t) + mfexp( rw_S50_d(g,t) ) - 1.;
#      }
#      else
#      {
#        // Not using dome, so get asymptotic by matching ascending
#        // and descending curves exactly
#        S95_gt_d(g,t) = S50_gt_a(g,t); // these look odd, but
#        S50_gt_d(g,t) = S95_gt_a(g,t); // work in selectivity func.
#      } // end dome check
#    } // end t-loop
#
#    // Selectivity function parameters are now ready to be used in
#    // computing selectivity curves for each year.
#    // Note here that selectivity is actually length-based given
#    // mean length-at-age, so male and female selectivity-at-age
#    // differs.
#    for( int t=1; t<=nT; t++ )
#    {
#      // males
#      dvariable tmpA  = log(19.)/( S95_gt_a(g,t) - S50_gt_a(g,t) );
#      dvar_vector xA  = (-1.)*tmpA*(lenAge_m - S50_gt_a(g,t) );
#      sel_gta_m(g)(t) = 1./(1. + mfexp(xA));
#      // females
#      xA = (-1.)*tmpA*(lenAge_f - S50_gt_a(g,t) );
#      sel_gta_f(g)(t) = 1./( 1. + mfexp(xA));
#
#      // Compute descending limb curve, multiply ascending x descending,
#      // and re-scale by maximum value.
#      if( domeSelectivity(g) )
#      {
#        // males
#        dvariable tmpD  = log(19.)/( S95_gt_d(g,t) - S50_gt_d(g,t) );
#        dvar_vector xD  = (-1.)*tmpD*(lenAge_m - S50_gt_d(g,t) );
#        sel_gta_m(g)(t) = elem_prod(sel_gta_m(g)(t),1./(1.+mfexp(xD)));
#        sel_gta_m(g)(t) = sel_gta_m(g)(t)/max(sel_gta_m(g)(t));
#        // females
#        xD   = (-1.)*tmpD*(lenAge_f - S50_gt_d(g,t) );
#        sel_gta_f(g)(t) = elem_prod( sel_gta_f(g)(t),1./(1.+mfexp(xD)));
#        sel_gta_f(g)(t) = sel_gta_f(g)(t)/max(sel_gta_f(g)(t));
#      } // end dome check
#    } // end loop over year
#  } // end loop over gear
