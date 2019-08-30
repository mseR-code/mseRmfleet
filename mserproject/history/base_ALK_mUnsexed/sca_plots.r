#-----------------------------------------------------------------------#
#-----------------------------------------------------------------------#
#-- Plots for scal Atlantic Halibut models                                   --#
#-- Most of these functions are based on mseR plotting conventions
# Notes:
#  1. Need to plot the selectivity functions by fishery and year. 
#     Need a 3-panel figure, one plot for each fishery. Put
#     multiple years on same graph, but fisheries on different graphs.
#  2. Need to output Bayesian MCMC samples in scal.tpl, then make a 
#     function here to read them and produce summary tables. There is
#     some code already here to do much of that, but will need
#     customizing from crab to halibut.
#-----------------------------------------------------------------------#
rm( list=ls() )
source("mseRtools.r")
source("scal_globals.r")
#source("mcmcpairs.r")
require(MCMCpack)

makeRetroList <- function( years=NULL )
{
  if( is.null(years) )
    years <- seq(from=2007, to=2013, by=1 )
  pinString <- "-ainp scal_base.pin"
  exeFile   <- "./scal "
  i <- 0
  for( t in years )
  {
    cat( t, file="scal.dat" )
    if( .Platform$OS.type=="unix" )
    { 
      system( command=paste( exeFile," ",pinString," -nohess -maxfn 2000",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe ",pinString," -nohess -maxfn 2000",sep=""), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
    }
    i <- i + 1
    repObj[[i]] <- lisread("scal.rep")
  }
  return( repObj )
}
plotRetro <- function(  )
{
  load("retroList.RData")
  # Base plot on full dataset
  firstYear <- 1970
  lastYear  <- 2013
  baseRep <- retroList[[7]]
  xLim    <- c(firstYear,lastYear)
  yLim    <- c(0,baseRep$SSB0)
  plot( firstYear:lastYear, baseRep$SSBt/1000., xlim=xLim, ylim=c(0,10),
        las=1, type="l", xlab="",ylab="", lwd=2 )
  for( i in 1:6 )
  {
    retroYear <- retroList[[i]]$retroYear
    tSteps    <- baseRep$nT - (baseRep$retroYear-retroYear)
    lines( firstYear:retroYear, retroList[[i]]$SSBt[1:tSteps]/1000.,
            col=i )
  }
  mtext( side=1, text="Year", outer=F, line=2 )
  mtext( side=2, text="Spawning biomass (000s tonnes)", outer=F, line=2 )


}
# This rep file loading can be improved by (i) adding some time/date
# info to the first line of the rep file, (ii) reading the first line
# using scan(), (iii) letting the scal_plots user know when the rep 
# file was created and perhaps whether it is a converged fit.
repFileName <- "sableopmod.rep"
if( file.exists(repFileName) )
{
  repObj <- lisread(repFileName)
  cat( paste(repFileName," successfully imported...","\n",sep="") )  
}
if( !file.exists(repFileName) )
{
  cat("No .rep file to import...","\n")  
}

#------------------------------------------------------------------------------#
#-- Usage
#   These plots all require a list object argument ("obj") defined
#      by the ADMB rep file. For gbcod, the list will be "scal.rep", which
#      can be input and formatted to a list via the R command line:
#      > repObj <- lisread( "scal.rep" )
#   The above list in "repFileName" can then be used as the arguments to the 
#   function.
#      An example for plotting Mt output is:
#      > plotAll_M( obj=repObj )
#   IMPORTANT: if the ADMB output is changed, then repObj needs to be re-created.
#      Otherwise, the output plots will use the old repObj stored in the current
#      R environment.
# catch-equation w discarding
# > f<-expression(s*F*p*B*(1-exp(-(s*F*(p+(1-p)*D)+M)))/(s*F*(p+(1-p)*D)+M))
# derivative wrt F
# > D(f,"F")
# (s * p * B * (1 - exp(-(s * F * (p + (1 - p) * D) + M))) + s * 
#     F * p * B * (exp(-(s * F * (p + (1 - p) * D) + M)) * (s * 
#     (p + (1 - p) * D))))/(s * F * (p + (1 - p) * D) + M) - s * 
#     F * p * B * (1 - exp(-(s * F * (p + (1 - p) * D) + M))) * 
#     (s * (p + (1 - p) * D))/(s * F * (p + (1 - p) * D) + M)^2
# >

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
makeSummaryTable <- function( repFolder="/repFiles" )
{
  repFileList <- c("scal_M0.1est.rep","scal_M0.1fix.rep","scal_M0.15est.rep",
                  "scal_M0.15fix.rep","scal_M0.20est.rep","scal_M0.20fix.rep",
                  "scal_M0.2F0.1fix.rep","scal_M0.15fix_dM.1.rep",
                  "scal_M0.15fix_dM.8.rep")

  fixedReps <- c(2,4,6,7)
  tableFile <- "repTable.txt"

  # Report table objects
  likeM <- vector(length=length(repFileList))
  tau_RV<- vector(length=length(repFileList))
  tau_HS<- vector(length=length(repFileList))

  avgR <- vector(length=length(repFileList))
  SSB0 <- vector(length=length(repFileList))

  SSB2013 <- vector(length=length(repFileList))
  legalBt <- vector(length=length(repFileList))

  muM_m <- vector(length=length(repFileList))
  muM_f <- vector(length=length(repFileList))

  F2013 <- vector(length=length(repFileList))
  U2013 <- vector(length=length(repFileList))

  for( i in 1:length(repFileList) )
  {
    # build matrix of results
    d       <- paste(getwd(),repFolder,"/",repFileList[i],sep="")
    repFile <- lisread(d)
    likeM[i] <- ifelse(repFile$mPrior>1.,repFile$mPrior,NA)
    tau_RV[i]<- repFile$tauIndex[1]
    tau_HS[i]<- repFile$tauIndex[2]

    avgR[i] <- repFile$avgR
    SSB0[i] <- repFile$SSB0

    SSB2013[i] <- repFile$SSBt[repFile$nT]
    legalBt[i] <- repFile$legalBt[repFile$nT]

    muM_m[i] <- repFile$M[1]
    muM_f[i] <- repFile$M[2]

    F2013[i] <- repFile$Ftg[repFile$nT]
    U2013[i] <- repFile$Utg[repFile$nT]
  }
  # write the table
  for( i in 1:length(repFileList) )
  {
    if( i==1 )
      app <- F
    else
      app <- T

    writeVec <- c(likeM[i],tau_RV[i],tau_HS[i],
                avgR[i],SSB0[i],SSB2013[i],legalBt[i],
                muM_m[i], muM_f[i],F2013[i],U2013[i] 
                )
    cat( writeVec,"\n", file=tableFile, sep=" ", append=app )

  }


}

plotMCMC <- function()
{
  source("mcmc.r")
}

plotHCR <- function( obj, dep_Bmsy=0.35, lcp=0.4, ucp=0.8 )
{
  nT <- obj$nT
  # unfished biomass used as biological ref pt base
  SSB0 <- obj$SSB0
  # estimated SSB time-series
  SSB  <- obj$SSBt
  # estimated fishing mortality
  Ft   <- -log(1.-obj$Utg)
  # estimated natural mortality - female
  M_f <- obj$M[2]
  # implied control points (set as args above)
  lcp_B <- SSB0*dep_Bmsy*lcp
  ucp_B <- SSB0*dep_Bmsy*ucp

  xLim <- c(0, max(SSB) )
  yLim <- c(0, max(Ft) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    points( SSB, Ft, pch=19, col="gray" )
    lines(  SSB, Ft, col="gray" )
    arrows( x0=SSB[1:(nT-1)],x1=SSB[2:nT],
            y0=Ft[1:(nT-1)],y1=Ft[2:nT],
            length=0.1, col="gray"
          )

    points( SSB[nT], Ft[nT], pch=19, col="black" )

    #segments(x0=lcp_B, x1=lcp_B, y0=0, y1=yLim[2], lty="dashed")
    #segments(x0=ucp_B, x1=ucp_B, y0=0, y1=yLim[2], lty="dashed")

    #points( SSB0, 0., cex=1.5, pch=19 )

    abline( h=M_f, lty="dashed" )

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mtext( side=1, line=2.5, cex=.CEXLAB, outer=FALSE, "Spawning stock biomass" )
  mtext( side=2, line=3, cex=.CEXLAB, outer=FALSE, "Fishing mortality rate" )
  
  #panLegend( 0.5,.4, legTxt="F = M_f",
  #           bty="n" )
}

plotLenResiduals <- function( repObj )
{

  plot( x=NULL, y=NULL, xlim=c(1,nT), ylim=c(1,nAges),
        xlab="Year",ylab="Age", main="residPropAge" )
  resCol <- vector("numeric")
  for( t in 1:nT )
  {
    resid <- 5.*(result$obsPropAge[t,]-result$predPropAge[t,])
    resCol[resid > 0] <- "blue"
    resCol[resid <= 0] <- "red"
    points( rep(t,nAges),c(1:nAges), cex=resid, col=resCol )  
  }
}
  
# plotBiomass
#   plot Total, Legal, and SSB time-series.
#   Could add a vector specifying which series
#   to plot in case user doesn't want them all.
plotBiomass <- function( obj, scaler=1.e3 )
{
  nT       <- obj$nT
  lenAge_f <- obj$lenAge_f
  lenAge_m <- obj$lenAge_m

  Bta_m <- obj$Bta_m
  Bta_f <- obj$Bta_f
  totalBt <- rowSums(Bta_m + Bta_f)

  # Use length-at-age to determine whether each element of
  # the biomass-at-age is legal and return legal biomass
  # only
  setLegal <- function( bvec,type="m",sizeLimit=15 )
  {
    legal <- rep(0.,length(bvec))
    lenAge <- switch(type,
                     m = lenAge_m,
                     f = lenAge_f
                     )
    legal[ lenAge >= sizeLimit ] <- 1.
    # return the vector of legal biomass-at-age
    return( bvec*legal )
  }
  # subset the biomass-at-age matrices for the years in which
  # the size limit was active
  idxSizeLimitYear <- obj$sizeLimYear-.INITYEAR+1
  tmpB_m  <- Bta_m[idxSizeLimitYear:nT,]
  
  # compute legal biomass-at-age by year
  tmpB_m  <- apply( X=tmpB_m, MARGIN=1, FUN=setLegal, 
                    type="m", sizeLimit=obj$sizeLimit[1] )
  
  # replace original biomass-at-age with legal amounts
  Bta_m[idxSizeLimitYear:nT,] <- t(tmpB_m)
  # repeat for females
  tmpB_f  <- Bta_f[idxSizeLimitYear:nT,]
  tmpB_f   <- apply( X=tmpB_f, MARGIN=1, FUN=setLegal, 
                    type="f", sizeLimit=obj$sizeLimit[1] )
  Bta_f[idxSizeLimitYear:nT,] <- t(tmpB_f)
  legalBt <- rowSums( Bta_m + Bta_f )

  # reset graphical parameters.
  graphics.off()
  xLim   <- c( .INITYEAR, .LASTYEAR )
  yLimit <- c( 0, max(totalBt) )/scaler
  plot( xLim, yLimit, type="n", axes=F, xlab="Year", ylab="" )

  # Add three biomass time-series
  lines( c( xLim[1]:xLim[2] ), legalBt/scaler, col="black", lty="dashed", lwd=2 )
  lines( c( xLim[1]:xLim[2] ), totalBt/scaler, col="black", lty="solid", lwd=2 )
  lines( c( xLim[1]:xLim[2] ), obj$SSBt/scaler, col="black", lty="dotted", lwd=2 )
  
  #points( xLim[1], obj$SSB0/scaler, pch=19, cex=3 )
  xPos <- seq( .INITYEAR,.LASTYEAR, 2 )
  xLabs <- paste( xPos )
  axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabs )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  panLegend( 0.1,1,legTxt="Total",lty="solid",lwd=2, bty="n" )
  panLegend( 0.1,.95,legTxt="Legal",lty="dashed",lwd=2, bty="n" )
  panLegend( 0.1,.90,legTxt="Spawning",lty="dotted",lwd=2, bty="n" )

  mtext( side=2, line=2.5, text="Biomass (tonnes)", cex=.CEXAXIS2)
  box(bty="l")
   
} # end plotBiomass

# plotSelectivity
#   Plot the selectivity-at-age curves by fishery
#   for a given type: m=male, f=female
plotSelectivity <- function( obj, type="m" )
{

  nT <- 1
  ageClasses <- obj$ages
  nSelBlocks <- obj$nT
  nFisheries <- obj$nFisheries
  fNames <- c("LL.3","LL.4","OT.3","OT.4","RV_4VWX","HS")
  lType      <- c("solid", "solid", "dotted", "dotted","dashed","dashed" )
  lCol       <- c("black","blue","black","blue","green","red")

  sel_1       <- switch(type,
                      m = obj$sel_gta_m1,
                      f = obj$sel_gta_f1)
  sel_2       <- switch(type,
                      m = obj$sel_gta_m2,
                      f = obj$sel_gta_f2)
  sel_3       <- switch(type,
                      m = obj$sel_gta_m3,
                      f = obj$sel_gta_f3)
  sel_4       <- switch(type,
                      m = obj$sel_gta_m4,
                      f = obj$sel_gta_f4)
  sel_5       <- switch(type,
                      m = obj$sel_gta_m5,
                      f = obj$sel_gta_f5)
  sel_6       <- switch(type,
                      m = obj$sel_gta_m6,
                      f = obj$sel_gta_f6)
  par( mfrow=c(3,2),oma=c(3,3,1,1),mar=c(2,2,1,1) )
  for( g in 1:nFisheries )
  {
    nSelLines <- 0
    for( j in 1:nT )
    {
      nSelLines <- nSelLines+1
      selName <- paste("sel_",g,"[",j,",]",sep="")
      #browser()
      selObj  <- eval(parse(text=selName))

      if( j==1 )
      {  
        plot( ageClasses, selObj, type="l", lty="solid", 
              lwd=2, bty="n", axes=F, xlab="",
              ylab="", ylim=c(0,1) )
        axis(side=1)
        axis(side=2,las=1)
      }
      else
      {
        lines( ageClasses, selObj, lwd=2 )  
      }
    }  
    panLegend( 0.5,1.-0.05*nSelLines,
               legTxt=paste(fNames[g],1970+j-1,sep=""), bty="n", 
               cex=0.6 )
  }
  mtext( side=1, line=1.5, outer=TRUE, text="Age", cex=1)
  mtext( side=2, line=1.5, outer=TRUE, text="Selectivity",cex=1)

} # end plotSelectivity

plotWtAge <- function( obj, label=NULL,
                   gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{

  ages   <- obj$ages
  Wta_m    <- obj$wtAge_m*1.e3 # wtAge is in tonnes
  Wta_f    <- obj$wtAge_f*1.e3 # wtAge is in tonnes
  maxWt  <- max(max(Wta_m,Wta_f))
  maxAge <- max(ages)
  xRange <- c( 0,maxAge )

  matYr <- matrix(NA, nrow=35, ncol=35, byrow=T )
  matWt <- matrix(NA, nrow=35, ncol=35, byrow=T )
  for( cohRow in 1:35 )
  {
    maxCol <- min( cohRow + maxAge -1, ncol(matWt) )
    a <- 0
    for( j in cohRow:maxCol )
    {
      a <- a + 1
      matYr[ cohRow, j ] <- (.INITYEAR + cohRow -1) + a - 1
      matWt[ cohRow, j ] <- Wta[ cohRow, a ]
    }
  }
  xLim <- c( .INITYEAR,.LASTYEAR )
  yLim <- c( 0,max(Wta) )
  
  # Weight at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  for( t in 1:nrow(matWt) )
  {
    lines( matYr[t,], matWt[t,], lty=1, lwd=1 )
    points( matYr[t,], matWt[t,], bg="white", col="black", cex=1, pch=21 )
  }
  for( w in seq(2,14,by=2) )
    abline( h=w, lty="dotted", lwd=0.5, col="gray" )
  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.CEXLAB, outer=FALSE, "Cohort/Year" )
  mtext( side=2, line=2, cex=.CEXLAB, outer=FALSE, "Weight (kg)" )
}     # .plotWgtAtAge function

plotBta <- function( repObj, doType="mat" )
{

  Bta_f <- repObj$Bta_f[44,]
  Mta_f <- repObj$matAge_f
  ages  <- 1:30
  xLim  <- c(0,max(ages))
  yLimit <- c(0,max(Bta_f))/1000.
  xVals <- ages
  delta <- 0.2


  plot( xLim, yLimit, type="n", axes=F, xlab="", ylab="" )
  # Catch bars.
  rect( xVals-delta, 0, xVals+delta, Bta_f/1000., col="gray88" )

  if( doType=="mat" )
  {
    ogive <- repObj$matAge_f
    lines( ages, ogive*yLimit[2], lwd=2 )
  }
  if( doType=="sel")
  {
    ogive <- repObj$sel_gta_f3[1,]
    lines( ages, ogive*yLimit[2], lwd=2, col="blue" )
  }
  if( doType=="b" )
  {
    ogive <- repObj$matAge_f
    lines( ages, ogive*yLimit[2], lwd=2 )
    ogive <- repObj$sel_gta_f3[1,]
    lines( ages, ogive*yLimit[2], lwd=2, col="blue" )
  }
  yPos  <- seq( 0,1, length=30 )
  yLabs <- paste( yPos )


  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  #axis( side=3, cex.axis=.CEXAXIS2, las=1, at=yPos, labels=yLabs)

  mtext( side=1, line=2.5, cex=.CEXLAB, outer=FALSE, "Age" )
  mtext( side=2, line=2.5, cex=.CEXLAB, 
        outer=FALSE, "Biomass-at-age (1000s tonnes)" )

  # Various SSBs
  # 7+
  B5 <- floor( sum( Bta_f[5:30] ) )
  B6 <- floor( sum( Bta_f[6:30] ) )
  B7 <- floor( sum( Bta_f[7:30] ) )
  B8 <- floor(sum( Bta_f[8:30] ) )
  B9 <- floor(sum( Bta_f[9:30] ) )

  panLegend( 0.6, 0.85, legTxt=paste("B_5+ = ", B5, sep=""), bty="n" )
  panLegend( 0.6, 0.8, legTxt=paste("B_6+ = ", B6, sep=""), bty="n" )
  panLegend( 0.6, 0.75, legTxt=paste("B_7+ = ", B7, sep=""), bty="n" )
  panLegend( 0.6, 0.7, legTxt=paste("B_8+ = ", B8, sep=""), bty="n" )
  panLegend( 0.6, 0.65, legTxt=paste("B_9+ = ", B9, sep=""), bty="n" )

}
# plotUt
# Plots legal exploitation rate
plotUt <- function( obj, plotCatch=F )
{
  graphics.off()
  par( mar=c(6,6,2,2) )
  Ut        <- obj$Utg
  nT        <- obj$nT
  katch     <- rowSums(obj$landCatchMatrix[,3:6])
  
  xLim  <- c( .INITYEAR, .LASTYEAR )
  xVals <- c( xLim[1]:xLim[2] )
  xPos  <- seq( .INITYEAR,.INITYEAR+nT-1, 2 )
  xLabs <- paste( xPos )
  delta <- 0.2
  
  # Catch plot
  if( plotCatch )
  {
    layout(matrix(c(1,1,2,2,2,2),3,2,byrow=T))  
    yLimit <- c( 0, max(katch) )
    plot( xLim, yLimit, type="n", axes=F, xlab="", ylab="" )
    
    # Catch bars.
    rect( xVals-delta, 0, xVals+delta, katch, col="gray88" )
    axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabs )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )

    mtext( side=1, line=3, cex=.CEXLAB, outer=TRUE, "Year" )
    mtext( side=2, line=4, cex=.CEXLAB, outer=FALSE, "Catch" )    
  }
  
  # Utg (legal exploitation rate) plot
  yLimit <- c( 0, max(Ut) )
  plot( xLim, yLimit, type="n", axes=F, xlab="", ylab="" )

  lines( xVals, Ut,  col="black", lty="solid", lwd=1 )
  points( xVals, Ut, bg="black", cex=1.2, pch=21 )


  mTag <- 0.14499809 
  xTag <- seq(2007,2013,by=1)
  fTag <- c(0.13081265, 0.18729876, 0.12574487, 0.10534638,
            0.06666605, 0.12068382, 0.07054571)

  sdTag <- c(0.021828412, 0.019019445, 0.013729921, 0.015431054,
             0.010111959, 0.019282025, 0.011452970 )

  points( xTag, fTag, pch=23, bg="blue" )

  tPlus  <- fTag + 2.*sdTag
  tMinus <- fTag - 2.*sdTag
  x0 <- xTag
  x1 <- xTag
  y0 <- tMinus
  y1 <- tPlus
  segments( x0, y0, x1, y1 )


  panLegend( .6,.9, legTxt="SCAL", pch=19, lty="solid", bty="n" )
  panLegend( .6,.8, legTxt="TAGGING", pch=23, pt.bg="blue", bty="n" )

  
  axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabs )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  mtext( side=1, line=3, cex=.CEXLAB, outer=FALSE, "Year" )
  mtext( side=2, line=4, cex=.CEXLAB, outer=FALSE, "Legal exploitation rate" )
  
  box()

}     # plotUt function

# plotLenFit
# Plots observed length-frequency (bars) and fitted model length-frequency
# for each year. A separate page is generated for each fishery/survey
plotLenFit <- function( obj, modelCheck=F, doType="m", resGear=1, png=F )
{
  # the model uses pl_m(g)(lengths)(t)
  # data are lenObsProp_m(g)

  firstBin <- obj$firstBin
  lastBin  <- obj$lastBin
  minLen   <- obj$minLen
  maxLen   <- obj$maxLen
  nGear    <- 6
  nLens    <- 54 #nrow( pl_m[[1]] )
  nT       <- 44 #ncol( pl_m[[1]] )
  lenFirstYear <- obj$lenFirstYear
  lenLastYear  <- obj$lenLastYear

  # Model: pl_m,f,c are lists of arrays with dimensions( nLens,nT ).
  pl      <- vector("list")
  pl[[1]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[1]] <- switch(doType,
                    m = obj$pl_m1,
                    f = obj$pl_f1,
                    c = obj$pl_c1
                    )
  pl[[2]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[2]] <- switch(doType,
                    m = obj$pl_m2,
                    f = obj$pl_f2,
                    c = obj$pl_c2
                    )
  pl[[3]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[3]] <- switch(doType,
                    m = obj$pl_m3,
                    f = obj$pl_f3,
                    c = obj$pl_c3
                    )
  pl[[4]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[4]] <- switch(doType,
                    m = obj$pl_m4,
                    f = obj$pl_f4,
                    c = obj$pl_c4
                    )
  pl[[5]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[5]] <- switch(doType,
                    m = obj$pl_m5,
                    f = obj$pl_f5,
                    c = obj$pl_c5
                    )
  pl[[6]] <- matrix(NA, nrow=nLens,ncol=nT)
  pl[[6]] <- switch(doType,
                    m = obj$pl_m6,
                    f = obj$pl_f6,
                    c = obj$pl_c6
                    )
  
  lenClasses <- vector("list")
  lenClasses[[1]] <- seq( minLen[1], maxLen[1], by=obj$binSize )
  lenClasses[[2]] <- seq( minLen[2], maxLen[2], by=obj$binSize )
  lenClasses[[3]] <- seq( minLen[3], maxLen[3], by=obj$binSize )
  lenClasses[[4]] <- seq( minLen[4], maxLen[4], by=obj$binSize )
  lenClasses[[5]] <- seq( minLen[5], maxLen[5], by=obj$binSize )
  lenClasses[[6]] <- seq( minLen[6], maxLen[6], by=obj$binSize )

  # These are the lengths.
  xAxisLengths    <- obj$lfBins
  xLim <- range( xAxisLengths )
  
  # OBS: lenObsProp is a 3-dim array with dimensions( nGear,nLens,nT ).
  lenObsProp      <- array( 0,dim=c(nGear,nLens,nT) )
  lenObsProp[1,firstBin[1]:lastBin[1],] <- switch(doType,
                                                  m = obj$lenObsProp_m1,
                                                  f = obj$lenObsProp_f1,
                                                  c = obj$lenObsProp_c1
                                                  )
  lenObsProp[2,firstBin[2]:lastBin[2],] <- switch(doType,
                                                  m = obj$lenObsProp_m2,
                                                  f = obj$lenObsProp_f2,
                                                  c = obj$lenObsProp_c2
                                                  )
  lenObsProp[3,firstBin[3]:lastBin[3],] <- switch(doType,
                                                  m = obj$lenObsProp_m3,
                                                  f = obj$lenObsProp_f3,
                                                  c = obj$lenObsProp_c3
                                                  )
  lenObsProp[4,firstBin[4]:lastBin[4],] <- switch(doType,
                                                  m = obj$lenObsProp_m4,
                                                  f = obj$lenObsProp_f4,
                                                  c = obj$lenObsProp_c4
                                                  )
  lenObsProp[5,firstBin[5]:lastBin[5],] <- switch(doType,
                                                  m = obj$lenObsProp_m5,
                                                  f = obj$lenObsProp_f5,
                                                  c = obj$lenObsProp_c5
                                                  )
  lenObsProp[6,firstBin[6]:lastBin[6],] <- switch(doType,
                                                  m = obj$lenObsProp_m6,
                                                  f = obj$lenObsProp_f6,
                                                  c = obj$lenObsProp_c6
                                                  )
  # Starting Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  startBins <- matrix( 0,nrow=nGear,ncol=nT )
  startBins <- switch(doType,
                      m = obj$startBin_m,
                      f = obj$startBin_f,
                      c = obj$startBin_c
                      )
  # Ending Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  endBins <- matrix( 0,nrow=nGear,ncol=nT )
  endBins <- switch(doType,
                    m = obj$endBin_m,
                    f = obj$endBin_f,
                    c = obj$endBin_c
                    )
  # Starting Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  nObsBins <- matrix( 0,nrow=nGear,ncol=nT )
  nObsBins <- switch(doType,
                      m = obj$nObsBins_m,
                      f = obj$nObsBins_f,
                      c = obj$nObsBins_c
                      )
  # Get those -1 values outta there for plotting...convert to NA
  setMissing <- function( vec )
  {
    n <- length(vec)
    vec[ vec<0 ] <- NA
    return(vec)
  }
  for( i in 1:dim(lenObsProp)[1] )
    lenObsProp[i,,] <- apply( X=lenObsProp[i,,], MARGIN=2,FUN=setMissing )
 
 
  # These are the fitted len proportions.
  yLim <- NULL
  if ( is.null(yLim) )
    yLim <- c( 0,1 )

  for ( g in 1:nGear )
  {  
  cat("gear is ",g,"\n") 

    if ( g > 1 )
    {
      dev.new()
    }
    newDev <- 1
    
    yLim    <- c( 0, 0.25 )
    sumProp <- colSums( lenObsProp[g,,], na.rm=TRUE )      
    nPanels <- obj$lenLastYear[g] - obj$lenFirstYear[g] + 1

    myMar   <- c( 1.5,1.25,0.5,0.5 )
    par( oma=.OMA, mar=myMar, mfcol=c(5,5) )

    # Years where the sum is not 0, i.e., there are fitted lens.
    idxPlot <- c(1:nT)[sumProp>0.0]
    idxPlot <- seq(obj$lenFirstYear[g],obj$lenLastYear[g],by=1) 
    idxPlot <- idxPlot[sumProp[idxPlot]!=0.0]
    
    # List to hold the residuals
    if( g==resGear )
    {
      resids      <- vector("list",length=length(idxPlot)+3)
      cumulObs    <- rep(0,length(lenClasses[[g]]))
      cumulPreds  <- rep(0,length(lenClasses[[g]]))
      cumulResids <- rep(0,length(lenClasses[[g]]))
    }
    delta <- 1
    iPlot <- 0
    nPlot <- 0
    for ( i in idxPlot )
    {
      nPlot <- nPlot + 1
      if( (nPlot == 26) )
      {
        mtext( side=3, line=0, cex=.CEXLAB, outer=TRUE, 
        switch(g,
               "LL.3",
               "LL.4",
               "OT.3",
               "OT.4",
               "RV_4VWX",
               "Halibut Survey" 
               )
              ) 

        dev.new()
        newDev <- 0
        myMar   <- c( 1.5,1.25,0.5,0.5 )
        par( oma=.OMA, mar=myMar, mfcol=c(5,5) )
      }
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      abline( h=0 )

      # The observed length proportions.
      rect( lenClasses[[g]]-delta, 0, lenClasses[[g]]+delta, 
            lenObsProp[g,,i], col="white",lwd=0.5 )
      
      # The fitted length proportions.      
      lines( lenClasses[[g]], pl[[g]][,i], lty=1, lwd=2, col="red" )
      points( lenClasses[[g]], pl[[g]][,i], bg="white", 
              col="black", cex=0.5, lwd=1, pch=21 )
      
      if( g==resGear )
      {
        iPlot <- iPlot + 1
        resids[[iPlot]]$points    <- vector("numeric",length(lenClasses[[g]]))  
        resids[[iPlot]]$stdResids <- vector("numeric",length(lenClasses[[g]]))  
        resids[[iPlot]]$std       <- vector("numeric",length(lenClasses[[g]]))  
        
        resids[[iPlot]]$points    <- lenObsProp[g,,i] - pl[[g]][,i]
        resids[[iPlot]]$std       <- sd( resids[[iPlot]]$points, na.rm=TRUE )  
        resids[[iPlot]]$stdResids <- resids[[iPlot]]$points/resids[[iPlot]]$std
        resids[[iPlot]]$stdResids[is.na(resids[[iPlot]]$stdResids)] <- 0
        resids[[iPlot]]$year    <- paste( i+.INITYEAR-1,",",i,sep="") 

        tmp <- lenObsProp[g,,i]
        tmp[is.na(tmp)] <- 0.
        cumulObs <- cumulObs + tmp
        
        tmp <- pl[[g]][,i]
        tmp[is.na(tmp)] <- 0.
        cumulPreds <- cumulPreds + tmp

        tmp <- resids[[iPlot]]$points
        tmp[ is.na(tmp) ] <- 0.
        cumulResids <- cumulResids + tmp
        #browser()       
      }
      resids[[iPlot+1]]$cumulObs   <- cumulObs
      resids[[iPlot+2]]$cumulPreds <- cumulPreds
      resids[[iPlot+3]]$cumulResids<- cumulResids
  
      if ( .USEYEAR )
      { 
        xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
        panLab( 0.5, 0.9, cex=1.0, paste( i+.INITYEAR-1,",",i,sep="") )
      }
      else      
        panLab( 0.5, 0.9, cex=1.0, paste( "Year",i ) )
      
      mfg <- par( "mfg" )
      
      axis( side=1, cex.axis=.CEXAXIS, pos=0 )
      
      if ( mfg[2]==1 )
        axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
      else
        axis (side=2, labels=FALSE )
        
    }     # i=1,..,nT
  mtext( side=3, line=0, cex=.CEXLAB, outer=TRUE, 
  switch(g,
         "LL.3",
         "LL.4",
         "OT.3",
         "OT.4",
         "RV_4VWX",
         "Halibut Survey" 
         )
        ) 
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Length class (cm)" )
    mtext( side=2, line=2, cex=.CEXLAB, outer=TRUE, "Proportion-at-length" )

  }
  return( resids )
}     # plotLenFit function


# plotPropFemale
# Plots observed proportion female-at-length and fitted
# proportions.
plotPropFemale <- function( obj, modelCheck=F, doType="f", resGear=1, png=F )
{
  # read in scal_lengths_mods until new rep created.
  scal_lengths_mod <- lisread("scal_lengths_mod.dat")

  # the model uses pl_m(g)(lengths)(t)
  # data are lenObsProp_m(g)

  firstBin <- obj$firstBin
  lastBin  <- obj$lastBin
  minLen   <- obj$minLen
  maxLen   <- obj$maxLen
  nGear    <- 3
  nLens    <- 54 #nrow( pl_m[[1]] )
  nT       <- 44 #ncol( pl_m[[1]] )
  lenFirstYear <- obj$lenFirstYear
  lenLastYear  <- obj$lenLastYear

  # Model: pl_m,f,c are lists of arrays with dimensions( nLens,nT ).
  pFemale_g      <- vector("list")

  pFemale_g[[1]] <- matrix(NA, nrow=nLens,ncol=nT)
  # pFemale_g[[1]] <- switch(doType,
  #                   m = obj$pl_m1,
  #                   f = obj$pl_f1,
  #                   c = obj$pl_c1
  #                   )
  pFemale_g[[1]] <- repObj$pFemale_1

  pFemale_g[[2]] <- matrix(NA, nrow=nLens,ncol=nT)
  # pFemale_g[[2]] <- switch(doType,
  #                   m = obj$pl_m2,
  #                   f = obj$pl_f2,
  #                   c = obj$pl_c2
  #                   )
  pFemale_g[[2]] <- repObj$pFemale_2

  pFemale_g[[3]] <- matrix(NA, nrow=nLens,ncol=nT)
  # pFemale_g[[3]] <- switch(doType,
  #                   m = obj$pl_m3,
  #                   f = obj$pl_f3,
  #                   c = obj$pl_c3
  #                   )
  pFemale_g[[3]] <- repObj$pFemale_3
  
  lenClasses <- vector("list")
  lenClasses[[1]] <- seq( minLen[1], maxLen[1], by=obj$binSize )
  lenClasses[[2]] <- seq( minLen[2], maxLen[2], by=obj$binSize )
  lenClasses[[3]] <- seq( minLen[3], maxLen[3], by=obj$binSize )

  # These are the lengths.
  xAxisLengths    <- obj$lfBins
  xLim <- range( xAxisLengths )
  
  # OBS: lenObsProp is a 3-dim array with dimensions( nGear,nLens,nT ).
  lenObsProp      <- array( 0,dim=c(nGear,nLens,nT) )
  # lenObsProp[1,firstBin[1]:lastBin[1],] <- switch(doType,
  #                                                 m = obj$lenObsProp_m1,
  #                                                 f = obj$lenObsProp_f1,
  #                                                 c = obj$lenObsProp_c1
  #                                                 )
  lenObsProp[1,firstBin[1]:lastBin[1],] <- scal_lengths_mod$PropFemale1

  # lenObsProp[2,firstBin[2]:lastBin[2],] <- switch(doType,
  #                                                 m = obj$lenObsProp_m2,
  #                                                 f = obj$lenObsProp_f2,
  #                                                 c = obj$lenObsProp_c2
  #                                                 )
  lenObsProp[2,firstBin[2]:lastBin[2],] <- scal_lengths_mod$PropFemale2

  # lenObsProp[3,firstBin[3]:lastBin[3],] <- switch(doType,
  #                                                 m = obj$lenObsProp_m3,
  #                                                 f = obj$lenObsProp_f3,
  #                                                 c = obj$lenObsProp_c3
  #                                                 )
  lenObsProp[3,firstBin[3]:lastBin[3],] <- scal_lengths_mod$PropFemale3

  # Starting Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  startBins <- matrix( 0,nrow=nGear,ncol=nT )
  startBins <- switch(doType,
                      m = obj$startBin_m,
                      f = obj$startBin_f,
                      c = obj$startBin_c
                      )
  # Ending Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  endBins <- matrix( 0,nrow=nGear,ncol=nT )
  endBins <- switch(doType,
                    m = obj$endBin_m,
                    f = obj$endBin_f,
                    c = obj$endBin_c
                    )
  # Starting Bins: only using length bins where lenObsProp > 0.01. All 
  # entries < 0.01 are accumulated into start and end bin classes
  nObsBins <- matrix( 0,nrow=nGear,ncol=nT )
  nObsBins <- switch(doType,
                      m = obj$nObsBins_m,
                      f = obj$nObsBins_f,
                      c = obj$nObsBins_c
                      )
  # Get those -1 values outta there for plotting...convert to NA
  setMissing <- function( vec )
  {
    n <- length(vec)
    vec[ vec<0 ] <- NA
    return(vec)
  }
  for( i in 1:dim(lenObsProp)[1] )
    lenObsProp[i,,] <- apply( X=lenObsProp[i,,], MARGIN=2,FUN=setMissing )
 
 
  # These are the fitted len proportions.
  yLim <- NULL
  if ( is.null(yLim) )
    yLim <- c( 0,1.1 )

  for ( g in 1:nGear )
  {   

    if ( g > 1 )
    {
      dev.new()
    }
    
    yLim    <- c( 0, 1.1 )
    sumProp <- colSums( lenObsProp[g,,], na.rm=TRUE )      
    nPanels <- obj$lenLastYear[g] - obj$lenFirstYear[g] + 1

    myMar   <- c( 1.5,1.25,0.5,0.5 )
    par( oma=.OMA, mar=myMar, mfcol=c(5,5) )

    # Years where the sum is not 0, i.e., there are fitted lens.
    idxPlot <- c(1:nT)[sumProp>0.0]
    idxPlot <- seq(obj$lenFirstYear[g],obj$lenLastYear[g],by=1) 
    idxPlot <- idxPlot[sumProp[idxPlot]!=0.0]
    
    # List to hold the residuals
    if( g==resGear )
    {
      resids      <- vector("list",length=length(idxPlot)+3)
      cumulObs    <- rep(0,length(lenClasses[[g]]))
      cumulPreds  <- rep(0,length(lenClasses[[g]]))
      cumulResids <- rep(0,length(lenClasses[[g]]))
    }
    delta <- 1
    iPlot <- 0
    for ( i in idxPlot )
    {
      if( g==2 & i==26 )
      {
        dev.new()
        myMar   <- c( 1.5,1.25,0.5,0.5 )
        par( oma=.OMA, mar=myMar, mfcol=c(5,5) )
      }

      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
      abline( h=0 )

      # The observed length proportions.
      rect( lenClasses[[g]]-delta, 0, lenClasses[[g]]+delta, 
            lenObsProp[g,,i], col="white",lwd=0.5 )
      
      # The fitted length proportions.      
      lines( lenClasses[[g]], pFemale_g[[g]][,i], lty=1, lwd=2, col="red" )
      points( lenClasses[[g]], pFemale_g[[g]][,i], bg="white", 
              col="black", cex=0.5, lwd=1, pch=21 )
      
      if( g==resGear )
      {
        iPlot <- iPlot + 1
        resids[[iPlot]]$points    <- vector("numeric",length(lenClasses[[g]]))  
        resids[[iPlot]]$stdResids <- vector("numeric",length(lenClasses[[g]]))  
        resids[[iPlot]]$std       <- vector("numeric",length(lenClasses[[g]]))  
        
        resids[[iPlot]]$points    <- lenObsProp[g,,i] - pFemale_g[[g]][,i]
        resids[[iPlot]]$std       <- sd( resids[[iPlot]]$points, na.rm=TRUE )  
        resids[[iPlot]]$stdResids <- resids[[iPlot]]$points/resids[[iPlot]]$std
        resids[[iPlot]]$stdResids[is.na(resids[[iPlot]]$stdResids)] <- 0
        resids[[iPlot]]$year    <- paste( i+.INITYEAR-1,",",i,sep="") 

        tmp <- lenObsProp[g,,i]
        tmp[is.na(tmp)] <- 0.
        cumulObs <- cumulObs + tmp
        
        tmp <- pFemale_g[[g]][,i]
        tmp[is.na(tmp)] <- 0.
        cumulPreds <- cumulPreds + tmp

        tmp <- resids[[iPlot]]$points
        tmp[ is.na(tmp) ] <- 0.
        cumulResids <- cumulResids + tmp
        #browser()       
      }
      resids[[iPlot+1]]$cumulObs   <- cumulObs
      resids[[iPlot+2]]$cumulPreds <- cumulPreds
      resids[[iPlot+3]]$cumulResids<- cumulResids
  
      if ( .USEYEAR )
      { 
        xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
        panLab( 0.5, 0.9, cex=1.0, paste( i+.INITYEAR-1,",",i,sep="") )
      }
      else      
        panLab( 0.5, 0.9, cex=1.0, paste( "Year",i ) )
      
      mfg <- par( "mfg" )
      
      axis( side=1, cex.axis=.CEXAXIS, pos=0 )
      
      if ( mfg[2]==1 )
        axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
      else
        axis (side=2, labels=FALSE )
        
    }     # i=1,..,nT
    mtext( side=3, line=0, cex=.CEXLAB, outer=TRUE, 
              switch(g,
                     "Commercial",
                     "RV_4VWX",
                     "Halibut Survey" 
                     )
          ) 
    mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Length class (cm)" )
    mtext( side=2, line=2, cex=.CEXLAB, outer=TRUE, "Proportion-Female-at-length" )

  }
  return( resids )
}


# plotIndexFit
# Plots observed total survey catch and fitted exploitable biomass - 
# biomass available via survey-specific selectivity. All data is 
# re-scaled to biomass units via division by "q"
# All surveys are shown on same page
plotIndexFit <- function( obj, ssb=FALSE )
{
  #png( filename="Index.png" )
  expBtg <- obj$expBtg
  Itg    <- obj$Itg
  Itg[ Itg < 0 ] <- NA
  nT     <- obj$nT
  nIndex <- nrow(expBtg)
  xLim <- c( .INITYEAR, .LASTYEAR )
  scaler <- c(1.e6,1.e3)
  
  myMar <- c( 3,5,0.5,0.5 )
  par( oma=c(1,3,1,1), mar=myMar, mfcol=c(nIndex,1) )
  legs <- c("(A)","(B)")
  yMax1  <- max( max(expBtg[1,],na.rm=TRUE), max(Itg[1,],na.rm=TRUE)  )
  yMax2  <- max( max(expBtg[2,],na.rm=TRUE), max(Itg[2,],na.rm=TRUE)  ) 
  yMax   <- c( yMax1/scaler[1], yMax2/scaler[2] )
  for( i in 1:nIndex )
  {
    yLimit <- c( 0, yMax[i] )
    yLab <- c("Number of halibut (millions)",
              "Halibut biomass (1000s tonnes)")
    plot( xLim, yLimit, type="n", axes=F, xlab="", 
          ylab=yLab[i] )

    # Model biomass
    lines( c( xLim[1]:xLim[2] ), expBtg[i,]/scaler[i], col="black", 
           lty="solid", lwd=2 )       
  
    # Biomass index data.
    points( c(xLim[1]:xLim[2] ), Itg[i,]/scaler[i], bg="white", cex=1.8, pch=21 )

    xPos  <- seq( .INITYEAR,.INITYEAR+nT-1, 2 )
    xLabs <- paste( xPos )
    axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabs )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )  
  }
  mtext( side=1, line=3, text="Year" )

  #dev.off()
}     # plotIndexFit function

# plotAll_R
# Plots estimated total age-1 recruitment (top) and recruitment deviations
# from random walk (bottom). The parameters controlling recruitment are 
# priorSD_R (prior standard deviation on recruitment residuals) and 
# gamma_R, which is the estimated autocorrelation in recruitment. 
plotAll_R <- function( obj )
{
  par( oma=c(3.5,2,1,1), mar=c(2,1,1,1), mfrow=c(2,1 ) )
  plotRecruitment( obj=obj )
  plotRecDevs( obj=obj )
}

plotSR <- function( obj, ssb=FALSE, newPlot=F )
{

  # This will need to use an Rt variable.
  Rt     <- (obj$Nta_m[,1]+obj$Nta_f[,1])/1.e6
  nT     <- obj$nT
  SSBt   <- obj$SSBt
  SSB0   <- obj$SSB0
  R0     <- obj$R0/1.e6
    
  xLim   <- c( 0, max(SSBt) )
  yLim <- c( 0, max(Rt) )

  plot( xLim, yLim, type="n", axes=F, xlab="", ylab="" )

  # SSB[t]-Recruits[t+1]
  points( SSBt[-nT], Rt[-1], pch=19 )
  #points( SSB0, R0, pch=21 )
  r <- Rt[-1]
  s <- SSBt[-nT]
  t <- length(r)
  arrows( x0=s[-t],x1=s[-1],
        y0=r[-t],y1=r[-1],
        length=0.1, col="gray"
      )
  points( SSBt[nT-1], Rt[nT], pch=19, col="red")

  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  mtext( side=1, line=3, text="Spawning biomass (tonnes)")
  mtext( side=2, line=3, text="Age-1 recruits (millions)")
  #panLegend( .75, .5, legTxt="(SSB0,R0)", bty="n")

}

plotRecruitment <- function( obj, ssb=FALSE, newPlot=F )
{

  sdFile <- read.table( "scal.std", as.is=TRUE, header=TRUE, sep="" )
  # This will need to use an Rt variable.
  Rt     <- sdFile[sdFile$name=="Rt","value"]/1.e6
  sdRt   <- sdFile[sdFile$name=="Rt","stddev"]/1.e6
  nT     <- obj$nT
  if( !newPlot) 
  {
    #par( oma=.OMA)
    cexY <- .CEXAXIS
  }
  else
    cexY <- 0.8
    
  xLim <- c( .INITYEAR, .LASTYEAR )
  yLimit <- c( 0, max(Rt) )
  plot( xLim, yLimit, type="n", axes=F, xlab="Year", ylab="" )

  # Recruits
  lines( c( xLim[1]:xLim[2] ), Rt, col="black", lty="solid", lwd=2 )
  
  avgR <- obj$avgR/1.e6
  abline( h=avgR, lty="dashed", col="gray" )

  rPlus  <- Rt + 2.*sdRt
  rMinus <- Rt - 2.*sdRt
  x0 <- xLim[1]:xLim[2]
  x1 <- xLim[1]:xLim[2]
  y0 <- rMinus
  y1 <- rPlus
  segments( x0, y0, x1, y1 )
  
  xPos <- seq( .INITYEAR,.LASTYEAR, 2 )
  xLabs <- paste( xPos )
  axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabs )
  axis( side=2, cex.axis=cexY, las=.YAXISLAS )
  mtext( side=2, line=3, text="Recruitment (millions)", cex=cexY)
  box(bty="l")
   
}     # plotRecruitment function

plotRecDevs <- function( obj, label=NULL, gfx=list( annotate=TRUE, bygears=FALSE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Recruitment deviations.
  recDevs <- obj$recDevs
  priorSD_R <- round( obj$priorSD_R, digits=2)
  nT      <- length( recDevs )
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( .INITYEAR,.LASTYEAR )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( recDevs,na.rm=TRUE )
   
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( h=0, lty=3 )
      
  lines( (xLim[1]+1):xLim[2], recDevs, col="black", lty=1, lwd=1 )
  points( (xLim[1]+1):xLim[2], recDevs, bg="black", cex=1.2, pch=21 )

  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  mtext( side=1, line=1.5, cex=.CEXLAB, outer=TRUE, "Year" )
  mtext( side=2, line=3, cex=.CEXLAB, outer=FALSE, "Recruitment Deviation" )
  
  parNames    <- vector(length=1)
  parNames[1] <- paste( "priorSD_R = ", priorSD_R, sep="" )
  
  panLegend( 0.05,0.95, legTxt=parNames,
             col="black", lty=.LTYGROUP1,
             pch=19,
             lwd=2, bty="n" )

  return( invisible() )
}     # .plotMLErecDevs
