#------------------------------------------------------------------------------#
# (c) mseR: Management Strategy Evaluation in R, Sablefish Version 3.x         #
#                                                                              #
#     Copyright 2008-2012, 2013 by A.R. Kronlund and S.P. Cox.                 #
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
#     "Anecdotal evidence is all you need to prove your point; scientific      #
#      proof is what you require of everybody else."                           #
#                                                                              #
#--------------------- mseRplots.r: mseR Plotting Functions -------------------#
#--                                                                          --#
#-- mseRplot.r: An mseR support file that provides a plotting functions.     --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--          K.   Holt (Pacific Biological Station, Nanaimo, B.C.)           --#
#--                                                                          --#
#-- Contributors:                                                            --#
#--                                                                          --#
#--          A.F. Sinclair (contributions from PBSref)                       --#
#--          T.K. Deering  (contributions from PBSref)                       --#
#--                                                                          --#
#-- First Implementation: 09-Feb-10                                          --#
#                                                                              #
# NOTES:                                                                       #
#                                                                              #
# 1. To display hidden functions, e.g., .foo, enter command: ls(all.names=TRUE)#
#                                                                              #
# References:                                                                  #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# Schnute, J.T., Couture-Beil, A., Haigh, R., and Egeli, A. and A.R. Kronlund. #
#   2010. PBS Modelling 2.00: users guide revised from Canadian Technical      #
#   Report of Fisheries and Aquatic Science 2674: v+146 p.                     #
#                                                                              #
# This script contains all the plotting functions for mseR-Finfish.  The plots #
# are grouped below by their calling GUI: guiSim, guiView, guiPerf.            #
                                                                               #
# (A) Shared plot functions (used by more than one GUI)                        #
#
# .plotCtlPars
# .plotDesign
# .plotRefPoints
# .plotStatus
#                                                                              #
# (B) Plot functions for guiSim                                                #
#                                                                              #
# .plotSimRefPoints                                                            #
#                                                                              #
# (C) Plot functions for guiView                                               #
#                                                                              #
# .getMethodLabel  : Get a text label for plotting for required methodId.      #
#                                                                              #
# .plotAgeFreq     : Plot bar plots of the obs. and pred. age props.           #
# .plotAgeOM       : Plot the operating model true age props.                  #
# .plotBt          : Plot total and spawning biomass vs. time                  #
# .plotBtDtFt      : Plot spawning biomass, catch, & fishing mort. vs. time    #
# .plotBtDtRt      : Plot spawning biomass, catch, recruitment vs. time        #
# .plotDt          : Plot catch biomass vs. time                               #
# .plotFit         : Plot predicted biomass time series from all pr. mod fits  #
# .plotFt          : Plot fishing mortality vs. time                           #
# .plotItBt        : Plot the observed survey points against SSB               #
# .plotIt          : Plot the observed survey series vs. time                  #
# .plotMt          : Plot natural mortality vs. time.                          #
# .plotNt          : Plot total and spawning numbers vs. time                  #
# .plotRt          : Plot the recruitment numbers vs. time                     #
# .plotRtBt        : Plot the stock-recruitment points and curve               #
# .plotParEsts     : Plot timeseries of B0 and r estimates from pr. model fits #
# .plotRefPtSeries : Plot reference point time series against objectives.      #
# .plotYield       : Plot true and estimated yield curves                      #
#                                                                              #
# (D) Plot function for guiPerf                                                #
#                                                                              #
# -------- Mid-level (Level 2) functions that re-route to lower level -------- #
#                                                                              #
# .doBarPlots       : Sets up inputs for all lower level barplot functions     #
# .doQuantBoxPlots  : Sets up inputs for all lower level boxplot functions     #
# .doTulipPlots     : Sets up inputs for all lower level tulip plot functions  #
# .doConvPlots      : Sets up inputs for all lower level plots on conv. diag.  #
# .doOtherPlots     : Sets up inputs for all "other" functions                 #
#                                                                              #
# ---- Lower-level (Level 3) functions that produce plots ---------------------#
#
# .plotBarsByPeriod  : Barplots summarizing performance by time period         #                                                             
# .plotBarsByStat    : Barplots summarizing performance by performance stat    #
# .plotQboxDep       : Boxplots of depletion relative to status zones          #
# .plotQboxSSB       : Boxplots of spawning biomass relative to status zones   #
# .plotMaxGrad       : Maximum gradient for ADMB optimization solution         #
# .plotExitCodes     : Frequency of exit codes from ADMB optimizations         #
# .plotFuncCalls     : Number of function calls required by ADMB optimization  #
# .plotTimeConv      : Time required (in secs) to solve ADMB optimization      #
# .plotTulipCatch    : Tuilp (simulation envelope) plots of catch over time    #
# .plotTulipDepletion: Tulip (simulation envelope) plots of depltn over time   #
# .plotTulipF        : Tuilp (simulation envelope) plots of F over time        #
# .plotTulipBmsy     : Tuilp (simulation envelope) plots Bmsy ests over time   #
# .plotTulipFmsy     : Tuilp (simulation envelope) plots Fmsy ests over time   #
# .plotTulipF        : Tuilp (simulation envelope) plots of F over time        #
# .plotFvsSSB        : Fishing mortality vs. ssb pooled over all reps & years  #
# .plotYieldU                                                                             #
# INSTRUCTIONS FOR ADDING NEW PLOTS TO GUIVIEW WINDOW                          #
#                                                                              #
#     (i) Create a new plot function (.plotXxx) that has a similar format as   #
#            the exist guiViewplot functions (e.g., .plotBiomass, .plotCatch)  #
#     (ii) Add a new if statement to .doViewPlots() function to call new       #
#             .plotXxx when required.                                          #
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- (A) Shared plot functions (used by more than one GUI)                    --#
#------------------------------------------------------------------------------#

.addXaxis <- function( xLim=xLim, initYear=.INITYEAR, side=1, years=FALSE, cexAxis=.CEXAXIS )
{         
  if ( years )
  {
    xPos <- seq( .INITYEAR,.INITYEAR+xLim[2]-1, .XAXISTICKBY )
    xSeq <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )
    
    if ( side==1 )
    {  
      axis( side=1, at=xSeq, cex.axis=cexAxis, labels=xLabs )
      # axis( side=3, at=xSeq, labels=FALSE )
    }
    if ( side==3 )
      axis( side=3, at=xSeq, cex.axis=cexAxis, labels=xLabs )
  }
  else
  {
    if ( side==1 )
    {
      axis( side=1, cex.axis=cexAxis )
      axis( side=3, labels=FALSE )
    }
    
    if ( side==3 )
    {
      axis( side=3, cex.axis=cexAxis )
      axis( side=1, labels=FALSE )    
    }
  }
}     # END function .addXaxis

.getRowCol <- function( nSim )
{
  if ( nSim<=3 )
    mfRow <- c(nSim,1)
  else if ( nSim<=6 )
    mfRow <- c(round((nSim+0.1)/2),2)
  else
    mfRow <- c(3,2)
  mfRow
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

.setTextContrastColor <- function( color )
{
  result <- ifelse( mean(col2rgb(color)) > 127, "black","white" )
  result
}

.plotColorChart <- function()
{
  colCount <- 25
  rowCount <- 27
  
  RGBColors <- col2rgb( colors()[1:length(colors())] )
  HSVColors <- rgb2hsv( RGBColors[1,], RGBColors[2,], RGBColors[3,], maxColorValue=255 )
  HueOrder  <- order( HSVColors[1,], HSVColors[2,], HSVColors[3,] )
  
  textContrastColor <- unlist( lapply( colors(), .setTextContrastColor) )
  
  plot( c(1,colCount),c(0,rowCount), type="n", axes=FALSE, xlab="",
        ylab="", ylim=c(rowCount,0) )
  
  mtext( side=3, line=0, cex=.CEXTITLE4, "R Colors" )
  for ( j in 0:(rowCount-1) )
  {
    for ( i in 1:colCount )
    {
      k <- j * colCount + i
      if ( k <= length( colors() ) )
      {
        rect( i-0.5,j-0.5, i+0.5,j+0.5, border="black", col=colors()[ HueOrder[k] ] )
        text( i,j, paste( HueOrder[k] ), cex=0.7, col=textContrastColor[ HueOrder[k] ] )
      }
    }
  }
  return( invisible() )
}     # END function .plotColorChart

.plotCtlPars <- function( ctlPars, type=NULL )
{
  calcSettings <- function( x, nCol=4, byrow=TRUE )
  {
    result <- list()
    
    nGroups <- length( x )
    
    # How many rows to display all legends?
    nRows <- 0
    tmpCol <- nCol
    for ( i in 1:nGroups )
    {
      tmpCol <- nCol
      if ( names(x)[i]=="hcrPars" )
        tmpCol <- 3
      nRows <- nRows + round( length(x[[i]]) / tmpCol + 0.5 )
    }
    # Add a row for each group header.
    if ( byrow )
      nRows <- nRows + (nGroups * 2)
    else
      nRows <- max( sapply( x, length ) ) + 2
    
    # Calculate total height in user coordinates.
    usr <- par ("usr")
    yHeight  <- (usr[4] - usr[3]) * 0.9
    
    # Start with cex=1. 
    myCex <- 1.0
    
    # Character height then add 100% for interspace.
    charHgt  <- strheight( "X128", cex=myCex, font=1 ) * 1.25
    
    # Add space for legend headers.
    yTot <- (nRows * charHgt) + (nGroups * charHgt * 2)
    
    while ( yTot > yHeight )
    {
      myCex  <- myCex - 0.05
      # Character height then add 100% for interspace.
      charHgt  <- strheight( "X128", cex=myCex, font=1 ) * 1.25
      yTot <- (nRows * charHgt) + (nGroups * charHgt * 2)      
    }
    
    # Return vector of columns, cex.
    result$cex       <- myCex
    result$charHgt   <- charHgt
    result$nGroups   <- nGroups
    result$nRows     <- nRows
    result$yTot      <- yTot
    result
  }     # END internal function calcSettings.
  
  #----------------------------------------------------------------------------#
  
  # Open a graphics device.
  .plotStatus( "" )
  nCols <- 1
  titleText <- "Simulation Control Parameters"
  
  # Plot the control parameters specified by type.
  if ( is.null(type) )
  {
    opModPars <- ctlPars[ substring( ctlPars$parameter,1,5 )=="opMod", ]
    opModPars <- paste( substring( opModPars$parameter,7),"=",opModPars$value, sep=" " )      
    opModPars <- substring( opModPars,1,50 )
      
    refPtPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="refPts", ]
    refPtPars    <- paste( substring( refPtPars$parameter,8),"=",
                           round( as.numeric(refPtPars$value), digits=3), sep=" " )            

    dataPars   <- ctlPars[ substring( ctlPars$parameter,1,7 )=="mp$data", ]
    dataPars   <- paste( substring( dataPars$parameter,9 ),"=",dataPars$value, sep=" " )
    dataPars   <- substring( dataPars,1,30 )
      
    assessPars <- ctlPars[ substring( ctlPars$parameter,1,9 )=="mp$assess", ]
    assessPars <- paste( substring( assessPars$parameter,11 ),"=",assessPars$value, sep=" " )
      
    hcrPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="mp$hcr", ]
    hcrPars    <- paste( substring( hcrPars$parameter,8, ),"=",hcrPars$value, sep=" " )

    x <- list( opModPars=opModPars, refPtPars=refPtPars, dataPars=dataPars,
               assessPars=assessPars, hcrPars=hcrPars )
      
    subTitleText <- c( "OM","Ref Pts","Data","Method","HCR" )
    
    result <- calcSettings( x, byrow=TRUE )    
  }
  else if ( type=="opMod" )
  {
    nCols <- 1
      
    opModPars <- ctlPars[ substring( ctlPars$parameter,1,5 )=="opMod", ]
    opModPars <- paste( substring( opModPars$parameter,7),"=",opModPars$value, sep=" " )      
      
    refPtPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="refPts", ]
    refPtPars    <- paste( substring( refPtPars$parameter,8),"=",refPtPars$value, sep=" " )            
      
    x <- list( opModPars=opModPars, refPtPars=refPtPars )
      
    titleText <- "Operating Model Parameters"
    subTitleText <- c( "OM","Ref Pts" )
    
    result <- calcSettings( x, byrow=FALSE )    
  }
  else if ( type=="mp" )
  {
    nCols  <- 1
      
    dataPars   <- ctlPars[ substring( ctlPars$parameter,1,7 )=="mp$data", ]
    dataPars   <- paste( substring( dataPars$parameter,9 ),"=",dataPars$value, sep=" " )
    dataPars   <- substring( dataPars,1,30 )
          
    assessPars <- ctlPars[ substring( ctlPars$parameter,1,9 )=="mp$assess", ]
    assessPars <- paste( substring( assessPars$parameter,11 ),"=",assessPars$value, sep=" " )
      
    hcrPars    <- ctlPars[ substring( ctlPars$parameter,1,6 )=="mp$hcr", ]
    hcrPars    <- paste( substring( hcrPars$parameter,8, ),"=",hcrPars$value, sep=" " )

    x <- list( dataPars=dataPars, assessPars=assessPars, hcrPars=hcrPars )
      
    titleText    <- "Management Procedure Parameters"
    subTitleText <- c( "Data","Assessement","HCR" )
    
    result <- calcSettings( x, byrow=FALSE )    
  }

  nGroups <- length( x )                  # Number of groups of parameters to show.
  xDelta  <- 1.0 / (nGroups + 0.75)       # Offset between groups.
  
  mtext( side=3, line=0, cex=1.2, titleText )        

  # Dumping all the simulation control parmaters.
  if ( is.null(type) )
  {
    xPos <- 0.5
    yPos <- 1.04

    for ( i in 1:nGroups )
    {
      if ( names(x)[i]=="hcrPars" | names(x)[i]=="opModPars" )
        nCols <- 3
      else
        nCols <- 4
        
      nPars <- length( x[[i]] )
      nRows <- trunc( (nPars / nCols) + 0.5 )
      
      temp <- legend( x=xPos, y=yPos, xjust=0.5, yjust=1, ncol=nCols,
              legend=x[[i]], bty="n", title=subTitleText[i], adj=c(0,1),
              title.col="blue", text.font=1, cex=result$cex )
              
      yBottom <- temp$rect$top-temp$rect$h
    
      yPos <- yBottom
    }
  }
  else
  {
    xPos <- 0.0
    yPos <- 1.04  
  
    for ( i in 1:nGroups )
    {
      xPos <- xPos + xDelta
      legend( x=xPos, y=yPos, xjust=0.5, yjust=1, ncol=nCols,
              legend=x[[i]], bty="n", title=subTitleText[i], title.adj=0.5,
              title.col="blue", cex=result$cex )
    }
  }
}     # END function .plotCtlPars


.plotDesign <- function( obj, iObj=1, nObj=1, gfx=list( annotate=TRUE,
                         doLegend=TRUE, xLim=NULL, yLim=NULL, years=FALSE ) )
{
  # NOTE: When called from guiPerf, this is inside a loop!
  
  addRects <- function( xl, yb, xr, yt, clr="black", dens=-1, offset=0.5 )
  {
    rect( xl-offset, yb, xr-offset, yt, border=TRUE, col=clr, density=dens )
  }
  
  symex <- 1
  tmp <- .calcTimes( obj )
  
  # Need to modify to loop over a list as an argument.
  nT    <- obj$opMod$nT
  tMP   <- obj$opMod$tMP
  nGear <- obj$opMod$nGear

  xPos    <- c( 1:nT )
  xOffset <- 0.1
  
  yLabel  <- paste( obj$gui$scenarioLabel,"-",obj$gui$mpLabel, sep="" )
  yLabel  <- substring( yLabel, 1, 15 )
  gNames  <- obj$opMod$gNames

  idxCatch  <- 1
  idxIndex  <- idxCatch  + nObj + 1
  idxAges   <- idxIndex  + (nGear * nObj) + 2
  idxMethod <- idxAges   + (nGear * nObj) + 1
  idxCtlPts <- idxMethod + nObj + 1
  
  idx <- c( idxCatch,idxIndex,idxAges,idxMethod,idxCtlPts )
  names( idx ) <- c( "Catch","Index","Ages","Method","Ctl Pts" )

  idx2 <- idx + iObj

  # Number of headings to plot (Catch, Index, Ages, Method, CtlPts etc).
  nCats  <- length( idx )
  #nLines <- (nObj * nCats) + nCats
  nLines <- max( idx ) + 1

  xLim <- range( xPos )
  yLim <- c( 1,nLines+1 )
  
  if ( iObj == 1 )
    plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="", ylim=rev(yLim) )

  cxy <- par( "cxy" )
  yOffset <- cxy[2] / 3.
  usr <- par( "usr" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  # Plot catch indictor.
  x <- xPos
  y <- rep( idx2[ "Catch" ], nT )
  
  #points( x, y, cex=symex, pch=22, bg="black" )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 ) 

  text( usr[1], y[1], yLabel, cex=.CEXLAB*0.8, pos=2, xpd=TRUE )

  # Plot symbol at times where index value is obtained for Period1 and Period2.
  for ( iGear in 1:nGear )
  {
    x    <- tmp$per1Index[[iGear]]
    yVal <- idx2["Index"] + ((iObj-1)*nGear) + (iGear-1)    
    
    if ( all(x > 0) )
    {
      y <- tmp$indexOn[ x, iGear ]
      y <- ifelse( y, yVal, NA )
      addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )
    }

    x <- tmp$per2Index[[iGear]]
    if ( all( x > 0 ) )
    {
      y <- tmp$indexOn[ x, iGear ]
      y <- ifelse( y, yVal, NA )
      addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="blue", dens=-1 )
    }
    
    if ( iGear==1 )
      text( usr[1], yVal, yLabel, cex=.CEXLAB*0.8, pos=2, xpd=TRUE )
    
    text( usr[2], yVal, gNames[iGear], cex=.CEXLAB*0.8, pos=4,
         xpd=TRUE )
  }
  
  # Plot the ageing data frequency.
  for ( iGear in 1:nGear )
  {
    x <- tmp$per1Ages[[iGear]]
    yVal <- idx2["Ages"] + ((iObj-1)*nGear) + (iGear-1)        
    
    if ( all(x > 0) )
    {
      y <- tmp$agesOn[ x,iGear ]
      y <- ifelse( y, yVal, NA )
      addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )
    }

    x <- tmp$per2Ages[[iGear]]
    if ( all(x > 0) )
    {
      y <- tmp$agesOn[ x,iGear ]
      y <- ifelse( y, yVal, NA )
      addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="gray", dens=-1 )  
    }

    if ( iGear==1 )
      text( usr[1], yVal, yLabel, cex=.CEXLAB*0.8, pos=2, xpd=TRUE )
      
    text( usr[2], yVal, gNames[iGear], cex=.CEXLAB*0.8, pos=4,
          xpd=TRUE )
  }

  # Plot symbol at times where assessment is obtained for Period1 and Period2.
  x <- tmp$per1Method
  y <- tmp$methodOn[ x ]
  y <- ifelse( y, idx2[ "Method" ], NA )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, dens=0 )  

  x <- tmp$per2Method
  y <- tmp$methodOn[ x ]
  y <- ifelse( y, idx2[ "Method" ], NA )
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="green", dens=-1 )  

  if ( gfx$doLegend )
  {
    methodId <- obj$mp$assess$methodId
    label    <- .METHODLAB[ methodId ]
    
    if ( methodId == .CAAMOD )
    { 
      #if ( obj$mp$assess$caaAges )
      #  label <- paste( label, "(Ages On" )
      #else
      #  label <- paste( label, "(Ages Off" )
        
      #if ( obj$mp$assess$caaSurveyRel )
      #  label <- paste( label,", Rel. Index)" )
      #else
      #  label <- paste( label,", Abs. Index)" )
    }
    
    if ( methodId == .DDMOD )
    {
      #if ( obj$mp$assess$ddSurveyRel )
      #  label <- paste( label,"(Rel. Index)" )
      #else
      #  label <- paste( label,"(Abs. Index)" )    
    }    
    
    if ( methodId == .PMOD )
    {
      #if ( obj$mp$assess$spSurveyRel )
      #  label <- paste( label,"(Rel. Index)" )
      #else
      #  label <- paste( label,"(Abs. Index)" )    
    }
      
    hcrType <- obj$mp$hcr$hcrType
    label   <- paste( label, hcrType )
    
    if ( obj$mp$hcr$forecast )
      label <- paste( label, "(Catch=model)" )
    else
      label <- paste( label, "(Catch=constant)" )
    
    text( 0, y, adj=0, cex=0.6, label )
  }

  text( usr[1], y[1], yLabel, cex=.CEXLAB*0.8, pos=2, xpd=TRUE )

  # Plot the control point frequency.

  x <- xPos[ tmp$ctlPtsOn ]
  y <- rep( idx2["Ctl Pts"], length(xPos) )[ tmp$ctlPtsOn ]
  
  addRects( x+xOffset, y-yOffset, x+1-xOffset, y+yOffset, clr="black", dens=-1 )
  
  if ( gfx$doLegend )
  {
    if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
      label <- "Bmsy Base"
    else
      label <- "B0 Base"
      
    if ( obj$mp$hcr$hcrType == "variableF" )
    {
      label <- paste( label,
        paste( "(LB=", obj$mp$hcr$lowerBoundMult,
               " UB=",obj$mp$hcr$upperBoundMult,")", sep="" ) )
    }
    
    text( 0, y, adj=0, cex=0.6, label )    
  }

  text( usr[1], y[1], yLabel, cex=.CEXLAB*0.8, pos=2, xpd=TRUE )

  if ( iObj == 1 )
  {  
    .addXaxis( xLim=xLim, initYear=.INITYEAR, gfx$years )
    axis( side=2, cex.axis=.CEXAXIS2, at=idx, labels=names(idx), las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    box()

    mtext( side=1, line=0, cex=.CEXLAB2, outer=TRUE, "Year" )
  }
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=T, "Management Procedure Design" )
  }
  
  return( invisible() )
}     # END function .plotDesign


.plotStatus <- function( plotLabel="No Data Available" )
{
  par( oma=c(1,1,1,1), mar=c(0,0,0,0), mfrow=c(1,1) )
  plot( c(0,1),c(0,1), type="n", axes=FALSE, xlab="", ylab="" )
  box()

  panLab( 0.5,0.5, cex=1.5, plotLabel )
  return( invisible() )
}

#------------------------------------------------------------------------------#
# Reference Point Plotting Functions for ALL GUIs                              #
#------------------------------------------------------------------------------#

.addRefPointsLegend <- function( x=0.5, y=0.5, checked )
{
  # Is there anything in the legend, or is nothing checked?
  if ( sum(checked)==0 )
  {
    cat( "\nmseRrefPoints (.addRefPointsLegend): Legend vector of length 0.\n" )
    return()
  }
  
  labels <- c( "F0","F0.1","Fmsy","Fspr40","Fmax","Fcrash" )
  names(labels) <- c("F0","F01","Fmsy","F40","Fmax","Fcra" )
  
  pchVec <- rep( 21,length(labels) )
  names(pchVec) <- names(labels)
  
  ptBg   <- c(.F0COL,.F01COL,.FmsyCOL,.FsprCOL,.FmaxCOL,.FcraCOL)
  names(ptBg) <- names(labels)

  # Now show only those reference points that are checked in the GUI.
  # This is tricky, we want the reference points where checked=TRUE, but
  # we have to get the names(checked) where checked=TRUE in case the order
  # of the reference points in the vectors differ.
  
  labels <- labels[ names(checked)[checked] ]
  pchVec <- pchVec[ names(checked)[checked] ]
  ptBg   <- ptBg[ names(checked)[checked] ]  
      
  # Add a legend.
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=.CEXSYM24,
             cex=.CEXLEG2, bty=.LEGBTY )
  
  return( invisible() )
}     # END function addRefPointsLegend

# .plotSimRefPoints    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotSimRefPoints <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  # guiSim has no plotting limits at this time.
  
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL,yLim=NULL ) )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Reference Points" )
}     # END function .plotSimRefPoints


.plotRecSSB <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$ssb)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$recruits) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$recruits)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList  
  
    lines( .posVal(refs$ssb), .posVal(refs$recruits), lwd=.LWD2 )

    # Adding steepness lines at B20=0.2*B0, R20, 
    # and steepness R20/B0.
    lines( c(refs$B20,refs$B20), c(0,        refs$R20), lty=.LWD2 )
    lines( c(0,       refs$B20), c(refs$R20, refs$R20), lty=.LWD2 )
  
    lines( c(refs$B0,refs$B0), c(0,       refs$R0), lty=.LWD2 )
    lines( c(0,      refs$B0), c(refs$R0, refs$R0), lty=.LWD2 )

    # Adding steepness label
    h <- round( refs$R20/refs$R0, digits=2 )
    xPos <- refs$B20
    yPos <- refs$R20 * 0.8
    text( xPos, yPos, cex=.CEXANNO2, pos=4, paste( "h=",refs$rSteepness,sep="") )
    
    if ( checked["F0"] )  
      points( refs$ssbF0,   refs$recruitsF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$ssbF01,  refs$recruitsF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$ssbFcra, refs$recruitsFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$ssbFmax, refs$recruitsFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$ssbF40,  refs$recruitsF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
    if ( checked["Fmsy"] )
    {
      points( refs$ssbFmsy, refs$recruitsFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$ssbFmsy, refs$recruitsFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Spawning Biomass" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Recruits" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotRecSSB    


# Spawning biomass as a function of F.
# Can pass a list of blobs.
# I think idNum is a label.
# gfx is graphics parameter list for plotting.
.plotSsbF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ssb)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
    
    lines( .posVal(refs$F), .posVal(refs$ssb), lwd=.LWD2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$ssbF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$ssbF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$ssbFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$ssbFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$F40, refs$ssbF40,   cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$ssbFmsy, cex=.CEXSYM24, bg=.FmsyCOL, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$ssbFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Spawning Biomass" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
  return( invisible() )
}    # END function .plotSsbF


.plotSsbPerRecF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
 #   if ( n==1 )
 #     xLim <- c( 0, max(obj$refPtList$Fcra) )
 #   else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ssbpr) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ssbpr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList  
    
    lines( .posVal(refs$F), .posVal(refs$ssbpr), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$ssbprF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$ssbprF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$ssbprFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$ssbprFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
   if ( checked["F40"] )
      points( refs$F40,  refs$ssbprF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$ssbprFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$ssbprFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "SSB per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
    
  return( invisible() )
}     # END function .plotSsbPerRecF    


.plotYieldF <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  refPts <- obj$ctlList$refPts
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$yield) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$yield)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList    
  
    lines( .posVal(refs$F), .posVal(refs$yield), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$F0, refs$yieldF0,     cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$yieldF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$yieldFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$yieldFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )

    if ( checked["F40"] )
      points( refs$F40,  refs$yieldF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$yieldFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$yieldFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotYieldF    


.plotYieldSSB <- function( obj, idNum=NULL, gfx )
{
  checked  <- gfx$checked
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$ssb) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$ssb)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
 #   if ( n==1 )
 #     yLim <- c( 0, max(obj$refPtList$yield) )
 #   else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$yield)) )
  }
   
  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
        
    lines( .posVal(refs$ssb), .posVal(refs$yield), lwd=2 )
    
    if ( checked["F0"] )  
      points( refs$ssbF0,   refs$yieldF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$ssbF01,  refs$yieldF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$ssbFcra, refs$yieldFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$ssbFmax, refs$yieldFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )
    if ( checked["F40"] )
      points( refs$ssbF40,  refs$yieldF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$ssbFmsy, refs$yieldFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$ssbFmsy, refs$yieldFmsy, idNum[i], .CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Spawning Biomass" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}     # END function .plotYieldSSB      


.plotYprF <- function( obj, idNum=NULL, gfx )
{
  annotate <- gfx$annotate
  checked  <- gfx$checked
  
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  # Determine whether 1 curve, or n curves to plot.
  n <- length( obj )
  if ( is.null(idNum) )
    idNum <- c(1:n)  
  
  # Find maximum range of x-axis and y-axis.
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c( 0,0 )
#    if ( n==1 )
#      xLim <- c( 0, max(obj$refPtList$Fcra) )
#    else
      for ( i in 1:n )
        xLim <- c( 0,max(xLim[2],max(obj[[i]]$refPtList$Fcra)) )        
  }

  yLim <- gfx$yLim      
  if ( is.null( yLim ) )
  {
    yLim <- c( 0,0 )
#    if ( n==1 )
#      yLim <- c( 0, max(obj$refPtList$ypr) )
#    else
      for ( i in 1:n )
        yLim <- c( 0,max(yLim[2],max(obj[[i]]$refPtList$ypr)) )
  }

  # Plot yield against fishing mortality.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
#    if ( n==1 )
#      refs <- obj$refPtList
#    else
      refs <- obj[[i]]$refPtList
        
    lines( .posVal(refs$F), .posVal(refs$ypr), lwd=2 )

    if ( checked["F0"] )  
      points( refs$F0,   refs$yprF0,   cex=.CEXSYM24, bg=.F0BG,   pch=.F0PCH )
    if ( checked["F01"] )
      points( refs$F01,  refs$yprF01,  cex=.CEXSYM24, bg=.F01BG,  pch=.F01PCH )    
    if ( checked["Fcra"] )
      points( refs$Fcra, refs$yprFcra, cex=.CEXSYM24, bg=.FcraBG, pch=.FcraPCH )
    if ( checked["Fmax"] )
      points( refs$Fmax, refs$yprFmax, cex=.CEXSYM24, bg=.FmaxBG, pch=.FmaxPCH )

    if ( checked["F40"] )
      points( refs$F40,  refs$yprF40,  cex=.CEXSYM24, bg=.FsprBG, pch=.FsprPCH )        
      
    if ( checked["Fmsy"] )
    {
      points( refs$Fmsy, refs$yprFmsy, cex=.CEXSYM24, bg=.FmsyBG, pch=.FmsyPCH )
      if ( gfx$annotate & (n>1) )
        text( refs$Fmsy, refs$yprFmsy, idNum[i], cex=.CEXANNO )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "F" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Yield Per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.90, checked )
}     # END function .plotYprF    


# .plotRefPoints (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPoints <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=xLim,yLim=yLim ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSBR,maxSSBR) ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minYield,maxYield) ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minF,maxF),yLim=c(minSSB,maxSSB) ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minRec,maxRec) ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(minSSB,maxSSB),yLim=c(minYield,maxYield) ) )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Reference Points" )
}     # END function .plotRefPoints

#------------------------------------------------------------------------------#
#-- (B) Plotting function for guiSim                                           #
#------------------------------------------------------------------------------#

# .plotLifeHist  (Plot the operating model life history parameters)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotLifeHist <- function( obj, gfx=c( annotate=TRUE, doLegend=TRUE,
                           xLim=xlim, yLim=ylim ) )
{
  par( oma=c(2,1,2,1), mar=c(3,4,1,1), mfrow=c(3,2) )

  lh <- calcRefPoints( obj, rpList=list( FALL=TRUE ) )

  # Numbers at age in year 1.
  .plotNumAtAge( lh, gfx=gfx )

   # Weight versus length.  
  .plotWgtLen( lh, gfx=gfx )

  # Length-at-age.
  .plotLenAtAge( lh, gfx=gfx )
  
  # Maturity at age.
  .plotMatAtAge( lh, gfx=gfx )

  # Weight at age.
  .plotWgtAtAge( lh, gfx=gfx )

  # Selectivity at age.
  .plotSelAtAge( lh, gfx=gfx )

  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Life History" )
}     # END function .plotLifeHist


.plotLenAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )
    yLim <- c( 0,max(obj$lenAge) )
  
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$lenAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( "Length-at-age", .LenUNIT ) )
}     # END function .plotLenAtAge


.plotMatAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$matAge) )
    
  # Maturity at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$matAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )

  if ( gfx$annotate )
  {  
    A50 <- max( obj$ages[ obj$matAge <= 0.5001 ] )
    A95 <- max( obj$ages[ obj$matAge <= 0.9501 ] )
    segments( A50, 0.0,  A50, 0.5,  lty=2, lwd=2 )
    segments( 0.0, 0.5,  A50, 0.5,  lty=2, lwd=2 )
    segments( A95, 0.0,  A95, 0.95, lty=2, lwd=2 )
    segments( 0.0, 0.95, A95, 0.95, lty=2, lwd=2 )
  }
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Maturity-at-age" )
}     # END function .plotMatAtAge


.plotNumAtAge <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  obj$numAgeYr1 <- obj$numAgeYr1/1.e6
  yLim <- c( 0,max(obj$numAgeYr1 ) )
  yLim[2] <- round( yLim[2],digits=1 )
  # Numbers at age in year 1.
  barplot( obj$numAgeYr1,names.arg=obj$ages, axes=FALSE, xlab="", ylab="",
           ylim=yLim )
  axis (side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )  
  box()

  if ( gfx$doLegend )
  {    
    legend( "top",legend=c(paste("Max:",round(max(obj$numAgeYr1),digits=4)),
            paste("Min:", round(min(obj$numAgeYr1), digits=4))),
            cex=.CEXLEG2, bty="n")
  }
            
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, .AgeLAB )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Number at Age-1" )
}     # END function .plotNumAtAge


.plotWgtAtAge <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  xRange <- c( 0,max(obj$ages) )

  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$wtAge)*1.e6 )
  
  # Weight at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$wtAge*1.e6, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, paste( .AgeLAB, .AgeUNIT ) )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( "Weight-at-age", .WgtUNIT) )
}     # END function .plotWgtAtAge


.plotWgtLen <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  obj$wtAge <- 1.e6*obj$wtAge
  
  if ( is.null(xLim) )
    xLim <- c( 0, max( obj$lenAge ) )

  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$wtAge ) )
  
  # Weight against length.
  plot( obj$lenAge, obj$wtAge, type="n", axes=FALSE,
        xlab="",xlim=xLim,ylab="",ylim=yLim )
  lines( obj$lenAge, obj$wtAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
    
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, paste( .LenLAB, .LenUNIT ) )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, paste( .WgtLAB, .WgtUNIT ) )    
}     # END function .plotWgtLen


# .plotSelAge  (Plot the operating model selectivity at age)
# Purpose:      Plot the life history schedules specified by the operating model.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotSelAtAge <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0, max( obj$ages ) )

  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$selAge ) )
    
  # Selectivity at age.
  plot( obj$ages, obj$selAge, type="n", axes=FALSE,
    xlab="", ylab="", xlim=xLim, ylim=yLim )
  lines( obj$ages, obj$selAge, col=.LhCOL, lty=.LhLTY, lwd=.LhLWD )

  age50 <- max( obj$ages[ obj$selAge <= 0.5001 ] )
  age95 <- max( obj$ages[ obj$selAge <= 0.9501 ] )
  
  segments( age50, 0, age50, 0.5,  lty=2, lwd=2 )
  segments( 0,   0.5, age50, 0.5,  lty=2, lwd=2 )
  segments( age95, 0, age95, 0.95, lty=2, lwd=2 )
  segments( 0,  0.95, age95, 0.95, lty=2, lwd=2 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )  
  box()
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, "Age" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Selectivity-at-age" )

  result <- cbind( Age=obj$ages, Selectivity=obj$selAge )
  result
}     # END function .plotSelAtAge

.plotMPindexParsFF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                               xLim=NULL, yLim=NULL ) )
{
  nT           <- obj$opMod$nT
  tMP          <- obj$opMod$tMP

  val <- .calcTimes( obj )
  idx1 <- val$per1Survey
  idx2 <- val$per2Survey  
  t1Survey <- val$t1Survey
  t2Survey <- val$t2Survey
  
  # Generate example observation error CVs for the survey periods
  surveyError <- rep( 0, nT )
  # Draw CVs from (inverse gamma?) statistical distribution between Min and Max CV.
  
  # First survey period
  tauSurvey1Mean <- obj$mp$data$tauSurvey1Mean   # Assuming 1st input is mean CV
  tauSurvey1Var  <- obj$mp$data$tauSurvey1SD^2   # Second input is SD of CV
  
  alpha <- tauSurvey1Mean^2/tauSurvey1Var + 2.
  beta  <- tauSurvey1Mean*( tauSurvey1Mean^2/tauSurvey1Var + 1. )
  surveyError[t1Survey:(t2Survey-1)] <- rinvgamma( n=(t2Survey - t1Survey), shape=alpha, scale=beta )
  
  # Second survey period
  tauSurvey2Mean <- obj$mp$data$tauSurvey2Mean
  tauSurvey2Var  <- obj$mp$data$tauSurvey2SD^2

  alpha <- tauSurvey2Mean^2/tauSurvey2Var + 2.
  beta  <- tauSurvey2Mean*( tauSurvey2Mean^2/tauSurvey2Var + 1. )    
  surveyError[t2Survey:nT] <- rinvgamma( n=(nT - t2Survey + 1), shape=alpha, scale=beta )
  
  if ( !obj$mp$data$tauRandom )
  {
    # Survey errors fixed.
    tauSurvey1Mean <- obj$mp$data$tauSurvey1Mean
    tauSurvey2Mean <- obj$mp$data$tauSurvey2Mean
    # First survey period
    surveyError[t1Survey:(t2Survey-1)] <- tauSurvey1Mean
    # Second survey period
    surveyError[t2Survey:nT]           <- tauSurvey2Mean
  }
  
  # Normally-distributed survey errors ARK WHY THIS STEP?
  epsilont <- surveyError*rnorm(nT,0,1) - surveyError*surveyError/2.

  # Assign to "errors" object within operating model list.
  surveyCV <- surveyError
  
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c(0,nT)
    
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 0,max(surveyCV,na.rm=TRUE) )
  
  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  segments( t1Survey, tauSurvey1Mean, t2Survey-1, tauSurvey1Mean, col="black", lty=1, lwd=3 )
  segments( t2Survey, tauSurvey2Mean, nT,         tauSurvey2Mean, col="black", lty=1, lwd=3 )
   
  usr <- par( "usr" )
  yDelta <- usr[4]-usr[3]
  
  points( idx1, surveyCV[ idx1 ], cex=.ItCEX, pch=.ItPCH, bg=.ItBG )
  points( idx2, surveyCV[ idx2 ], cex=.ItCEX, pch=.ItPCH, bg=.ItBG )
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE3, cex=.CEXLAB2, "Survey CV" )
  
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$mp$data$tauSurvey2SD,   digits=2 )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( paste( "Period 1 CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" )),
      col=c(.ItCOL, .ItCOL),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }

}     # END function .plotMPindexPars

#------------------------------------------------------------------------------#
#-- (C) Plotting function for guiView                                          #
#------------------------------------------------------------------------------#

.plotAgeBubbles <- function( oat, pat, pltTitle="Proportions At Age", pwr=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 1,dim(oat)[2] )
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 1,( dim(oat)[1]+2 ) )

  par( oma=c(2.5,3,1,1), mar=c(1,1,1,1), mfrow=c(2,1 ) )

  # Observed age proportions.
  plotBubbles( oat, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Observed" )

  # Predicted age proportions.
  plotBubbles( pat, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Predicted" )

  # Residuals.
  ageResids <- oat - pat
  plotBubbles( ageResids, powr=pwr, xlim=xLim, ylim=yLim, xaxt="n" )
  axis( side=1 )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Residuals" )

  mtext( side=1, line=0, cex=.CEXLAB2, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, outer=TRUE, "Age Class" )
}     # END function .plotAgeBubbles


.plotAgeFreq <- function( patBars, patLines=NULL, pltTitle="Proportions At Age",
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE,
            xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  pLim <- c( 0, 0.25 )

  if ( !is.null(patBars) )
    ageDim <- dim(patBars)
  
  yearClasses <- c(1:ageDim[1])

  if ( is.null(xLim) )
    xLim <- c( 1,ageDim[1] )

  if ( is.null(yLim) )
   yLim <- c( 1,ageDim[2] )

   years  <- c( yLim[1]:yLim[2] )

   nYears <- length( years )

   myMar <- c(0.5,0.5,2,0)
   myOma <- c(4,5,4,3)

   if ( nYears <= 20 )
     par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/4),4 ) )
   if ( (nYears > 20 ) && (nYears <= 50) )
     par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/5),5 ) )
   if ( (nYears > 50 ) && (nYears <= 80 ) )
     par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/8), 8) )
   if ( (nYears > 80 ) )
     par( oma=myOma, mar=myMar, mfcol=c (ceiling(nYears/10),10) )

   for ( t in years )
   {
     plot( xLim, pLim, type="n", axes=FALSE, xlab="", ylab="" )

     # Observed age proportions - these are plotted as vertical lines..
     if ( !is.null(patBars) )
       lines( yearClasses, y=patBars[,t], type="h", lwd=.LWD2 )

     # Predicted age proportions - these are plotted as red a line.
     if ( !is.null(patLines) )
       lines( yearClasses, y=patLines[,t], type="l", lwd=.LWD2, col="red" )

     if ( gfx$useYears )
       panLab( 0.1, 0.9, cex=.CEXLAB4, t+.INITYEAR-1 )
     else
       panLab( 0.1, 0.9, cex=.CEXLAB4, t )     

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
   }

   mtext( side=1, line=.OUTLINE2, cex=.CEXLAB2, outer=TRUE, "Age Class" )
   mtext( side=2, line=.OUTLINE3, cex=.CEXLAB2, outer=TRUE, "Age Proportions" )
   
   if ( gfx$annotate )
   {
     mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, pltTitle )
   }
 
}     # END function .plotAgeFreq


.plotBtCtFt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL,
  useYears=FALSE ) )
{
  Bmsy <- obj$refPtList$ssbFmsy

  Bt <- obj$om$SBt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- apply( obj$om$Dtg,c(1,2),sum )[ iRep, ]  
  
  # Extract Ft for the 3 commercial gears.
  Ft <- obj$om$Ftg[ iRep,(1:ncol(obj$om$Ft)),c(1:3) ]
  Mt <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt))]

  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # Plot LRP and USR
  B0  <- obj$ctlList$opMod$B0
  LRP <- .BlimHerring
  TRP <- .USRHerring
  

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- range(c( tMP - 1,nT ))
  else
    if ( is.null(xLim) )
      xLim <- range(c( 0,nT ))

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )

  if ( is.null(yLim2) )
    yLim2 <- range( c(0,Ct) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Ft) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  abline( h=c(LRP*B0, TRP*B0), lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
          col = c(.LrpCOL, .TrpCOL) )
  panLegend(  x = 0.8, y = 0.9,
              legTxt = c("LRP", "TRP"), 
              lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
              col = c(.LrpCOL, .TrpCOL))
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Biomass" )

  # Panel 2: Plot catch.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ct, col=.CtCOL, lty=.CtLTY, lwd=.CtLWD )
  points( c(1:nT), Ct, bg=.CtBG, cex=.CtCEX, col=.CtCOL, pch=.CtPCH )

  # lines( c(1:nT), Dt, col=.DtCOL, lty=.DtLTY, lwd=.DtLWD )
  # points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX, col=.DtCOL, pch=.DtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  # abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Catch" )
  
  # Panel 3: Plot fishing mortality by gear.

  .FtgCOL <- brewer.pal(n = ncol(Ft), name = "Dark2")

  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )
  lines( x = 1:nT, y = Mt, col = .MtCOL, lty = .MtLTY, lwd = .MtLWD )
  for ( g in 1:ncol(Ft) )
    lines( c(1:nT), Ft[,g], col=.FtgCOL[g], lty=.FtgLTY[g], lwd=.FtgLWD[g] )
    
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  
  if ( gfx$doLegend )
  {
    panLegend(  0.8,0.9, bty = "n",
                legTxt=c(expression(M[t]),obj$ctlList$opMod$gNames[1:3]), cex=1.2,
                col=c(.MtCOL,.FtgCOL[1:3]), 
                lty=c(.MtLTY, .FtgLTY[1:3]), 
                lwd=c(.MtLWD, .FtgLWD[1:3]) )
  }
  
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Mortality" )
  
  mtext( side=1, line=.OUTLINE, cex=.CEXAXIS2, outer=TRUE, "Year" )  
}     # END function .plotBtCtFt


.plotBtCtRt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=FALSE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL,
  useYears=FALSE ) )
{
  # Stock status zone boundaries.
  Bmsy      <- obj$refPtList$ssbFmsy

  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- apply( obj$om$Dtg,c(1,2),sum )[ iRep, ]
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]

  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # Plot LRP and USR
  B0  <- obj$ctlList$opMod$B0
  LRP <- .BlimHerring
  TRP <- .TRPHerring
  

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- range(c( tMP - 1,nT ))
  else
    if ( is.null(xLim) )
      xLim <- range(c( 0,nT ))

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )

  if ( is.null(yLim2) )
    yLim2 <- range( c(0,Ct) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Rt) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  abline( h=c(LRP*B0, TRP*B0), lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
          col = c(.LrpCOL, .TrpCOL) )
  panLegend(  x = 0.8, y = 0.9,
              legTxt = c("LRP", "TRP"), 
              lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
              col = c(.LrpCOL, .TrpCOL))
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Spawning biomass" )

  # Panel 2: Plot catch.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ct, col=.CtCOL, lty=.CtLTY, lwd=.CtLWD )
  points( c(1:nT), Ct, bg=.CtBG, cex=.CtCEX, col=.CtCOL, pch=.CtPCH )

  lines( c(1:nT), Dt, col=.DtCOL, lty=.DtLTY, lwd=.DtLWD )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX, col=.DtCOL, pch=.DtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  abline( h=obj$refPtList$yieldUmsy, lty=.MSYLTY )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Catch biomass" )

  # Panel 3: Plot recruits.
  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Rt, col=.RtCOL, lty=.RtLTY, lwd=.RtLWD )
  points( c(1:nT), Rt, bg=.RtBG, cex=.RtCEX, col=.RtCOL, pch=.RtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Recruits" )
  
  mtext( side=1, line=.OUTLINE, cex=.CEXAXIS2, outer=TRUE, "Year" )  
}     # END function .plotBtCtRt


.plotBtUtRt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=FALSE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL,
  useYears=FALSE ) )
{
  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  
  Bmsy <- obj$refPtList$ssbFmsy

  Mt <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt)) ]

  Bt <- obj$om$SBt[ iRep,(2:ncol(obj$om$FBt)) ]
  B0 <- obj$ctlList$opMod$B0
  
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]


  
  nCol       <- dim( obj$om$legalHR )[2]
  legalHR    <- obj$om$legalHR[ iRep,c(2:nCol) ]
  spawnHR    <- obj$om$spawnHR[ iRep,c(2:nCol) ]

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # Plot LRP and USR
  B0  <- obj$ctlList$opMod$B0
  LRP <- .BlimHerring
  TRP <- .USRHerring
  

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- range(c( tMP - 1,nT ))
  else
    if ( is.null(xLim) )
      xLim <- range(c( 0,nT ))

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )

  if ( is.null(yLim2) )
    yLim2 <- range( c(0,1) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Rt) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  abline( h=c(LRP*B0, TRP*B0), lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
          col = c(.LrpCOL, .TrpCOL) )
  panLegend(  x = 0.8, y = 0.9,
              legTxt = c("LRP", "TRP"), 
              lty=c(.LrpLTY,.TrpLTY), lwd = c(.LrpLWD, .TrpLWD),
              col = c(.LrpCOL, .TrpCOL))
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Biomass (kt)" )

  # Panel 2: Plot harvest rate.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )
  
  lines( c(1:nT), legalHR,    col=.LegUtCOL,  lty=.LegUtLTY,  lwd=.LegUtLWD )
  lines( c(1:nT), spawnHR,    col="darkgreen",  lty=.LegUtLTY,  lwd=.LegUtLWD )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  
  abline( h = obj$ctlList$mp$hcr$targHRHerring, lty = 2, lwd = 1 )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYear )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Harvest rate" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.75,0.95, legTxt=c("Total HR","Spawn HR"), cex=1.2,
      col = c(.LegUtCOL, "darkgreen"), bty = "n",
      lwd=c(.LegUtLWD,.SlegUtLWD) )    
  }

  # Panel 3: Plot recruits.
  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Rt, col=.RtCOL, lty=.RtLTY, lwd=.RtLWD )
  points( c(1:nT), Rt, bg=.RtBG, cex=.RtCEX, col=.RtCOL, pch=.RtPCH )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYear )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB2, "Recruits" )
  
  mtext( side=1, line=.OUTLINE, cex=.CEXAXIS2, outer=TRUE, "Year" )
}     # END function .plotBtUtRt


#.plotBtDtFt    (plot spawning biomass, catch, & fishing mort. vs. time)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual catch biomass, and the third
#                showing fishing mortality.  All values are from operating model.
# Notes:       Labelled "SSB Cat F" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotBtDtFt <- function( obj, iSim=1, iRep=1, gfx=list( animate=TRUE, annotate=TRUE,
                         doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # Stock status zone boundaries on SSB scale.
  #Bmsy      <- obj$pars$Fmsy
  #zoneLimit <- obj$pars$limitBoundMult * Bmsy
  #zoneUpper <- obj$pars$upperBoundMult * Bmsy

  Bt  <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Dt  <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Ft  <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]
  It  <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  It  <- It / obj$ctlList$opMod$q
  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  nTimes <- 1
  if ( gfx$animate )
  {
    ani.options( interval=0.05 )
    nTimes <- nT
  }

  idx <- c(1:nT)

  for ( i in seq_len(nTimes) )
  {
    if ( gfx$animate )
    {
      idx <- c(1:i)
      dev.hold()
    }

    # Panel 1: Plot biomass and survey index.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[1,]
    else
    {
      yLim <- c(0,max(c(Bt,It), na.rm=T))
    }
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
    points( idx, It[idx], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
    points( idx, It[idx] )
  
    abline( h=obj$ctlList$refPts$ssbFmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )

    browser()
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Biomass and Index" )
    mtext( side=1, line=.INLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Bmsy"), cex=.CEXLEG2,
              col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD, bg="white", bty=.LEGBTY  )
    }

    # Panel 2: Plot catch.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[2,]
    else
    {
      yLim <- c(0,max(Dt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Dt[idx], col=.DtCOL, lty=.DtLTY,  lwd=.DtLWD )
    points( idx, Dt[idx], bg=.DtBG,    cex=.DtCEX, pch=.DtPCH )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    abline( h=obj$ctlList$refPts$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .DtLAB, " (",.DtUNIT,")", sep="" ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MSY"), cex=.CEXLEG2,
        bg="white", col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD, bty=.LEGBTY )
    }

    # Panel 3: Plot fishing mortality.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[3,]
    else
    {
      yLim <- c(0,max(Ft))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Ft[idx], lty=.FtLTY,  lwd=.FtLWD, col=.FtCOL )
    abline( h=obj$ctlList$refPts$Fmsy, lty=.FmsyLTY, col=.FmsyCOL, lwd=.FmsyLWD )
    abline( h=obj$ctlList$refPts$Fcra, lty=.FcraLTY, col=.FcraCOL, lwd=.FcraLWD )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .FtLAB, .FtUNIT ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Fmsy","Fcrash"), cex=.CEXLEG2,
                col=c(.FmsyCOL,.FcraCOL), lty=c(.FmsyLTY,.FcraLTY),
                lwd=c( .FmsyLWD, .FcraLWD ), bg="white", bty=.LEGBTY )
    }
    
    if ( gfx$animate )
      ani.pause()
  }
}     # END function .plotBtDtFt


#.plotBtDtRt    (plot spawning biomass, catch, & recruitment. vs. time)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual catch biomass, and the third
#                showing annual recruits.  All values are from operating model.
# Notes:       Labelled "SSB Cat R" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotBtDtRt <- function( obj, iSim=1, iRep=1, gfx=list( animate=FALSE, annotate=TRUE,
                         doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  Bt  <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Dt  <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Rt  <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  It  <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  It  <- It / obj$ctlList$opMod$q
  nT  <- length( Bt )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  nTimes <- 1
  if ( gfx$animate )
  {
    ani.options( interval=0.05 )
    nTimes <- nT
  }

  idx <- c(1:nT)

  for ( i in seq_len(nTimes) )
  {
    if ( gfx$animate )
    {
      idx <- c(1:i)
      dev.hold()
    }

    # Panel 1: Plot biomass and survey index.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[1,]
    else
    {
      yLim <- c(0,max(c(Bt,It), na.rm=T))
    }
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    
    lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
    points( idx, It[idx], bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
    points( idx, It[idx] )
  
    abline( h=obj$ctlList$refPts$ssbFmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Biomass and Survey Index" )
    mtext( side=1, line=.INLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Bmsy"), cex=.CEXLEG2,
              col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD, bg="white", bty=.LEGBTY  )
    }

    # Panel 2: Plot catch.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[2,]
    else
    {
      yLim <- c(0,max(Dt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( idx, Dt[idx], col=.DtCOL, lty=.DtLTY,  lwd=.DtLWD )
    points( idx, Dt[idx], bg=.DtBG,    cex=.DtCEX, pch=.DtPCH )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    abline( h=obj$ctlList$refPts$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .DtLAB, " (",.DtUNIT,")", sep="" ) )

    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("MSY"), cex=.CEXLEG2,
        bg="white", col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD, bty=.LEGBTY )
    }

    # Panel 3: Plot recruitment.
    # Y-axis limits.
    if ( !is.null(gfx$yLim) )
      yLim <- gfx$yLim[3,]
    else
    {
      yLim <- c(0,max(Rt))
    }

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    lines( idx, Rt[idx], lty=.RtLTY, lwd=.RtLWD )
    points( idx, Rt[idx], bg=.RtBG, cex=.RtCEX, pch=.RtPCH )  
    
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )  
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB2, paste( .RtLAB, .RtUNIT ) )

    if ( gfx$doLegend )
    {
    }
    
    if ( gfx$animate )
      ani.pause()
  }
}     # END function .plotBtDtRt


.plotMt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ),
                     lab = TRUE )
{
  M   <- obj$ctlList$opMod$M
  Mt  <- obj$om$Mt[ iRep,(2:ncol(obj$om$Mt)) ]
  nT  <- length( Mt )
  tMP <- obj$ctlList$opMod$tMP
  
  # What about Mt fits?
  retroMt <- obj$mp$assess$retroMt
  retroMt <- retroMt[retroMt[,"iRep"] == iRep, 2:ncol(retroMt)]

  # Turn deviations into M values
  for( c in 2:ncol(retroMt))
  {
    if(c == 2) 
    {
      retroMt[,c] <- M
      next
    }
    retroMt[,c] <- retroMt[,c-1] * exp(retroMt[,c])
  }

  xLim <- gfx$xLim
  yLim <- gfx$yLim
    
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( Mt ) )
  }
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  # abline( h=M, col=.MCOL, lty=.MLTY, lwd=.MLWD )

  #Plot retroMt values
  for( r in 1:nrow(retroMt) )
  {
    Mcol <- "grey70"
    lines( x = (1:nT), y = retroMt[r,2:(nT+1)], col = Mcol ) 
  }

  lines( x = (1:nT), y = retroMt[1,2:(nT+1)], col = "green", lwd = 3 ) 
  lines( x = (1:nT), y = retroMt[2,2:(nT+1)], col = "darkgreen", lwd = 3 ) 
  # lines( x = (1:nT), y = retroMt[nrow(retroMt),2:(nT+1)], col = "red", lwd = 3 ) 
   
  lines( c(1:nT), Mt, col=.MtCOL, lty=.MtLTY, lwd=.MtLWD )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  


    
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
    
  box()
  if(lab)
  { 
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Natural Mortality" )
  }

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Natural Mortality" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05, 0.25, legTxt=c( .MtLAB," ",
               paste( "Mean =", obj$ctlList$opMod$M ),
               paste( "Std. Dev. =", obj$ctlList$opMod$sigmaM ),
               paste( "Correlation =", obj$ctlList$opMod$gammaM ) ),
               cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }
}     # END function .plotMt



#.plotRt        Plot the recruitment numbers vs. time
# Purpose:      Plot the number of fish recruited into fishery in each year
#                   for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=vwUseYears ) )
{
  Rt   <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  nT   <- length( Rt )
  tMP  <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max( Rt ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT),  Rt, lty=.RtLTY, lwd=.RtLWD )
  points( c(1:nT), Rt, bg=.RtBG,   cex=.RtCEX, pch=.RtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( .RtLAB," (",.RtUNIT,")",sep="" ) )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Recruits" )
}     # END function .plotRt


#.plotRtBt  (plot the stock-recruitment points and curve)
# Purpose:  Plots annual spawner vs. recruits values as well as underlying
#                   spawner-recruitment relationship for a single replicate,
#                   irep, from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRtBt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                      doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  Bt <- obj$om$SBt[ iRep,(2:ncol(obj$om$Bt)) ]

  if( !is.null(obj$ctlList$opMod$posteriorDraws) )
  {
    postDraw    <- obj$ctlList$opMod$posteriorDraws[iRep]
    B0          <- obj$ctlList$opMod$mcmcPar[postDraw,"sbo"]
    rSteepness  <- obj$ctlList$opMod$mcmcPar[postDraw,"h"]

    # Read in mcmcM to rescale R0
    mcmcM       <- read.csv(paste(obj$ctlList$opMod$posteriorSamples,"/mcmcMt.csv",sep = ""), header =T )
    Mbar        <- mean(as.numeric(mcmcM[postDraw,]))

    # Rescale R0 and recalc rec.a
    R0          <- obj$ctlList$opMod$mcmcPar[postDraw,"ro"] * exp(Mbar)
    rec.a       <- 4.*rSteepness*R0 / ( B0*(1.-rSteepness) )
    rec.b       <- (5.*rSteepness-1.) / ( B0*(1.-rSteepness) )

    ssb         <- seq(0,4*B0, length = 100)
    recruits    <- rec.a * ssb / (1 + rec.b*ssb)
  } else {
    ssb      <- refPtList$ssb
    recruits <- refPtList$recruits
  }


  nT  <- length( Rt )
  tMP <- obj$ctlList$opMod$tMP

  refPtList <- obj$refPtList

  pchVec <- rep( 21,nT )
  pchVec[ 1:(tMP-2) ] <- 22
  colVec <- rep( "gray85",nT )
  colVec[ 1:(tMP-2) ] <- "white"

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,max( Bt ) )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(Rt) )
    
  # Lag the biomass (t) relative to the recruits (t+1).
  Rt <- Rt[ 2:length(Rt) ]
  Bt <- Bt[ 1:(length(Bt)-1) ]

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  points( Bt, Rt, cex=.CEXSYM4, bg=colVec, pch=pchVec )


  lines( ssb[ssb >= 0.0], recruits[ssb>=0.0], lty=1, lwd=.LWD2 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Spawning Biomass (year t-1)" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Recruits Numbers (year t)" )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )  

  if ( gfx$doLegend )
    panLegend( 0.7,0.95, legTxt=c("History","Projection"), cex=.CEXLEG2,
      pt.cex=.CEXSYM4, pch=c(22,21), pt.bg=c("white","gray86"), bty=.LEGBTY )

  return( invisible() )
}     # END function .plotRtBt


#.plotRecSpawnMP  (plot the stock-recruitment points and curve from the MP, not OM)
# Purpose:      Plots (estimated) annual spawner vs. recruits values as well as underlying
#                   spawner-recruitment relationship for a single replicate,
#                   irep, from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRecSpawnMP <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL) )
{
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP
  nProj <- nT - tMP + 1

  # Extract the recruits and biomass from the management procedure assessment.
  # Since there are nT-tMP+1 attempts, we'll take the terminal estimates.

  idx <- obj$mp$assess$Rt[,"iRep"]==iRep & obj$mp$assess$Rt[,"tStep"]==nT
  Rt  <- obj$mp$assess$Rt[ idx,(3:ncol(obj$mp$assess$Rt)) ]

  #idx <- obj$mp$assess$spawnBt[,"iRep"]==iRep & obj$mp$assess$spawnBt[,"tStep"]==nT
  
  idx <- obj$mp$assess$retroSpawnBt[,"iRep"]==iRep & obj$mp$assess$retroSpawnBt[,"tStep"]==nT 
  Bt  <- obj$mp$assess$retroSpawnBt[ idx,(3:ncol(obj$mp$assess$retroSpawnBt)) ]

  rp <- calcRefPoints( obj$ctlList$opMod, rpList=list( FALL=TRUE ) )

  pchVec <- rep( 21,nT )
  pchVec[ 1:(tMP-2) ] <- 22
  colVec <- rep( "gray85",nT )
  colVec[ 1:(tMP-2) ] <- "white"

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
  {
    xLim <- c( 0, max(Bt) )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max(Rt,na.rm=TRUE) )
  }

  # Lag the biomass (t) relative to the recruits (t+1).
  Rt <- Rt[ 2:length(Rt) ]
  Bt <- Bt[ 1:(length(Bt)-1) ]

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  points( Bt, Rt, cex=.CEXSYM4, bg=colVec, pch=pchVec )
  
  # Add the ESTIMATED stock-recruit relationship for the terminal estimate.
  omB <- seq( 0,obj$ctlList$opMod$B0,length=100 )
  idx <- obj$mp$assess$mpdPars[,"iRep"]==iRep & obj$mp$assess$mpdPars[,"tStep"]==nT
  recA <- obj$mp$assess$mpdPars$rec.a[ idx ]
  recB <- obj$mp$assess$mpdPars$rec.b[ idx ]
  omR <- recA * omB / ( 1.0 + recB * omB )
  lines( omB,omR, col="red", lwd=2 )

  # Add the TRUE stock-recruit relationship.
  lines( rp$ssb[rp$ssb >= 0.0], rp$recruits[rp$ssb>=0.0], col=.OmCOL, lty=.OmLTY, lwd=.OmLWD )
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Spawning Biomass (year t-1)" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Recruit Numbers (year t)" )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )  

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95,legTxt=c("History","Projection"), pt.cex=.CEXSYM2,
      pch=c(22,21), pt.bg=c("white","gray86"), bty=.LEGBTY )
      
    panLegend( 0.05,0.85,legTxt=c("True","Estimated"),lty=c(1,1), lwd=c(.LWD2, .LWD2),
               col=c("black","red"), bty=.LEGBTY )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Rec-Spawners" )
  }
  return( invisible() )
}     # END function .plotRecSpawnMP


#.plotIt        Plot the survey index series vs. time
# Purpose:      Plot the number of fish recruited into fishery in each year
#                   for a single replicate, irep, from
#                   simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotIt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  ItOM <- obj$om$It[ iRep,(2:ncol(obj$om$It)) ]
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  nT <- length( It )
  tMP  <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max( It, na.rm=TRUE ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), ItOM, col=.ItCOL, lty=.ItLTY, lwd=.ItLWD )
  points( c(1:nT), It, bg=.ItBG, col=.ItCOL, fg=.ItFG, cex=.ItCEX, pch=.ItPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( .ItLAB ,sep="" ) )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c( paste( .ItLAB, "(OM)" ), paste( .ItLAB, "(Obs)" ) ),
               pt.bg=c(NA,.ItBG), col=c( .ItCOL, .ItCOL ), lty=c( .ItLTY,NA ),
               lwd=c(.ItLWD,NA), pch=c(NA,.ItPCH), pt.cex=c(NA,.ItCEX), bg="white",
               bty=.LEGBTY, cex=.CEXLEG2 )
  }
  
  # Now plot the CVs.
  
  surveyCV <- obj$om$surveyCV[ iRep,(2:ncol(obj$om$surveyCV)) ]
  plot( xLim, c(0,max(surveyCV,na.rm=TRUE)), type="n", axes=FALSE, xlab="", ylab="" )

  tmp <- .calcTimes( obj$ctlList )
  
  lines( tmp$per1Survey, surveyCV[ tmp$per1Survey ], type="h", col=.Per1COL, lwd=3 )  
  lines( tmp$per2Survey, surveyCV[ tmp$per2Survey ], type="h", col=.Per2COL, lwd=3 )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Survey CV" )
  
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( paste( "Period 1 CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" )),
      col=c(.Per1COL, .Per2COL),
      lwd=c(3,3), bg="white", bty=.LEGBTY, cex=.CEXLEG2 )  
  }

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Survey Index Series" )
}     # END function .plotIt

#.plotItBt  (plot the observed survey points against true SSB)
# Purpose:      Plots annual survey indices of spawning boimass against
#               true spawning biomass from the operating model for a single
#               replicate, irep, from simulation scenario isim.
# Notes:        Labelled "Survey vs Biomass" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotItBt <- function( obj, iSim=1, iRep=1, xAxis, yAxis, annotate=TRUE, showProj=FALSE,
                           yScale=FALSE, xScale=FALSE  )
{
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  
  # Scale It to spawning biomass.
  It <- It / obj$ctlList$opMod$q

  nT <- length( It )

  # X-axis limits.
  if ( xScale )
  {
    xAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
    xLim <- c( 0,max(Bt) )
  }
  if ( !xScale )
    xLim <- xAxis

  # Y-axis limits.
  if ( yScale )
  {
    yAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
    yLim <- c( 0,max(It) )
  }
  if ( !yScale )
    yLim <- yAxis

  lmFit <- lm( It~Bt )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( a=0.0, b=1.0, col="black", lty=1, lwd=.LWD2 )
  abline( lmFit,        col="red",   lty=2, lwd=.LWD2 )
  
  points( Bt, It, cex=.CEXSYM8, col="black", pch=21 )
  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  box()
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, paste( .BtLAB, " (",.BtUNIT,")", sep="") )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .ItLAB )

  if ( annotate )
    mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Survey vs. SSB" )
}     # END function .plotItBt


#.plotObsSurvey  (plot observed biomass estimates vs. time)
# Purpose:      Plots annual estimates of spawning boimass in each estimation
#               year as well as true spawning biomass for a single replicate,
#               irep, from simulation scenario isim.
# Notes:        Labelled "SSB Survey Fit" in guiView
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotObsSurvey <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                    doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # What stock assessment method?
  methodId   <- obj$ctlList$mp$assess$methodId
  methodName <- .METHODLAB[ methodId ]

  omBt       <- obj$om$Bt
  omBt       <- omBt[ iRep, (2:ncol(omBt)) ]

  omBexp     <- obj$om$Bexp
  omBexp     <- omBexp[ iRep, (2:ncol(omBexp)) ]
  
  exploitBt  <- obj$mp$assess$exploitBt
  exploitBt  <- exploitBt[ iRep,(2:ncol(exploitBt)) ]
  
  It        <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  
  # Now correct It for last fitted q.
  qMP <- obj$mp$assess$mpdPars$q
  qTemp <- qMP[ !is.na(qMP) ]
  It <- It / qTemp[ length(qTemp) ]
  
  surveyCV1 <- obj$om$surveyCV[,"surveyCV1"]
  surveyCV2 <- obj$om$surveyCV[,"surveyCV2"]
  nT        <- obj$ctlList$opMod$nT
  tMP       <- obj$ctlList$opMod$tMP

  # X-axis limits.
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0,max(c(omBt,It),na.rm=TRUE) )
  }

  plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
  
  lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )  
  lines( c(1:nT), omBexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )  
  
  points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )
  
  # Plot terminal biomass estimates - these might be the one-year ahead projection.
  # This gets back to whether it is beginning of year or end of year biomass.
  
  lines( c(1:nT), exploitBt, col=.BtEstCOL, lty=.BtEstLTY, lwd=.BtEstLWD )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()

  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .ItLAB )

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Spawning and Estimated Biomass" )
    
  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
    
    methodId <- obj$ctlList$mp$assess$methodId
    label <- paste( .METHODLAB[methodId]," (",obj$ctlList$mp$hcr$hcrType,")", sep="" )
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( label, paste( "Period 1 Survey CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 Survey CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" ),
              .BtLAB, .BexpLAB, .BtEstLAB, "Observed survey biomass" ),
      col=c("white", "white","white",.BtCOL,.BexpCOL,.BtEstCOL,.ItFG),
      lty=c(NA,NA,NA,.BtLTY,.BexpLTY,.BtEstLTY,NA),
      pt.cex=c(NA,NA,NA,NA,NA,NA,.ItCEX), pt.bg=c(NA,NA,NA,NA,NA,NA,.ItBG),
      lwd=c(NA,NA,NA,.BtLWD,.BexpLWD,.BtEstLWD,NA), pch=c(NA,NA,NA,NA,NA,NA,.ItPCH),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )  
  }
}     # END function .plotObsSurvey


#.plotModelBtFit  (plot observed biomass estimates vs. time)
# Purpose:      Plot all predicted biomass states from annual production model
#               fits for replicate irep from simulation scenario isim.
# Notes:        Labelled "SSB Stepwise Fits" in guiView
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (17-Aug-09)
.plotModelBtFit<- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                            doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # What stock assessment method?
  assessMethod <- obj$ctlList$mp$methodId

  omBt      <- obj$om$Bt
  omBt      <- omBt[ iRep, (2:ncol(omBt)) ]

  omBexp    <- obj$om$Bexp
  omBexp    <- omBexp[ iRep, (2:ncol(omBexp)) ]
  
  # Same issues as elswhere, which Bt is this?
  assessBt  <- obj$mp$assess$Bt
  assessBt  <- assessBt[ iRep,(2:ncol(assessBt)) ]
  
  It        <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  surveyCV1 <- obj$om$surveyCV[,"surveyCV1"]
  surveyCV2 <- obj$om$surveyCV[,"surveyCV2"]
  nT <- length( It )
  
  methodId <- obj$ctlList$mp$assess$methodId

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    xLim <- c(1,nT)
    
  if ( gfx$showProj )
    xLim <- c( obj$ctlList$opMod$tMP - 1,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(It,na.rm=TRUE) )
  }

  plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )

  abline( v=obj$ctlList$opMod$tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.LWD )

  lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  lines( c(1:nT), omBexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
  points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )  
  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box( lwd=.LWD )
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4,  "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4 , paste( .BtLAB," (",.BtUNIT,")", sep="" ) )

  # Add lines for all retrospective exploitable biomass states
  retroExpBt <- obj$mp$assess$retroExpBt[ obj$mp$assess$retroExpBt[,1]==iRep, ]

  for ( j in 1:nrow(retroExpBt) )
  {
    tStep <- retroExpBt[ j,"tStep" ]
    lines( retroExpBt[ j, 3:ncol(retroExpBt) ], col=.BtStepCOL, lty=.BtStepLTY, lwd=.BtStepLWD )
  }

  if ( gfx$annotate )
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE,
      paste( "Stepwise Model Fits: ", .METHODLAB[methodId], sep="" ) )  

  if ( gfx$doLegend )
  {
    tauSurvey1Mean <- round( obj$ctlList$mp$data$tauSurvey1Mean, digits=2 )
    tauSurvey1SD   <- round( obj$ctlList$mp$data$tauSurvey1SD,   digits=2 )
    tauSurvey2Mean <- round( obj$ctlList$mp$data$tauSurvey2Mean, digits=2 )
    tauSurvey2SD   <- round( obj$ctlList$mp$data$tauSurvey2SD,   digits=2 )
    
    methodId <- obj$ctlList$mp$assess$methodId
    label <- paste( .METHODLAB[methodId]," (",obj$ctlList$mp$hcr$hcrType,")", sep="" )    
        
    panLegend( 0.05,0.95, adj=0,
    legTxt=c( label, paste( "Period 1 Survey CV: ",
                format( tauSurvey1Mean )," (", format( tauSurvey1SD ),")", sep="" ),
              paste( "Period 2 Survey CV: ",
                format( tauSurvey2Mean )," (", format( tauSurvey2SD ),")", sep="" ),
              .BtLAB, .BexpLAB, .BtEstLAB, "Observed survey biomass" ),
      pt.bg=c( NA,NA,NA,NA,NA,NA, .ItBG ), pt.cex=c(NA,NA,NA,NA,NA,NA, .ItCEX ),
      col=c("white","white","white",.BtCOL,.BexpCOL,.BtStepCOL,.ItCOL), lty=c(NA,NA,NA,.BtLTY,.BexpLTY,.BtStepLTY,NA),
      lwd=c(NA,NA,NA,.BtLWD,.BexpLWD,.BtStepLWD,NA), pch=c(NA,NA,NA,NA,NA,NA,.ItPCH),
      cex=.CEXLEG, bg="white", bty=.LEGBTY )
  }
}     # END .plotModelBtFit


#.plotParEsts   (plot annual parameter estimates)
# Purpose:      Plot annual parameter estimates from stepwise model
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09)
.plotParEsts <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                          doLegend=TRUE, showProj=FALSE, yLim=NULL, xLim=NULL ) )
{
  iReplicate <- iRep
  nT         <- obj$ctlList$opMod$nT
  tMP        <- obj$ctlList$opMod$tMP

  # What stock assessment method?
  methodId <- obj$ctlList$mp$assess$methodId

  if ( methodId >= .PMOD  )
  {
    # Extract annual parameter estimates.
    mpdPars  <- obj$mp$assess$mpdPars

    iRepCol <- mpdPars[1] # extract this col only because iRep heading in mpdPars
                          # conflicts with function arguments

    BmsyEsts <- mpdPars$ssbFmsy[ iRepCol==iRep ]
    FmsyEsts <- mpdPars$Fmsy[ iRepCol==iRep ]
    
    MsyEsts <- rep( NA, sum( iRepCol==iRep ) )
    if ( any( names(mpdPars)=="Msy" ) )
      MsyEsts  <- mpdPars$Msy[ iRepCol==iRep ]

    BmsyUp <- 0
    BmsyLo <- 0
    FmsyUp <- 0
    FmsyLo <- 0

    if ( methodId == .PMOD )
    {
      # KRH - std not yet saved for caa model
      # Extract upper and lower error bars (+ 2 sd)
      #BmsyUp<- exp( log(BmsyEsts)+ 2*(stdBmsy[iRepCol==iRep]) )
      #BmsyLo<- exp( log(BmsyEsts)- 2*(stdBmsy[iRepCol==iRep]) )
      #FmsyUp<- exp( log(FmsyEsts)+ 2*(stdFmsy[iRepCol==iRep]) )
      #FmsyLo<- exp( log(FmsyEsts)- 2*(stdFmsy[iRepCol==iRep]) )
    }

    # Get OM equilibrium values for comparison.
    Bmsy        <- obj$refPtList$ssbFmsy
    Fmsy        <- obj$refPtList$Fmsy
    Umsy        <- obj$refPtList$Umsy
    legalHRFmsy <- obj$refPtList$legalHRFmsy
    
    yieldFmsy     <- obj$refPtList$yieldFmsy
    landedFmsy    <- obj$refPtList$landedFmsy
    discardedFmsy <- obj$refPtList$discardedFmsy

    xLim <- gfx$xLim
    yLim <- gfx$yLim[1,]

    if ( is.null(xLim) )
      xLim <- c(1,nT)

    if ( gfx$showProj )
      xLim <- c( tMP - 1, nT )

    if ( is.null(yLim) )
    {
      yLim <- c(0, max( c( Bmsy, mpdPars$ssbFmsy ) ) )
    }
 
    plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    
    
    points( tMP:nT, BmsyEsts, bg=.BmsyBG, col=.BmsyCOL, cex=.BmsyCEX, pch=.BmsyPCH )

    # KRH - std not yet saved for caa model
    if ( methodId == .PMOD )
    {
      #idx <- BmsyLo!=BmsyUp
      #arrows( (tMP:nT)[idx], BmsyLo[idx], (tMP:nT)[idx], BmsyUp[idx],
      #  angle=0, col="gray50", lty=1, lwd=2 )
    }

    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    
    axis( side=1, cex.axis=.CEXAXIS3 )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .BmsyLAB )
    
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.5, legTxt=c("MDP Estimate","Bmsy"),
                 pt.bg=c(.BmsyBG,NA), pt.cex=c(.BmsyCEX,NA), lty=c(NA,.BmsyLTY),
                 pch=c(.BmsyPCH, NA), col=c(.BmsyCOL,.BmsyCOL ),
                 lwd=c(1,.BmsyLWD), cex=.CEXLEG2, bg="white", bty=.LEGBTY )
    }

    # Plot Fmsy estimates.
    yLim <- gfx$yLim[2,]
    if ( is.null(yLim) )
    {
      yLim <- c( 0,max( c( Fmsy, mpdPars$Fmsy ) ) )
    }

    plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
    
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    
    
    points( tMP:nT, FmsyEsts, bg=.FmsyBG, cex=.FmsyCEX, col=.FmsyCOL, pch=.FmsyPCH )

    axis( side=1, cex.axis=.CEXAXIS3 )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    abline( h=Umsy, col=.UmsyCOL, lty=.UmsyLTY, lwd=.UmsyLWD )
    abline( h=legalHRFmsy, col=.FmsyCOL, lty=.FmsyLTY+2, lwd=.FmsyLWD )    

    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.5, legTxt=c("MPD Estimate","Fmsy", "Umsy","Legal U @ Fmsy"),
                 pt.bg=c(.FmsyBG,NA,NA,NA), pt.cex=c(.FmsyCEX,NA,NA,NA),
                 lty=c(NA,.FmsyLTY,.UmsyLTY,.FmsyLTY+2),
                 pch=c(.FmsyPCH,NA,NA,NA), col=c(.FmsyCOL,.FmsyCOL,.UmsyCOL,.FmsyCOL ),
                 lwd=c(.FmsyLWD,.FmsyLWD,.UmsyLWD,.FmsyLWD), cex=.CEXLEG2, bg="white", bty=.LEGBTY )
    }
    
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .FmsyLAB )
    
    # Plot MSY estimates.

    yLim <- gfx$yLim[3,]
    if ( is.null(yLim) )
    {
      yLim <- c( 0,max( c( landedFmsy, mpdPars$Msy ) ) )
    }

    plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
    
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    
    
    points( tMP:nT, MsyEsts, bg=.MSYBG, cex=.MSYCEX, col=.MSYCOL, pch=.MSYPCH )

    axis( side=1, cex.axis=.CEXAXIS3 )
    axis( side=2, cex.axis=.CEXAXIS3, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    #abline( h=yieldFmsy,  col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    
    abline( h=landedFmsy,    col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    abline( h=discardedFmsy, col=.FmsyCOL, lty=.FmsyLTY+1, lwd=.FmsyLWD )
    
    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.5, legTxt=c("MPD Estimate","Landed @ Fmsy","Discarded @ Fmsy"),
                 pt.bg=c(.MSYBG,NA,NA), pt.cex=c(.MSYCEX,NA,NA), lty=c(NA,.FmsyLTY,.FmsyLTY+1),
                 pch=c(.FmsyPCH,NA,NA), col=c(.FmsyCOL,.FmsyCOL,.FmsyCOL ),
                 lwd=c(.FmsyLWD,.FmsyLWD,.FmsyLWD), cex=.CEXLEG2, bg="white", bty=.LEGBTY )
    }
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .MSYLAB )
    
    if ( gfx$annotate )
      mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE,
         paste( .METHODLAB[methodId], "parameter estimates" ) )
  }
  
  return( invisible() )
}     # END function .plotParEsts

#.plotParCor   (plot correlation between Fmsy and Bmsy estimates)
# Purpose:      Plot annual parameter estimates of Fmsy vs. Bmsy from model
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09)
.plotParCor <- function( obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # What stock assessment method?
  methodId <- obj$ctlList$mp$assess$methodId

  if ( methodId >= .PMOD  )
  {
    # Extract annual parameter estimates.
    
    mpdPars  <- obj$mp$assess$mpdPars
    
    # Extract this column because iRep heading in esetPars conflicts with fun pars.
    iRepCol <- mpdPars[,"iRep" ]

    BmsyEsts <- mpdPars$ssbFmsy[ iRepCol==iRep ]
    FmsyEsts <- mpdPars$Fmsy[ iRepCol==iRep ]

    # Get equilibrium values for comparisson
    equilBmsy <- obj$ctlList$refPts$ssbFmsy
    equilFmsy <- obj$ctlList$refPts$Fmsy

    # Specify axes
    xLim <- gfx$xLim
    if ( is.null(xLim) )
    {
      xAvg <- mean( mpdPars$ssbFmsy )
      #xLim <- c(0,mean(xAvg))
      
      xLim <-c( 0, max( c( BmsyEsts, equilBmsy, na.rm=TRUE ) ) )
    }

    yLim <- gfx$yLim
    if ( is.null(yLim) )
    {
      yAvg <- mean( mpdPars$Fmsy )
      yLim <- c( 0, max( c( FmsyEsts, equilFmsy ), na.rm=TRUE ) )
    }

    plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
    
    points( BmsyEsts,  FmsyEsts,  cex=.CEXSYM4, pch=.BmsyPCH, col="black" )
    points( equilBmsy, equilFmsy, cex=.CEXSYM8, pch=.BmsyPCH, bg="red" )
    
    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, .BmsyLAB )
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .FmsyLAB )

    if ( gfx$annotate )
    {
      mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Parameter Correlation" )
      panLegend( 0.05,0.95, legTxt=c("Estimates","Equilibrium"),
                 bg="white", cex=.CEXLEG2, pch=c(.BmsyPCH,.BmsyPCH),
                 pt.bg=c("white","red"), pt.cex=.CEXSYM8,
                 col=c("black", "red"), bty=.LEGBTY)
    }
  }
}     # END function .plotParCor


# plotFvsSSB   (plot the DFO target harvest control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.  This plot shows the rule managers
#               were trying to implement.  In contrast, plotViewRealHCR shows
#               the pairs of F and SSB that actually occurred. In cases when
#               reference points are estimated, multiple lines are drawn to
#               show how HCR based on these points changes over time.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotFvsSSB <- function( obj, iSim=1, iRep=1, phase=FALSE, gfx=list( annotate=TRUE,
                          doGrid=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$ctlList$refPts$ssbFmsy
  Fmsy  <- obj$ctlList$refPts$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP

  # True spawning biomass and true fishing mortality
  idx   <- tMP:nT  
  Bt <- obj$om$FBt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  browser()

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
  {
    if ( phase )
    {
      Bt <- Bt / Bmsy
      xLim <- c( 0, max( 1,Bt ) )
    }
    else
      xLim <- c( 0,max(c(Bmsy,Bt)) )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    if ( phase )
    {
      Ft <- Ft / Fmsy
      yLim <- c( 0,max( 1, Ft ) )
    }
    else
      yLim <- c( 0,max(c(Fmsy,Ft)) )
  }

  plot( xLim,yLim, type="n",axes=FALSE, xlab="",ylab="" )

  # Indicate lRef, uRef, and Bref for true case.
  if ( phase )
  {
    abline( v=1, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( h=1, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    

  }
  else
  {
    abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }

  # NOW plot trajectory of target Ft by assessBt for tMP forward...
  # (Assume that time order has not be changed)

  colVec <- rev( heat.colors( n=length(Bt) ) )

  points( Bt, Ft, cex=.CEXSYM6, bg=colVec, pch=21 )
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, label=FALSE )
  
  if ( !phase )
  {
    depletion <- seq( 0,B0,B0/10.0 )
    axis( side=3, cex.axis=.CEXAXIS4, at=depletion, labels=depletion/B0 )
    
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, paste( .BtLAB," (",.BtUNIT,")", sep="" ) )
    mtext( side=2, line=.INLINE4, cex=.CEXLAB4, .FtLAB )    
    mtext( side=3, line=.INLINE3, cex=.CEXLAB4, "Depletion" )
    
    mainLab <- "Fishing Mortality vs. Spawning Biomass"    
  }
  else
  {
    axis( side=3, label=FALSE )
    mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Bt / Bmsy" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Ft / Fmsy" )
    
    mainLab <- "Phase Plot"
    if ( gfx$annotate )
      mtext( side=3, line=2, cex=.CEXTITLE4, "Phase Plot" )  
  }

  box()

  if ( gfx$doLegend )
  {
  }
  return( invisible() )
}     # END function .plotFvsSSB

# .plotHCRmpVariableF (plot a status-based rule)
# Purpose:      Plot a status-based harvest control rule, indicating the Lower
#               Bound, Upper Bound, and Removal Reference.
#               In cases when reference points are estimated, multiple lines are
#               drawn to show how HCR based on these points changes over time.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotHCRmpVariableF <- function( obj, iSim=1, iRep=1, gfx=list(annotate=TRUE,
             doGrid=FALSE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL) )
{
  # Get zone reference points.
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$ctlList$refPts$ssbFmsy
  Fmsy  <- obj$ctlList$refPts$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP

  # Indexing vector for projection years.     
  idx   <- tMP:nT

  # Determine base for stock status.whether objectives are based on Bmsy or B0.
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    Btarget <- Bmsy
    
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    Btarget <- B0

  # Determine HCR type and get properties.
  hcrType <- obj$mp$hcr$specs$hcrType

  # True biomass and realized fishing mortality
  omBt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  omFt <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  # Extract target control rules that were used in a subset of years:
  # Select every k-th year to plot HCR for in the middle of MP time series
  start  <- tMP
  end    <- nT
  len    <- trunc( (end - start)/5. )
  #len <- 1
  midYrs <- seq( start, end, length=len )

  # In addition to these years, always plot first and last year
  #plotYrs <- c( tMP, midYrs ,nT )
  plotYrs <- c(tMP:nT)

  # Are these MLE value or MCMC values?
  lowerBound <- obj$mp$hcr$lowerBound[ iRep,idx ]
  upperBound <- obj$mp$hcr$upperBound[ iRep,idx ]
  
  # Does this come from MLE or MCMC?
  refRemRate <- obj$mp$hcr$Fref[ iRep,idx ]

  # Estimated spawning biomass and intended target fishing mortality from rule.
  mpBt <- obj$mp$assess$spawnBt[ iRep,(2:ncol(obj$mp$assess$spawnBt)) ][idx]
    
  mpFt <- obj$mp$hcr$targetFt[ iRep,(2:ncol(obj$mp$hcr$targetFt)) ][idx]

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,max( c(B0,mpBt,omBt) ) )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(c(mpFt,omFt), na.rm=TRUE) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )

  # Add HCR for selected years to plot (first, last years have diffnt line types)
  for (j in 1:length(mpBt))
  {
    lty.tmp <- 1
    lwd.tmp <- 1
    col.tmp <- "grey30"
    
    if ( j==1 )    # first year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "blue"
    }
    
    if ( j==length(plotYrs))   # last year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "red"
    }

    if ( (hcrType=="variableF") | (hcrType=="variableFopMod") )
    {
      segments(           0,               0, lowerBound[j],             0,
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
      segments( lowerBound[j],             0, upperBound[j], refRemRate[j],
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
      segments( upperBound[j], refRemRate[j],        usr[2], refRemRate[j],
                col=col.tmp, lty=lty.tmp, lwd=lwd.tmp )
    }
  }

  # Plot trajectory of target Ft by assessBt for tMP forward, assume time order.
  colVec <- rev( heat.colors( n=length(mpBt) ) )

  # Test of whiskers... x0, y0, x1, y1
  if ( gfx$annotate )
  {
    segments( mpBt,mpFt, omBt,omFt )
    points( omBt,omFt, cex=.CEXSYM8, bg=colVec, pch=22 )
  }
  
  points( mpBt, mpFt, cex=.CEXSYM8, bg=colVec, pch=21 )

  # Axes.
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  
  #depletion <- seq( 0,B0,B0/10.0 )
  #axis( side=3, at=depletion, labels=depletion/B0 )
  
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Estimated Stock Status" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB4, "Intended Removal Rate (F)" )

  if ( gfx$doLegend )
  {
    panLegend( 0.8, 0.95, legTxt=c( "OM","MP" ), cex=.CEXLEG2, pch=c(22,21),
               pt.cex=c(.CEXSYM8, .CEXSYM8), bty=.LEGBTY )
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=1, cex=.CEXTITLE4, outer=TRUE, "Harvest Control Rule" )
    
}     # END function .plotHCRmpVariableF

# .plotHCRmpConstantF (plot a status-based rule)
# Purpose:      Plot a status-based harvest control rule, indicating the Lower
#               Bound, Upper Bound, and Removal Reference.
#               In cases when reference points are estimated, multiple lines are
#               drawn to show how HCR based on these points changes over time.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotHCRmpConstantF <- function( obj, iSim=1, iRep=1, gfx=list(annotate=TRUE,
             doGrid=FALSE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL) )
{
  # Get OM reference points.
  B0    <- obj$ctlList$opMod$B0
  Bmsy  <- obj$ctlList$refPts$ssbFmsy
  Fmsy  <- obj$ctlList$refPts$Fmsy
  nT    <- obj$ctlList$opMod$nT
  tMP   <- obj$ctlList$opMod$tMP

  # Indexing vector for projection years.     
  idx   <- tMP:nT

  # Determine base for stock status.whether objectives are based on Bmsy or B0.
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    Btarget <- Bmsy
    
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    Btarget <- B0

  # Determine HCR type and get properties.
  hcrType <- obj$mp$hcr$specs$hcrType

  # True spawning biomass and realized fishing mortality (on exploitable?).
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ][idx]

  # Extract target control rules that were used in a subset of years:
  # Select every k-th year to plot HCR for in the middle of MP time series
  start  <- tMP
  end    <- nT
  len    <- trunc( (end - start)/5. )
  midYrs <- seq( start, end, length=len )

  # In addition to these years, always plot first and last year
  plotYrs <- c( tMP, midYrs ,nT )

  lowerBound <- obj$mp$hcr$lowerBound[ iRep,idx ]
  upperBound <- obj$mp$hcr$upperBound[ iRep,idx ]
  refRemRate <- obj$mp$hcr$Fref[ iRep,idx ]

  # Estimated spawning biomass and intended target fishing mortality from rule.
  assessBt <- obj$mp$assess$Bt[ iRep,(2:ncol(obj$mp$assess$Bt)) ][idx]
  targetFt <- obj$mp$hcr$targetFt[ iRep,(2:ncol(obj$mp$hcr$targetFt)) ][idx]

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 0,B0 )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0,max(c(targetFt,Ft)) )

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )

  # Add HCR for selected years to plot (first, last years have diffnt line types)
  for (j in 1:length(assessBt))
  {
    lty.tmp <- 1
    lwd.tmp <- 1
    col.tmp <- "grey30"
    
    if ( j==1 )    # first year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "grey30"
    }
    if ( j==length(plotYrs))   # last year
    {
      lty.tmp <- 1
      lwd.tmp <- 1
      col.tmp <- "grey30"
    }

    segments( 0, refRemRate[j], usr[2], refRemRate[j], lty=lty.tmp, lwd=lwd.tmp )    
  }

  # Plot trajectory of target Ft by assessBt for tMP forward, assume time order.
  colVec <- rev( heat.colors( n=length(assessBt) ) )

  # Test of whiskers... x0, y0, x1, y1
  segments( assessBt,targetFt, Bt,Ft )
  points( assessBt, targetFt, cex=.CEXSYM4, bg=colVec, pch=21 )

  # Axes.
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  
  #depletion <- seq( 0,B0,B0/10.0 )
  #axis( side=3, at=depletion, labels=depletion/B0 )
  
  box()
  
  mtext( side=1, line=2.5, cex=.CEXLAB4, "Estimated Stock Status" )
  mtext( side=2, line=3.0, cex=.CEXLAB4, "Intended Removal Rate (F)" )

  if ( gfx$doLegend )
  {
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=1, cex=.CEXTITLE4, outer=TRUE, "Harvest Control Rule" )
    
}     # END function .plotHCRmpConstantF


# .plotRefPtSeries (plot the times series of reference points against objectives.
# Purpose:      This plot shows trajectories of Operating Model and MP estimates
#               of biomass and fishing mortality.
# Parameters:   obj - the blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotRefPtSeries <- function( obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE,colorZones=TRUE,doLegend=TRUE,xLim=NULL,yLim=NULL,
            yLim2=NULL ) )
{
  # Extract plot parameters.
  xLim       <- gfx$xLim
  yLim       <- gfx$yLim
  yLim2      <- gfx$yLim2

  nT         <- obj$ctlList$opMod$nT
  tMP        <- obj$ctlList$opMod$tMP

  # Extract relevant reference point bases: SPC edited 4June
  B0         <- obj$ctlList$opMod$B0
  Bmsy       <- obj$ctlList$refPts$ssbFmsy
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    base <- "Bmsy"
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    base <- "B0"

 #  if( obj$mp$hcr$specs$remRefBase=="rrBaseFmsy" & obj$mp$hcr$specs$remRefSource=="rrSrceEst" )
 #    remRate <- obj$ctlList$refPts$Fmsy
  # if ( obj$mp$hcr$specs$remRefBase != "rrBaseFmsy" )
  #   remRate <- obj$mp$hcr$specs$remRateInput

  remRate <- obj$mp$hcr$targHRHerring

  # HCR control point multipliers.
  # lowerBoundMult <- obj$ctlList$mp$hcr$lowerBoundMult
  # upperBoundMult <- obj$ctlList$mp$hcr$upperBoundMult

  # Lower and upper control points for HCR, plus base.
  lowerB  <- obj$mp$hcr$lowerBound
  upperB  <- obj$mp$hcr$upperBound
  Bref    <- obj$mp$hcr$Bref

  if( all(is.na(lowerB) ) )
  {
    lowerB <- rep(obj$mp$hcr$herringCutoffVal,nT)
    if( obj$mp$hcr$herringCutoffType == "relative")
      lowerB <- lowerB * B0
    upperB <- lowerB / (1 - remRate)
  }

  # Operating model Bt and Ft.
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]

  # Management procedure estimated Bt and Ft.
  BtEst <- obj$mp$assess$Bt[ iRep, (2:ncol(obj$om$Bt)) ]

  # Panel 1: Stock status - spawning biomass.

  # X-axis limits (Year).
  if ( is.null(xLim) )
  {
    xLim <- c( tMP-1,nT+1 )
  }

  # Y-axis limits (Spawning biomass).
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max( c( Bt,BtEst ), na.rm=TRUE ) )
  }

  plot( xLim,yLim, type="n",axes=FALSE,xaxs="i",xlab="",yaxs="i",ylab="" )

  usr <- par( "usr" )   # Get the plot region user coordinate limits.

  if ( base=="Bmsy" )
    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    
  if ( base=="B0" )
    abline( h=B0, col=.B0COL, lty=.B0LTY, lwd=.B0LWD )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  # Operating model spawning stock biomass.
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )

  # Estimated spawning stock biomass from method.
  lines( c(1:nT), BtEst, col=.BtEstCOL, lty=.BtEstLTY, lwd=.BtEstLWD )

  # Lower control bounds.
  idx <- lowerB[,"iRep"] == iRep
  points( c(1:nT), lowerB[idx,-1], bg=.HCRLBBG, col=.HCRLBCOL, cex=.HCRLBCEX,
          fg=.HCRLBFG, lty=.HCRLBLTY, lwd=.HCRLBLWD, pch=.HCRLBPCH )

  # Upper control bounds.
  idx       <- upperB[,"iRep"] == iRep
  points( c(1:nT), upperB[idx,-1], bg=.HCRUBBG, col=.HCRUBCOL, cex=.HCRUBCEX,
          fg=.HCRUBBG, lty=.HCRUBLTY, lwd=.HCRUBLWD, pch=.HCRUBPCH )

  # Estimated HCR base control point.
  idx <- Bref[,"iRep"] == iRep
  points( c(1:nT), Bref[idx,-1], bg=.BrefCOL, cex=.BrefCEX, fg=.BrefFG,
          lty=.BrefLTY, lwd=.BrefLWD, pch=.BrefPCH )

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  #depletion <- seq( 0,B0,B0/10.0 )
  #axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=2, line=.INLINE4, cex=.CEXLAB4, "Stock Status" )

  if ( gfx$doLegend )
  {
    panLegend( 0.6,0.95,
      legTxt=c( .BmsyLAB, .BtLAB, .BtEstLAB,
                paste( "HCR",.BrefLAB ), paste( "HCR",.HCRUBLAB ),
                paste( "HCR",.HCRLBLAB ) ),
      col=c( .BmsyCOL, .BtCOL, .BtEstCOL, .BrefFG, .HCRUBFG, .HCRLBFG ),
      pch=c(NA,NA,NA,.BrefPCH,.HCRUBPCH,.HCRLBPCH),
      pt.cex=c(NA,NA,NA,.BrefCEX,.HCRUBCEX,.HCRLBCEX),
      pt.bg=c(NA,NA,NA,.BrefBG,.HCRUBBG,.HCRLBBG ),
      lwd=c( .BmsyLWD, .BtLWD, .BtEstLWD,NA,NA,NA),
      lty=c(.BmsyLTY,.BtLTY,.BtEstLTY,NA,NA,NA),
      bg="white", bty=.LEGBTY )
  }

  # Panel 2: Fishing mortality.
  
  # Y-axis limits (Spawning biomass).
  if ( is.null(yLim2) )
  {
    yLim2 <- c( 0, max( Ft ) )
  }

  plot( xLim,yLim2, type="n",axes=FALSE,xaxs="i",xlab="",yaxs="i",ylab="" )

  abline( h=obj$ctlList$refPts$Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
 
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD ) 

  # Plot the OM fishing mortality.
  lines( c(1:nT), Ft, col=.FtCOL, lty=.FtLTY, lwd=.FtLWD )

  # Extract the reference and target (post rule) fishing mortality values.
  idx <- obj$mp$hcr$Fref[,"iRep"] == iRep
  Fref <- obj$mp$hcr$Fref[ idx, c(2:ncol(obj$mp$hcr$Fref)) ]

  idx <- obj$mp$hcr$targetFt[,"iRep"] == iRep
  Fhcr <- obj$mp$hcr$targetFt[ idx, c(2:ncol(obj$mp$hcr$targetFt)) ]

  idx <- Fref!=Fhcr
  arrows( c(1:nT)[idx], Fref[idx], c(1:nT)[idx], Fhcr[idx], col="gray", lwd=2, angle=15, length=0.15 )
  points( c(1:nT), Fref, bg=.FrefBG, cex=.FrefCEX*1.25, fg=.FrefFG, pch=.FrefPCH )
  points( c(1:nT), Fhcr, bg="yellow", cex=.FhcrCEX, fg=.FhcrFG, pch=.FhcrPCH )

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, cex.axis=.CEXAXIS4 )
  box()

  mtext( side=1, line=0.0,      cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB4,             "Removal Rate" )

  if ( gfx$doLegend )
  {
    panLegend( 0.6,0.95, legTxt=c( .FmsyLAB, .FtLAB, "RR Rate", "Removal Rate"  ),
      col=c( .FmsyCOL, .FtCOL, .FrefFG, .FhcrFG ),
      pch=c(NA,NA,.FrefPCH,.FhcrPCH),
      pt.cex=c( NA,NA,.FhcrCEX,.FhcrCEX ),
      pt.bg=c( NA,NA, .FrefBG, "yellow" ),
      lwd=c( .FmsyLWD, .FtLWD, NA, NA ),
      lty=c(.FmsyLTY,.FtLTY,NA,NA),
      bg="white", bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotRefPtSeries


#.plotRetroFits (Plot OM exploitable, spawning, and retrospective fits)
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual survey indices and stock
#                assessment model fits, and the third showing annual estimates.
# Notes:       Labelled "SSB Fit Assmt" in guiView window
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       K.R. Holt

.plotRetroFits <- function( obj, iSim, iRep, varName="exploitBt", 
   gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
             years=FALSE ) )
{
  nT   <- obj$ctlList$opMod$nT
  tMP  <- obj$ctlList$opMod$tMP

  # Operating model.
  Bt   <- obj$om$SBt[ iRep,(2:ncol(obj$om$Bt)) ]
  
  # Multi-gear model, there is no exploitable biomass that is easy - use legal.
  Bleg      <- obj$om$legalB[ iRep,(2:ncol(obj$om$legalB)) ]
  Ct        <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  ItgScaled <- obj$mp$assess$ItgScaled
  
  idx   <- obj$mp$assess$mpdPars$iRep==iRep  
  tStep <- obj$mp$assess$mpdPars$tStep[ idx ]
  #qMP   <- obj$mp$assess$mpdPars$q[ idx ]

  Bmsy          <- obj$refPtList$ssbFmsy
  landedFmsy    <- obj$refPtList$landedFmsy
  discardedFmsy <- obj$refPtList$discardedFmsy
  
  browser()

  # Management procedure.
  #Itg   <- obj$mp$assess$ItgScaled[ iRep,(2:ncol(obj$mp$assess$ItgScaled)), ]
  
  if ( varName=="exploitBt" )
  {
    retroBt <- obj$mp$assess$retroExpBt
    bioCOL <- .BexpRetroCOL
    bioLAB <- .BexpRetroLAB
    bioLTY <- .BexpRetroLTY
    bioLWD <- .BexpRetroLWD
    yLabel <- .BexpRetroLAB
  }
  else
  {
    retroBt <- obj$mp$assess$retroSpawnBt
    bioCOL <- .BspawnRetroCOL
    bioLAB <- .BspawnRetroLAB
    bioLTY <- .BspawnRetroLTY
    bioLWD <- .BspawnRetroLWD
    yLabel <- .BspawnRetroLAB
  }
  
  runStatus <- obj$mp$assess$runStatus

  # X-axis limits (same for both panels)
  xLim <- gfx$xLim
  yLim <- gfx$yLim[1,]
  
  if ( is.null(xLim) )
    xLim <- c(1,nT)

  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 1: Plot true spawning and explolitable biomass and survey index.
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    idx <- xLim[1]:xLim[2]
    yLim <- c(0, max(c( Bt[idx], Bmsy, Bleg[idx] ), na.rm=TRUE ) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  #abline( h=landedFmsy,    col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
  #abline( h=discardedFmsy, col=.FmsyCOL, lty=.FmsyLTY+1, lwd=.FmsyLWD )  
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  
  
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  lines( c(1:nT), Bleg, col=.BlegCOL, lty=.BlegLTY, lwd=.BlegLWD )  
  
  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$years )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=1, line=0.5, cex=.CEXLAB4, outer=TRUE, "Year" )

  if ( gfx$doLegend )
  {                                                               
    panLegend( 0.2,0.975, legTxt=c(.BtLAB,.BlegLAB, .BmsyLAB),
            lty=c(.BtLTY,.BlegLTY, .BmsyLTY), lwd=c(.BtLWD,.BlegLWD,.BmsyLWD),
            col=c(.BtCOL,.BlegCOL, .BmsyCOL), pch=c(NA,NA,NA),
            pt.cex=c(NA,NA,NA), pt.bg=c(NA,NA,NA),
            bg="white", cex=.CEXLEG, bty=.LEGBTY  )
  }

  # Panel 2: Plot retrospective "stepwise" fits

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(retroBt) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  lines( c(1:nT), Bleg, col=.BlegCOL, lty=.BlegLTY, lwd=.BlegLWD )      
  
  # Add the scaled survey points for the terminal fit only.  
  #points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$years )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  # Add lines for all retrospective biomass states
  retroBt   <- retroBt[ retroBt[,1]==iRep, ]
  runStatus <- runStatus[ runStatus[,1]==iRep, ]
  
  for ( j in 1:nrow(retroBt) )
  {
    lines( retroBt[ j, 3:ncol(retroBt) ], col=.BexpRetroCOL, lty=.BexpRetroLTY, lwd=.BexpRetroLWD )  
    
    if ( runStatus$hessPosDef[j]==FALSE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="magenta", lty=.BtStepLTY, lwd=.BtStepLWD )
    if ( runStatus$assessFailed[j]==TRUE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="darkgreen", lty=.BtStepLTY, lwd=.BtStepLWD )
  
    if ( gfx$showProj )
    {
      points( tMP+j-1, retroBt[ j,tMP+j+2-1 ], cex=.BexpRetroCEX+0.2, bg="yellow", pch=21 )
      points( tMP+j-2, retroBt[ j,tMP+j+2-2 ], cex=.BexpRetroCEX+0.2, bg="black",  pch=21 )
    }
  }
  
  # Plot index scaled by terminal estimated q's.
  dimItg <- dim( obj$mp$assess$ItgScaled )
  
  if ( gfx$annotate )
  {
    for ( g in 1:dimItg[3] )
      points( c(1:nT), ItgScaled[iRep,,g], bg=.ItgBG[g], cex=.ItgCEX[g],
              col=.ItgCOL[g], pch=.ItgPCH[g] )
  }

  if ( gfx$doLegend )
  {
    if ( gfx$showProj==FALSE )
    {
      panLegend( 0.2,0.975, legTxt=c(.BtLAB, .BlegLAB, .BexpRetroLAB ),
              lty=c(.BtLTY,.BlegLTY, .BexpRetroLTY ), lwd=c(.BtLWD,.BlegLWD, .BexpRetroLWD),
              col=c(.BtCOL,.BlegCOL, .BexpRetroCOL ), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
    else
    {
      panLegend( 0.2,0.975, legTxt=c(.BtLAB, .BlegLAB, .BexpRetroLAB,"Terminal estimate","Projection" ),
              lty=c(.BtLTY,.BlegLTY, .BexpRetroLTY,NA,NA),
              lwd=c(.BtLWD,.BlegLWD, .BexpRetroLWD,NA,NA),
              col=c(.BtCOL,.BlegCOL, .BexpRetroCOL,"black","black"),
              pt.bg=c(NA,NA,NA,"black","yellow"),
              pt.cex=c(NA,NA,NA,1.6,1.6), pch=c(NA,NA,NA,21,21),
              bg="white", cex=.CEXLEG, bty=.LEGBTY  )    
    }
  }  
  
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE,
         paste( "Biomass"," (",.BtUNIT,")", sep="" ) )
         
  # Panel 3: Plot first and last retrospective "stepwise" fits

  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c(0, max(retroBt) )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  lines( c(1:nT), Bleg, col=.BlegCOL, lty=.BlegLTY, lwd=.BlegLWD )      
  
  # Add the scaled survey points for the terminal fit only.  
  #points( c(1:nT), It, bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$years )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=4, labels=FALSE )
  box()

  # Add lines for all retrospective biomass states
  retroBt   <- retroBt[ retroBt[,1]==iRep, ]
  runStatus <- runStatus[ runStatus[,1]==iRep, ]
  
  iCount <- 3
  for ( j in c(1,nrow(retroBt)) )
  {
    iCount <- iCount - 1
    lines( retroBt[ j, 3:ncol(retroBt) ], col=.BexpRetroCOL, lty=iCount, lwd=.BexpRetroLWD )  
    
    if ( runStatus$hessPosDef[j]==FALSE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="magenta", lty=.BexpRetroCOL+0.2, lwd=.BexpRetroLWD )
    if ( runStatus$assessFailed[j]==TRUE )
      lines( retroBt[ j, 3:ncol(retroBt) ], col="darkgreen", lty=.BtStepLTY, lwd=.BtStepLWD )
  
    if ( gfx$showProj )
    {
      points( tMP+j-1, retroBt[ j,tMP+j+2-1 ], bg="yellow", cex=.BexpRetroCEX+0.2, pch=21 )
      points( tMP+j-2, retroBt[ j,tMP+j+2-2 ], bg="black",  cex=.BexpRetroCEX+0.2, pch=21 )
    }
  }
  
  # Plot index scaled by terminal estimated q.
  if ( gfx$annotate )
  {
    for ( g in 1:dimItg[3] )
      points( c(1:nT), ItgScaled[iRep,,g], bg=.ItgBG[g], cex=.ItgCEX[g],
              col=.ItgCOL[g], pch=.ItgPCH[g] )
  }

  if ( gfx$doLegend )
  {
    if ( gfx$showProj==FALSE )
    {
      panLegend( 0.2,0.975, legTxt=c(.BtLAB, .BlegLAB, .BexpRetroLAB ),
              lty=c(.BtLTY,.BlegLTY, .BexpRetroLTY ), lwd=c(.BtLWD,.BlegLWD, .BexpRetroLWD),
              col=c(.BtCOL,.BlegCOL, .BexpRetroCOL ), bg="white", cex=.CEXLEG, bty=.LEGBTY  )
    }
    else
    {
      panLegend( 0.2,0.975, legTxt=c(.BtLAB, .BlegLAB, .BexpRetroLAB,"Terminal estimate","Projection" ),
              lty=c(.BtLTY,.BlegLTY, .BexpRetroLTY,NA,NA),
              lwd=c(.BtLWD,.BlegLWD, .BexpRetroLWD,NA,NA),
              col=c(.BtCOL,.BlegCOL, .BexpRetroCOL,"black","black"),
              pt.bg=c(NA,NA,NA,"black","yellow"),
              pt.cex=c(NA,NA,NA,1.6,1.6), pch=c(NA,NA,NA,21,21),
              bg="white", cex=.CEXLEG, bty=.LEGBTY  )    
    }
  }      
  
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE,
         paste( "Biomass"," (",.BtUNIT,")", sep="" ) )         
  
  mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Retrospective Fits" )
    
}     # END function .plotRetroFits

#------------------------------------------------------------------------------#
#--  Convergence and ADMB plots.                                             --#
#------------------------------------------------------------------------------#

#.plotDiagnostics (plot annual parameter estimates from production model)                   
# Purpose:      Plot annual parameter estimates from stepwise model 
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09), Revised A.R. Kronlund (02-Aug-13)
.plotDiagnostics <- function( obj, iSim=1, iRep=1,
                      gfx=list( annotate=TRUE, doLegend=FALSE, xLim=NULL, yLim=NULL ) )
{
  runStatus <- obj$mp$assess$runStatus
  idx       <- runStatus$iRep == iRep
  nT        <- max( runStatus$tStep )
  tMP       <- obj$ctlList$opMod$tMP
  
  # Extract time step, maxGrad and iExit code for replicate.
  tStep   <- runStatus$tStep[ idx ]
  convT   <- as.numeric(runStatus$convT[ idx ])
  iExit   <- runStatus$iExit[ idx ]  
  maxGrad <- as.numeric(runStatus$maxGrad[ idx ])
  nEval   <- as.numeric(runStatus$nEval[ idx ])
  objFun  <- as.numeric( runStatus$objFun[ idx ] )
  
  deadFlag   <- as.logical(runStatus$deadFlag)
  fisheryClosed <- as.logical(runStatus$fisheryClosed)
  hessPosDef <- as.logical(runStatus$hessPosDef)
  assessFailed <- as.logical(runStatus$assessFailed)
  
  tColor <- rep( "white", length(tStep) )
  tColor <- ifelse( iExit==0, .EXIT0COL, tColor )
  tColor <- ifelse( iExit==1, .EXIT1COL, tColor )
  tColor <- ifelse( iExit==2, .EXIT2COL, tColor )
  tColor <- ifelse( iExit==3, .EXIT3COL, tColor )
  
  # Plot maximum gradient, coloring the lines by iExit code.
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( tMP,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( c(0,max( maxGrad ),.MAXGRADCRIT ) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  browser()
  lines( tStep, maxGrad, col=tColor, type="h", lwd=3 )
  
  usr <- par( "usr" )
  yMid <- rep( (usr[4]-usr[3])/3.0, length(tStep) )
  
  if ( any(fisheryClosed==TRUE) )
  {
    points( tStep[fisheryClosed], yMid[fisheryClosed], bg="white", cex=4, pch=21 )  
    text( tStep[fisheryClosed],   yMid[fisheryClosed], "C", col="red", cex=1.5 )
  }

  if ( any(deadFlag)==TRUE )
  {
    points( tStep[deadFlag], yMid[deadFlag], bg="white", cex=4, pch=21 )  
    text( tStep[deadFlag],   yMid[deadFlag], "D", col="red", cex=1.5 )
  }
  
  yMid <- yMid * 2
  if ( any(!hessPosDef) )
  {
    points( tStep[!hessPosDef], yMid[!hessPosDef], bg="white", cex=4, pch=21 )
    text( tStep[!hessPosDef], yMid[!hessPosDef], "H", col="red", cex=1.5 )
  }

  yMid <- yMid * 2.2
  if ( any(assessFailed) )
  {
    points( tStep[assessFailed], yMid[assessFailed], bg="white", cex=4, pch=21 )
    text( tStep[assessFailed], yMid[assessFailed], "A", col="red", cex=1.5 )
  }

  
  abline( h=0, col="black", lty=3, lwd=2 )  
  abline( h=.MAXGRADCRIT, col="black", lty=2, lwd=2 )
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Max Gradient" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               pt.cex=c(.EXIT0COL,.EXIT1CEX,.EXIT2CEX,.EXIT3CEX), lwd=c(3,3,3,3),
               bg="white", bty=.LEGBTY )
  }
  
  # Plot function calls, coloring by Exit Code.
  
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,max( c(.MAXFUNCALLS,nEval) ) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, nEval, col=tColor, type="h", lwd=3 )
  
  abline( h=0, col="black", lty=3, lwd=2 )
  abline( h=.MAXFUNCALLS, col="black", lty=2, lwd=2 )  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4,             "Fun Calls" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  
  # Plot convergence time, coloring by Exit Code.
  
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,max(convT) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, convT, col=tColor, type="h", lwd=3 )
  
  abline( h=0, col="black", lty=3, lwd=2 )  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3,  cex=.CEXLAB4,             "Converge Time" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  
  # Plot objective function, coloring by Exit Code.
   
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- range( 0,min(objFun) )
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  lines( tStep, objFun, col=tColor, type="h", lwd=3 )

  abline( h=0, col="black", lty=3, lwd=2 )  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4,             "Objective Fun" )

  if ( gfx$annotate)
  {
    panLegend( 0.05,0.95, legTxt=c("iExit=0","iExit=1","Exit=2","Exit=3" ),
               lty=c(1,1,1,1), col=c(.EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL),
               lwd=c(3,3,3,3), bg="white", bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotDiagnostics

.plotDiagSim <-function( obj, gfx=list( annotate=TRUE, doLegend=FALSE,
                                        xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
#             code 0 = solution equals initial value
#             code 1 = normal exit, all derivatives satisfy conditions,
#             code 2 = error in the derivative
#             code 3 = maximum number of function calls exceeded)

  runStatus <- obj$mp$assess$runStatus
  nT        <- max( as.numeric(runStatus$tStep) )
  nReps     <- max( as.numeric(runStatus$iRep) )
  tMP       <- min( as.numeric(runStatus$tStep) )
  
  # Extract time step, maxGrad and iExit code for replicate.
  iRep    <- runStatus$iRep
  tStep   <- runStatus$tStep
  convT   <- runStatus$convT
  iExit   <- as.numeric(runStatus$iExit)  
  maxGrad <- as.numeric(runStatus$maxGrad)
  nEval   <- as.numeric(runStatus$nEval)
  objFun  <- runStatus$objFun

  fisheryClosed <- as.logical( runStatus$fisheryClosed )
  deadFlag      <- as.logical( runStatus$deadFlag )
  hessPosDef    <- as.logical( runStatus$hessPosDef )
  assessFailed    <- as.logical( runStatus$assessFailed )
  
  nSteps <- nrow( runStatus )

  cexVec <- rep( .EXIT1CEX, nSteps )
  symVec <- rep( 21, nSteps )
  
  nSteps <- nrow( runStatus )
  
  # Exit code status.
  exitColor <- rep( .EXIT1COL, nSteps )
  exitColor <- ifelse( iExit==0, .EXIT0COL, exitColor )
  exitColor <- ifelse( iExit==2, .EXIT2COL, exitColor )
  exitColor <- ifelse( iExit==3, .EXIT3COL, exitColor )

  bgCol  <- rep( .EXIT1COL, nSteps )
  bgCol <- ifelse( iExit==0, .EXIT0COL, bgCol )  
  bgCol <- ifelse( iExit==2, .EXIT2COL, bgCol )
  bgCol <- ifelse( iExit==3, .EXIT3COL, bgCol )

  colVec <- bgCol
  fgVec  <- bgCol
 
  # Gradient maximum exceeded?
  gradProblem <- maxGrad > .MAXGRADCRIT
  cexVec[ gradProblem ] <- .CEXANNO2  
  colVec[ gradProblem ] <- "red"
  symVec[ gradProblem ] <- 71     # ASCII number 71 is "G".
  
  cexVec[ fisheryClosed ] <- .CEXANNO2
  symVec[ fisheryClosed ] <- 67     # ASCII number 68 is "D", 67 id "C".

  cexVec[ deadFlag ] <- .CEXANNO2
  symVec[ deadFlag ] <- 68     # ASCII number 68 is "D", 67 id "C".
  
  cexVec[ !hessPosDef ] <- .CEXANNO2
  symVec[ !hessPosDef ] <- 72     # ASCII number 72 is "H".

  cexVec[ assessFailed ] <- .CEXANNO2
  symVec[ assessFailed ] <- 72     # ASCII number 72 is "H".  
  
  idx <- fisheryClosed * !hessPosDef 
  cexVec[ idx ] <- .CEXANNO2
  symVec[ idx ] <- 88    # ASCII number 88 is "X".
  
  idx <- deadFlag * !hessPosDef
  cexVec[ idx ] <- .CEXANNO2
  symVec[ idx ] <- 89    # ASCII number 89 is "Y".
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( tMP,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c(0.9,nReps+0.1 )

  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  
  # (1) If there are no problems, then plot a green.
  # (2) If there are Exit code problems plot symbol in color corresponding to
  #     the exit code.
  # (3) If there are gradient problems plot "G" - use exit code color.
  # (4) If model parameter threshold problems plot "F" for failure in exit color.
  
  # tStep and iRep are vectors so that all combinations of tStep and iRep are
  # plotted at once.

  points( tStep, iRep, bg=bgCol, pch=symVec, cex=cexVec, col=fgVec )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, at=c(1:nReps), cex.axis=.CEXAXIS2, labels=c(1:nReps), las=.YAXISLAS )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Year" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB4, outer=TRUE, "Replicate" )

  #gfx$doLegend <- FALSE
  if ( gfx$doLegend )
  {
    panLegend( 0.5,0.7, xjust=0.5, ncol=2,
               legTxt=c( "E0: Stalled", "E1: Clean", "E2: Bad derivative",
                         "E3: Max funcalls",
                         paste(": maxGrad >",.MAXGRADCRIT), ": CLOSED",
                         ": Hessian NPD", ": CLOSED and HESSIAN",
                         ": DEAD", ": DEAD and HESSIAN"
                        ),
               pt.bg=c( .EXIT0COL,.EXIT1COL,.EXIT2COL,.EXIT3COL,"red","blue","black","black", "black"),
               pch=c( 21, 21, 21, 21, 71, 67, 72, 88, 68,89 ),
               pt.cex=c( rep(.EXIT1CEX,4), rep(.CEXANNO2,6) ),
               bg="white", cex=.CEXLEG, bty="o" )
  }
  return( invisible() )  
}     # END function .plotDiagSim

#------------------------------------------------------------------------------#
#-- Perf Plotting Functions:                                                 --#
#------------------------------------------------------------------------------#

.plotObjectives <- function( obj,
                             depCEX=1,  depCOL="red",    depLTY=2,  depLWD=2,
                             probCEX=1, probCOL="black", probLTY=2, probLWD=2,
                             yrCEX=1,   yrCOL="blue",    yrLTY=2,   yrLWD=2 )
{
  usr <- par( "usr" )
  
  if ( obj$outcome=="dep" )
  {
    segments( obj$objYear, usr[3], obj$objYear, obj$objDep,
              col=depCOL, lty=depLTY, lwd=depLWD )
    .addTarget( obj$objYear, obj$objDep, cexCenter=depCEX, colCenter=depCOL,
                colRing=depCOL )
     text( obj$objYear+0.5, usr[3]+0.065, adj=0, cex=.CEXANNO, col=depCOL,
          paste( "D:",round(obj$objDep,digits=3),sep="" ) )        
  }
  else if ( obj$outcome=="year" )
  {
    abline( h=obj$objDep, lty=depLTY, lwd=depLWD )
    segments( obj$objYear, usr[3], obj$objYear, obj$objDep,
              col=yrCOL, lty=yrLTY, lwd=yrLWD )
    .addTarget( obj$objYear, obj$objDep, cexCenter=yrCEX, colCenter=yrCOL,
                colRing=yrCOL )
    text( obj$objYear+0.5, usr[3]+0.025, adj=0, cex=.CEXANNO, col=yrCOL,
          paste( "Y:",obj$objYear,sep="" ) )
  }
  else if ( obj$outcome=="prob" )
  {
    .addTarget( obj$objYear, obj$objDep, cexCenter=probCEX, colCenter=probCOL,
                colRing=probCOL )
    text( obj$objYear+0.5, usr[3]+0.105, adj=0, cex=.CEXANNO, col=probCOL,
          paste( "C:",round(obj$objProb,digits=3),sep="" ) )
  }
  return( invisible() )
}     # END function .plotObjectives


.plotStrategy <- function( obj, base="Bmsy", phase=FALSE,
   mults=list( pfLimitMultBmsy=0.4, pfUpperMultBmsy=0.8, pfTargetMultBmsy=1.0,
               pfLimitMultB0=0.2, pfUpperMultB0=0.35, pfTargetMultB0=0.4 ),
               gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         image=FALSE, xLim=NULL, yLim=NULL ),... )
{
  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tProj <- c(tMP:nT)

  # Get spawning biomass and fishing mortality for all replicates.
  Bt <- obj$om$Bt[ ,(2:ncol(obj$om$Bt)) ]
  Ft <- obj$om$Ft[ ,(2:ncol(obj$om$Ft)) ]
    
  B0   <- obj$refPtList$B0
  Bmsy <- obj$refPtList$ssbFmsy
  Fmsy <- obj$refPtList$Fmsy
  
  # Get the status-based reference points on the operating model scale.
  if ( base=="Bmsy" )
  {
    baseVal <- Bmsy
    LRP <- mults$pfLimitMultBmsy  * Bmsy
    USR <- mults$pfUpperMultBmsy  * Bmsy
    TRP <- mults$pfTargetMultBmsy * Bmsy
  }
  
  if (  base=="B0" )
  {
    baseVal <- B0
    LRP <- mults$pfLimitMultB0  * B0
    USR <- mults$pfUpperMultB0  * B0
    TRP <- mults$pfTargetMultB0 * B0
  }

  # Get the Fishing mortality at MSY from the operating model.
  Fmsy <- obj$ctlList$refPts$Fmsy

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # Subset the SSB and F from the projection period.
  Bt <- Bt[ ,tProj ]
  Ft <- Ft[ ,tProj ]
  
  if ( phase )
  {
    Bt  <- Bt / baseVal
    Ft  <- Ft / baseVal
    LRP <- LRP / baseVal
    USR <- USR / baseVal
    TRP <- TRP / baseVal
  }
   
  if ( is.null(xLim) )
    xLim <- range( c(0,Bt) )

  if ( is.null(yLim) )
    yLim <- range( c(0,Ft) )

  plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", yaxs="i", ylab="")

  if ( gfx$image )
  {
    # Image the points.
    xBrks <- seq( 0,100,1 )
    yBrks <- seq( 0,5,0.005 )
    
    if ( phase )
    {
      xBrks <- seq( 0, 3, 0.05 )
      yBrks <- seq( 0, 3, 0.05 )
    }
  
    xCuts  <- cut( Bt, breaks=xBrks )
    yCuts  <- cut( Ft, breaks=yBrks )
    counts <- table( xCuts, yCuts )  
    #image(  xBrks, yBrks, counts, col=rev(heat.colors(64)), add=TRUE,axes = FALSE)
    #contour( xBrks, yBrks, counts, add = TRUE )
    
    x <- cbind( Bt, Ft )
    #est <- bkde2D( x, bandwidth=c(1,0.01), range.x=list( c(0,max(Bt)),c(0,max(Ft)) ) )
    #est <- bkde2D( x, bandwidth=c(20,0.5) )    
    #contour( est$x1, est$x2, est$fhat, levels=c(0.1,0.25,0.5,0.75,0.9) )
  
    #for ( i in 1:nrow(Bt) )
    #  points( Bt[i,],Ft[i,], pch=21, bg="black", col="black", cex=0.2 )
  }
  else
  {
    # Add DFO zones and points.
    usr <- par( "usr" )
    rect(   0, 0,    LRP, usr[4], border=NA, col=.CriticalCOL )
    rect( LRP, 0,    USR, usr[4], border=NA, col=.CautiousCOL )
    rect( USR, 0, usr[2], usr[4], border=NA, col=.HealthyCOL )    

    for ( i in 1:nrow(Bt) )
      points( Bt[i,],Ft[i,], pch=21, bg="lightgray", cex=0.8 )
  }
  
  # Add points at quantiles of Bt and Ft.
  quantsBt <- quantile( Bt, probs=c(0.10,0.25,0.5,0.75,0.90) )
  quantsFt <- quantile( Ft, probs=c(0.10,0.25,0.5,0.75,0.90) )
  #points( quantsBt, quantsFt, bg="red", col="red", cex=.CEXSYM20, pch=21 )

  segments( quantsBt[1],quantsFt[3],quantsBt[5],quantsFt[3], col="red", lwd=3 ) 
  segments( quantsBt[3],quantsFt[1],quantsBt[3],quantsFt[5], col="red", lwd=3 )
  points( quantsBt[3], quantsFt[3], bg="red", col="red", cex=.CEXSYM20, pch=21 )

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=2, labels=FALSE, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=4, labels=FALSE, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  if ( phase )
  {
    abline( h=1, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD  )
    abline( v=1, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD  )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )
  }
  else
  {
    abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD  )
    abline( v=TRP,  col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD  )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }
  
  abline( v=LRP, col=.LrpCOL, lty=.LrpLTY, lwd=.LrpLWD )
  abline( v=USR, col=.UsrCOL, lty=.UsrLTY, lwd=.UsrLWD )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }

  if ( gfx$annotate )
  {
    usr <- par( "usr" )
    zoneMidPts <-c( LRP/2.0, ((USR-LRP)/2.0)+LRP, ((usr[2]-USR)/2.0)+USR )
    xPos <- zoneMidPts / usr[2]
    
    fMidPts <- c( Fmsy/2.0, (usr[4]-Fmsy)/2.0 + Fmsy )
    yPos    <- fMidPts / usr[4]

    # Note that Bt and Ft were restricted to projection period  using tProj.
    
    # Stock Status: Calculate number and proportion in each zone from tMP:nT.
    
    nZone <- table( cut( Bt, breaks=c(0,LRP,USR,max(Bt) ) ) )
    pZone <- nZone / sum( nZone )

    zoneLabels <- c( .CriticalLAB, .CautiousLAB, .HealthyLAB )
    panLab( xPos, 0.975, adj=0.5, zoneLabels )
    panLab( xPos, 0.9,   adj=0.5, cex=.CEXANNO2, col="blue",
            format( round( pZone,2 ), nsmall=2 ) )

    # Overfishing vs. Not overfishing.
    nZone <- table( cut( Ft, breaks=c(0,Fmsy,max(Ft)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    zoneLabels <- c( "Not Overfishing","Overfishing" )
    panLab( 0.05, Fmsy/usr[4], cex=.CEXANNO2, col="red", pos=1, paste( pZone[1] ) )
    panLab( 0.05, Fmsy/usr[4], cex=.CEXANNO2, col="red", pos=3, paste( pZone[2] ) )
    
    # Overfished vs. not overfished.
    nZone <- table( cut( Bt, breaks=c(0,TRP,max(Bt)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    zoneLabels <- c( "Overfished","Not Overfished" )
    panLab( TRP/usr[2], 0.1, cex=.CEXANNO2, col="magenta", pos=2, paste( pZone[1] ) )
    panLab( TRP/usr[2], 0.1, cex=.CEXANNO2, col="magenta", pos=4, paste( pZone[2] ) )    
    
    # Each of the 6 zones created by LRP, USR, and Reference Removal Rate.
    nZone <- table( cut( Bt, breaks=c(0,LRP,USR,max(Bt) ) ),
                    cut( Ft, breaks=c(0,Fmsy,max(Ft)) ) )
    pZone <- nZone / sum( nZone )
    pZone <- format( round( pZone,2 ), nsmall=2 )
    panLab( xPos, yPos[1], pZone[,1], cex=.CEXANNO2, col="darkgreen" )
    panLab( xPos, yPos[2], pZone[,2], cex=.CEXANNO2, col="darkgreen" )
  }

  if ( gfx$doLegend )
  {
  }

  usr <- par( "usr" )

  box()
  return( invisible() )
}     # END function .plotStrategy

#-----------------------------------------------------------------------------##
#-- Level 3 mseR Peformance GUI Plotting Functions                          --##
#-----------------------------------------------------------------------------##

# .plotBarsByPeriod  (barplots showing distribution of specified stat by period)                   
# Purpose:      Produces a bar plot showing the median and quanitile values for 
#                the specified performance statistic (depletion, catch or AAV) 
#               for each of three different time periods (short, medium, long)
# Parameters:   A stats dataframe that inlcudes previously calculated median 
#                and quantile values for performance statistics to be plotted.  
#                This dataframe in compiled in ".doBarplots" function. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBarsByPeriod <- function( stats, xvars=c("depletion","catch","aav"), 
                        refPoints=NULL, gfx=list( xLim=NULL, yLim=NULL ) )
{
  # This plot treats the periods as columns, variables as rows so that we have
  # to plot the simulations by variable within period.

  simList    <- unique( stats$simLabel )
  periodList <- unique( stats$period   )

  # Loop over the summary periods.
  for ( i in 1:length(periodList) )
  {
    # Extract the statistics for period i.
    idx <- stats$period==periodList[i]
    simStats <- stats[ idx, ]
    
    # Loop over the x-variables.
    for ( j in 1:length(xvars) )
    {
      # Get the y-values for the plot, which are categorical, then reverse them.
      yVals <- c( 1:nrow(simStats) )
      yVals <- rev(yVals)
      y.Lim <- range( yVals )
      
      if ( length(xvars)==1 )
        x.Lim <- gfx$xLim
      else
        x.Lim <- gfx$xLim[j,]
        
      if ( is.null(x.Lim) )
      {
        # Determine the limits of the x-axis.
        if ( xvars[j]=="depletion" )
         x.Lim <- range( stats[,c("Q1finalDep","Q2finalDep")] )
        else if ( xvars[j]=="catch" )
          x.Lim <- range( stats[,c("Q1AvgCatch","Q2AvgCatch")] )
        else if ( xvars[j]=="discards" )
          x.Lim <- range( stats[,c("Q1AvgDiscards","Q2AvgDiscards")] )          
        else if ( xvars[j]=="AAV" )
          x.Lim <- range( stats[,c("Q1AAV","Q2AAV")] )
      }

      # Set up the plot panel region.
      plot( x.Lim,y.Lim, type="n", axes=F, xlab="", ylab="" )

      # Plot the relevant points and line segments.
      if ( xvars[j]=="depletion" )
      {
        # ARK: Aggregate statistics removed at request of Cox 02-Mar-09.
        #points( simStats$medAvgDep, yVals, cex=.PERFCEX, pch=.PERFSYM )
        #segments( simStats$Q1AvgDep, yVals, simStats$Q2AvgDep, yVals )
        #xLabel <- "Depletion"

        points( simStats$medFinalDep, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1finalDep, yVals, simStats$Q2finalDep, yVals )
        xLabel <- "Final Depletion"
      }
      else if ( xvars[j]=="catch" )
      {
        points( simStats$medAvgCatch, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgCatch, yVals, simStats$Q2AvgCatch, yVals )
        xLabel <- "Catch"
      }
      else if ( xvars[j]=="discards" )
      {
        points( simStats$medAvgDiscards, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgDiscards, yVals, simStats$Q2AvgDiscards, yVals )
        xLabel <- "Discards"      
      }            
      else if ( xvars[j]=="AAV" )
      {
        points( simStats$medAAV, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AAV, yVals, simStats$Q2AAV, yVals )
        xLabel <- "AAV"
      }

      mfg <- par( "mfg" )
      if ( mfg[2]==mfg[4] )
        if ( length(xvars)==1 )
          mtext( side=1, line=.INLINE, cex=.CEXLAB4, outer=TRUE, xLabel )
        else
          mtext( side=4, line=.INLINE, cex=.CEXLAB4, xLabel )

      if ( mfg[2]==1 )
        axis( side=2, at=yVals, cex.axis=.CEXAXIS4, las=.YAXISLAS, substring(simList,1,12) )

      axis( side=1, cex.axis=.CEXAXIS4 )
      axis( side=3, labels=FALSE )
      box()

      # Add the period label.
      t1 <- unique( simStats[,"t1" ] )
      t2 <- unique( simStats[,"t2" ] )
      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
        paste( periodList[i]," (",t1,",",t2,")", sep="") )

    }     # Loop over j x-variables.
  }     # Loop over i periods.
   
  return( invisible() )
}     # END function .plotBarsByPeriod


# .plotBarsByStats  (barplots showing all 3 perf statistics in specified period)                   
# Purpose:      Produces a bar plot showing the median and quanitile values for 
#                three performance statistics (depletion, catch and AAV) 
#                calculated over the specified time period
# Parameters:   A stats dataframe that inlcudes previously calculated median 
#                and quanitile values for performance statistics to be plotted.
#                This dataframe in compiled in ".doBarplots" function.  
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBarsByStats <- function( stats, periodList=c("Short","Medium","Long"), 
        refPoints=NULL, gfx=list( xLim=NULL, yLim=NULL ) )
{
  # This plot treats the periods as rows, variables as columns so that we have
  # to plot the simulations by period within variable.

  simList <- unique( stats$simLabel )
  xvars   <- c( "depletion","catch","discards","AAV" )

  # Loop over the summary periods.
  for ( i in 1:length(periodList) )
  {
    # Extract the statistics for period i.
    idx <- stats$period==periodList[i]
    simStats <- stats[ idx, ]

    # Loop over the x-variables.
    for ( j in 1:length(xvars) )
    {
      # Get the y-values for the plot, which are categorical, then reverse them.
      yVals <- c( 1:nrow(simStats) )
      yVals <- rev(yVals)
      y.Lim  <- range( yVals )

      if ( length(xvars)==1 )
        x.Lim <- gfx$xLim
      else
        x.Lim <- gfx$xLim[j,]
      
      # Limits are set on stats to have common axes across periods.
      if ( is.null(x.Lim) )
      {
        # Determine the limits of the x-axis.
        if ( xvars[j]=="depletion" )
          x.Lim <- range( stats[,c("Q1finalDep","Q2finalDep")] )
        else if ( xvars[j]=="catch" )
          x.Lim <- range( stats[,c("Q1AvgCatch","Q2AvgCatch")] )
        else if ( xvars[j]=="discards" )
          x.Lim <- range( stats[,c("Q1AvgDiscards","Q2AvgDiscards")] )          
        else if ( xvars[j]=="AAV" )
          x.Lim <- range( stats[,c("Q1AAV","Q2AAV")] )
      }

      # Set up the plot panel region.
      plot( x.Lim,y.Lim, type="n", axes=FALSE, xlab="", ylab="" )

      # Plot the relevant points and line segments.
      if ( xvars[j]=="depletion" )
      {
        # ARK: Aggregate by period removed at request of Cox 02-Mar-09.
        #points( simStats$medAvgDep, yVals, cex=.PERFCEX, pch=.PERFSYM )
        #segments( simStats$Q1AvgDep, yVals, simStats$Q2AvgDep, yVals )
        #xLabel <- "Depletion"

        # Final depletion at end of period.
        points( simStats$medFinalDep, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1finalDep, yVals, simStats$Q2finalDep, yVals )
        xLabel <- "Final Depletion"
      }
      else if ( xvars[j]=="catch" )
      {
        points( simStats$medAvgCatch, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgCatch, yVals, simStats$Q2AvgCatch, yVals )
        xLabel <- "Catch"
      }
      else if ( xvars[j]=="discards" )
      {
        points( simStats$medAvgDiscards, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AvgDiscards, yVals, simStats$Q2AvgDiscards, yVals )
        xLabel <- "Discards"      
      }      
      else if ( xvars[j]=="AAV" )
      {
        points( simStats$medAAV, yVals, cex=.CEXSYM8, pch=16 )
        segments( simStats$Q1AAV, yVals, simStats$Q2AAV, yVals )
        xLabel <- "AAV"
      }

      mfg <- par( "mfg" )
      #if ( mfg[1]==mfg[3] )
        mtext( side=1, line=3, cex=.CEXLAB4, xLabel )

      if ( mfg[2]==1 )
        axis( side=2, at=yVals, cex.axis=.CEXAXIS4, las=.YAXISLAS, substring(simList,1,12) )

      axis( side=1, cex.axis=.CEXAXIS4 )
      axis( side=3, labels=FALSE )
      box()

      # Add the period label.
      if ( mfg[2]==mfg[4] )
      {
        t1 <- unique( simStats[,"t1" ] )
        t2 <- unique( simStats[,"t2" ] )
        if ( length( periodList)==1 )
          mtext( side=3, line=.OUTLINE-1, cex=.CEXLAB4, outer=TRUE,
            paste( periodList[i]," (",t1,",",t2,")", sep="") )
        else
          mtext( side=4, line=.INLINE,  cex=.CEXLAB4,
            paste( periodList[i]," (",t1,",",t2,")", sep="") )
      }
    }     # Loop over j x-variables.
  }     # Loop over i periods.
  return( invisible() )
}     # END function .plotBarsByStats


# .plotBxpStatus  (boxplots of depletion relative to status zones)                   
# Purpose:      Produces boxplots of depletion values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of depletions 
#                from the OM. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBxpStatus <- function( obj, period, quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                    yLabel="",
                    gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                              xLim=NULL, yLim=NULL ) )
{
  nSim     <- length(obj)
  simLabel <- character( nSim )
  
  xLim <- c( (1-0.5),nSim+0.5 )
  
  yLim <- gfx$yLim   
  if ( is.null(yLim) )
  {
    yLim <- c(0,0.01)
    for ( i in 1:nSim )
      yLim[2] <- max( c(yLim[2],
                      max( obj[[i]][,c(8:ncol( obj[[i]] ))], na.rm=TRUE )),
                      na.rm=TRUE )
    yLim[2] <- yLim[2] * 1.1
  }

  # Columns of obj with Rep in the first 3 letters of name.
  iCol <- substring( names(obj[[1]]),1,3 ) == "Rep"
  
  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    plot(xLim, yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="", yaxs="i")

    # Get the plot region user coordinate limits.
    usr <- par( "usr" )

    # Loop over simList...
    for ( j in c(1:length(obj)) )
    {
      tmp <- obj[[j]][ period==period$pName[i], ]
      simLabel[j] <- tmp$simLabel
      
      limitRef  <- tmp$lrpBmsy
      upperRef  <- tmp$usrBmsy
      targetRef <- tmp$trpBmsy

      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5

      # Colour the Critical, Cautious and Healthy Zones.
      # Note these are done based on the Status Zones, not the HCR ref points.
      colorZones <- TRUE
      if ( colorZones )
      {
        rect( x1,        0, x2, limitRef, border=NA, col=.CriticalCOL )
        rect( x1, limitRef, x2, upperRef, border=NA, col=.CautiousCOL )
        rect( x1, upperRef, x2,   usr[4], border=NA, col=.HealthyCOL )
      }

      yVals <- as.numeric( tmp[ ,iCol ] )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25
      
      quantVals <- quantile( yVals, probs=quantProbs, na.rm=TRUE )
      rect( j-delta, quantVals[2], j+delta,quantVals[4] )
      medVal <- median(yVals)
 
      segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=.CEXSYM24 )
      
      .addTarget( j, targetRef, cexCenter=1.4, colCenter="red" )

      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
             paste( tmp$period," (",tmp$t1,",",tmp$t2,")", sep="" ) )
    }     # Loop j over simulations.
    
    axis( side=1, at=c(1:nSim), cex.axis=.CEXAXIS4, labels=simLabel )
    axis( side=2, las=.YAXISLAS, cex.axis=.CEXAXIS4 )
    axis( side=3, labels=F )
    axis( side=4, las=.YAXISLAS, cex.axis=.CEXAXIS4 )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, yLabel )
    box()
  }     # Loop i over periods.
}     # END function .plotBxpStatus

# .plotBxpFmort  (boxplots of depletion relative to status zones)                   
# Purpose:      Produces boxplots of depletion values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of depletions 
#                from the OM. 
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
.plotBxpFmort <- function( obj, period, quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                    yLabel="",
                    gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                              xLim=NULL, yLim=NULL ) )
{
  nSim     <- length(obj)
  simLabel <- character( nSim )
  
  xLim <- c( (1-0.5),nSim+0.5 )
  
  yLim <- gfx$yLim   
  if ( is.null(yLim) )
  {
    yLim <- c(0,0.1)
    for ( i in 1:nSim )
      yLim[2] <- max( yLim[2], max(obj[[i]][,c(14:ncol(obj[[i]]))] ) )
    yLim[2] <- yLim[2] * 1.1
  }

  iCol <- 15

  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    plot(xLim, yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="", yaxs="i")

    # Get the plot region user coordinate limits.
    usr <- par( "usr" )

    # Loop over simList...
    for ( j in c(1:length(obj)) )
    {
      tmp <- obj[[j]][ period==period$pName[i], ]
      simLabel[j] <- tmp$simLabel

      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5

      Fmsy  <- tmp$Fmsy
      segments( x1,Fmsy,x2,Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25

      yVals <- as.numeric( tmp[ c(iCol:ncol(tmp) ) ] )     
      quantVals <- quantile( yVals, probs=quantProbs, na.rm=TRUE )

      rect( j-delta, quantVals[2], j+delta,quantVals[4] )
      medVal <- median(yVals)
 
      segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=.CEXSYM24 )
      #points( j, Fmsy, pch=21, bg="red", cex=.CEXSYM24 )
      .addTarget( j, Fmsy, cexCenter=1.4, colCenter="red" )

      mtext( side=3, line=.INLINE, cex=.CEXLAB2,
             paste( tmp$period," (",tmp$t1,",",tmp$t2,")", sep="" ) )  
 
    }     # Loop j over simulations.

    axis( side=1, at=c(1:nrow(tmp)), cex.axis=.CEXAXIS4, labels=tmp$simLabel )
    axis( side=2, las=.YAXISLAS,     cex.axis=.CEXAXIS4 )
    axis( side=3, labels=F )
    axis( side=4, las=.YAXISLAS,     cex.axis=.CEXAXIS4 )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB4, outer=TRUE, yLabel )
    box()
  }     # Loop i over periods.
  
  return( invisible() )
}     # END function .plotBxpFmort

                 
# .plotQboxDep  (boxplots of spawning biomass relative to status zones)                   
# Purpose:      Produces boxplots of ssb values from operating model (reps
#                 and years combined) for each of three time periods, and shows
#                  status zones  
# Parameters:  An object containing the nRep by 1:nT matrix of SSB  from the OM.
# Returns:      NULL (invisibility). 
# Source:       A.R. Kronlund
# Note:        Not currently an option in guiSim window (K. Holt, 25-Nov-09)
.plotQboxSSB <- function( ssbObj, obj, period,
                    quantProbs=c(0.05,0.1,0.5,0.9,0.95), yLim, fixY )
{
     # ssbObj: the nRep by 1:nT matrix of SSB from the OM.
    xLim <- c( (1-0.5),(length(unique(obj$simLabel) )+0.5) )

    if (fixY == TRUE) 
       yLim <- c( 0,max(ssbObj) )
    if (fixY == FALSE)
      yLim <- yLim
      

     par( oma=c(2,3,2,2), mar=c(3,2,2,2), mfrow=c(nrow(period),1) )

     nReps <- nrow( ssbObj )

     # Loop over the periods.
     for ( i in 1:nrow(period) )
     {
       tmp <- obj[ obj$period==period$pName[i], ]
       t1  <- period$t1[ i ]
       t2  <- period$t2[ i ]
       tdx <- c( t1:t2 )

       plot( xLim,yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="" )

       # Get the plot region user coordinate limits.
       usr <- par( "usr" )

       for ( j in 1:nrow(tmp) )
       {
         Dmsy      <- tmp$Dmsy[ j ]
         zoneLimit <- tmp$zoneLimit[ j ]
         zoneUpper <- tmp$zoneUpper[ j ]

         # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
         x1 <- j - 0.5
         x2 <- j + 0.5

         # Colour the Critical, Cautious and Healthy Zones.
         # Note these are done based on the Status Zones, not the HCR ref points.
         colorZones <- TRUE
         if ( colorZones )
         {
           rect( x1,         0, x2, zoneLimit, border=NA, col=.CriticalBG )
           rect( x1, zoneLimit, x2, zoneUpper, border=NA, col=.CautiousBG )
           rect( x1, zoneUpper, x2,    usr[4], border=NA, col=.HealthyBG )
         }

         yVals <- ssbObj[ ,c(t1:t2) ]

         # Now lay down the median, 25th, and 75th quantiles.
         delta <- 0.25
         quantVals <- quantile( yVals, probs=quantProbs )
         rect( j-delta, quantVals[2], j+delta,quantVals[4] )
         medVal <- median(yVals)
         segments( j-delta, medVal, j+delta, medVal, lty=1, lwd=2 )

         points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
                  pch=21, bg="white", cex=1 )
         points( j, Dmsy, pch=21, bg="red", cex=2.0 )

         mtext( side=3, line=1, cex=1.0,
           paste( tmp$period[j]," (",tmp$t1[j],",",tmp$t2[j],")", sep="" ) )

       }     # Loop j over simulations.

       axis( side=1, at=c(1:nrow(tmp)), labels=tmp$simLabel )
       axis( side=2, las=2, cex.axis=1 )
       axis( side=3, labels=F )
       axis( side=4, las=2, cex.axis=1 )
       mtext( side=2, line=1, cex=1.2, outer=TRUE, "Depletion" )
       box()
       
     }     # Loop i over periods.
}     # .plotQboxSSB


# .plotTulipCatch (tulip simulation envelope for catch)
# Purpose:        Display tulip (simulation envelope) catch for one simulation.
# Parameters:     obj is the object containing the Rdata blob list.
#                 Allow percentiles to be specified.
# Returns:        NULL (invisibly)
# Source:         A.R. Kronlund
.plotTulipCatch <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                             xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                             allQuants=TRUE,
                         gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                                 showProj=TRUE, xLim=xLim, yLim=yLim, useYears=TRUE ),... )
{
  
  opMod <- obj$ctlList$opMod  
  tmp <- calcRefPoints( as.ref(opMod) )
  tmp <- deref( tmp )
  obj$ctlList$refPts <- tmp

  nReps <- obj$ctlList$gui$nReps
  Ct    <- obj$om$Ct[ ,(2:ncol(obj$om$Ct)) ]
  MSY   <- obj$ctlList$refPts$yieldFmsy

  # Time indices (could use obj$par$nT).
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tVec  <- c(1:ncol(Ct))

  # Specify axes for plot
  if(is.null(xLim)) xLim <- gfx$xLim
  if(is.null(yLim)) yLim <- gfx$yLim
  
  peakCatchYear <- apply(X=Ct[,tMP:nT],MARGIN=1,FUN=function(x){which.max(x)})

  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Ct[ ,tdx ], na.rm=TRUE )
    # yLim <- c(0,.MAXCATCH)
  }
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( xLim[1],xLim[2],by = 5 )
  years <- seq(.INITYEAR,by = 1, length = nT)

  # X-axis (bottom): panel is in the last row.
  if( gfx$useYears ) 
    labs <- years[xSeq]
  else 
    labs <- xSeq
  
  axis(side = 1, at = xSeq, labels = labs, cex.axis=.CEXAXIS2 )
  

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    axis(side = 3, at = xSeq, labels = labs, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )


  yTicks <- round(seq(from=0,to=yLim[2],length=5),2)
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, at=yTicks, ... )

  axis( side=4, labels=FALSE, at=yTicks )
  #axis( side=4, labels=seq(from=0,to=30,by=2), at=yTicks )
  
  if ( gfx$grids )
  {
    abline( v=seq( xLim[1],  xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10),   col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }


  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  quants <- apply( Ct, 2, quantile, probs=qProbs )
  
  medCt <- apply(X=Ct,MARGIN=2,FUN=quantile,probs=0.5)

  peakMedianCatchYear <- which.max(medCt[tMP:nT])

  nQuants <- nrow( quants )
  
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=.TULQLWD, col=.TULQCOL, lty=.TULQLTY )
    lines( tVec,quants[4,], lwd=.TULQLWD, col=.TULQCOL, lty=.TULQLTY )
  }

  if( .PLOT.MED.PEAKCATCH )
  {
    segments( x0=tMP+peakMedianCatchYear-1,x1=tMP+peakMedianCatchYear-1,
              y0=0, y1=max(medCt[tMP:nT]), lty="dotted",col="black" )
    segments( x0=tMP,x1=tMP+peakMedianCatchYear-1,
          y0=max(medCt[tMP:nT]), y1=max(medCt[tMP:nT]), 
          lty="dotted",col="black" )

    panLab( x=(peakMedianCatchYear+6)/nT , y=0.1, 
          txt=paste(.INITYEAR+tMP+peakMedianCatchYear-1) )

  }
  # Add median 2016 TAC

  usr <- par( "usr" )

  if ( !is.null(MSY) && refPts )
  {
    usr <- par( "usr" )
    points( c(usr[1],usr[2]), c(MSY,MSY), xpd=T, bg=.MSYBG, col=.MSYCOL, cex=.MSYCEX, pch=.MSYPCH )
  }
  
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Ct[i,], col="black", lty=1, lwd=1 )
  }
  box()
  return( invisible() )
}     # END function .plotTulipCatch




# .plotTulipDis (tulip simulation envelope for catch)
# Purpose:        Display tulip (simulation envelope) catch for one simulation.
# Parameters:     obj is the object containing the Rdata blob list.
#                 Allow percentiles to be specified.
# Returns:        NULL (invisibly)
.plotTulipDis <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                     failedReps=NULL,
                     allQuants=TRUE, gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
                               showProj=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ),... )
{
  # Time indices (could use obj$par$nT).
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  nReps <- obj$ctlList$gui$nReps

  # Get the released catch biomass.
  Dt <- obj$om$Dt[ ,(2:ncol(obj$om$Dt)) ]
  tVec  <- c(1:ncol(Dt))
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( (tMP - 1),nT )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(Dt) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )
   
  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    .addXaxis( xLim, initYear=1970, side=1, years=gfx$useYears )
  else
    axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
  # X-axis (top): panel is in the first row.
  #if ( mfg[1]==1 )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
  #else
    #axis( side=3, at=xSeq, cex.axis=cax, labels=FALSE )  
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )

  if ( gfx$doGrid )
  {
    abline( v=seq( xLim[1], xLim[2], 5), lwd=0.5, col=.COLGRID )
    abline( h=seq( yLim[1], yLim[2], 5), lwd=0.5, col=.COLGRID )
  }

  abline( v=tMP, lwd=1.5, lty=2 )

  quants <- apply( Dt, 2, quantile, probs=qProbs )

  # Plot the extreme quantiles.
  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
    lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
  }
  
  if ( gfx$annotate )
    if ( !is.null(failedReps) )
      panLab( 0.5, 0.02, cex=0.8,
        paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )      

  usr <- par( "usr" )

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= (nReps-length(failedReps)) ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Dt[i,] )
  }
  box()  
  return( invisible() )
}     # END function .plotTulipDis


# .plotTulipF        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) F for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund
.plotTulipHR <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                         allQuants=TRUE,
                         gfx=list( annotate=TRUE, grids=FALSE, doLegend=TRUE,
                         showProj=TRUE, xLim=NULL, yLim=NULL, useYears=NULL), 
                         ...)
{
  
  Ft    <- obj$om$legalHR[ ,(2:ncol(obj$om$legalHR)) ]
  Fmsy  <- obj$refPtList$Fmsy
  nReps <- obj$ctlList$gui$nReps

  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tVec  <- c(1:ncol(Ft))

  # Specify axis limits for plots
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Ft[ ,tdx ], na.rm=TRUE )
  }
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
    #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
  #else
   # axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )



  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
  #else
   # axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }
  
  axis( side=4, labels=FALSE )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  

  quants <- apply( Ft, 2, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }
  abline( h=obj$ctlList$mp$hcr$targHRHerring, lty="dashed" )

  usr <- par( "usr" )

  box()

  #if ( !is.null(Fmsy) && refPts )
  #{
  #  urs <- par( "usr" )
  #  points( c(usr[1],usr[2]), c(Fmsy,Fmsy), xpd=T, bg=.FmsyBG, col=.FmsyCOL, cex=.FmsyCEX, pch=.FmsyPCH )
  #}

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Ft[i,], col="black", lty=1, lwd=1 )
  }
  return( invisible() )
}     # END function .plotTulipHR
  

# .plotTulipFmsy        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) Bmsy estimates for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund  / K. Holt
.plotTulipFmsy <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         refPts=TRUE, allQuants=TRUE,
                         gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         showProj=TRUE, xLim=NULL, yLim=NULL ), ... )
{
  # Extract time indices.
  tMP  <- obj$ctlList$opMod$tMP
  nT   <- obj$ctlList$opMod$nT
  
  mpdPars <- obj$mp$assess$mpdPars
  nReps <- length( unique( mpdPars$iRep ) )

  Fmsy <- obj$refPtList$Fmsy
  F01  <- obj$refPtList$F01
     
  # Specify axis limits for plotting
  xLim <- gfx$xLim
  yLim <- gfx$yLim
   
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1,nT )
   
  if ( gfx$showProj )
    xLim <- c( tMP,nT )
      
  # Create an x-axis vector   
  xSeq <- seq( xLim[1],xLim[2], 5 )  

  # Time indices
  tVec  <- sort( unique( mpdPars$tStep ) )

  # Set the y-axis limits.
  yLim <- gfx$yLim
  
  if ( is.null(yLim) )
  {
    tdx <- max( xLim[1], tMP):(xLim[2] ) - tMP + 1      

    yLim <- range( mpdPars$Fmsy, na.rm=TRUE )
    if ( refPts )
      yLim <- range( c(mpdPars$Fmsy,Fmsy), na.rm=TRUE )
  }

  # Create plot
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
  mfg <- par( "mfg" )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  
  axis( side=4, labels=FALSE )
  
  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  val <- split( mpdPars$Fmsy, mpdPars$tStep )
    
  quants <- sapply( val, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )
  
  # Plot the quantiles.
  lines( tVec,quants[3,], col="black", lwd=.LWD2, lty=1 )     # Median.
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  box()

  usr <- par( "usr" )

  if ( refPts )
  {
   urs <- par( "usr" )
   points( c(usr[1],usr[2]), c(Fmsy,Fmsy), xpd=T, bg=.FmsyBG, col=.FmsyCOL,
           cex=.FmsyCEX, pch=.FmsyPCH )
   abline( h=Fmsy, col=.FmsyCOL, lty=.FmsyLTY, lwd=.FmsyLWD )
     
   #points( usr[2], F01, xpd=T, bg=.F01BG, col=.F01COL,
   #        cex=.CEXSYM4, pch=.F01PCH )
  }
    
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    val <- split( mpdPars$Fmsy, mpdPars$iRep )
    for ( i in traces )
    {
     lines( tVec, val[[i]], col="black", lty=1, lwd=1 )
     points( tVec, val[[i]], bg="white", fg="black", pch=21 , cex=2 )
    }
  }
    
  return( invisible() )
}     # END function .plotTulipFmsy


#------------------------------------------------------------------------------#
# ADMB Diagnostic plots for guiPerf.                                           #
#------------------------------------------------------------------------------#

# Function: .plotConvTime
# Purpose: Produces boxplots showing the time required to reach convergence 
#           (in seconds) by ADMB optimization for all years within a given replicate
#           (if plot typ = convCallsRep) or all replicates within a given year
#           (if plot typ = convCallsYr). 
# Author: K.Holt (29-Jan-10)
.plotConvTime <- function( obj, typ="convTimeYr", gfx=list( annotate=TRUE, doLegend=TRUE,
                           xLim=NULL, yLim=NULL ) )
{
  nReps <- length( unique( obj$iRep ) )
  nT    <- max( obj$tStep )
  tMP   <- min( obj$tStep )
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  if ( is.null(yLim) )
    yLim <- c( 0, max( obj$convT ) )
      
  if ( typ == "convTimeYr" )
  {
    boxplot( obj$convT~obj$tStep, axes=FALSE, col="grey", ylim=yLim )
    xLabel <- seq( tMP, nT, by=10 )
    xPos   <- seq( 1,(nT-tMP+1), by=10 )

    # Add points where convergence failed...
    idx <- obj$iExit != 1
    points( jitter( obj$tStep[idx]-tMP+1 ), obj$convT[idx], bg="red", cex=.CEXSYM8, pch=21 )     
  }
    
  if ( typ == "convTimeRep" )
  {
    boxplot( obj$convT~obj$iRep, axes=FALSE, col="grey", ylim=yLim )
    xLabel <- seq( 1, nReps, by=1 )
    xPos   <- seq( 1, nReps, by=1 )
    
    # Add points where convergence failed...
    idx <- obj$iExit != 1
    points( jitter( obj$iRep[idx] ), obj$convT[idx], bg="red", cex=.CEXSYM8, pch=21 ) 
  }
  box()

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS4, at=xPos, labels=xLabel )
  else
    axis( side=1, at=xPos, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  else
      axis( side=2, labels=FALSE )

  if ( mfg[1]==1 )
    axis( side=3, at=xPos, labels=xLabel, cex.axis=.CEXAXIS4 )
  else
    axis( side=3, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  else
    axis( side=4, labels=FALSE )

  return( invisible() )
}     # END function .plotConvTime


# Function: .plotMaxGrad
# Purpose: Boxplot of maximum gradients (log scale) among years within each 
#           replicate (if plot typ = convGradRep)or among replicates for each year
#           (if plot typ = convGradYr) relative to the threshold level of 10-4 
#           that is used to define a sucessful convergence in teh optimization 
#           (note: a large maximum gradient from optimization indicates 
#            convergence failure).
# Input:   The "runStatus" data frame from blob$mp$assess
# Author: K.Holt (26-Aug-09), modified A.R. Kronlund (07-Dec-12).
.plotMaxGrad <- function( obj, typ, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  nReps <- length( unique( obj$iRep ) )
  nT    <- max( obj$tStep )
  tMP   <- min( obj$tStep )
  
  if ( is.null( gfx$xLim ) )
    xLim <- c( 1, max( obj$iRep ) )
    
  if ( is.null( gfx$yLim ) )
    yLim <- c( 0, mean( obj$maxGrad ) )

  if ( typ == "convGradRep" )
  {
    boxplot( obj$maxGrad~obj$iRep, axes=FALSE, log="y" )
    abline( h=.MAXGRADCRIT, col="red", lty=1, lwd=2 )
    xLabel <- seq( 1, nReps, 1 )
    xPos   <- seq( 1, nReps, 1 )
    box()
  }

  if ( typ=="convGradYr" )
  {
    boxplot( obj$maxGrad~obj$tStep, axes=FALSE, log="y"  )
    abline( h=.MAXGRADCRIT, col="red", lty=1, lwd=2 )
    xLabel <- seq( tMP, nT, by=5 )
    xPos   <- seq( 1, ( nT-tMP+1), by=5 )
    box()
  }

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
   axis( side=1, cex.axis=.CEXAXIS2, at=xPos, labels=xLabel )
  else
   axis( side=1, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  else
    axis( side=2, labels=FALSE )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2, at=xPos, labels=xLabel )
  else
    axis( side=3, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  else
    axis( side=4, labels=FALSE )

  return( invisible() )
}     # END function .plotMaxGrad


# .plotFtBt   (plot realized fishing mortality vs. SSB in relation to HCR)
# Purpose:     Shows the pairs of F and SSB that actually occurred
#              (i.e.,realized) pooled over all years and all repitions.  The 
#              harvest control rule used is also show, as are the Critical, 
#              Cautious, and Healthy zones.  The portion of points falling 
#              in each zone are shown in the legend.
# Parameters:   obj - the saved blob for simulation, colorZones=TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotFtBt <- function( obj, qProbs=c(0.05,0.1,0.5,0.9,0.95),
               refPts=pfRefPts, allQuants=TRUE, phase=FALSE,
               gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                         image=FALSE, xLim=NULL, yLim=NULL ),... )
{       
  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tProj <- c(tMP:nT)

  # Get spawning biomass and fishing mortality for all replicates.
  Bt <- obj$om$Bt[ ,(2:ncol(obj$om$Bt)) ]
  
  # For some reason Ft was not built with iRep.
  Ft <- obj$om$legalHR
  #dimFt <- dim( Ft )
    
  B0   <- obj$ctlList$opMod$B0
  Bmsy <- obj$ctlList$refPts$ssbFmsy
  Fmsy <- obj$ctlList$refPts$Umsy

  xLim <- gfx$xLim
  yLim <- gfx$yLim
#browser()
  # Subset the SSB and F from the projection period.
  Bt <- Bt[ ,tProj ]
  Ft <- Ft[ ,tProj ]
  
  if ( phase )
  {
    Bt <- Bt / Bmsy
    Ft <- Ft / Fmsy
  }
   
  if ( is.null(xLim) )
    xLim <- range( c(0,Bt) )

  if ( is.null(yLim) )
    yLim <- range( c(0,Ft) )

  plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", yaxs="i", ylab="")

  if ( gfx$image )
  {
    # Image the points.
    xBrks <- seq( 0,100,1 )
    yBrks <- seq( 0,5,0.005 )
    
    if ( phase )
    {
      xBrks <- seq( 0, 3, 0.05 )
      yBrks <- seq( 0, 3, 0.05 )
    }
  
    xCuts  <- cut( Bt, breaks=xBrks )
    yCuts  <- cut( Ft, breaks=yBrks )
    counts <- table( xCuts, yCuts )  
    image(  xBrks, yBrks, counts, col=rev(heat.colors(64)), add=TRUE,axes = FALSE)
    #contour( xBrks, yBrks, counts, add = TRUE )
    
    for ( i in 1:nrow(Bt) )
    {
      for ( g in c(1:dimFt[3]) )
        points( Bt[i,],Ft[,i,g], bg="white", col="black", cex=0.8, pch=21 )
    }
  }
  else
  {
      x <- cbind( x1=Bt, x2=Ft )
      points( Bt,Ft, pch=20, col=densCols( x, nbin=64, colramp=heat.colors ) )
  }

  # Enquire from par which figure in the array of figures is being drawn:
  mfg <- par( "mfg" )

  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=2, labels=FALSE, las=.YAXISLAS )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=4, labels=FALSE, las=.YAXISLAS )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
    abline( h=seq(yLim[1], yLim[2], length=10), col=.GRIDCOL, lty=.GRIDLTY, lwd=.GRIDLWD )
  }

  if ( phase )
  {
    abline( h=1, col=.FmsyCOL, lwd=.FmsyLWD, lty=.FmsyLTY )
    abline( v=1, col=.BmsyCOL, lwd=.BmsyLWD, lty=.BmsyLTY )
    .addTarget( 1, 1, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }
  else
  {
    abline( h=Fmsy, col=.FmsyCOL, lwd=.FmsyLWD, lty=.FmsyLTY )
    abline( v=Bmsy, col=.BmsyCOL, lwd=.BmsyLWD, lty=.BmsyLTY )
    .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  }

  if ( gfx$annotate )
  {
    ## Calculate number and proportion in each zone from tMP:nT.
    #nZone <- table( cut( Bt, breaks=c(0,zoneLimit,zoneUpper,max(Bt) ) ) )
    #pZone <- nZone / length(Bt)

    #zoneLabels <- c( paste( format(round(pZone[1],2),digits=2,nsmall=2)," Critical",sep=" " ),
    #                 paste( format(round(pZone[2],2),digits=2,nsmall=2)," Cautious",sep=" " ),
    #                 paste( format(round(pZone[3],2),digits=2,nsmall=2)," Healthy",sep=" " ) )
    
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, "Legal HR vs. Spawning Biomass" )
  }

  if ( gfx$doLegend )
  {
  }

  usr <- par( "usr" )

  box()
  return( invisible() )
}     # END function .plotFtBt.

#------------------------------------------------------------------------------#
#-- Sim Plotting Functions:                                                  --#
#------------------------------------------------------------------------------#

#-- Harvest Control Rule plotting functions.                                 --#

# .plotHCRvariableF (Plot a status-based control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
   
.plotHCRvariableF <- function( obj, refPts, rateType="U",
                       gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  B0     <- obj$opMod$B0
  Bmsy   <- refPts$ssbFmsy
  
  Fmsy   <- refPts$Fmsy
  F01    <- refPts$F01
  #sprX   <- obj$mp$hcr$sprX
  sprX   <- 40
  FsprX  <- refPts$F40
  Finput <- obj$mp$hcr$inputF
  
  Umsy   <- refPts$Umsy
  Uleg   <- refPts$legalHRFmsy
  U01    <- refPts$U01
  UsprX  <- refPts$U40
  Uinput <- obj$mp$hcr$inputF
  
  if ( rateType=="F" )
  {
    nameX  <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
    valX   <- c(  Fmsy,  F01,                      FsprX,  Finput )
  }
  
  if ( rateType=="U" )
  {
    nameX  <- c( "Umsy", "U01", paste( "Uspr",sprX,sep=""), "Uinput", "Ulegal" )
    valX   <- c(  Umsy,  U01,   UsprX,                       Uinput,  Uleg )
  }

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  idxRemRate <- 0
  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
  {
    remRefRate <- ifelse( rateType=="U", Umsy, Fmsy )
    idxRemRate <- 1
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
  {
    remRefRate <- ifelse( rateType=="U", U01, F01 )
    idxRemRate <= 2
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- ifelse( rateType=="U", Usprx, FsprX )
    idxRemRate <- 3
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
  {
    remRefRate <- Finput
    idxRemRate <- 4
  }
  if ( obj$mp$hcr$remRefBase == "rrBaseUlegal" )
  {
    remRefRate <- Uleg
    idxRemRate <- 5
  }

  # Plot harvest control rule.
  plot( c(0,B0*1.02),c(0,1.1*max(valX,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Status-based rule control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB

  # Draw the options for HCRs.
  for ( i in 1:length(valX) )
  {
    rrr <- valX[ i ]
    segments(          0,   0, lowerCtlPt,   0, col="black", lty=1, lwd=1 )
    segments( lowerCtlPt,   0, upperCtlPt, rrr, col="black", lty=1, lwd=1 )
    segments( upperCtlPt, rrr,     usr[2], rrr, col="black", lty=1, lwd=1 )
  }

  # Draw the selected intended removal rate curves.
  segments(          0,          0, lowerCtlPt,          0, col="blue", lty=1, lwd=4 )
  segments( lowerCtlPt,          0, upperCtlPt, remRefRate, col="blue", lty=1, lwd=4 )
  segments( upperCtlPt, remRefRate,     usr[2], remRefRate, col="blue", lty=1, lwd=4 )

  # Indicate lRef, uRef, and Bmsy.
  abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
  abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )
  
  abline( v=B0,   col=.B0COL,   lty=.B0LTY,   lwd=.B0LWD )
  abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )

  # Plot Fmsy~Bmsy.
  if ( rateType=="F" )
  {
    points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
    points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
    points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
    points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )      
  }
  
  if ( rateType=="U" )
  {
    points( upperCtlPt, Umsy,   pch=.UmsyPCH, bg=.UmsyBG, cex=.CEXSYM20 )
    points( upperCtlPt, U01,    pch=.U01PCH,  bg=.U01BG,  cex=.CEXSYM20 )
    points( upperCtlPt, UsprX,  pch=.UsprPCH, bg=.UsprBG, cex=.CEXSYM20 )
    points( upperCtlPt, Uinput, pch=.UcraPCH, bg=.UcraBG, cex=.CEXSYM20 )
    points( upperCtlPt, Uleg,   pch=.UlegPCH, bg=.UlegBG, cex=.CEXSYM20 )      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Spawning Biomass" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( "Removal Rate (",rateType,")", sep="" ) )

  if ( gfx$annotate )
  {
    lowerB <- round( lowerCtlPt,digits=2 )
    upperB <- round( upperCtlPt,digits=2 )
    
    #panLab( 0.8, 0.55, bquote( "B"["lower"]==.(lowerB) ) )
    #panLab( 0.8, 0.50, bquote( "B"["upper"]==.(upperB) ) )
    
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- paste( nameX,"=", round(valX,digits=4) )
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    if ( rateType=="F" )
      ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    if ( rateType=="U" )
      ptBg <- c( .UmsyCOL, .U01COL, .UsprCOL, .UcraCOL, .UlegCOL )

    names(ptBg) <- labels
     
    # Add a legend.
    colVec <- rep( "white", length(nameX) )
    colVec[ idxRemRate ] <- "blue"
    ltyVec <- rep( 1, length(nameX) )
    lwdVec <- rep( 4, length(nameX) )
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2, cex=1,
               title=paste( "LB =",lowerB," UB =",upperB ),
               col=colVec, lty=ltyVec, lwd=lwdVec, bty=.LEGBTY )                
  }

  return( invisible() )
}     # END function .plotHCRvariableF

# .plotHCRconstantC       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantC <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  constCatch <- obj$mp$hcr$constCatch
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Annual harvest rate.
  Bt <- seq( 0,B0, B0/10 )
  Ut <- constCatch / Bt
  
  # Annual instantaneous rate.
  Ft <- 1-exp(-Ut)
  lines( Bt, Ft, col="blue", lwd=3 )

  # Draw the selected intended removal rate curves.
  #abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )    

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantC

# .plotHCRconstantF       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
    remRefRate <- Fmsy
  else if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
    remRefRate <- F01
  else if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
  }
  else if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
    remRefRate <- Finput
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )

  # Status-based rule control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Draw the selected intended removal rate curves.
  abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )   

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantF


# .plotHCRdeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRdeclineRisk <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  # Three plot panels showing the increasing, stable, and decreasing acceptable
  # probability of decline curves.
 
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Harvest control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr )
  cat( "\n(.plotHCRdeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    usr <- par( "usr" )
    
    for ( j in 1:3 )
      lines( x=c(usr[1],lowerCtlPt, lowerCtlPt,upperCtlPt, upperCtlPt,usr[2] ),
             y=pDecline[i,], lty=1,lwd=2 )
  
    # Indicate lower and upper harvest control rule points.
    abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
    abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )

    if ( gfx$annotate )
    {
      panLab( 0.8,0.5,  cex=1.4, trendLabel[i] )
      panLab( 0.8,0.4, cex=1.4, paste( "Trend Years = ", obj$mp$hcr$trendYears,
        " (",obj$mp$hcr$lowerTrendPct,", ",obj$mp$hcr$upperTrendPct,")", sep="" ) )
      panLab( 0.8,0.3, cex=1.4, paste( "Proj Years = ",  obj$mp$hcr$nProjYears,sep="" ) )
#      panLab( 0.8,0.2, cex=1.4,
#        paste( "MCMC (thin) = ",  obj$nMCMC, " (",obj$nThin,")",sep="" ) )
      
      # Acceptable probability of decline levels.
      offset <- 0.01
      panLab( offset + 0.0,            0.8, adj=0, pDecline[i,1] )
      panLab( lowerCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,2] )      
      panLab( offset+lowerCtlPt/B0,    0.8, adj=0, pDecline[i,3] )
      panLab( upperCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,4] )
      panLab( offset+upperCtlPt/B0,    0.8, adj=0, pDecline[i,5] )
      panLab( 1 - offset,              0.8, adj=1, pDecline[i,6] )
    }

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0, B0/10.0 )
    axis( side=3, at=depletion, labels=depletion/B0 )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB4, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=0.5, cex=.CEXTITLE4, outer=TRUE, "Biomass" )
   
  return( invisible() )
}     # END function .plotHCRdeclineRisk

# .plotHCRviewDeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRviewDeclineRisk <- function( obj, iSim=1, iRep=1,
   gfx=list( annotate=TRUE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Six plot panels, with left column showing acceptable probability of decline
  # and right panels showing SSB_t for increasing, stable and decreasing recent
  # stock trends.
    
  #layout( matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(1.5,2,2,2) )

  B0   <- obj$ctlList$opMod$B0
  Bmsy <- obj$refPtList$ssbFmsy
  
  # Always default to showing HCR based on "true" biomass ref points, regardless
  # of whether Bmsy or B0 is used as a base (K.Holt):
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    base <- Bmsy
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    base <- B0
  
  # If Fmsy is used for reference removal rate, show HCR based on equilibrium value:
  #if ( obj$mp$hcr$guiPars$remRefTyp=="Fmsy" )
  #  remRate <- obj$mp$specespars$Fmsy     # K.Holt
  
  # Rule control points on operating model scale.
  lowerB <- obj$mp$hcr$specs$lowerBoundMult * base  
  upperB <- obj$mp$hcr$specs$upperBoundMult * base

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr$specs )
  cat( "\n(.plotHCRviewDeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )
  nT  <- obj$mp$hcr$specs$nT
  tMP <- obj$mp$hcr$specs$tMP

  colVec <- rev( heat.colors( nT-tMP+1 ) )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    mfg <- par( "mfg" )
    usr <- par( "usr" )
    
    # So called "true" line, may wish to omit.
    lines( x=c(usr[1],lowerB, lowerB,upperB, upperB,usr[2] ),
           y=pDecline[i,], lty=1,lwd=2 )
  
    lowerBound <- obj$mp$hcr$lowerBound
    lowerBound <- lowerBound[ iRep,c(2:ncol(lowerBound)) ]
    upperBound <- obj$mp$hcr$upperBound
    upperBound <- upperBound[ iRep,c(2:ncol(upperBound)) ]
    
    # Is the year in the current category of increasing, stable, decreasing?
    idx   <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE    
    
    # These are the lines for each update of control points.  But we should only
    # plot the lines corresponding to the increasing, decreasing, stable.
    for ( j in c(tMP:nT) )
    {
      # Is the trend in the current category?
      if ( idx[j] )
      {
        lines( x=c( usr[1], lowerBound[j], lowerBound[j],
                    upperBound[j], upperBound[j], usr[2] ),
               y=pDecline[i,], col="gray", lty=1, lwd=1 )
      }
    }
  
    # Indicate acceptability probability of decline control bounds.
    abline( v=lowerB, col=.HCRLBCOL, lty=.HCRLBLTY,  lwd=.HCRLBLWD )
    abline( v=upperB, col=.HCRLBCOL, lty=.HCRUBLTY,  lwd=.HCRUBLWD )
    abline( v=Bmsy,   col=.BmsyCOL,  lty=.BmsyLTY,   lwd=.BmsyLWD )    

    # Add points.
    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    pStar <- obj$mp$hcr$pStar[ iRep,2:(nT+1) ]
    
    points( ssb[idx], pStar[idx], bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    panLab( 0.7,0.1, cex=.CEXANNO2, trendLabel[i] )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0,B0/10.0 )
    
    if ( mfg[1]==1 )
      axis( side=3, at=depletion, cex.axis=.CEXAXIS4, labels=depletion/B0 )
    else
      axis( side=3, at=depletion, labels=FALSE )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB2, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=3, cex=.CEXTITLE2, "Estimated Stock Status" )

  Bexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  Bt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  nT   <- length( Bt )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( c(Bexp,Bt) ) )
  }

  for ( i in 1:length(trendLabel) )
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
    lines( c(1:nT), Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )   

    # Add slopes and points.
    idx <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE

    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    trendBio <- obj$mp$assess$trendBio[ iRep, 2:(nT+1) ]
    maxSlope <- max( abs( trendBio ), na.rm=T )
    
    # Add slope lines.
    usr <- par( "usr" )
    yDelta <- usr[4] - usr[3]
    mult <- (0.4 * yDelta) / maxSlope

    # ARK (24-Mar-13) Removed "-1" multiplcation.
    #segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult*-1.0) ) 
    segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult) )
    
    x <- c(1:nT)[idx]
    y <- ssb[idx]
    points( x, y, bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis (side=4, labels=FALSE )
    box()

    if ( gfx$doLegend )
    {
    }
  }
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year" )
  mtext( side=4, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Spawning Stock Biomass" )

  if ( gfx$annotate )
  {
    methodLab     <- .METHODLAB[obj$ctlList$mp$assess$methodId]  
    nProjYears    <- obj$mp$hcr$specs$nProjYears
    trendYears    <- obj$mp$hcr$specs$trendYears
    upperTrendPct <- obj$mp$hcr$specs$upperTrendPct
    lowerTrendPct <- obj$mp$hcr$specs$lowerTrendPct
    
    label <- paste( "Decline Risk: ", methodLab,", Proj=",nProjYears,
                    " Trend=",trendYears," (",lowerTrendPct,",",upperTrendPct,
                    ")", sep="" )
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, label )
  }
     
  return( invisible() )
}     # END function .plotHCRviewDeclineRisk


# .plotYield    (plot true and estimated yield curves)
# Purpose:      Plot the true yield curve (based on equilibrium analsyis)
#               and the estimated curve from the surplus production model fit
#               in the final year of the management procedure.  Dots on each
#               curve show the spawning stock biomass level associated with MSY.
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       K.R. Holt
.plotYield <- function(obj, iSim=1, iRep=1,
  gfx=list( annotate=TRUE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Plot yield against spawning stock biomass.
  rp <- calcRefPoints( obj$pars, rpList=list( FALL=TRUE ) )

  assessMethod<-obj$guiPars$assess
  if ( assessMethod=="pMod" )
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )

    # Extract parameter estimates in last year:
    mpdPars <-obj$mp$assess$mpdPars
    B0Est<-mpdPars[7][mpdPars[1]==iRep & mpdPars[2]==obj$pars$nT] *2
    rEst<- mpdPars[8][mpdPars[1]==iRep& mpdPars[2]==obj$pars$nT ] *2
    msyEst<-rEst*B0Est/4
    BmsyEst<-B0Est/2

    # Calculate estimated yied curve:
    X<-seq(0,(B0Est+1),by=1)
    Y<-rEst*X*(1-(X/B0Est))

    # Specify axis limits
    # x-axis limits.
    if ( xScale )
    {
      xAvg <- mean(mpdPars[7])
      xLim<-c(0,max((1.2*xAvg),obj$guiPars$pars$B0))
    }
    else
      xLim <- xAxis
    # y-axis limits.
    if ( yScale )
    {
      yAvg <- mean(mpdPars[8]*mpdPars[7]/4)
      yLim <- c(0,max(1.2*mean(yAvg), obj$guiPars$other$yieldFmsy))
    }
    else
      yLim<-yAxis

    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Add true curve
    lines( rp$ssb, rp$yield, lwd=2 )
    points( rp$ssbFmsy, rp$yieldFmsy, cex=.CEXSYM4, bg=.BmsyBG, pch=.BmsyPCH )

    # Add estimated curve
    lines(X,Y, lwd=2, col="red")
    points( BmsyEst, msyEst, cex=.CEXSYM4, bg=.FmsyBG, pch=21 )

    # Make it look pretty
    axis( side=1 )
    axis( side=2, las=.YAXISLAS )
    box()
    mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Spawning Biomass" )
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Yield" )

    if ( annotate )
      {
        panLegend( 0.05,0.95, legTxt=c("Equilibrium Analysis","Production Model Estimate"),
            lty=c(1,1), col=c("black", "red"), lwd=c(2,2), bg="white"  )
        mtext( side=3, line=0, outer=TRUE, cex=.CEXTITLE4, "Yield Curve", bty=.LEGBTY )
      }
  }
  else
  {
    plot( c(0,1),c(0,1),type="n", axes=FALSE, xlab="", ylab="" )
    panLab( 0.5, 0.5, cex=1.4, "Plot not Implemented for this Procedure" )
  }
}

.plotAcceptProbDecline <- function(
  pDecline=matrix( c( 0.01,0.01,0.01, 0.05,0.05,0.025, 0.05,0.05,0.025,
                      0.95,0.50,0.25, 0.95,0.50,0.25,  0.95,0.95,0.95 ),
                      nrow=3, ncol=6 ),
                      lowerLimit=0.4, upperLimit=0.8 )
{
  cat( "\nMSG (.plotAcceptProbDecline) Acceptable Probability of Decline:\n\n" )
  print( pDecline )
  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy).    
  xLim <- c(0,1)
  trendLabels <- c( "Increasing","Stable","Decreasing" )
  for ( i in 1:nrow(pDecline) )
  {
    plot( xLim,c(0,1), type="n", axes=FALSE, xlab="", xlim=xLim, ylab="" )

    for ( j in 1:3 )
      lines( x=c(xLim[1],lowerLimit, lowerLimit,upperLimit, upperLimit,xLim[2]),
             y=pDecline[i,],
             lty=1,lwd=2 )
               
    abline( v=lowerLimit, lty=2 )
    abline( v=upperLimit, lty=2 )

    text( 0.01, 0.9, adj=0, trendLabels[i] )

    axis( side=1 )
    axis( side=2, las=2 )
    box()

    mtext( side=1, line=1, cex=1.0, outer=TRUE, "Stock Status (Depletion)" )
    mtext( side=2, line=1, cex=1.0, outer=TRUE,
           "Acceptable Probability of Decline" )
  }
}     # END function .plotAcceptProbDecline.

#.plotPaAdj    (plot spawning biomass, HCR, catch showing AP Adjustment.
# Purpose:     Produces three plot panels, the first showing annual spawning
#                biomass, the second showing annual perceived HCR, and third
#                showing catch.
# Parameters:   Object with saved simulation results, sim and rep number
#                 to be plotted, and plot specifications.
# Returns:      NULL (invisibly)
# Source:       A.R. Kronlund
.plotPaAdj <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  
  # Stock status zone boundaries on SSB scale.
  Bmsy <- obj$ctlList$refPts$ssbFmsy
  Fmsy <- obj$ctlList$refPts$Fmsy
  Bt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  
  # Get terminal biomass trajectory.
  estBt <- obj$mp$assess$Bt[ iRep, (2:ncol(obj$mp$assess$Bt)) ]

  # Factor to adjust exploitable biomass for error.  
  eBioAdj <- obj$mp$hcr$eBioAdj[ iRep,2:ncol(obj$mp$hcr$eBioAdj) ]
  
  Dt <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
  Ft <- obj$om$Ft[ iRep,(2:ncol(obj$om$Ft)) ]
  
  targetF <- obj$mp$hcr$targetF[ iRep,c(2:ncol(obj$mp$hcr$targetF)) ]
  
  It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]

  xLim <- gfx$xLim

  # X-axis limits (same for all three panels)
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 1: Plot biomass and adjustment.
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[1,]
  else
  {
    yLim <- c(0,max(Bt))
  }

  colVec <- rev( heat.colors(nT-tMP+1) )
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  
  # Adjustment.
  adjB <- eBioAdj * estBt
  segments( c(tMP:nT), estBt[tMP:nT], c(tMP:nT), adjB[tMP:nT] )

  #points( c(tMP:nT), adjB[tMP:nT], bg="black", cex=.CEXSYM20, pch=21 )
  
  points( c(tMP:nT), estBt[tMP:nT], bg=colVec, cex=.CEXSYM20, pch=21 )
    
  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "OM and MP Biomass" )
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, "Year" )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.BmsyLAB),lty=.BmsyLTY,col=.BmsyCOL, lwd=.BmsyLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }

  # Panel 2: Plot F vs. SSB.
  
  # Determine HCR type and get properties.
  hcrType <- obj$ctlList$mp$hcr$hcrType
  if ( hcrType=="constantF" | hcrType=="variableF" )
  {
    if( obj$mp$hcr$specs$remRefBase == "rrBaseFmsy" )
      remRate <- obj$ctlList$refpts$Fmsy
    if ( obj$mp$hcr$specs$remRefBase == "rrBaseF01" )
      remRate <- obj$ctlList$refPts$F01
    if ( obj$mp$hcr$specs$remRefBase =="rrBaseFspr" )
      remRate <- obj$ctlList$refPts$FsprX
  }

  # Calculate targeted (intended) HCR bound points.
  lowerBoundMult <- obj$ctlList$mp$hcr$lowerBoundMult
  upperBoundMult <- obj$ctlList$mp$hcr$upperBoundMult
  
  # Panel 1: Plot biomass and adjustment.
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(gfx$xLim) )
    xLim <- c(0,max( c(Bmsy,estBt[tMP:nT]), na.rm=TRUE))
  
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[2,]
  else
    yLim <- c(0,max(c(Fmsy,targetF),na.rm=TRUE))
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  arrows( estBt[tMP:nT], targetF[tMP:nT], adjB[tMP:nT], targetF[tMP:nT],
          length=0.10, angle=20 )
  points( estBt[tMP:nT],   targetF[tMP:nT], bg=colVec, cex=.FtCEX*1.4, pch=.FtPCH )

  abline( h=Fmsy, lty=.FmsyLTY, col=.FmsyCOL, lwd=.FmsyLWD )
  abline( h=obj$ctlList$refPts$Fcra, lty=.FcraLTY, col=.FcraCOL )
  abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  
   .addTarget( Bmsy, Fmsy, cexCenter=1.4, colCenter=.BmsyCOL, colRing=.BmsyCOL )    
  
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  mtext( side=2, line=4, cex=.CEXLAB2, "Fishing Mortality" )
  mtext( side=1, line=2, cex=.CEXLAB2, "Biomass" )  

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.FmsyLAB),lty=.FmsyLTY,col=.FmsyCOL, lwd=.FmsyLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }

  # X-axis limits (same for all three panels)
  if ( is.null(gfx$xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )

  # Panel 3: Plot catch.
  # Y-axis limits.
  if ( !is.null(gfx$yLim) )
    yLim <- gfx$yLim[3,]
  else
  {
    yLim <- c(0,max(Dt))
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  abline( h=obj$ctlList$refPts$yieldFmsy, lty=.MSYLTY, col=.MSYCOL, lwd=.MSYLWD )

  lines( c(1:nT), Dt, lty=.DtLTY, lwd=.DtLWD, col=.DtCOL )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX*1.5, pch=.DtPCH )
  points( c(tMP:nT), Dt[ tMP:nT ], bg=colVec, cex=.DtCEX*1.5, pch=.DtPCH )

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE2, cex=.CEXLAB2, "Year"  )
  mtext( side=2, line=.INLINE4, cex=.CEXLAB2, "Catch" )

  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95, legTxt=c(.MSYLAB),lty=.MSYLTY,col=.MSYCOL, lwd=.MSYLWD,
               cex=.CEXLEG2, bg="white", bty=.LEGBTY )
  }
}     # END function .plotPaAdj

.plotDeclineRisk <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  drawTrend <- function( y2, slope, xDelta, t )
  {
    x2 <- t
    x1 <- x2 - xDelta + 1
    #y1 <- -1 * (xDelta * slope) + y2
    y1 <- y2 * exp( xDelta * slope*-1 )
    segments( x1, y1, x2, y2 )
  }

  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  idx <- tMP:nT
  
  lowerQuota <- obj$ctlList$mp$hcr$lowerQuota
  upperQuota <- obj$ctlList$mp$hcr$upperQuota
  nQlevels   <- obj$ctlList$mp$hcr$nQlevels

  pStar      <- obj$mp$hcr$pStar
  pStar      <- pStar[ pStar[,1]==iRep,c(2:ncol(pStar)) ]
  
  trendYears <- obj$ctlList$mp$hcr$trendYears

  trendBio   <- obj$mp$assess$trendBio
  trendBio   <- trendBio[ trendBio[,1]==iRep,c(2:ncol(trendBio)) ]
  
  trendVal   <- obj$mp$assess$trendVal
  trendVal   <- trendVal[ trendVal[,1]==iRep,c(2:ncol(trendVal)) ]  
  
  Bt         <- obj$om$Bt
  Bt         <- Bt[ Bt[,"iRep"]==iRep, c(2:ncol(Bt)) ]
  
  retroSpawnBt <- obj$mp$assess$retroSpawnBt
  retroSpawnBt <- retroSpawnBt[ retroSpawnBt[,"iRep"]==iRep, c(2:ncol(retroSpawnBt)) ]
  
  # Now the last element in retroSpawnBt is the projSpawnBio, so we need the
  # second to last element for spawnBT in any retrospective year.  This is what
  # is used in the trend calculation, not the projected spawning biomass.
  
  trendSpawnBT <- rep( NA,nrow(retroSpawnBt) )
  for ( i in 1:length(trendSpawnBT) )
    trendSpawnBT[i] <- retroSpawnBt[ i, retroSpawnBt[i,"tStep"]-1 ]
  print( trendSpawnBT )
 
  Dt         <- obj$om$Dt
  Dt         <- Dt[ Dt[,"iRep"]==iRep, c(2:ncol(Dt)) ]
  
  MSY        <- obj$refPtList$yieldFmsy
  Bmsy       <- obj$refPtList$ssbFmsy

  # Plotting attributes.
  trendCol <- rep( "white", length(trendBio) )
  trendCol <- ifelse( trendVal==1, "green", trendCol )
  trendCol <- ifelse( trendVal==3, "red", trendCol )
  
  trendSym <- rep( 21, length(trendBio) )
  trendSym <- ifelse( trendVal==1, 24, trendSym )
  trendSym <- ifelse( trendVal==3, 25, trendSym )
  
  xLim <- gfx$xLim
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  
  if ( gfx$showProj )
    xLim <- c( (tMP - trendYears-1),nT )
  
  # Plot OM spawning biomass and terminal spawning biomass for each
  # time step.  Add trend slope, and color symbols as increasing, stable, decreasing.

  yLim <- gfx$yLim[,1]  
  if ( is.null(yLim) )
    yLim <- c( 0,max(Bt[xLim[1]:xLim[2]]))
  
  plot( c(1:nT), Bt, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
 
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  
  drawTrend( trendSpawnBT, trendBio[idx], trendYears, idx )

  points( idx, trendSpawnBT, bg=trendCol[idx], cex=.BtCEX, pch=.BtPCH )
    
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.4,
      legTxt=c( .BmsyLAB, .BtLAB, paste( "Trend (",trendYears," year)",sep="" ),
                "Est. SSB (increasing)","Est. SSB (stable)","Est. SSB (decreasing)" ),
      col=c( .BmsyCOL, .BtCOL, "black", "black","black","black" ),
      pch=c(NA,NA,NA, .BtPCH, .BtPCH, .BtPCH ),
      pt.cex=c(NA,NA,NA,.BtCEX, .BtCEX, .BtCEX),
      pt.bg=c(NA,NA,NA, "green","white","red" ),
      lwd=c(.BmsyLWD, .BtLWD,1, NA,NA,NA ),
      lty=c(.BmsyLTY,.BtLTY,1, NA,NA,NA ),
      bg="white", bty=.LEGBTY )  
  }

  mtext( side=2, line=3, cex=.CEXLAB4, paste( "Biomass"," (",.BexpUNIT,")", sep="") )
  
  # Plot pStar and catch over time.
  yLim <- gfx$yLim[,2]  
  if ( is.null(yLim) )
    yLim <- c(0,max(Dt))  
   
  plot( c(1:nT), Dt, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
  abline( h=MSY, col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD )
  lines( c(1:nT), Dt, type="h", col="gray", lty=1, lwd=4 )
  axis( side=4, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  
  par( new=TRUE )
  yLim <- c(0,1)
  plot( c(1:nT), pStar, type="n", axes=FALSE, xlim=xLim, ylim=yLim )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD ) 
    
  points( c(1:nT), pStar, bg=trendCol, cex=.CEXSYM8, pch=trendSym )
  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
    
  box()
  
  if ( gfx$doLegend )
  {
    panLegend( 0.05,0.95,
      legTxt=c( .MSYLAB, .DtLAB, "pStar (increasing)", "pStar (stable)",
                "pStar (decreasing)" ),
      col=c( .MSYCOL, "gray", "black","black","black" ),
      pch=c(NA,NA,24,21,25), pt.cex=c(NA,NA,.CEXSYM8,.CEXSYM8,.CEXSYM8),
      pt.bg=c(NA,NA,"green","white","red"),
      lwd=c( .MSYLWD, 4,NA,NA,NA),
      lty=c(.MSYLTY, 1,NA,NA,NA),
      bg="white", bty=.LEGBTY )  
  }  
  
  mtext( side=2, line=3, cex=.CEXLAB4, "pStar" )
  mtext( side=4, line=3, cex=.CEXLAB4, paste( .DtLAB," (",.DtUNIT,")", sep="" ) )
  
  mtext( side=1, line=0, cex=.CEXLAB4, outer=TRUE, "Year" )
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=-1, cex=.CEXTITLE4, outer=TRUE,
      paste( "Decline Risk Rule:", .METHODLAB[obj$ctlList$mp$assess$methodId] ) )
  }
   
}     # END function .plotDeclineRisk


.plotRetroStat1 <- function( obj, iSim=1, iRep=1, statName="SumRelErr",
  gfx=list( annotate=TRUE, doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  retroStats <- obj$mp$assess$retroStats
  retroVals  <- obj$mp$assess$retroVals[,,statName]
  
  nReps      <- nrow( retroStats )
  nT         <- obj$ctlList$opMod$nT
  retroYears <- obj$ctlList$mp$assess$retroYears
  tMP        <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  if ( is.null( xLim ) )
  {
    xLim <- c(1,nT)
  }
  
  if ( gfx$showProj )
  {
    xLim <- c( tMP-retroYears-1, nT )
  }
  
  yLim <- range( retroVals[ ,c(3:ncol(retroVals)) ], na.rm=TRUE ) 
  
  # Plot the retrospective values.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
  
  vals <- retroVals[ retroVals[,"iRep"]==iRep, ]
  idx  <- c( 3:(nT+2) )
  nSteps <- nrow(vals)
  colVec <- rev( heat.colors(nrow(vals)) )
  
  for ( i in 1:nSteps )
  {
    lines( c(1:nT), vals[ i, idx ] )
    tStep <- vals[ i,"tStep" ]
    points( tStep, vals[ i, tStep+2 ], bg=colVec[i], cex=1.4, pch=21 )
  }
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE2, cex=.CEXLAB4, "Year" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Retrospective Values" ) 
  
  # Plot the retrospective statistics.
  xLim <- c(1,nReps)
  yLim <- range( retroStats[ ,statName ], na.rm=T )
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  abline( h=mean( retroStats[ ,statName ] ), lty=3, lwd=2 )
  
  points( c(1:nReps), retroStats[ ,statName ], bg="white", cex=1.5, pch=21 )
  points( iRep, retroStats[ iRep,statName ], bg="black", cex=1.5, pch=21 )
  
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2 )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Replicate" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB4, paste( "Statistics: ", statName,sep="" ) )   
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, statName )
  }
  
}     # END function .plotRetroStat1

.plotHistory <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                          xLim=NULL, yLim=NULL ) )
{
  tMP        <- obj$opMod$tMP

  # Get the history file.
  hstFile <- file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE )
  if ( !file.exists( hstFile ) )
  {
    cat( "\nMSG (.plotHistory) Stock history file not found: ", hstFile,"\n" )
    alarm()
  }
  else
  {
    if ( obj$opMod$historyType=="omFile" )
    {
      histPars <- read.table( hstFile, as.is=TRUE, header=TRUE, sep="," )
    
      tStep    <- histPars$Year
      omegat   <- histPars$omegat
      Ft       <- histPars$Ft
    }
    
    # This is trick, yay!
    mpObj   <- .createMP( obj )
    initPop <- .initPop( mpObj )

    tStep <- c(1:(tMP-1))

    Bt   <- initPop$om$Bt[1:(tMP-1)]
    Bexp <- initPop$om$Bexp[1:(tMP-1)]
    Dt   <- initPop$om$Dt[1:(tMP-1)]
    Ft   <- initPop$om$Ft[1:(tMP-1)]
    It   <- initPop$mp$data$It[1:(tMP-1)]
    
    Rt   <- initPop$om$Rt[1:(tMP-1)]
    
    Bmsy <- obj$refPts$ssbFmsy
    MSY  <- obj$refPts$yieldFmsy
    
    xLim <- gfx$xLim
    if ( is.null( xLim ) )
    {
      xLim <- range( tStep )
    }
    
    yLim <- c( 0,max(Rt) )
  
    # Plot the log(recruitment deviations).
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
  
    points( tStep,Rt, bg=.RtBG, cex=.RtCEX, col=.RtCOL, pch=.RtPCH )
  
    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()
  
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Recruitment" ) 

    # Plot the catches and fishing mortalities.
    yLim <- c(0,max(Dt,na.rm=T))
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
    
    abline( h=MSY, col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD )    
    lines( tStep, Dt, type="h", col=.DtCOL, lwd=4 )
    
    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    axis( side=3, labels=FALSE )    
    
    par( new=T )
    yLim <- c(0,max(Ft, na.rm=TRUE))
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )
    
    points( tStep, Ft, bg=.FtBG, cex=.FtCEX, col=.FtCOL, pch=.FtPCH )  

    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
  
    if ( gfx$doLegend )
    {
      panLegend( 0.025,0.95, legTxt=c(.FtLAB, .DtLAB, .MSYLAB ),
            lty=c(NA, .DtLTY, .MSYLTY), lwd=c(NA,4,.MSYLWD),
            col=c(.FtCOL,.DtCOL, .MSYCOL), pch=c(.FtPCH,NA,NA),
            pt.cex=c(.FtCEX,NA,NA),
            pt.bg=c(.FtBG,NA,NA), bg="white", cex=.CEXLEG2, bty=.LEGBTY  )
    }  
  
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, .FtLAB )
    mtext( side=4, line=.INLINE4, cex=.CEXLAB4, .DtLAB )
    
    # Plot the reconstructed biomass trajectories.
    yLim <- c( 0,max(Bt) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )

    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    
  
    lines( c(1:(tMP-1)),  Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
    lines( c(1:(tMP-1)),  Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )  
    points( c(1:(tMP-1)), It,   bg=.ItBG, cex=.ItCEX, col=.ItCOL, pch=.ItPCH )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
 
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
    box()

    if ( gfx$doLegend )
    {
      panLegend( 0.025,0.35, legTxt=c(.BtLAB, .BexpLAB, .BmsyLAB, .ItLAB),
            lty=c(.BtLTY,.BexpLTY, .BmsyLTY, NA ), lwd=c(.BtLWD,.BexpLWD,.BmsyLWD, NA),
            col=c(.BtCOL,.BexpCOL, .BmsyCOL, .ItCOL ), pch=c(NA,NA,NA,.ItPCH),
            pt.cex=c(NA,NA,NA,.ItCEX),
            pt.bg=c(NA,NA,NA,.ItBG), bg="white", cex=.CEXLEG2, bty=.LEGBTY  )
    }
    mtext( side=2, line=.INLINE3, cex=.CEXLAB4, "Biomass" )  
  }  
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB4, "Year" )
  
  if ( gfx$annotate )
  {
    mtext( side=3, line=0, cex=.CEXTITLE4, outer=TRUE, "Initialization History" )
  }
}     # END function .plotHistory

#------------------------------------------------------------------------------#
#-- Sablefish Closed Loop Simulation Plots                                   --#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- Plotting Functions: Life History                                         --#
#------------------------------------------------------------------------------#

.plotLenAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )
    yLim <- c( 0,max(obj$Lal) )
  
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  for( l in 1:ncol(obj$Lal) )  
    lines( obj$ages, obj$Lal[,l], lty=1 )
  
  if ( gfx$annotate )
    abline( h=obj$sizeLim,lty=2, lwd=2 )
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Length-at-age" )
}

.plotMatAtAge <- function( obj, gfx=list( annotate=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$Ma) )
    
  # Maturity at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( obj$ages, obj$Ma, lty=1 )

  if ( gfx$annotate )
  {  
    A50 <- max( obj$ages[ obj$Ma <= 0.5001 ] )
    A95 <- max( obj$ages[ obj$Ma <= 0.9501 ] )
    segments( A50, 0.0,  A50, 0.5,  lty=2 )
    segments( 0.0, 0.5,  A50, 0.5,  lty=2 )
    segments( A95, 0.0,  A95, 0.95, lty=2 )
    segments( 0.0, 0.95, A95, 0.95, lty=2 )
  }
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Maturity-at-age" )
}

.plotNumAtAge <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  yLim <- c( 0,max(obj$numAgeYr1 ) )
  yLim[2] <- round( yLim[2],digits=1 )
  
  # Numbers at age in year 1.
  barplot( obj$numAgeYr1,names.arg=obj$ages, axes=FALSE, xlab="", ylab="",
           ylim=yLim )
  axis (side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()

  if ( gfx$doLegend )
  {    
    legend( "top",legend=c(paste("Max:",round(max(obj$numAgeYr1),digits=4)),
            paste("Min:", round(min(obj$numAgeYr1), digits=4))), bty="n")
  }
            
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Number at Age-1" )
}

.plotWgtAtAge <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  xRange <- c( 0,max(obj$ages) )
  
  if ( is.null(xLim) )
    xLim <- c( 0,max(obj$ages) )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(obj$Wal) )
  
  # Weight at age.
  plot( xLim,yLim,type="n",axes=FALSE,xlab="",ylab="" )
  for( l in 1:ncol(obj$Wal) )  
    lines( obj$ages, obj$Wal[,l], lty=1 )
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Age" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Weight-at-age" )
}

.plotWgtLen <- function( obj, gfx=list( xLim=NULL, yLim=NULL ) )
{
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim
  
  if ( is.null(xLim) )
    xLim <- c( min(rowMeans(obj$Lal)),max(rowMeans(obj$Lal)) )

  if ( is.null(yLim) )
    yLim <- c( min(rowMeans(obj$Wal)),max(rowMeans(obj$Wal)) )
  
  # Weight against length.
  plot( rowMeans(obj$Lal), rowMeans(obj$Wal), type="n", axes=FALSE,
        xlab="",xlim=xLim,ylab="",ylim=yLim )
  lines( rowMeans(obj$Lal), rowMeans(obj$Wal), lty=1 )
    
  axis( side=1, cex.axis=.REFCAX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
    
  mtext( side=1, line=2.5, cex=.REFCEX, "Length" )
  mtext( side=2, line=3.0, cex=.REFCEX, "Weight" )    
}

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
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=.CEXSYM8, cex=.CEXLAB4 )
}

.plotRecSSB <- function( obj, gfx )
{
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim

  xRange <- range( c(0,max(obj$ssb)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$recruits)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot recruits against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$recruits), lwd=2 )
    
  # Adding steepness lines at B20=0.2*B0, R20, 
  # and steepness R20/B0.
  lines( c(obj$B20,obj$B20), c(0,      obj$R20), lty=2 )
  lines( c(0,      obj$B20), c(obj$R20,obj$R20), lty=2 )
  
  lines( c(obj$B0,obj$B0), c(0,     obj$R0), lty=2 )
  lines( c(0,     obj$B0), c(obj$R0,obj$R0), lty=2 )
  
  # Adding steepness label
  h <- round( obj$R20/obj$R0, digits=2 )
  xPos <- obj$B20
  yPos <- obj$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",obj$rSteepness,sep="") )
  
  if ( checked["F0"] )  
    points( obj$ssbF0,   obj$recruitsF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$ssbF01,  obj$recruitsF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$ssbFcra, obj$recruitsFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$ssbFmax, obj$recruitsFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$ssbFmsy, obj$recruitsFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$ssbF40,  obj$recruitsF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )      
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Recruits" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.4, checked=checked )
}

.plotRecSSB2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$recruits)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$ssb), .posVal(obj[[i]]$om$recruits), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )

    # Adding steepness lines at B20=0.2*B0, R20, 
    # and steepness R20/B0.
    lines( c(obj[[i]]$om$B20,obj[[i]]$om$B20), c(0,      obj[[i]]$om$R20), lty=2 )
    lines( c(0,      obj[[i]]$om$B20), c(obj[[i]]$om$R20,obj[[i]]$om$R20), lty=2 )
  
    lines( c(obj[[i]]$om$B0,obj[[i]]$om$B0), c(0,     obj[[i]]$om$R0), lty=2 )
    lines( c(0,     obj[[i]]$om$B0), c(obj[[i]]$om$R0,obj[[i]]$om$R0), lty=2 )

    # Adding steepness label
    h <- round( obj[[i]]$om$R20/obj[[i]]$om$R0, digits=2 )
    xPos <- obj[[i]]$om$B20
    yPos <- obj[[i]]$om$R20 * 0.8
    text( xPos, yPos, cex=1, pos=4, paste( "h=",obj[[i]]$om$rSteepness,sep="") )
    
    if ( checked["F0"] )  
      points( obj[[i]]$om$ssbF0, obj[[i]]$om$recruitsF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$ssbF01,  obj[[i]]$om$recruitsF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$ssbFcra, obj[[i]]$om$recruitsFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$ssbFmax, obj[[i]]$om$recruitsFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$ssbF40,  obj[[i]]$om$recruitsF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$ssbFmsy, obj[[i]]$om$recruitsFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$ssbFmsy, obj[[i]]$om$recruitsFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Recruits" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}    

.plotSsbF <- function( obj, gfx )
{
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
    points( obj$F0,   obj$ssbF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$ssbF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$ssbFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$ssbFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$ssbF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotSsbF2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$F)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$ssb)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$F), .posVal(obj[[i]]$om$ssb), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["F0"] )  
      points( obj[[i]]$om$F0, obj[[i]]$om$ssbF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$F01,  obj[[i]]$om$ssbF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$Fcra, obj[[i]]$om$ssbFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$Fmax, obj[[i]]$om$ssbFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$F40,  obj[[i]]$om$ssbF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$Fmsy, obj[[i]]$om$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$Fmsy, obj[[i]]$om$ssbFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}    

.plotSsbPerRecF <- function( obj, gfx )
{
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
    points( obj$F0,   obj$ssbprF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$ssbprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$ssbprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$ssbprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$ssbprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )       
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotSsbPerRecF2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$F)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$ssbpr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$F), .posVal(obj[[i]]$om$ssbpr), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["F0"] )  
      points( obj[[i]]$om$F0, obj[[i]]$om$ssbprF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$F01,  obj[[i]]$om$ssbprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$Fcra, obj[[i]]$om$ssbprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$Fmax, obj[[i]]$om$ssbprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$F40,  obj[[i]]$om$ssbprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$Fmsy, obj[[i]]$om$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$Fmsy, obj[[i]]$om$ssbprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}    

.plotYieldF <- function( obj, gfx )
{
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
    points( obj$F0,   obj$yieldF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )    

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotYieldF2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$Fcra)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$yield)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$F), .posVal(obj[[i]]$om$landed), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["F0"] )  
      points( obj[[i]]$om$F0, obj[[i]]$om$yieldF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$F01,  obj[[i]]$om$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$Fcra, obj[[i]]$om$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$Fmax, obj[[i]]$om$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$F40,  obj[[i]]$om$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$Fmsy, obj[[i]]$om$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$Fmsy, obj[[i]]$om$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}    

.plotYieldSSB <- function( obj, gfx )
{
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
    points( obj$ssbF0,   obj$yieldF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$ssbF01,  obj$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$ssbFcra, obj$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$ssbFmax, obj$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$ssbFmsy, obj$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$ssbF40,  obj$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )
   
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Landed Yield" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.9, checked )
}

.plotYieldSSB2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$landed)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$ssb), .posVal(obj[[i]]$om$landed), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["F0"] )  
      points( obj[[i]]$om$ssbF0, obj[[i]]$om$yieldF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$ssbF01,  obj[[i]]$om$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$ssbFcra, obj[[i]]$om$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$ssbFmax, obj[[i]]$om$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$ssbF40,  obj[[i]]$om$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$ssbFmsy, obj[[i]]$om$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$ssbFmsy, obj[[i]]$om$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Landed Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}      

.plotYprF <- function( obj, gfx )
{
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
    points( obj$F0,   obj$yprF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["F01"] )
    points( obj$F01,  obj$yprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Fcra"] )
    points( obj$Fcra, obj$yprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Fmax"] )
    points( obj$Fmax, obj$yprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Fmsy"] )
    points( obj$Fmsy, obj$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["F40"] )
    points( obj$F40,  obj$yprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )    
    
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield per Recruit" )
  box()
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.7, y=0.4, checked )
}

.plotYprF2 <- function( obj, idNum=NULL, gfx )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$om$Fcra)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$om$ypr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    lines( .posVal(obj[[i]]$om$F), .posVal(obj[[i]]$om$yprL), lwd=2 )

    if ( checked["F0"] )  
      points( obj[[i]]$om$F0, obj[[i]]$om$yprF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["F01"] )
      points( obj[[i]]$om$F01,  obj[[i]]$om$yprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Fcra"] )
      points( obj[[i]]$om$Fcra, obj[[i]]$om$yprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Fmax"] )
      points( obj[[i]]$om$Fmax, obj[[i]]$om$yprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["F40"] )
      points( obj[[i]]$om$F40,  obj[[i]]$om$yprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Fmsy"] )
    {
      points( obj[[i]]$om$Fmsy, obj[[i]]$om$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( obj[[i]]$om$Fmsy, obj[[i]]$om$yprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "F" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield Per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegend( x=0.8, y=0.95, checked )
}    

# .plotRefPtsF    (Plot fishery reference points for the operating model)
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPointsF <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
   
  .plotYprF( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinF,simMaxF),yLim=c(simMinYPR,simMaxYPR) ) )
  .plotSsbPerRecF( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinF,simMaxF),yLim=c(simMinSSBR,simMaxSSBR) ) )
  .plotYieldF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinF,simMaxF),yLim=c(simMinYield,simMaxYield) ) )
  .plotSsbF( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinF,simMaxF),yLim=c(simMinSSB,simMaxSSB) ) )  
  .plotRecSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinSSB,simMaxSSB),yLim=c(simMinRec,simMaxRec) ) )
  .plotYieldSSB( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=c(simMinSSB,simMaxSSB),yLim=c(simMinYield,simMaxYield) ) )

  mtext( side=3, line=0, cex=.CEXLAB2, outer=TRUE, "Reference Points" )
}

# U-based reference point plots.

.addRefPointsLegendU <- function( x=0.5, y=0.5, checked )
{
  # Is there anything in the legend, or is nothing checked?
  if ( sum(checked)==0 )
  {
    cat( "\nmseRrefPoints (.addRefPointsLegend): Legend vector of length 0.\n" )
    return()
  }
  
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
  panLegend( x, y, legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=.CEXSYM8, cex=.CEXAXIS2 )
}     # function .addRefPointsLegendU

.plotRecSSBU <- function( obj, gfx )
{
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
  xLim     <- gfx$xLim
  yLim     <- gfx$yLim

  xRange <- range( c(0,max(obj$ssb)) )
  if ( setXaxis )
    xRange <- xLim  

  yRange <- range( c(0,max(obj$recruits)) )
  if ( setYaxis )
    yRange <- yLim
  
  # Plot recruits against spawning stock biomass.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( .posVal(obj$ssb), .posVal(obj$recruits), lwd=2 )
    
  # Adding steepness lines at B20=0.2*B0, R20, 
  # and steepness R20/B0.
  lines( c(obj$B20,obj$B20), c(0,      obj$R20), lty=2 )
  lines( c(0,      obj$B20), c(obj$R20,obj$R20), lty=2 )
  
  lines( c(obj$B0,obj$B0), c(0,     obj$R0), lty=2 )
  lines( c(0,     obj$B0), c(obj$R0,obj$R0), lty=2 )
  
  # Adding steepness label
  h <- round( obj$R20/obj$R0, digits=2 )
  xPos <- obj$B20
  yPos <- obj$R20 * 0.8
  text( xPos, yPos, cex=1.2, pos=4, paste( "h=",obj$rSteepness,sep="") )
  
  if ( checked["U0"] )  
    points( obj$ssbF0,   obj$recruitsF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$ssbF01,  obj$recruitsF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$ssbFcra, obj$recruitsFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$ssbFmax, obj$recruitsFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    points( obj$ssbFmsy, obj$recruitsFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["U40"] )
    points( obj$ssbF40,  obj$recruitsF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )      
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Recruits" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$recruits)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]    
    
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
      points( refPts$ssbF0, refPts$recruitsF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$ssbF01, refPts$recruitsF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$ssbFcra, refPts$recruitsFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$ssbFmax, refPts$recruitsFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$ssbF40,  refPts$recruitsF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      points( refPts$ssbFmsy, refPts$recruitsFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( refPts$ssbFmsy, refPts$recruitsFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Recruits" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.6, y=0.4, checked )
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
    points( obj$U0,   obj$ssbF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$ssbF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$ssbFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$ssbFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (09-Dec-10) Changed to legalHRFmsy
    #points( obj$Umsy, obj$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )        
  if ( checked["U40"] )
    points( obj$U40,  obj$ssbF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$ssb)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    #refPts <- obj[[i]]$rep$refPoints
    refPts <- obj[[i]]        
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$ssb), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$ssbF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01, refPts$ssbF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$ssbFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$ssbFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40, refPts$ssbF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$ssbFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        text( refPts$legalHRFmsy, refPts$ssbFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Legal HR" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "SSB" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
}

.plotSsbPerRecU <- function( obj, gfx )
{
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
    points( obj$U0,   obj$ssbprF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$ssbprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$ssbprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$ssbprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
    #points( obj$Umsy, obj$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$ssbprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )       
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "SSB per Recruit" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$ssbpr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]
        
    lines( .posVal(refPts$legalHR), .posVal(refPts$ssbpr), lwd=2 )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$ssbprF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$ssbprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$ssbprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$ssbprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$ssbprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$ssbprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        text( refPts$legalHRFmsy, refPts$ssbprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Legal HR" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "SSB per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
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
    points( obj$U0,   obj$yieldF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
    #points( obj$Umsy, obj$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )    

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$Ucra)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$yieldFmsy)) )
  }
   
  # Plot yield against Legal HR
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]
        
    lines( .posVal(refPts$legalHR), .posVal(refPts$landed), lwd=2 )
    
    if ( checked["U0"] )  
      points( refPts$U0, refPts$yieldF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        text( refPts$legalHRFmsy, refPts$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Legal HR" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked )
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
    points( obj$ssbF0,   obj$yieldF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$ssbF01,  obj$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$ssbFcra, obj$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$ssbFmax, obj$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    points( obj$ssbFmsy, obj$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
  if ( checked["U40"] )
    points( obj$ssbF40,  obj$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )
   
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Landed Yield" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$ssb)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$landed)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
    
  for ( i in 1:n )
  {
    refPts <- obj[[i]]        
    
    lines( .posVal(refPts$ssb), .posVal(refPts$landed), lwd=2 )
#      lines( .posVal(obj$F), .posVal(obj$discarded), lwd=2, lty="dashed" )
    
    if ( checked["U0"] )  
      points( refPts$ssbF0, refPts$yieldF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$ssbF01,  refPts$yieldF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$ssbFcra, refPts$yieldFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$ssbFmax, refPts$yieldFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$ssbF40,  refPts$yieldF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      points( refPts$ssbFmsy, refPts$yieldFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      if ( annotate )
        text( refPts$ssbFmsy, refPts$yieldFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "SSB" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Landed Yield" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.95, checked )
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
    points( obj$U0,   obj$yprF0,   cex=.CEXSYM8, bg=.REFCOLF0,   pch=21 )
  if ( checked["U01"] )
    points( obj$U01,  obj$yprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
  if ( checked["Ucra"] )
    points( obj$Ucra, obj$yprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
  if ( checked["Umax"] )
    points( obj$Umax, obj$yprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )
  if ( checked["Umsy"] )
    # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
    #points( obj$Umsy, obj$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
    points( obj$legalHRFmsy, obj$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )    
  if ( checked["U40"] )
    points( obj$U40,  obj$yprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )    
    
  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "U" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Yield per Recruit" )
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
    xRange <- c( 0,max(xRange[2],max(obj[[i]]$legalHR)) )
    yRange <- c( 0,max(yRange[2],max(obj[[i]]$ypr)) )
  }
    
  # Plot yield against fishing mortality.
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )

  for ( i in 1:n )
  {
    refPts <- obj[[i]]        
    
    lines( .posVal(refPts$legalHR), .posVal(refPts$yprL), lwd=2 )

    if ( checked["U0"] )  
      points( refPts$U0, refPts$yprF0, cex=.CEXSYM8, bg=.REFCOLF0, pch=21 )
    if ( checked["U01"] )
      points( refPts$U01,  refPts$yprF01,  cex=.CEXSYM8, bg=.REFCOLF01,  pch=21 )    
    if ( checked["Ucra"] )
      points( refPts$Ucra, refPts$yprFcra, cex=.CEXSYM8, bg=.REFCOLFCRA, pch=21 )
    if ( checked["Umax"] )
      points( refPts$Umax, refPts$yprFmax, cex=.CEXSYM8, bg=.REFCOLFMAX, pch=21 )

    if ( checked["U40"] )
      points( refPts$U40,  refPts$yprF40,  cex=.CEXSYM8, bg=.REFCOLF40,  pch=21 )        
      
    if ( checked["Umsy"] )
    {
      # ARK (11-Dec-10) Changed Umsy to legalHRFmsy.
      #points( refPts$Umsy, refPts$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )
      points( refPts$legalHRFmsy, refPts$yprFmsy, cex=.CEXSYM8, bg=.REFCOLFMSY, pch=21 )      
      if ( annotate )
        text( refPts$legalHRFmsy, refPts$yprFmsy, idNum[i], cex=0.8 )
    }      
  }

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "U" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Yield Per Recruit" )
  box()  
  
  if ( gfx$doLegend )
    .addRefPointsLegendU( x=0.7, y=0.4, checked )
}

# .plotRefPointsU  (Plot fishery reference points for the operating model, but on
#               the scale of harvest rates (for sablefOpMod legal HR).
# Purpose:      Plot the reference points for the operating model returned by
#               calcReferencePoints (mseRrefPoint_funs.r).
# Parameters:   obj - the list objected returned by calcReferencePoints.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund    
.plotRefPointsU <- function( obj, gfx=list( annotate=TRUE, checked=checked,
                    doLegend=TRUE, setXaxis=FALSE, setYaxis=FALSE ) )
{
  win <- .getWinName()
  guiInfo <- getWinVal( scope="L",winName=win )
  
  annotate <- gfx$annotate
  checked  <- gfx$checked
  setXaxis <- gfx$setXaxis
  setYaxis <- gfx$setYaxis
   
  .plotYprU( obj,gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )
  .plotSsbPerRecU( obj, gfx=list( annotate=annotate,checked=checked,
                 doLegend=gfx$doLegend,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )
  .plotYieldU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )
  .plotSsbU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )  
  .plotRecSSBU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )
  .plotYieldSSBU( obj, gfx=list( annotate=FALSE,checked=checked, doLegend=FALSE,
                 setXaxis=setXaxis, setYaxis=setYaxis,
                 xLim=NULL, yLim=NULL ) )

  mtext( side=3, line=0, cex=.CEXTITLE2, outer=TRUE, "Reference Points" )
}          

#------------------------------------------------------------------------------#
#-- Plotting Functions: Selectivity                                          --#
#------------------------------------------------------------------------------#

.plotSalg <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL) )
{
  Salg    <- obj$Salg
  dimSalg <- dim( Salg )
  nAges   <- dimSalg[1]
  nGear   <- dimSalg[3]
  nGrps   <- dimSalg[2]
  
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
      lines( c(1:nAges),Salg[,i,g], lty=1 )
      
    panLab( 0.025,0.90, adj=0, cex=1.2, obj$gNames[g] )  
        
    axis( side=1, cex.axis=.REFCAX )
    axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
    box()
  }
  
  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Age" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Selectivity" )
}     # End function .plotSalg


.plotSlgOLD <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE, bygear=TRUE,
                           doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  if ( is.null(gfx$xLim) )
    xRange <- c( obj$L1,trunc(max(obj$Lal)) )
  else
    xRange <- gfx$xLim
    
  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim
  
  nGear   <- obj$nGear
  L50Cg1  <- obj$L50Cg1
  L50Cg2  <- obj$L50Cg2
  L95Cg1  <- obj$L95Cg1
  L95Cg2  <- obj$L95Cg2
  sizeLim <- obj$sizeLim
  
  len <- seq( xRange[1],xRange[2],0.25 )

  Slg <- matrix( NA, nrow=length(len), ncol=nGear )
  for( g in 1:nGear )
  {
    tmp1 <- exp( (-1.)*log(19)*(len-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
    tmp2 <- exp( (-1.)*log(19)*(len-L50Cg2[g])/(L95Cg2[g] - L50Cg2[g]) )
    tmpS <- (1./(1.+tmp1))*(1./(1.+tmp2))
    Slg[,g] <- tmpS/max( tmpS )
  }
  
  # Separate plot for each gear type.
  if ( gfx$bygear )
  {
#    # HACK: ARK 25-Nov-09
#    if ( nGear==2 )
#      mfRow <- c(2,1)
#    else if ( nGear==3 )
#      mfRow <- c(3,1)
#    else
#      mfRow <- c(2,2)
  
#    par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    
    for ( g in 1:nGear  )
    {
      plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
      lines( len,Slg[,g], lty=.LTYSlg[g], lwd=.LWDSlg[g] )

      if ( gfx$annotate )
        abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )
    
      panLab( 0.05,0.90, adj=0, cex=1.3, obj$gNames[g] )
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
    }
  }
  # Single plot with gear overlay.
  else
  {
 #   par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
    plot( xRange, yRange, type="n",axes=F,xlab="",ylab="" )
    for ( g in 1:nGear  )
      lines( len,Slg[,g], col=.COLSlg[g], lty=.LTYSlg[g], lwd=.LWDSlg[g] )

    if ( gfx$annotate )
      abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )

    axis( side=1, cex.axis=.REFCAX )
    axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
    box()
      
    if ( gfx$doLegend )
    {
      panLegend( 0.8,0.35, legTxt=obj$gNames, bg="white",
                 col=.COLSlg, lty=.LTYSlg, lwd=.LWDSlg, pt.cex=1.2 )    
    }
  }
  
  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Length" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Selectivity" )
}


.plotSlg <- function( obj, label=NULL,
   gfx=list( autolayout=TRUE, annotate=TRUE, bygear=TRUE,
             doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  if ( is.null(gfx$xLim) )
    xRange <- c( obj$L1,trunc(max(obj$Lal)) )
  else
    xRange <- gfx$xLim
    
  if ( is.null(gfx$yLim) )
    yRange <- c(0,1)
  else
    yRange <- gfx$yLim
  
  nGear   <- obj$nGear
  
  # Initial values.
  L50Cg1  <- obj$L50Cg1
  L50Cg2  <- obj$L50Cg2
  L95Cg1  <- obj$L95Cg1
  L95Cg2  <- obj$L95Cg2
  
  sizeLim <- obj$sizeLim
  selType <- obj$selType
  
  cat( "\nMSG (.plotSlg) Selectivity parameters:\n" )
  cat( "\nL50Cg1: ",L50Cg1,"\n" )
  cat( "\nL95Cg1: ",L95Cg1,"\n" )
  cat( "\nL95Cg2: ",L95Cg2,"\n" )    
  cat( "\nL50Cg2: ",L50Cg2,"\n" )
  
  if ( !is.null(obj$L50_1_post) )
  {
    cat( "\nMSG (.plotSlg) POST selectivity parameters (plotted):\n" )
    cat( "\nL50_1_post: ",obj$L50_1_post,"\n" )
    cat( "\nL95_1_post: ",obj$L95_1_post,"\n" )  
    cat( "\nL50_2_post: ",obj$L50_2_post,"\n" )
    cat( "\nL95_1_post: ",obj$L95_2_post,"\n" )  
  }  
  
  seriesNames <- paste( "Series",c(1:nGear) )
  gNames      <- obj$gNames  

  xRange[1] <- 0.
  xRange[2] <- 270
  len <- seq( xRange[1],xRange[2],0.25 )
  #len <- seq( 0,270,0.25 )

  Slg <- matrix( NA, nrow=length(len), ncol=nGear )
  
  for( g in 1:nGear )
  {
    if ( selType[g]==1 )     # use asymptotic function
    { 
      tmp1 <- exp( (-1.)*log(19.0)*(len-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
      Slg[,g] = 1./(1.+tmp1)
    }        
  
    if ( selType[g]==2 )     # use dome-shaped function 
    { 
      tmp1 <- exp( (-1.)*log(19.0)*(len-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
      tmp2 <- exp( (-1.)*log(19.0)*(len-L50Cg2[g])/(L95Cg2[g] - L50Cg2[g]) )
      Slg[,g] <- (1./(1.+tmp1)) * (1./(1.+tmp2) )
    }
    Slg[,g] <- Slg[,g]/max(Slg[,g])
  }

  trapPostSlg <- rep( NA, nrow=length(len) ) 
  if ( !is.null(obj$L50_1_post) )
  {
    g <- 1
    L50_1_post <- obj$L50_1_post
    L95_1_post <- obj$L95_1_post
    L50_2_post <- obj$L50_2_post
    L95_2_post <- obj$L95_2_post
    
    if ( selType[g]==1 )     # use asymptotic function
    { 
      tmp1 <- exp( (-1.)*log(19.0)*(len-L50_1_post)/(L95_1_post - L50_1_post) )
      trapPostSlg = 1./(1.+tmp1)
    }        
  
    if ( selType[1]==2 )     # use dome-shaped function 
    { 
      tmp1 <- exp( (-1.)*log(19.0)*(len-L50_1_post)/(L95_1_post - L50_1_post) )
      tmp2 <- exp( (-1.)*log(19.0)*(len-L50_2_post)/(L95_2_post - L50_2_post) )
      trapPostSlg <- (1./(1.+tmp1)) * (1./(1.+tmp2) )
    }
    trapPostSlg <- trapPostSlg/max(trapPostSlg)    
  }
 
  # Separate plot for each gear type.
  if ( gfx$bygear )
  {
    for ( g in 1:nGear  )
    {
      plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
      lines( len,Slg[,g], lty=.SlgLTY[g], lwd=.SlgLWD[g] )
      if ( !is.null(obj$L50_1_post) & g==1 )
        lines( len, trapPostSlg, col=.SlgCOL[g], lty=.SlgLTY, lwd=.SlgLWD[g] )
        
      if ( gfx$annotate )
        abline( v=sizeLim, col=.SizeLimCOL, lty=.SizeLimLTY, lwd=.SizeLimLWD )
    
      panLab( 0.05,0.90, adj=0, cex=1.5, gNames[g] )
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
      
      if ( gfx$annotate )
        if ( !is.null(label) & g==1 )
          mtext( side=3, line=0.5, cex=.CEXLAB, label )      
      
      if ( gfx$annotate )
      {
        rdig <- 5
        parVals <- paste( "L50Cg1  ",   format(L50Cg1[g],digits=rdig,width=6),
                          "   L95Cg1 ", format(L95Cg1[g],digits=rdig,width=6),
                          "\n",       
                          "L95Cg2  ", format(L95Cg2[g],digits=rdig,width=6),
                          "   L50Cg2  ",   format(L50Cg2[g],digits=rdig,width=6), sep="" )
                          
        if ( !is.null(obj$L50_1_post) )
          parVals <- paste( parVals, "\n\n",
                       "L50Cg1Post ", format(L50_1_post,digits=rdig,width=6),
                       "  L95Cg1Post ", format(L95_1_post,digits=rdig,width=6),
                       "\n",
                       "L95Cg2Post ", format(L95_2_post,digits=rdig,width=6),
                       "  L50Cg2Post ", format(L50_2_post,digits=rdig,width=6), sep="" )
        
        panLab( 0.025,0.6, adj=0, cex=1.1, parVals )
      }
    }
  }
  # Single plot with gear overlay.
  else
  {
    plot( xRange, yRange, type="n",axes=F,xlab="",ylab="" )
    for ( g in 1:nGear  )
    {
     lines( len,Slg[,g], col=.SlgCOL[g], lty=.SlgLTY[g], lwd=.SlgLWD[g] )
     if ( !is.null(obj$L50_1_post) & g==1 )
       lines( len, trapPostSlg, col=.SlgCOL[g], lty=2, lwd=.SlgLWD[g] )
    }

    if ( gfx$annotate )
        abline( v=sizeLim, col=.SizeLimCOL, lty=.SizeLimLTY, lwd=.SizeLimLWD )

    axis( side=1, cex.axis=.CEXAXIS)
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    if ( gfx$annotate )
      if ( !is.null(label) )
        panLab( 0.05,0.9, adj=0, cex=.CEXLAB, label )
      
    if ( gfx$doLegend )
    {
      panLegend( 0.025,0.6, legTxt=gNames, bg="white", cex=0.8,
                 col=.SlgCOL, lty=.SlgLTY, lwd=.SlgLWD, pt.cex=1.2 )    
    }
  }
  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Length" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Selectivity" )
}     # .plotSlg function

#------------------------------------------------------------------------------#
#-- Plotting Functions: Gear Proportion Discarded and Mortality Rates.       --#
#------------------------------------------------------------------------------#

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
  
  # Vector for plotting range of lengths.  
  len <- seq( xRange[1],xRange[2],0.25 )

  Plg <- matrix( NA, nrow=length(len), ncol=nGear )
  for( g in 1:nGear )
  {
    tmp <- exp( (-1.)*log(19)*(len-L50Dg[g])/(L95Dg[g] - L50Dg[g]) )
    tmpP <- (1./(1.+ tmp))
    tmpP <- ifelse( len < sizeLim[g], 1.0, tmpP )
    Plg[,g] <- tmpP
  }
 
  # Separate plot for each gear type.
  if ( gfx$bygear )
  {
    mfRow <- .getGearRowCol( nGear )
    par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    
    for ( g in 1:nGear  )
    {
      plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
      lines( len,Plg[,g], lty=.PlgLTY[g], lwd=.PlgLWD[g] )
      
      #if ( gfx$annotate )
      abline( v=sizeLim, col=.SizeLimCOL, lty=.SizeLimLTY, lwd=.SizeLimLWD )
    
      if ( gfx$annotate )
        panLab( 0.6,0.90, adj=0, cex=1.2,
                paste( obj$gNames[g],"(dg =",round( obj$dg[g],digits=3 ),")"  ) )
 
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
    }
  }
  # Single plot with gear overlay.
  else
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
    plot( xRange, yRange, type="n",axes=F,xlab="",ylab="" )
    for ( g in 1:nGear  )
      lines( len,Plg[,g], col=.PlgCOL[g], lty=.PlgLTY[g], lwd=.PlgLWD[g] )

    if ( gfx$annotate )
      abline( v=sizeLim, col=.SizeLimCOL, lty=.SizeLimLTY, lwd=.SizeLimLWD )
    
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
      
    if ( gfx$doLegend )
    {
      tmp <- paste( obj$gNames, "(dg =",round( obj$dg, digits=3 ),")" )
      panLegend( 0.6,0.95, legTxt=tmp, bg="white",
                 col=.PlgCOL, lty=.PlgLTY, lwd=.PlgLWD, pt.cex=1.2 )    
    }
  }

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Length" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Proportion Released at Length" )
}     # END function .plotPlg

.plotPalg <- function( obj, gfx=list( autolayout=TRUE, annotate=TRUE,
                            doLegend=TRUE, xLim=NULL, yLim=NULL) )
{
  Palg    <- obj$Palg
  dimPalg <- dim( Palg )
  nAges   <- dimPalg[1]
  nGear   <- dimPalg[3]
  nGrps   <- dimPalg[2]

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
      lines( c(1:nAges),Palg[,i,g], lty=1 )
      
    panLab( 0.025,0.90, adj=0, cex=1.3, obj$gNames[g] )  
        
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
  }
  
  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Age" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Proportion Released at Age" )
}     # END function .plotPalg

.plotDisMortRate <- function( obj, annotate=TRUE )
{
  # Discard mortality rates.

  nGear <- obj$nGear
  xRange <- range( 1:nGear )
  yRange <- c(0,1)
  
  plot( xRange,yRange, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:length(obj$dg)), obj$dg, type="h", lwd=10 )
  #points( c(1:length(obj$dg)), obj$dg, pch=16 )
  
  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames
    
  axis( side=1, at=c(1:nGear), labels=gearLabels, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Gear" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Release Mortality Rate" )
}

#------------------------------------------------------------------------------#
# Age Proportions                                                              #
#------------------------------------------------------------------------------#

.plotCatAgeBubbles <- function( obj, gearNames=NULL,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, xLim=NULL, yLim=NULL ) )
{
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c( 1,dim(obj)[2] )
  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c( 1,( dim(obj)[1]+2 ) )
  
  nGear <- dim( obj )[3]
  if ( gfx$bygears )
  {
    for ( i in 1:nGear )
    {
      dev.new()
      plotBubbles( obj[,,i], xlim=xLim, ylim=yLim )
      
      mtext( side=1, line=2, cex=1.0, "Year" )
      mtext( side=2, line=2, cex=1.0, "Age Class" )
      if ( !is.null(gearNames) )
        mtext( side=3, line=0.5, cex=1.0, gearNames[i] )
      else
        mtext( side=3, line=0.5, cex=1.0, paste( "Gear",i ) )
    }
  }
  else
  {
    par( mfrow=c( nGear,1 ) )
    # Object is a 3-dimensional array by age, time, gear for this replicate.
    for ( i in 1:nGear )
    {
      plotBubbles( obj[,,i], xlim=xLim, ylim=yLim )
       
      mtext( side=1, line=0, cex=1.0, outer=TRUE, "Year" )
      mtext( side=2, line=0, cex=1.0, outer=TRUE, "Age Class" )
     
      if ( !is.null(gearNames) )
        mtext( side=3, line=0.5, cex=1.0, gearNames[i] )
      else
        mtext( side=3, line=0.5, cex=1.0, paste( "Gear",i ) )
    }
  }
}

.plotCatAgeFreq <- function( obj, gearNames=NULL,
  gfx=list( annotate=TRUE, bygears=FALSE, doLegend=TRUE, showProj=FALSE,
            xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim
    
  pLim <- c( 0, 0.25 )
  
  yearClasses <- c( 1:dim(obj)[1] )
  
  nGear <- dim( obj )[3]
  if ( gfx$bygears )
  {
    if ( is.null(xLim) )
      xLim <- c( 1,dim(obj)[1] )
    
    if ( is.null(yLim) )
     yLim <- c( 1,dim(obj)[2] )  
  
    for ( i in 1:nGear )
    {
      years  <- c( yLim[1]:yLim[2] )
      nYears <- length( years )
      dev.new()
      
      
      myMar <- c(0.5,0.5,0,0)
      myOma <- c(3,4,4,3)
      
      if ( nYears <= 20 )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/4),4 ) )
      if ( (nYears > 20 ) && (nYears <= 50) )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/5),5 ) )
      if ( (nYears > 50 ) && (nYears <= 80 ) )
        par( oma=myOma, mar=myMar, mfcol=c( ceiling(nYears/8), 8) )
      if ( (nYears > 80 ) )
        par( oma=myOma, mar=myMar, mfcol=c (ceiling(nYears/10),10) )
 
      for ( t in years )
      {     
        plot( xLim, pLim, type="n", axes=FALSE, xlab="", ylab="" )
        lines( yearClasses, y=obj[,t,i], type="h", lwd=2 )
        
        if ( gfx$useYears )
          panLab( 0.8, 0.9, cex=1, t+.INITYEAR-1 )
        else
          panLab( 0.8, 0.9, cex=1, t )
        
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
      }

      mtext( side=1, line=1.25, cex=1.0, outer=TRUE, "Year" )
      mtext( side=2, line=2.25, cex=1.0, outer=TRUE, "Age Class" )
      if ( !is.null(gearNames) )
        mtext( side=3, line=2, cex=1, outer=TRUE, gearNames[i] )
      else
        mtext( side=3, line=2, cex=1, outer=TRUE, paste( "Gear",i ) )
    }
  }
  else
  {
    if ( is.null(xLim) )
      xLim <- c( 1,dim(obj)[1] )
      
    if ( is.null(yLim) )
      yLim <- c( 1,dim(obj)[2] )
    par( oma=c(3,3,2,2), mar=c(2,2,1,1), mfrow=c( 1, nGear ) )
    # Object is a 3-dimensional array by age, time, gear for this replicate.
   
   # E.g., if year range is 1:40, then years=c(1,2,...,40).
   # Then make the years negative to plot from -1 to -40.
    years  <- c( yLim[1]:yLim[2] ) * -1
    
    for ( i in 1:nGear )
    {
      plot( xLim, range(years), type="n", axes=FALSE, xlab="", ylab="",
            yaxs="i" )
      axis( side=1 )
      axis( side=2, at=pretty(years), labels=-1*pretty(years), cex.axis=1.2, las=2 )
      box()
      
      for ( t in years )
      {
        segments( x=yearClasses, y0=t, x1=yearClasses, y1=t+obj[,-t,i]*2.5, lwd=3 )
      
      #  segments( x0=yearClasses, y0=yLim[2]-t+1, x1=yearClasses,
      #            y1=yLim[2]-t+1+(obj[,t,i]*3), lwd=3 )
      }   # Over years.
      
      if ( !is.null(gearNames) )
        mtext( side=3, line=0.5, cex=1.0, gearNames[i] )
      else
        mtext( side=3, line=0.5, cex=1.0, paste( "Gear",i ) )
      
      mtext( side=1, line=1, cex=1.2, outer=TRUE, "Age Class" )
      mtext( side=2, line=1, cex=1.2, outer=TRUE, "Year" )
    }   # Over gears.
  }
}

#------------------------------------------------------------------------------#

.plotYieldByGear <- function( obj, gfx )
{
  annotate <- gfx$annotate
  checked <- gfx$checked
  if ( sum( gfx$checked ) > 1 || sum( gfx$checked)==0 )
  {
    for ( i in 1:length( checked ) )
      checked[i] <- FALSE
    checked["Umsy"] <- TRUE
    #Fval <- obj$Umsy
    Fval <- obj$legalHRFmsy
  }
 
  if ( checked["U0"] )
  {
    Fval <- obj$U0
    yprL <- obj$yprLF0
    yprD <- obj$yprDF0
    yieldL <- obj$recruitsF0*obj$yprLF0
    yieldD <- obj$recruitsF0*obj$yprDF0      
  }
  
  if ( checked["U01"] )
  {
    Fval   <- obj$U01
    yprL   <- obj$yprLF01
    yprD   <- obj$yprDF01
    yieldL <- obj$recruitsF01*obj$yprLF01
    yieldD <- obj$recruitsF01*obj$yprDF01
  }
      
  if ( checked["Ucra"] )
  {
    Fval   <- obj$Ucra
    yprL   <- obj$yprLFcra
    yprD   <- obj$yprDFcra
    yieldL <- obj$recruitsFcra*obj$yprLFcra
    yieldD <- obj$recruitsFcra*obj$yprDFcra
  }
  
  if ( checked["Umax"] )
  {
    Fval   <- obj$Umax
    yprL   <- obj$yprLFmax
    yprD   <- obj$yprDFmax
    yieldL <- obj$recruitsFmax*obj$yprLFmax
    yieldD <- obj$recruitsFmax*obj$yprDFmax
  }
  
  if ( checked["Umsy"] )
  {
    #Fval   <- obj$Umsy
    Fval   <- obj$legalHRFmsy
    yprL   <- obj$yprLFmsy
    yprD   <- obj$yprDFmsy
    yieldL <- obj$recruitsFmsy*obj$yprLFmsy
    yieldD <- obj$recruitsFmsy*obj$yprDFmsy
  }
  
  if ( checked["U40"] )
  {
    Fval   <- obj$U40
    yprL   <- obj$yprLF40
    yprD   <- obj$yprDF40
    yieldL <- obj$recruitsF40*obj$yprLF40
    yieldD <- obj$recruitsF40*obj$yprDF40
  }
  
  par( oma=c(2,3,1,1), mar=c(2,2,1,1), mfrow=c(2,1) )
  
  # Plot the yield per recruit for landed and discarded.
  nGear  <- obj$nGear
  xRange <- range((1-1):(nGear+1))
  yRange <- c(0, max(c(yprL,yprD)) )
  
  plot( xRange, yRange, type="n", axes=FALSE, xlab="", ylab="" )
  points( c(1:length(yprL)),yprL, cex=1.2, pch=16 )
  points( c(1:length(yprD)),yprD, cex=1.2, pch=1  )
  
  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames  
  axis( side=1, at=c(1:nGear),labels=gearLabels, cex.axis=.REFCEX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  
  if ( annotate )
  {
    panLegend( 0.6,0.8, legTxt=c("Landed","Released"),pch=c(16,1) )
  }
  
  box()
  mtext( side=2, line=3, cex=.REFCEX, "Yield Per Recruit" )
  
  # Plot the yield for landed and discarded.
  xRange <- range((1-1):(nGear+1))
  yRange <- c( 0,max( c(yieldL,yieldD) ) )
  
  plot( xRange, yRange, type="n", axes=FALSE, xlab="",ylab="" )
  points( c(1:length(yieldL)),yieldL, cex=1.2, pch=16 )
  points( c(1:length(yieldD)),yieldD, cex=1.2, pch=1  )
  
  if ( annotate )
  {
     text( c(1:length(yieldL)),yieldL, labels=round(yieldL,digits=3), cex=1.0, pos=4 )
     text( c(1:length(yieldD)),yieldD, labels=round(yieldD,digits=3), cex=1.0, pos=4 )
  }
  
  if ( annotate )
  {
    panLegend( 0.6,0.9,
      legTxt=paste( c(names(checked)[checked],"Landed","Released"),
             round( c(Fval,sum(yieldL),sum(yieldD)),digits=3) ) )
  }

  gearLabels <- c(1:nGear )
  if ( !is.null(obj$gNames) && length(obj$gNames)==nGear )
    gearLabels <- obj$gNames
    
  axis( side=1, at=c(1:nGear), labels=gearLabels, cex.axis=.REFCEX )
  axis( side=2, cex.axis=.REFCAX, las=.YAXISLAS )
  box()
  
  mtext( side=2, line=3, cex=.REFCEX, "Yield" )
  
  #mtext( side=1, line=0, cex=.REFCEX, outer=TRUE, "Gear" )
}

#------------------------------------------------------------------------------#
#-- Plotting Functions: Comparing simulations                                --#
#------------------------------------------------------------------------------#

.plotFvsh <- function( simObj )
{
  # This needs simObj as the entire simulation object.
  # Then, loop over simObj$om from 1,...,nSim.
  
  nSim    <- length( simObj )
  FmsyVec <- numeric( nSim )
  hVec    <- numeric( nSim )
  
  for ( i in 1:nSim )
  {
    FmsyVec[i] <- simTracker[[i]]$om$Fmsy
    hVec[i]    <- simTracker[[i]]$om$rSteepness
  }
  # Sort x=axis ascending.
  idx     <- order( hVec )
  FmsyVec <- FmsyVec[idx]
  hVec    <- hVec[idx]  

  plot( range( hVec ), range( FmsyVec ), type="n", axes=FALSE, xlab="", ylab="" )
  lines( hVec, FmsyVec, lty=1 )
  points( hVec, FmsyVec, cex=1.2, pch=16 )
  axis( side=1 )
  axis( side=2 )
  box()
  
  mtext( side=1, line=2, cex=1, "Steepness" )
  mtext( side=2, line=2, cex=1, "Fmsy" )
}

.plotYieldVsF <- function( simObj, refGear=3, gfx )
{
  # simObj is the entire simulation object.
  
  annotate <- gfx$annotate
  checked  <- gfx$checked

  nSim   <- length( simObj )
  nGear  <- simObj[[1]]$om$nGear
  gNames <- simObj[[1]]$om$gNames
  Fval   <- numeric( nSim )
  Fvec   <- numeric( nSim )
  yieldL <- numeric( nGear )
  yieldD <- numeric( nGear )
  
  landed  <- rep( 0, nSim )
  discard <- rep( 0, nSim )

#  for ( i in 1:nSim )
#  {
#    recruits <- trialObj[[i]]$recruitsFmsy
#    yprL     <- trialObj[[i]]$yprLFmsy
#    yprD     <- trialObj[[i]]$yprDFmsy
    
    # This is the yield of the killed landed and discarded fish.
#    yieldL <- recruits*yprL
#    yieldD <- recruits*yprD
#    Fvec[i]    <- trialObj[[i]]$fg[refGear]
    
#    print( rbind( yieldL, yieldD ) )
    
#    if ( i==1 )
#      result <- rbind( yieldL, yieldD )
#    else
#    {
#      tmp <- rbind( yieldL, yieldD )
#      result <- cbind( result, tmp )
#    }
#  }

  result <- matrix( NA, nrow=2, ncol=nGear*nSim )
  
  icol <- 0
  for ( g in 1:nGear )
  {
    for ( i in 1:nSim )
    {
      if ( sum( gfx$checked ) > 1 || sum( gfx$checked)==0 )
      {
        for ( iChk in 1:length( checked ) )
          checked[iChk] <- FALSE
        checked["Fmsy"] <- TRUE
      }
         
      icol <- icol + 1
      recruits <- simObj[[i]]$om$recruitsFmsy
      yprL     <- simObj[[i]]$om$yprLFmsy
      yprD     <- simObj[[i]]$om$yprDFmsy
 
      if ( checked["F0"] )
      {
        Fval[i]  <- simObj[[i]]$om$F0
        recruits <- simObj[[i]]$om$recruitsF0
        yprL     <- simObj[[i]]$om$yprLF0
        yprD     <- simObj[[i]]$om$yprDF0
      }
  
      if ( checked["F01"] )
      {
        Fval[i]  <- simObj[[i]]$om$F01
        recruits <- simObj[[i]]$om$recruitsF01
        yprL     <- simObj[[i]]$om$yprLF01
        yprD     <- simObj[[i]]$om$yprDF01

      }
      
      if ( checked["Fcra"] )
      {
        Fval[i]  <- simObj[[i]]$om$Fcra
        recruits <- simObj[[i]]$om$recruitsFcra
        yprL     <- simObj[[i]]$om$yprLFcra
        yprD     <- simObj[[i]]$om$yprDFcra

      }
  
      if ( checked["Fmax"] )
      {
        Fval[i]  <- simObj[[i]]$om$Fmax
        recruits <- simObj[[i]]$om$recruitsFmax
        yprL     <- simObj[[i]]$om$yprLFmax
        yprD     <- simObj[[i]]$om$yprDFmax

      }
  
      if ( checked["Fmsy"] )
      {
        Fval[i]  <- simObj[[i]]$om$Fmsy
        recruits <- simObj[[i]]$om$recruitsFmsy
        yprL     <- simObj[[i]]$om$yprLFmsy
        yprD     <- simObj[[i]]$om$yprDFmsy

      }
  
      if ( checked["F40"] )
      {
        Fval[i]  <- simObj[[i]]$om$F40
        recruits <- simObj[[i]]$om$recruitsF40
        yprL     <- simObj[[i]]$om$yprLF40
        yprD     <- simObj[[i]]$om$yprDF40
      }
    
      # This is the yield of the killed landed and discarded fish.
      yieldL[g] <- recruits*yprL[g]
      yieldD[g] <- recruits*yprD[g]
      Fvec[i]   <- simObj[[i]]$om$fg[refGear]
      
      result[ 1,icol ] <- yieldL[g]
      result[ 2,icol ] <- yieldD[g]
      
      landed[i]  <- landed[i]  + yieldL[g]
      discard[i] <- discard[i] + yieldD[g]
    }
  }
  
  barplot( height=result, beside=TRUE,
           space=c(0.1,0.5),
           names.arg=paste( rep( gNames, rep(nSim,nGear) ),
                            rep( c(1:nSim),nGear),sep=""),
           col=c("black","darkgray")  )
  
  if ( annotate )
  {
    Flabel <- paste( names(checked)[checked],c(1:nSim),"  ",round(Fval,digits=3),sep="" )
#    Llabel <- paste( "Landed",c(1:nSim)  round( sum(yieldL), digits=3 ), sep="" )
#    Dlabel <- paste( "Discard", round( sum(yieldD), digits=3 ), sep="" )

    panLegend( 0.3,0.95, legTxt=Flabel, bty="n", cex=1 )
    panLegend( 0.55,0.95, legTxt=paste( "Land",c(1:nSim)," ",
               round(landed,digits=3), sep="" ),
               cex=1, bty="n", pch=15, col="black" )
  
    panLegend( 0.75,0.95, legTxt=paste( "Disc",c(1:nSim)," ",
               round(discard,digits=3), sep="" ),
               cex=1, bty="n", pch=15, col="darkgray" )
  }  
  
  box()
  
  mtext( side=2, line=2.5, cex=1.4, "Yield" )
}

#-----------------------------------------------------------------------------##
#-- Plotting Functions (some HIDDEN, e.g., .foo)                            --##
#-----------------------------------------------------------------------------##

.plotBioCatF <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL ) )
{
  par( oma=.VIEWMOMA, mar=.VIEWMMAR, mfrow=c(3,1) )

  cax <- .VIEWCAX

  # Stock status zone boundaries on SSB scale.
  Bmsy      <- obj$pars$ssbFmsy

  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- apply( obj$om$Dtg,c(1,2),sum )[ iRep, ]  
  
  # Extract Ft for the 3 commercial gears.
  Ft <- obj$om$Ftg[ iRep,(1:ncol(obj$om$Ft)),c(1:3) ]
  #It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]

  nT  <- length( Bt )
  tMP <- obj$pars$tMP

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c(.VIEWXLIM[1],min(.VIEWXLIM[2],nT) )

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )
  #yAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
  #yLim <- c(0,.VIEWYMULT*mean(yAvg))
  
  if ( is.null(yLim2) )
    yLim2 <- range( c(0,Ct) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Ft) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, lwd=2 )
  #points( c(1:nT), It, pch=16 )
  abline( h=obj$pars$ssbFmsy, lty=.REFLTYFMSY )
  #abline( h=zoneLimit,        lty=.ZONELIMLTY )
  #abline( h=zoneUpper,        lty=.ZONEUPPLTY )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Biomass" )

  # Panel 2: Plot catch.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ct, col=.CtCOL, lty=.CtLTY, lwd=.CtLWD )
  points( c(1:nT), Ct, bg=.CtBG, cex=.CtCEX, col=.CtCOL, pch=.CtPCH )

  lines( c(1:nT), Dt, col=.DtCOL, lty=.DtLTY, lwd=.DtLWD )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX, col=.DtCOL, pch=.DtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Retained & released biomass" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.7,0.95, legTxt=c("Retained","Released"), bg="white", cex=1.2,
               pt.bg=c("white",.COLDt), lty=c(.LTYCt,.LTYDt), lwd=c(.LWDCt,.LWDDt),
               pt.cex=1.4, pch=c(.SYMCt,.SYMDt) )
  }
  mtext( side=1, line=0.5, cex=1.0, outer=TRUE, "Year" )

  # Panel 3: Plot fishing mortality by gear.

  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )

  for ( g in 1:ncol(Ft) )
    lines( c(1:nT), Ft[,g], col=.COLFtg[g], lty=.LTYFtg[g], lwd=.LWDFtg[g] )
    
  abline( h=obj$pars$Fmsy, lty=.REFLTYFMSY )
  abline( h=obj$pars$Fcra, lty=.REFLTYFCRA )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  
  if ( gfx$doLegend )
  {
    panLegend( 0.8,0.9, legTxt=obj$pars$gNames[1:3], cex=1.2,
      col=.COLFtg[1:3], lty=.LTYFtg[1:3], lwd=.LWDFtg[1:3] )
  }
  
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Fishing Mortality" )
}

.plotBioCatR <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=FALSE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL ) )
{
  par( oma=.VIEWMOMA, mar=.VIEWMMAR, mfrow=c(3,1) )

  cax <- .VIEWCAX

  # Stock status zone boundaries.
  Bmsy      <- obj$pars$ssbFmsy

  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- apply( obj$om$Dtg,c(1,2),sum )[ iRep, ]
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  #It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]

  nT  <- length( Bt )
  tMP <- obj$pars$tMP

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c(.VIEWXLIM[1],min(.VIEWXLIM[2],nT) )

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )
  #yAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
  #yLim <- c(0,.VIEWYMULT*mean(yAvg))
  
  if ( is.null(yLim2) )
    yLim2 <- range( c(0,Ct) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Rt) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  #points( c(1:nT), It, pch=16 )
  abline( h=obj$pars$ssbFmsy, lty=.REFLTYFMSY )
  #abline( h=zoneLimit,        lty=.ZONELIMLTY )
  #abline( h=zoneUpper,        lty=.ZONEUPPLTY )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD ) 

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Biomass" )

  # Panel 2: Plot catch.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ct, col=.COLCt, lty=.LTYCt, lwd=.LWDCt )
  points( c(1:nT), Ct, bg="white", cex=1.2, col=.COLCt, pch=.SYMCt )

  lines( c(1:nT), Dt, col=.COLDt, lty=.LTYDt, lwd=.LWDDt )
  points( c(1:nT), Dt, bg="white", cex=1.2, col=.COLDt, pch=.SYMDt )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  abline( h=obj$pars$yieldFmsy, lty=.REFLTYMSY )

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Retained & Releases" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.7,0.95, legTxt=c("Retained","Released"), bg="white", cex=1.2,
               pt.bg=c("white",.COLDt), lty=c(.LTYCt,.LTYDt), lwd=c(.LWDCt,.LWDDt),
               pt.cex=1.4, pch=c(.SYMCt,.SYMDt) )
  }
  mtext( side=1, line=0.5, cex=1.0, outer=TRUE, "Year" )

  # Panel 3: Plot recruits.
  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Rt, col=.COLRt, lty=.LTYRt, lwd=.LWDRt )
  points( c(1:nT), Rt, bg="white", cex=1.2, col=.COLRt, pch=.SYMRt )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD ) 

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Recruits" )
}


.plotBioHrRec <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
  doLegend=FALSE, showProj=FALSE, xLim=NULL, yLim1=NULL,yLim2=NULL,yLim3=NULL ) )
{
  par( oma=.VIEWMOMA, mar=.VIEWMMAR, mfrow=c(3,1) )

  cax <- .VIEWCAX

  # Stock status zone boundaries.
  Bmsy      <- obj$pars$ssbFmsy

  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  Ct <- obj$om$Ct[ irep,(2:ncol(obj$om$Ct)) ]
  Dt <- obj$om$Dt[ irep,(2:ncol(obj$om$Dt)) ]

  totCat <- Ct + Dt
  legalHR <- (Ct + Dt )/ Bt

  nCol <- dim( obj$om$legalHR )[2]
  legalHR    <- legalHR[ iRep,c(2:nCol) ]
  sublegalHR <- obj$om$sublegalHR[ iRep,c(2:nCol) ]
  #It <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]

  nT  <- length( Bt )
  tMP <- obj$pars$tMP

  xLim  <- gfx$xLim
  yLim1 <- gfx$yLim1
  yLim2 <- gfx$yLim2
  yLim3 <- gfx$yLim3

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c(.VIEWXLIM[1],min(.VIEWXLIM[2],nT) )

  # Y-axis limits.
  if ( is.null(yLim1) )
    yLim1 <- range( c(0,Bt),na.rm=TRUE )
  #yAvg <- apply( obj$om$Bt[,(2:ncol(obj$om$Bt))],1,mean )
  #yLim <- c(0,.VIEWYMULT*mean(yAvg))
  
  if ( is.null(yLim2) )
    yLim2 <- range( c(0,c(legalHR,sublegalHR) ) )
  
  if ( is.null(yLim3) )
    yLim3 <- range( c(0,Rt) )

  # Panel 1: Plot biomass and survey index.
  plot( xLim, yLim1, type="n", axes=FALSE, xlab="", ylab="" )
  lines( c(1:nT), Bt, col=.BtCOL, lty=.BtLTY, lwd=.BtLWD )
  #points( c(1:nT), It, pch=16 )
  abline( h=obj$pars$ssbFmsy, lty=.REFLTYFMSY )
  #abline( h=zoneLimit,        lty=.ZONELIMLTY )
  #abline( h=zoneUpper,        lty=.ZONEUPPLTY )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD ) 

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Biomass" )

  # Panel 2: Plot catch.

  # Get the yScale values.
  plot( xLim, yLim2, type="n", axes=FALSE, xlab="", ylab="" )
  
  lines( c(1:nT), legalHR,    col=.COLLEGALHR,  lty=.LTYLEGALHR,  lwd=.LWDLEGALHR )
  lines( c(1:nT), sublegalHR, col=.COLSUBLEGHR, lty=.LTYSUBLEGHR, lwd=.LWDSUBLEGHR )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  #abline( h=equilBmsy, lty=2, col="black", lwd=2 )  
  
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Harvest Rate" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.7,0.95, legTxt=c("Legal","Sub-legal"), cex=1.2,
      lty=c( .LTYLEGALHR, .LTYSUBLEGHR ), lwd=c(.LWDLEGALHR,.LWDSUBLEGHR ) )    
  }

  # Panel 3: Plot recruits.
  plot( xLim, yLim3, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Rt, col=.COLRt, lty=.LTYRt, lwd=.LWDRt )
  points( c(1:nT), Rt, bg="white", cex=1.2, col=.COLRt, pch=.SYMRt )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1, cex.axis=.CEXAXIS2 )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
  box()
  mtext( side=2, line=.INLINE2+0.5, cex=.CEXLAB, "Recruits" )
}

.plotBt <- function( obj, iSim=1, iRep=1,
                     gfx=list( annotate=TRUE, doLegend=TRUE,
                     showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  Btot      <- obj$om$Btot[ iRep,(2:ncol(obj$om$Btot)) ]
  Bt        <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  legalB    <- obj$om$legalB[ iRep,(2:ncol(obj$om$legalB)) ]
  sublegalB <- obj$om$sublegalB[ iRep,(2:ncol(obj$om$sublegalB)) ]
  nT        <- length( Bt )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits (show projection takes precedence over xScale.
  if ( gfx$showProj )
    xLim <- c( obj$opMod$tMP - 1,nT )
  else
  {
    # X-axis limits drawn from global default settings.
    if ( is.null( xLim ) )
      xLim <- c(1,nT)
  }

  # Y-axis limits.
  if ( is.null( yLim ) )
    yLim <- range( c( 0,Bt,Btot) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Btot,      col=.BtotCOL,    lty=.BtotLTY,    lwd=.BtotLWD  )
  lines( c(1:nT), Bt,        col=.BtCOL,      lty=.BtLTY,      lwd=.BtLWD    )
  lines( c(1:nT), legalB,    col=.BlegCOL,    lty=.BlegLTY,    lwd=.BlegLWD  )
  lines( c(1:nT), sublegalB, col=.BslegCOL,   lty=.BslegLTY,   lwd=.BslegLWD )
  
  abline( h=obj$refPts$ssbFmsy,    lty=.BmsyLTY )
  abline( v=obj$ctlList$opMod$tMP, lty=.tMPLTY )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Biomass" )

  if ( gfx$doLegend )
    panLegend( 0.55,0.95, legTxt=c("Total biomass","Spawning biomass","Legal biomass",
               "Sublegal biomass"),
               bg="white", col=c(.BtotCOL,.BtCOL,.BlegCOL,.BslegCOL),
               lty=c(.BtotLTY,.BtLTY,.BlegLTY,.BslegLTY),
               lwd=c(.BtotLWD,.BtLWD,.BlegLWD,.BslegLWD) )

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE2, "Biomass" )
}     # END function .plotBt     


.plotSSBIndex <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
   doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL ) )
{
  Bt  <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  Itg <- obj$mp$data$Itg[ iRep,(1:ncol(obj$mp$data$Itg)), ]

  dimItg <- dim( obj$mp$data$Itg )
  nGear  <- dimItg[3]
  nT     <- dimItg[2]

  # X-axis limits (show projection takes precedence over xSet.
  xLim <- gfx$xLim
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
  {
    if ( is.null(xLim) )
      # X-axis limits drawn from global default settings.
      xLim <- c(.VIEWXLIM[1],min(.VIEWXLIM[2],nT) )
  }
  
  for ( i in 1:nGear )
  {
    # Y-axis limits.
    yLim <- gfx$yLim
    if ( is.null(yLim) )
      yLimI <- range( c(0,Bt,Itg[,i]),na.rm=TRUE )  
      
    lmFit <- lm( Itg[,i]~Bt )

    plot( xLim, yLimI, type="n", axes=FALSE, xlab="", ylab="" )

    abline( a=0.0, b=1.0, lty=1, lwd=2 )
    abline( lmFit, lty=2, lwd=2 )

    points( Bt, Itg[,i], pch=1, cex=.VIEWCEX )

    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
  }

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Spawning Stock Biomass" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Survey" )

  if ( gfx$annotate )
    mtext( side=3, line=-0.5, cex=.CEXLAB, outer=TRUE, "Index vs. SSB" )
    
  return( invisible() )
}

.plotCt <-function( obj, iSim, iRep, gfx=list( annotate=TRUE, doLegend=TRUE,
                         showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  # Stock status zone boundaries.
  yieldFmsy  <- obj$refPtList$yieldFmsy

  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- apply( obj$om$Dtg,c(1,2),sum )[ iRep, ]

  nT  <- length( Ct )
  tMP <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( 0,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c(0,Ct) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT),  Ct, col=.CtCOL, lty=.CtLTY, lwd=.CtLWD )
  points( c(1:nT), Ct, bg=.CtBG, cex=1.2, col=.CtCOL, pch=.CtPCH )

  lines( c(1:nT), Dt, col=.DtCOL, lty=.DtLTY, lwd=.DtLWD )
  points( c(1:nT), Dt, bg=.DtBG, cex=.DtCEX, col=.DtCOL, pch=.DtPCH )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  if ( gfx$annotate )
    abline( h=obj$refPtList$yieldFmsy, lty=.MSYLTY )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()

  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Retained & Released Biomass" )  

  if ( gfx$doLegend )
  {
    panLegend( 0.65,0.95, legTxt=c("Retained","Released"), bg="white",
               pt.bg=c("white",.DtCOL), lty=c(.CtLTY,.DtLTY), lwd=c(.CtLWD,.DtLWD),
               pt.cex=1.2, pch=c(.CtPCH,.DtPCH) )
  }
  
  # Get the ACTUAL input catches if they exist.
  tmp <- obj$ctlList
  if ( !is.null( tmp$opModData$catch ) )
  {
    retained <- tmp$opModData$catch$landCatchMatrix
    actualCt <- apply( retained[,3:ncol(retained)],1,sum ) / 1000.0
    points( c(1:(tMP-1)), actualCt[1:(tMP-1)], bg="green", pch=21 )
  }
  
  # Get the ACTUAL input releases if they exist.
  if ( !is.null( tmp$opModData$catch ) )
  {
    released <- tmp$opModData$catch$relCatchMatrix
    actualRel <- apply( released[,3:ncol(released)],1,sum ) / 1000.0
    points( c(1:(tMP-1)), actualRel[1:(tMP-1)], bg="red", pch=21 )
  }  
  
  result <- cbind( tStep=c(1:nT), Year=c(.INITYEAR:(.INITYEAR+nT-1)), Ct=Ct, Dt=Dt )
  result
}   # END function .plotCt


.plotCtg <- function( obj, iSim=1,iRep=1, gfx=list( annotate=TRUE, bygears=FALSE,
              doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  # Ctg  = Catch in year t for gear g.

  dimCtg <- dim( obj$om$Ctg )
  nGear  <- dimCtg[3]
  nT     <- dimCtg[2]
  nRep   <- dimCtg[1]
  tMP    <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits (show projection takes precedence over xSet.
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
  {
    if ( is.null(xLim) )
      # X-axis limits drawn from global default settings.
      xLim <- c( 0,nT )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c(0,obj$om$Ctg),na.rm=TRUE )

  if ( gfx$bygears )
  {
    for ( g in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

      Ct <- obj$om$Ctg[iRep,,g]
      lines( c(1:nT), Ct, col=.CtgCOL[g], lty=.CtgLTY[g], lwd=.CtgLWD[g] )
      points( c(1:nT), Ct, bg="white", cex=.CtgCEX, pch=.CtgPCH[g] )
      
      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      
      
      panLab( 0.025,0.90, adj=0, cex=1.2, obj$ctlList$opMod$gNames[g] ) 
      
      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()    
    }
    mtext( side=1, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE,
           paste( .CtLAB,"(",.CtUNIT,")" ) )
  }
  else
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nGear )
    {
       Ct <- obj$om$Ctg[iRep,,g]
       lines( c(1:nT), Ct, col=.CtgCOL[g], lty=.CtgLTY[g], lwd=.CtgLWD[g] )
       points( c(1:nT), Ct, bg=.CtgBG[g], cex=.CtgCEX[g], pch=.CtgPCH[g] )
    }

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB2, paste( .CtLAB,"(",.CtUNIT,")" ) )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.55,0.95, legTxt=obj$ctlList$opMod$gNames, bg="white",
                 pt.bg="white", col=.CtgCOL, lty=.CtgLTY, lwd=.CtgLWD-1, pt.cex=.CtgCEX,
                 pch=.CtgPCH )
    }
  }
  
  result <- data.frame( matrix( NA, nrow=nT, ncol=nGear + 2 ) )
  names( result ) <- c( "tStep","Year",paste( "Retained",c(1:nGear),sep="" ) )
  result$tStep <- c( 1:nT )
  result$Year  <- c( .INITYEAR:(.INITYEAR+nT-1) )
  for ( g in 1:nGear )
    result[ ,g+2 ] <- obj$om$Ctg[iRep,,g]
    
  print( result )

  if ( gfx$annotate )
    mtext( side=3, line=-0.5, cex=.CEXLAB2, outer=TRUE, "Catch Biomass" )
}     # END function .plotCtg


.plotCtgDtg <- function( obj, iSim, iRep, gfx=list( annotate=TRUE,bygears=FALSE,
  doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL, useYears=TRUE ) )
{
  # Ctg  = Catch in year t for gear g.
  # Dtg  = Discards in year t for gear g.

  dimCtg <- dim( obj$om$Ctg )
  nGear  <- dimCtg[3]
  nT     <- dimCtg[2]
  nRep   <- dimCtg[1]
  tMP    <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits (show projection takes precedence over xSet.
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
  {
    if ( is.null(xLim) )
      xLim <- c( 0,nT )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c(0,obj$om$Ctg),na.rm=TRUE )

  if ( gfx$bygears )
  {
    # Loop over the gears.
    for ( g in 1:nGear )
    {
      # Catch.
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      Ct <- obj$om$Ctg[iRep,,g]
      lines( c(1:nT), Ct, col=.CtgCOL[g], lty=.CtgLTY[g], lwd=.CtgLWD[g] )
      points( c(1:nT), Ct, bg=.CtgBG[g], col=.CtgCOL[g], pch=.CtgPCH[g] )

      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      
      panLab( 0.025,0.9, adj=0, cex=1.2, obj$ctlList$opMod$gNames[g] )

      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, las=.YAXISLAS )
      box()

       # Discards.
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" ) 
      Dt <- obj$om$Dtg[iRep,,g]
      lines( c(1:nT), Dt, col=.DtgCOL[g], lty=.DtgLTY[g], lwd=.DtgLWD[g] )
      points( c(1:nT), Dt, bg=.DtgBG[g], col=.DtgCOL[g], pch=.DtgPCH[g] )

      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      
      panLab( 0.025,0.9, adj=0, cex=1.2, obj$ctlList$opMod$gNames[g] )
      
      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
    }

    mtext( side=1, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Retained" )
    mtext( side=4, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Released" )   
  }
  else
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nGear )
    {
       # Catch.
       Ct <- obj$om$Ctg[iRep,,g]
       lines( c(1:nT), Ct, col=.CtgCOL[g], lty=.CtgLTY[g], lwd=.CtgLWD[g] )
       points( c(1:nT), Ct, bg=.CtgBG[g], col=.CtgCOL[g], pch=.CtgPCH[g] )

       # Discards.
       Dt <- obj$om$Dtg[iRep,,g]
       lines( c(1:nT), Dt, col=.DtgCOL[g], lty=.DtgLTY[g], lwd=.DtgLWD[g] )
       points( c(1:nT), Dt, bg=.DtgBG[g], col=.DtgCOL[g], pch=.DtgPCH[g] )
    }

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Biomass" )
 
    if ( gfx$doLegend )
    {
      legLab <- c( paste( obj$ctlList$opMod$gNames,"Retained" ),
                   paste( obj$ctlList$opMod$gNames,"Released" ) )
      panLegend( 0.65,0.95, legTxt=legLab, bg="white",
                 col=c(.CtgCOL,.DtgCOL),
                 lty=c(.CtgLTY,.DtgLTY),
                 lwd=c(.CtgLWD,.DtgLWD),
                 pt.bg=c(.CtgBG,.DtgBG), pt.cex=1.2,
                 pch=c(.CtgPCH,.DtgPCH) )
    }
  }
  
  if ( gfx$annotate )
    mtext( side=3, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Biomass" )
}


.plotDtg <- function( obj, iSim=1,iRep=1, gfx=list( annotate=TRUE, bygears=FALSE, 
                           doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # Dtg  = Discard biomass in year t for gear g.

  dimDtg <- dim( obj$om$Dtg )
  nGear  <- dimDtg[3]
  nT     <- dimDtg[2]
  nRep   <- dimDtg[1]
  tMP    <- obj$ctlList$opMod$tMP

  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # X-axis limits (show projection takes precedence over xSet.
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
  {
    # X-axis limits drawn from global default settings.
    if ( is.null(xLim) )
      xLim <- c( 0,nT )
  }

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- range( c(0,obj$om$Dtg[iRep,,]),na.rm=TRUE )
 
  if ( gfx$bygears )
  {
    for ( g in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

      Dt <- obj$om$Dtg[iRep,,g]
      lines( c(1:nT), Dt, col=.DtgCOL[g], lty=.DtgLTY[g], lwd=.DtgLWD[g] )
      points( c(1:nT), Dt, bg=.DtgBG[g], cex=.DtgCEX[g], pch=.DtgPCH[g] )
      
      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      

      panLab( 0.025,0.90, adj=0, cex=1.2, obj$ctlList$opMod$gNames[g] ) 
      
      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()    
    }
    mtext( side=1, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Released Biomass" )  
  }
  else
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nGear )
    {
       Dt <- obj$om$Dtg[iRep,,g]
       lines( c(1:nT), Dt, col=.DtgCOL[g], lty=.DtgLTY[g], lwd=.DtgLWD[g] )
       points( c(1:nT), Dt, bg=.DtgBG[g], cex=.DtgCEX[g], col=.DtgCOL[g], pch=.DtgPCH[g] )
    }

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
 
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Released Biomass" )
    
    if ( gfx$doLegend )
    {
      panLegend( 0.65,0.95, legTxt=obj$ctlList$opMod$gNames, bg="white",
                 pt.bg="white", col=.DtgCOL, lty=.DtgLTY, lwd=.DtgLWD,
                 pt.cex=.DtgCEX, pch=.DtgPCH )
    }
  }
  if ( gfx$annotate )
    mtext( side=3, line=-0.5, cex=.CEXLAB2, outer=TRUE, "Released Biomass" )
    
  result <- data.frame( matrix( NA, nrow=nT, ncol=nGear + 2 ) )
  names( result ) <- c( "tStep","Year",paste( "Released",c(1:nGear),sep="" ) )
  result$tStep <- c( 1:nT )
  result$Year  <- c( .INITYEAR:(.INITYEAR+nT-1) )
  for ( g in 1:nGear )
    result[ ,g+2 ] <- obj$om$Dtg[iRep,,g]
    
  print( result )  
}     # END function .plotDtg


.plotFt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE, doLegend=TRUE,
                     bygears=FALSE, showProj=FALSE, xLim=NULL, yLim=NULL ) )
{
  # Ftg  = fishing mortality in year t for gear g.

  dimFtg <- dim( obj$om$Ftg )
  nGear  <- dimFtg[3]
  nT     <- dimFtg[2]
  nRep   <- dimFtg[1]
  tMP    <- obj$ctlList$opMod$tMP
  
  gNames <- obj$ctlList$opMod$gNames

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( tMP,nT )
  else
    if ( is.null(xLim) )
      xLim <- c(0,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c(0,max(obj$om$Ftg[iRep,,]))

  if ( gfx$bygears )
  {
    mfRow <- .getGearRowCol( nGear )  
    par( oma=c(3,4,1,1), mar=c(2,3,1,1), mfrow=mfRow )
      
    for ( g in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      Ft <- obj$om$Ftg[iRep,,g]
      lines( c(1:nT), Ft, col=.FtgCOL[g], lty=.FtgLTY[g], lwd=.FtgLWD[g] )
      points( c(1:nT), Ft, bg=.FtgBG, col=.FtgCOL[g], pch=.FtgPCH )

      if ( gfx$annotate )
      {
        abline( h=obj$refPtList$Fmsy, lty=.FmsyLTY )
        abline( h=obj$refPtList$Fcra, lty=.FcraLTY )
      }
      
      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      

      panLab( 0.1,0.9, cex=.CEXLAB, gNames[g] )

      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()

      if ( gfx$doLegend )
      {
        panLegend( 0.5,0.95, legTxt=c("Fmsy","Fcrash"), cex=1.5,
                   bg="white", lty=c(.FmsyLTY,.FcraLTY) )
      }                
      
      mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE2, cex=.CEXLAB2, outer=TRUE, "Fishing Mortality" )
    }
    if ( gfx$annotate )
      mtext( side=3, line=-0.5, cex=.CEXLAB, outer=TRUE, "Fishing Mortality" )
  }
  else
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
    
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
    for ( g in 1:nGear )
    {
      Ft <- obj$om$Ftg[iRep,,g]
      lines( c(1:nT), Ft, col=.FtgCOL[g], lty=.FtgLTY[g], lwd=.FtgLWD[g] )
      points( c(1:nT), Ft, bg="white", col=.FtgCOL[g], pch=.FtgPCH )
    }
    
    if ( gfx$annotate )
    {
      abline( h=obj$refPtList$Fmsy, lty=.FmsyLTY )
      abline( h=obj$refPtList$Fcra, lty=.FcraLTY )
    }

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )    

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Fishing Mortality" )
    
    if ( gfx$annotate )
      mtext( side=3, line=1, cex=.CEXLAB2, "Fishing Mortality" )
      
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=c("Fmsy","Fcrash"),
                 bg="white", lty=c(.FmsyLTY,.FcraLTY) )
    }          
  }
}     # .plotF function


.plotIndexSeries <- function( obj, gfx=list( annotate=TRUE, bygears=TRUE,
          doLegend=TRUE, showProj=FALSE, xLim=NULL, ylim=NULL, useYears=FALSE ) )
{
  # Strategy here is to fool .plotItg into thinking it has a blob although one
  # does not exist at guiSim stage - thus we need something analogous to 
  # blob$mp$data$Itg to pass to .plotItg.
  
  # Or, we pass the blob.
  
  nGear <- obj$opMod$nGear
  
  # Extract matrix of indices.
  
  idxIndex <- obj$mp$data$inputIndex$idxIndex
  
  tmp <- obj$mp$data$inputIndex$idxSeries
  Itg <- array( NA, dim=c(1,ncol(tmp),nGear) )
  
  for ( g in 1:length(idxIndex) )
    Itg[ 1,,idxIndex[g] ] <- tmp[g,]
  
  Itg[ Itg==-1 ] <- NA
  
  #iRep <- rep( 1, nrow(Itg) )
  #for ( g in 1:nGear )
  #  Itg[ 1,1,g ] <- iRep

  result <- list()
  result$mp$data$Itg <- Itg
  result$ctlList$opMod$tMP <- obj$opMod$tMP
  result$ctlList$opMod$gNames <- obj$opMod$gNames
  
  .plotItg( result, iSim=1, iRep=1, gfx=gfx )
   
}     # END function .plotIndexSeries.                      

.plotItg <- function( obj, iSim=1,iRep=1, gfx=list( annotate=TRUE, bygears=TRUE,
              doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ) )
{
  # Itg  = Index in year t for gear g.

  dimItg <- dim( obj$mp$data$Itg )
  nGear  <- dimItg[3]
  nT     <- dimItg[2]
  nRep   <- dimItg[1]
  tMP    <- obj$ctlList$opMod$tMP
  
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits (show projection takes precedence over xSet.
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
  {
    if ( is.null(xLim) )
       xLim <- c( 0,nT )
  }

  if ( gfx$bygears )
  {
    mfRow <- .getGearRowCol( nGear )
    par( oma=.OMA, mar=.MAR, mfrow=mfRow ) 
    
    for ( g in 1:nGear )
    {
      # Y-axis limits.
      if ( is.null(yLim) )
        yLimG <- range( c(0,obj$mp$data$Itg[iRep,,g]),na.rm=TRUE )    
        
      plot( xLim, yLimG, type="n", axes=FALSE, xlab="", ylab="" )

      It <- obj$mp$data$Itg[iRep,,g]
      lines( c(1:nT), It, col=.ItgCOL[g], lty=.ItgLTY[g], lwd=.ItgLWD[g] )
      points( c(1:nT), It, bg=.ItgBG[g], cex=.ItgCEX[g], pch=.ItgPCH[g] )
      
      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )      
      
      panLab( 0.025,0.90, adj=0, cex=.CEXLAB2, obj$ctlList$opMod$gNames[g] ) 
      
      .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()    
    }
    mtext( side=1, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Index" )
  }
  else
  {
    par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )   
  
    # Y-axis limits.
    if ( is.null(yLim) )
      yLim <- range( c(0,obj$mp$data$Itg[iRep,,]),na.rm=TRUE )
        
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    # Loop over the gears.
    for ( g in 1:nGear )
    {
       It <- obj$mp$data$Itg[iRep,,g]
       lines( c(1:nT), It, col=.ItgCOL[g], lty=.ItgLTY[g], lwd=.ItgLWD[g] )
       points( c(1:nT), It, bg=.ItgBG[g], cex=.ItgCEX[g], pch=.ItgPCH[g] )
    }
  
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Stock index" )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.05,0.95, legTxt=obj$ctlList$opMod$gNames, bg="white",
                 col=.ItgCOL, lty=.ItgLTY, lwd=.ItgLWD, pt.cex=.ItgCEX,
                 pch=.ItgPCH )
    }
  }

  if ( gfx$annotate )
    mtext( side=3, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Stock Index" )
}     # END function .plotItg


.plotNt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     doLegend=TRUE, showProj=FALSE, xLim=NULL, yLim=NULL,
                     useYears=FALSE ))
{
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  Ntot <- obj$om$Ntot[ iRep,(2:ncol(obj$om$Ntot)) ]
  Nt   <- obj$om$Nt[ iRep,(2:ncol(obj$om$Nt)) ]
  nT   <- length( Nt )
  tMP  <- obj$ctlList$opMod$tMP

  # X-axis limits.
  if ( gfx$showProj )
    xLim <- c( obj$ctlList$opMod$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( 0,nT )

  # Y-axis limits.
  if ( is.null(yLim) )
    yLim <- c( 0.0, max( Ntot) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  lines( c(1:nT), Ntot, col=.NtotCOL, lty=.NtotLTY, lwd=.NtotLWD )
  lines( c(1:nT), Nt,   col=.NtCOL,   lty=.NtLTY,   lwd=.NtLWD )
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=1, line=.INLINE1, cex=.CEXLAB2, "Year" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB2, "Numbers" )

  if ( gfx$doLegend )
    panLegend( 0.55,0.95, legTxt=c("Total numbers","Spawning numbers"),
               bg="white",
               col=c(.NtotCOL,.NtCOL), lty=c(.NtotLTY,.NtLTY),
               lwd=c(.NtotLWD,.NtLWD) )

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXTITLE2, "Numbers" )
}     # END function .plotNt


.plotObsSurvey <- function( obj, iSim=1, iRep=1, annotate=TRUE, showProj=FALSE,
                             yScale=FALSE )
{
  # Save par settings that can be restored (some are readonly).
  #oldpar <- par( no.readonly=TRUE )

  par( oma=.VIEWOMA, mar=.VIEWMAR, mfrow=c(1,1) )

  runMSElist <- .createList( obj$runMSEpar )

  # What stock assessment method?
  assessMethod <- runMSElist$mp$assess$assessMethod
  if ( assessMethod=="movinAvg" )
    assessMethod <- "MA"
  else if ( assessMethod=="kalman" )
    assessMethod <- "KF"

  omBt      <- obj$om$Bt
  omBt      <- omBt[ iRep, (2:ncol(omBt)) ]
  assessBt  <- obj$mp$assess$Bt
  assessBt  <- assessBt[ iRep,(2:ncol(assessBt)) ]
  It        <- obj$mp$data$It[ iRep,(2:ncol(obj$mp$data$It)) ]
  surveyCV1 <- obj$om$surveyCV[,"surveyCV1"]
  surveyCV2 <- obj$om$surveyCV[,"surveyCV2"]
  nT <- length( It )

  # X-axis limits.
  if ( showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    xLim <- c(.VIEWXLIM[1],min(.VIEWXLIM[2],nT) )
   # Y-axis limits.
  yLim <- range( c(0,It),na.rm=TRUE )
  if ( yScale )
  {
    yAvg <- apply( obj$mp$data$It[,(2:ncol(obj$mp$data$It))],1,mean,na.rm=TRUE )
    yLim <- c(0,.VIEWYMULT*mean(yAvg))
  }

  plot( xLim, yLim,type="n",axes=FALSE,xlab="",ylab="" )
  lines( c(1:nT), omBt, lty=1, lwd=3 )
  lines( c(1:nT), assessBt, col="red", lty=1 )

  # Uncomment this line if annual points on the fit are desired.
  #points( c(1:nT), assessBt, col="red", pch=16 )
  points( c(1:nT), It, pch=16 )
  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=1, line=2.5, cex=.VIEWCEX, "Year" )
  mtext( side=2, line=2.5, cex=.VIEWCEX, "Survey Index" )

  if ( annotate )
  {
    mtext( side=3, line=0, cex=.VIEWTEX, outer=TRUE, "SSB, Observed Survey, Fit" )
    panLegend( 0.05,0.95, adj=0,
      legTxt=c( paste("Old  Survey CV:",format(round(surveyCV1[iRep],digits=2),nsmall=2)),
                paste("New Survey CV:",format(round(surveyCV2[iRep],digits=2),nsmall=2)),
                "Spawning stock biomass",paste("Estimated SSB (",assessMethod,")",sep=""),
                "Observed survey biomass"),
      col=c("white","white","black","red","black"), lty=c(1,1,1,1,0),
      lwd=c(1,1,3,1,1), pch=c(16,16,NA,NA,16), bg="white" )
  }

  #par( oldpar )
}


.plotRecSpawners <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                              doLegend=TRUE, yLim=NULL, yLim=NULL ) )
{
  Rt <- obj$om$Rt[ iRep,(2:ncol(obj$om$Rt)) ]
  Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]

  # Lag the spawning biomass (Bt) relative to the recruits (R_t+1).
  Rt <- Rt[ 2:length(Rt) ]
  Bt <- Bt[ 1:(length(Bt)-1) ]
  nT <- length( Rt )

  tMP <- obj$pars$tMP

  # Use different symbols when t<(tMP-1).
  pchVec <- rep( 21,nT )
  pchVec[ 1:(tMP-2) ] <- 22

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( is.null(xLim) )
    xLim <- c( 0, max(Bt) )

  #yRange <- range( c( 0,Rt ) )
  if ( is.null(yLim) )
  {
    yAvg <- apply( obj$om$Rt[,(2:ncol(obj$om$Rt))],1,mean )
    yLim <- c(0,.VIEWYMULT*mean(yAvg))
  }

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )

  points( Bt, Rt, bg="white", cex=.VIEWCEX, pch=pchVec )

  #rp <- calcRefPoints( obj$pars )
  #lines( rp$ssb[rp$ssb >= 0.0], rp$recruits[rp$ssb>=0.0], lty=1, lwd=2 )
  
  iPosSSB <- obj$pars$ssb >= 0.0
  lines( obj$pars$ssb[iPosSSB], obj$pars$recruits[iPosSSB], lty=1, lwd=2 )
  
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Spawning Biomass (year t-1)" )
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Recruits Numbers (year t)" )

  if ( gfx$doLegend )
    panLegend( 0.05,0.95,legTxt=c("History","Projection"),pt.cex=1.2,pch=c(22,21))

  if ( gfx$annotate )
    mtext( side=3, line=0.5, cex=.CEXLAB, "Rec-Spawners" )
}

#------------------------------------------------------------------------------#
#-- Harvest Control Rule Plots                                                 #
#------------------------------------------------------------------------------#

#-- Harvest Control Rule plotting functions.                                 --#



# .plotHCRconstantC       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantC <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  constCatch <- obj$mp$hcr$constCatch
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Annual harvest rate.
  Bt <- seq( 0,B0, B0/10 )
  Ut <- constCatch / Bt
  
  # Annual instantaneous rate.
  Ft <- 1-exp(-Ut)
  lines( Bt, Ft, col="blue", lwd=3 )

  # Draw the selected intended removal rate curves.
  #abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )    

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantC

# .plotHCRconstantF       (Plot a harvest control rule for constant F)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR
.plotHCRconstantF <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{  
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  Fmsy   <- obj$refPts$Fmsy
  F01    <- obj$refPts$F01
  sprX   <- obj$mp$hcr$sprX
  FsprX  <- .getFxRefPtCalcs( obj$opMod, x=sprX )
  Finput <- obj$mp$hcr$inputF
  
  nameF <- c( "Fmsy","F01",paste( "Fspr",sprX,sep=""),"Finput" )
  valF  <- c(  Fmsy,  F01,                      FsprX,  Finput )

  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Set the reference removal rate.
  if ( obj$mp$hcr$remRefBase == "rrBaseFmsy" )
    remRefRate <- Fmsy
  else if ( obj$mp$hcr$remRefBase == "rrBaseF01" )
    remRefRate <- F01
  else if ( obj$mp$hcr$remRefBase == "rrBaseFspr" )
  {
    remRefRate <- FsprX
  }
  else if ( obj$mp$hcr$remRefBase == "rrBaseFinput" )
    remRefRate <- Finput
  
  # Plot harvest control rule.
  plot( c(0,B0),c(0,1.1*max(valF,na.rm=TRUE)), type="n", axes=FALSE,
        xaxs="i", xlab="", yaxs="i", ylab="" )
  
  usr <- par( "usr" )

  # Status-based rule control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB
  
  # Draw the options for HCRs.
  for ( i in 1:length(valF) )
  {
    rrr <- valF[ i ]
    abline( h=rrr, col="black", lty=1, lwd=1 )
  }

  # Draw the selected intended removal rate curves.
  abline( h=remRefRate, col="blue", lty=1, lwd=4 )

  # Plot Fmsy~Bmsy.
  points( upperCtlPt, Fmsy,   pch=.FmsyPCH, bg=.FmsyBG, cex=.CEXSYM20 )
  points( upperCtlPt, F01,    pch=.F01PCH,  bg=.F01BG,  cex=.CEXSYM20 )
  points( upperCtlPt, FsprX,  pch=.FsprPCH, bg=.FsprBG, cex=.CEXSYM20 )
  points( upperCtlPt, Finput, pch=.FcraPCH, bg=.FcraBG, cex=.CEXSYM20 )   

  axis( side=1, cex.axis=.CEXAXIS4 )
  axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=2.5, cex=.CEXLAB4, "(Estimated) Stock Status" )
  mtext( side=2, line=3.5, cex=.CEXLAB4, "(Estimated) Removal Rate" )

  if ( gfx$annotate )
  {
    #panLab( 0.8, 0.3, bquote( italic(B)["MSY"]==.(Bref) ) )
    #panLab( 0.8, 0.3, bquote( italic(B)["0"]==.(Bref) ) )    
    
    #panLab( 0.8, 0.45, bquote( italic(F)["removal"]==.(remRate) ) )
    #panLab( 0.8, 0.40, paste( "Risk adjusted =",obj$paAdj, sep="" ) )
    #if ( obj$paAdj )
    #{
    #  panLab( 0.8, 0.35, paste( "MCMC (thin) = ",obj$nMCMC," (",obj$nThin,")",
    #          sep="" ) )
    #}
    labels <- nameF
    pchVec <- rep( 21,length(labels) )
    names(pchVec) <- labels
  
    ptBg   <- c( .FmsyCOL, .F01COL,.FsprCOL,.FcraCOL )
    names(ptBg) <- labels
     
    # Add a legend.
    panLegend( 0.6, 0.3,legTxt=labels, pch=pchVec, pt.bg=ptBg, pt.cex=2,
               cex=1, bty=.LEGBTY )
  }
  return( invisible() )
}     # END function .plotHCRconstantF


# .plotHCRdeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRdeclineRisk <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE ) )
{
  # Three plot panels showing the increasing, stable, and decreasing acceptable
  # probability of decline curves.
 
  B0     <- obj$opMod$B0
  Bmsy   <- obj$refPts$ssbFmsy
  
  if ( obj$mp$hcr$statusBase == "statusBaseBmsy" )
    baseB <- Bmsy
  if ( obj$mp$hcr$statusBase == "statusBaseB0" )
    baseB <- B0

  # Harvest control points.
  upperCtlPt <- obj$mp$hcr$upperBoundMult * baseB
  lowerCtlPt <- obj$mp$hcr$lowerBoundMult * baseB

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr )
  cat( "\n(.plotHCRdeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    usr <- par( "usr" )
    
    for ( j in 1:3 )
      lines( x=c(usr[1],lowerCtlPt, lowerCtlPt,upperCtlPt, upperCtlPt,usr[2] ),
             y=pDecline[i,], lty=1,lwd=2 )
  
    # Indicate lower and upper harvest control rule points.
    abline( v=lowerCtlPt, col=.HCRLBCOL, lty=.HCRLBLTY, lwd=.HCRLBLWD )
    abline( v=upperCtlPt, col=.HCRUBCOL, lty=.HCRUBLTY, lwd=.HCRUBLWD )

    if ( gfx$annotate )
    {
      panLab( 0.8,0.5,  cex=1.4, trendLabel[i] )
      panLab( 0.8,0.4, cex=1.4, paste( "Trend Years = ", obj$mp$hcr$trendYears,
        " (",obj$mp$hcr$lowerTrendPct,", ",obj$mp$hcr$upperTrendPct,")", sep="" ) )
      panLab( 0.8,0.3, cex=1.4, paste( "Proj Years = ",  obj$mp$hcr$nProjYears,sep="" ) )
#      panLab( 0.8,0.2, cex=1.4,
#        paste( "MCMC (thin) = ",  obj$nMCMC, " (",obj$nThin,")",sep="" ) )
      
      # Acceptable probability of decline levels.
      offset <- 0.01
      panLab( offset + 0.0,            0.8, adj=0, pDecline[i,1] )
      panLab( lowerCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,2] )      
      panLab( offset+lowerCtlPt/B0,    0.8, adj=0, pDecline[i,3] )
      panLab( upperCtlPt/B0 - offset,  0.8, adj=1, pDecline[i,4] )
      panLab( offset+upperCtlPt/B0,    0.8, adj=0, pDecline[i,5] )
      panLab( 1 - offset,              0.8, adj=1, pDecline[i,6] )
    }

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0, B0/10.0 )
    axis( side=3, at=depletion, labels=depletion/B0 )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB4, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=0.5, cex=.CEXTITLE4, outer=TRUE, "Biomass" )
   
  return( invisible() )
}     # END function .plotHCRdeclineRisk

# .plotHCRviewDeclineRisk  (Plot the "9-zone" harvest control rule described in Table 1 
#                         of DFO (2009) Precautionary Harvest Strategy)
# Purpose:      Plot the harvest control rule, which is expressed as the maximum  
#               acceptable probability of future stock decline as a function of 
#               current stock status and recent trends in stock status. 
#               Boundaries of Critical, Cautious, and Healthy zones are shown.
#                Three different plots will be produced for three different 
#               recent trends in stock status: increasing, stable, decreasing.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       modified by K.Holt from A.R. Kronlund's .plotHCR  
.plotHCRviewDeclineRisk <- function( obj, iSim=1, iRep=1,
   gfx=list( annotate=TRUE, doLegend=TRUE, showProj=TRUE, xLim=NULL, yLim=NULL ) )
{
  # Six plot panels, with left column showing acceptable probability of decline
  # and right panels showing SSB_t for increasing, stable and decreasing recent
  # stock trends.
    
  #layout( matrix(c(1,2,3,4), 4, 1, byrow = TRUE), heights=c(1.5,2,2,2) )

  B0   <- obj$ctlList$opMod$B0
  Bmsy <- obj$refPtList$ssbFmsy
  
  # Always default to showing HCR based on "true" biomass ref points, regardless
  # of whether Bmsy or B0 is used as a base (K.Holt):
  if ( obj$mp$hcr$specs$statusBase == "statusBaseBmsy" )
    base <- Bmsy
  
  if ( obj$mp$hcr$specs$statusBase == "statusBaseB0" )
    base <- B0
  
  # If Fmsy is used for reference removal rate, show HCR based on equilibrium value:
  #if ( obj$mp$hcr$guiPars$remRefTyp=="Fmsy" )
  #  remRate <- obj$mp$specespars$Fmsy     # K.Holt
  
  # Rule control points on operating model scale.
  lowerB <- obj$mp$hcr$specs$lowerBoundMult * base  
  upperB <- obj$mp$hcr$specs$upperBoundMult * base

  # Get the acceptable probability of decline values.
  pDecline <- .getPdecline( obj$mp$hcr$specs )
  cat( "\n(.plotHCRviewDeclineRisk) Acceptable probabiity of decline:\n\n" )
  print( pDecline )

  #----------------------------------------------------------------------------#  
  # DFO (2009) 9 zone HCR (Table 1 of PA Harvest Strategy)                     #
  #----------------------------------------------------------------------------#

  trendLabel <- c( "Increasing Trend","Stable Trend","Decreasing Trend" )
  nT  <- obj$mp$hcr$specs$nT
  tMP <- obj$mp$hcr$specs$tMP

  colVec <- rev( heat.colors( nT-tMP+1 ) )

  for ( i in 1:length(trendLabel) )
  {
    plot( c(0,B0),c(0,1), type="n", axes=FALSE, xlab="",ylab="", xaxs="i",yaxs="i")
  
    mfg <- par( "mfg" )
    usr <- par( "usr" )
    
    # So called "true" line, may wish to omit.
    lines( x=c(usr[1],lowerB, lowerB,upperB, upperB,usr[2] ),
           y=pDecline[i,], lty=1,lwd=2 )
  
    lowerBound <- obj$mp$hcr$lowerBound
    lowerBound <- lowerBound[ iRep,c(2:ncol(lowerBound)) ]
    upperBound <- obj$mp$hcr$upperBound
    upperBound <- upperBound[ iRep,c(2:ncol(upperBound)) ]
    
    # Is the year in the current category of increasing, stable, decreasing?
    idx   <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE    
    
    # These are the lines for each update of control points.  But we should only
    # plot the lines corresponding to the increasing, decreasing, stable.
    for ( j in c(tMP:nT) )
    {
      # Is the trend in the current category?
      if ( idx[j] )
      {
        lines( x=c( usr[1], lowerBound[j], lowerBound[j],
                    upperBound[j], upperBound[j], usr[2] ),
               y=pDecline[i,], col="gray", lty=1, lwd=1 )
      }
    }
  
    # Indicate acceptability probability of decline control bounds.
    abline( v=lowerB, col=.HCRLBCOL, lty=.HCRLBLTY,  lwd=.HCRLBLWD )
    abline( v=upperB, col=.HCRLBCOL, lty=.HCRUBLTY,  lwd=.HCRUBLWD )
    abline( v=Bmsy,   col=.BmsyCOL,  lty=.BmsyLTY,   lwd=.BmsyLWD )    

    # Add points.
    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    pStar <- obj$mp$hcr$pStar[ iRep,2:(nT+1) ]
    
    points( ssb[idx], pStar[idx], bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    panLab( 0.7,0.1, cex=.CEXANNO2, trendLabel[i] )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    depletion <- seq( 0,B0,B0/10.0 )
    
    if ( mfg[1]==1 )
      axis( side=3, at=depletion, cex.axis=.CEXAXIS4, labels=depletion/B0 )
    else
      axis( side=3, at=depletion, labels=FALSE )
    
    if ( i==2 )
      mtext( side=2, line=3, cex=.CEXLAB2, "Acceptable Probability of Decline" )
    
    box()
  }
  mtext( side=1, line=3, cex=.CEXTITLE2, "Estimated Stock Status" )

  Bexp <- obj$om$Bexp[ iRep,(2:ncol(obj$om$Bexp)) ]
  Bt   <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ]
  nT   <- length( Bt )

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
 
  # Y-axis limits.
  if ( is.null(yLim) )
  {
    yLim <- c( 0,max( c(Bexp,Bt) ) )
  }

  for ( i in 1:length(trendLabel) )
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

    lines( c(1:nT), Bt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
    lines( c(1:nT), Bexp, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
    abline( h=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

    # Add slopes and points.
    idx <- obj$mp$assess$trendVal[ iRep,2:(nT+1) ]==i
    idx[ is.na(idx) ] <- FALSE

    ssb   <- obj$mp$assess$ssb[ iRep,2:(nT+1) ]
    trendBio <- obj$mp$assess$trendBio[ iRep, 2:(nT+1) ]
    maxSlope <- max( abs( trendBio ), na.rm=T )
    
    # Add slope lines.
    usr <- par( "usr" )
    yDelta <- usr[4] - usr[3]
    mult <- (0.4 * yDelta) / maxSlope

    # ARK (24-Mar-13) Removed "-1" multiplcation.
    #segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult*-1.0) ) 
    segments( c(1:nT)[idx],ssb[idx],c(1:nT)[idx],ssb[idx]+(trendBio[idx]*mult) )
    
    x <- c(1:nT)[idx]
    y <- ssb[idx]
    points( x, y, bg=colVec[idx[tMP:nT]], cex=.CEXSYM20, pch=21 )

    axis( side=1, cex.axis=.CEXAXIS4 )
    axis( side=2, cex.axis=.CEXAXIS4, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis (side=4, labels=FALSE )
    box()

    if ( gfx$doLegend )
    {
    }
  }
  
  mtext( side=1, line=.INLINE3, cex=.CEXLAB2, "Year" )
  mtext( side=4, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Spawning Stock Biomass" )

  if ( gfx$annotate )
  {
    methodLab     <- .METHODLAB[obj$ctlList$mp$assess$methodId]  
    nProjYears    <- obj$mp$hcr$specs$nProjYears
    trendYears    <- obj$mp$hcr$specs$trendYears
    upperTrendPct <- obj$mp$hcr$specs$upperTrendPct
    lowerTrendPct <- obj$mp$hcr$specs$lowerTrendPct
    
    label <- paste( "Decline Risk: ", methodLab,", Proj=",nProjYears,
                    " Trend=",trendYears," (",lowerTrendPct,",",upperTrendPct,
                    ")", sep="" )
    mtext( side=3, line=.OUTLINE, cex=.CEXTITLE4, outer=TRUE, label )
  }
     
  return( invisible() )
}     # END function .plotHCRviewDeclineRisk

# .plotMpHCR    (Plot the harvest control rule as per MP)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.
# Parameters:   obj - sim node.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotMpHCR <- function( ctl, refs, gfx=list( annotate=TRUE, doLegend=FALSE,
                xLim=NULL, yLim=NULL ) )
{
  B0    <- ctl$opMod$B0        # Operating model B0.
  Bmsy  <- refs$ssbFmsy        # Operating model Bmsy (target biomass).
  Fmsy  <- obj$om$Fmsy         # Operating model Fmsy.
  Umsy  <- obj$om$legalHRFmsy

  # Set the omFtarg and omBtarg - this is here in case B0 is base later...
  omBtarg <- Bmsy
  
  #omFtarg <- Fmsy
  omUtarg <- Umsy

  limitRefMult <- ctl$mp$hcr$limitBoundMult
  upperRefMult <- ctl$mp$hcr$upperBoundMult

  # Intended control poitns for HCR.
  mpUB <- upperRefMult * omBtarg
  mpLB <- limitRefMult * omBtarg

  # Get the x-axis.
  Bvec <- seq( 0, B0, B0/100.0 )
  
  # Calculate "True" removal rate based on biomass relative to ref pts
  tmp  <- omUtarg * ( Bvec - mpLRP )/( mpUSR - mpLRP )
  tmp[ tmp < 0 ] <- 0.0

  Uvec <- ifelse( Bvec < mpUSR, tmp, Umsy  )  

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  plot( Bvec, Uvec, type="n", axes=FALSE, xlab="",xlim=xLim,ylab="",ylim=yLim )

  # Get the plot region user coordinate limits.
  usr <- par( "usr" )

  segments(    0,        0,  mpLRP,       0, lty=1, lwd=3 )
  segments( mpLRP,       0,  mpUSR, omUtarg, lty=1, lwd=3 )
  segments( mpUSR, omUtarg, usr[2], omUtarg, lty=1, lwd=3 )

  # Indicate lRef, uRef, and Bmsy.
  abline( v=mpLRP,   lty=.HCRLRPLTY,  lwd=.HCRLRPLWD )
  abline( v=mpUSR,   lty=.HCRUSRLTY,  lwd=.HCRUSRLWD )
  abline( v=omBtarg, lty=.REFLTYBMSY, lwd=2 )

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, labels=depletion/B0 )
  box()

  mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Stock Status" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Removal Rate" )
  mtext( side=3, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Depletion" )

  if ( gfx$doLegend  )
  {

     legLabs <- c( paste( "(",format(round(mpLRP,2),digits=2,nsmall=2),") LRP",sep="" ),
                   paste( "(",format(round(mpUSR,2),digits=2,nsmall=2),") USR",sep="" ),
                   paste( "(",format(round(omBtarg,2),digits=2,nsmall=2),") Bref",sep="" ),
                   paste( "(",format(round(omUtarg,3),digits=3,nsmall=3),") Utarg",sep="") )
     panLegend( 0.6, 0.25, legTxt=legLabs,
                lty=c(.HCRLRPLTY,.HCRUSRLTY,.REFLTYBMSY,NA), bg="white" )
  }     # endif doLegend
  
  return( invisible() )
}     # .plotMpHCR


# .plotHerringHCR (Plot the HCR as per the Herring HCR)
# Purpose:      Plot the harvest control rule (true) and applied.
# Parameters:   obj - a blob.
# Returns:      NULL (invisibly).
# Source:       S.D.N. Johnsons
.plotHerringHCR <- function( obj, iRep, gfx = list( annotate=FALSE, doLegend=TRUE,
                                                    xLim=NULL, yLim=NULL ) )
{
  # Get ctlList and OM B0
  ctlList <- obj$ctlList
  B0      <- ctlList$opMod$B0
  omBtarg <- .75 * ctlList$opMod$B0
  targHR  <- ctlList$mp$hcr$targHRHerring
  Umsy    <- ctlList$refPts$Umsy

  # Pull nT and tMP
  nT      <- ctlList$opMod$nT
  tMP     <- ctlList$opMod$tMP

  # HCR control point multipliers. - need to leverage these for the 
  # plots without Perfect Info

  if( ctlList$mp$hcr$rule == "herring" )
  {
    if( ctlList$mp$hcr$herringCutoffType == "absolute" )
      trueLowerBound <- ctlList$mp$hcr$herringCutoffVal
    else trueLowerBound <- ctlList$mp$hcr$herringCutoffVal * B0
    
    trueUpperBound <-  trueLowerBound / (1 - targHR)  
  }
  if( ctlList$mp$hcr$rule == "linear" )
  {
    trueLowerBound <- ctlList$mp$hcr$lowerBoundMult * B0
    trueUpperBound <- ctlList$mp$hcr$upperBoundMult * B0
  }
  

  # We need the following quantities.
  # Estimate of Bmsy for each year (Bref)
  # Estimate of UlegHR for each year (Fref)
  # Adjusted removal rate for each year (precautionaryFt)
  idx <- c(tMP:nT)     # Get the projection period, history has no MP.
  

  Bref  <- obj$mp$hcr$Bref[ iRep,c(2:ncol(obj$mp$hcr$Bref)) ]
  Bref <- Bref[idx]
  Fref  <- obj$mp$hcr$Fref[ iRep,c(2:ncol(obj$mp$hcr$Fref)) ]
  Fref <- Fref[idx]

  Uref <- targHR

  spawnHR <- obj$om$spawnHR[ iRep, 1 + idx ]

  # Get annual estimates of SSB from the management procedure.
  mpdPars    <- obj$mp$assess$mpdPars
  projExpBio <- mpdPars[ mpdPars$iRep==iRep, "projExpBio" ]
  if( is.null(projExpBio) ) projExpBio <- obj$om$FBt[iRep, 1 + idx]

  # Get annual estimates of lower and upp er control points from the management procedure. 
  lowerBound <- obj$mp$hcr$lowerBound[ iRep,c(2:ncol(obj$mp$hcr$lowerBound)) ]
  lowerBound <- lowerBound[idx]
  upperBound <- obj$mp$hcr$upperBound[ iRep,c(2:ncol(obj$mp$hcr$upperBound)) ]
  upperBound <- upperBound[idx]


  # Get the x-axis as max of true and estimated.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c(0,max(Bref,B0,na.rm=TRUE))

  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c(0,1.2 * targHR)

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  # Get the plot region user coordinate limits.
  usr <- par( "usr" )


  # Indicate the "true" control points based on true Bref, Fref.
  if ( gfx$annotate )
  {
    abline( v=trueLowerBound, col=.HCRLBCOL, lty=.HCRLBLTY,  lwd=.HCRLBLWD )
    abline( v=trueUpperBound, col=.HCRUBCOL, lty=.HCRUBLTY,  lwd=.HCRUBLWD )
    abline( v=omBtarg,        col=.BmsyCOL,  lty=.BmsyLTY,   lwd=.BmsyLWD )
    
    # abline( h=Umsy,    col=.UmsyCOL, lty=.UmsyLTY, lwd=.UmsyLWD )

  }
  
  # Lay down heat colors for HCRs applied by management procedure.
  colVec <- rev( heat.colors( n=length(Bref) ) )
  if( all(!is.na(lowerBound)) & all(!is.na(upperBound)) )
  {
    for ( i in 1:length(colVec) )
    {
      segments( 0,                   0, lowerBound[i],       0, col="gray", lwd=1 )
      if( ctlList$mp$hcr$rule == "herring" )
      {
        x <- seq( lowerBound[i], upperBound[i], length = 100 )
        y <- x
        for( xidx in 1:length(x))
          y[xidx] <- min( targHR, (x[xidx] - lowerBound[i])/x[xidx])
        lines(x,y, col = "gray", lwd = 1)  
      }
      if( ctlList$mp$hcr$rule == "linear" )
        segments( x0 = lowerBound[i],  y0 = 0, x1 =  upperBound[i], y1 =   Uref[i], col="gray", lwd=1 )
      
      segments( upperBound[i], Uref[i],        usr[2], Uref[i], col="gray", lwd=1 )
    }
  }
  
  points( projExpBio, spawnHR, bg=colVec, cex=1.2, pch=21 )
  
  # Plot the "True" harvest control rule.
  segments(    0,      0,  trueLowerBound,     0, lty=1, lwd=3 )
  # Plot fixed escapement rule
  if( ctlList$mp$hcr$rule == "herring" )
  {
    x <- seq(trueLowerBound, trueUpperBound, length = 100)
    y <- x
    for( xidx in 1:length(x))
      y[xidx] <- min( targHR, (x[xidx] - trueLowerBound)/x[xidx] )
    lines(x,y, lty = 1, lwd = 3)  
  }
  if(ctlList$mp$hcr$rule == "linear" )
    segments( x0 = trueLowerBound, x1 = trueUpperBound, y0 = 0, y1 = targHR, lwd = 3 )
  
  segments( trueUpperBound, targHR, usr[2],  targHR, lty=1, lwd=3 )  

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  box()

  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Estimated Stock Status (000s t)" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Effective Harvest Rate" )

  if ( gfx$doLegend  )
  {
    if( ctlList$mp$hcr$herringCutoffType == "absolute" )
      legCutoff <- paste( trueLowerBound, " kt", sep = "" )
    else legCutoff <- paste( 100 * ctlList$mp$hcr$herringCutoffVal, "%", sep = "")
    specs <- obj$mp$hcr$specs
    # Display what type of rule this puppy actually thinks it is.
    panLegend( 0.4, 0.5,
      legTxt=c( paste("Rule: ",ctlList$mp$hcr$hcrType),
                paste("Cutoff: ", legCutoff),
                paste("Rem Ref Base: ",ctlList$mp$hcr$remRefBase),
                paste("Rem Ref Source: ",ctlList$mp$hcr$remRefSource),
                paste("Status Base: ",ctlList$mp$hcr$statusBase),
                paste("Status Source: ",ctlList$mp$hcr$statusSource),
                paste("Umsy: ",round(Umsy,digits=3))) )
  }
  
  return( invisible() )
}     # END function .plotPmodHCR                                                      


# .plotPmodHCR (Plot the harvest control rule as per MP)
# Purpose:      Plot the harvest control rule (true) and applied.
# Parameters:   obj - a blob.
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotPmodHCR <- function( obj, iSim, iRep, gfx=list( annotate=TRUE, doLegend=FALSE,
                  xLim=NULL, yLim=NULL ) )
{
  ctlList <- obj$ctlList
  B0      <- ctlList$opMod$B0          # Operating model B0
  Bmsy    <- obj$refPtList$ssbFmsy     # Operating model Bmsy
  Umsy    <- obj$refPtList$Umsy        # Operating model Umsy
  UlegMsy <- obj$refPtList$legalHRFmsy # Operating model legal HR at Fmsy.
  
  legalHR <- obj$refPtList$legalHR     # Operating model legalHR
 
  omBtarg <- Bmsy
  omUtarg <- UlegMsy
  
  nT      <- ctlList$opMod$nT
  tMP     <- ctlList$opMod$tMP
  
  # HCR control point multipliers.
  lowerBoundMult <- ctlList$mp$hcr$lowerBoundMult
  upperBoundMult <- ctlList$mp$hcr$upperBoundMult

  # We need the following quantities.
  # Estimate of Bmsy for each year (Bref)
  # Estimate of UlegHR for each year (Fref)
  # Adjusted removal rate for each year (precautionaryFt)
  
  idx <- c(tMP:nT)     # Get the projection period, history has no MP.
  
  Bref  <- obj$mp$hcr$Bref[ iRep,c(2:ncol(obj$mp$hcr$Bref)) ]
  Bref <- Bref[idx]
  Fref  <- obj$mp$hcr$Fref[ iRep,c(2:ncol(obj$mp$hcr$Fref)) ]
  Fref <- Fref[idx]

  #Ftarg <- obj$mp$hcr$targetF[ iRep,c(2:ncol(obj$mp$hcr$targetF)) ]
  #Ftarg <- Ftarg[idx]
    
  Fprec <- obj$mp$hcr$precautionaryFt[  iRep,c(2:ncol(obj$mp$hcr$precautionaryFt)) ]
  Fprec <- Fprec[idx]

  # Get annual estimates of SSB from the management procedure.
  mpdPars    <- obj$mp$assess$mpdPars
  projExpBio <- mpdPars[ mpdPars$iRep==iRep, "projExpBio" ]

  # Get annual estimates of lower and upper control points from the management procedure.  
  lowerBound <- obj$mp$hcr$lowerBound[ iRep,c(2:ncol(obj$mp$hcr$lowerBound)) ]
  lowerBound <- lowerBound[idx]
  upperBound <- obj$mp$hcr$upperBound[ iRep,c(2:ncol(obj$mp$hcr$upperBound)) ]
  upperBound <- upperBound[idx]

  # Get the x-axis as max of true and estimated.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- c(0,max(Bref,B0,na.rm=TRUE))

  yLim <- gfx$yLim
  if ( is.null(yLim) )
    yLim <- c(0,max(Fref,Fprec,Umsy,UlegMsy,na.rm=TRUE) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  # Get the plot region user coordinate limits.
  usr <- par( "usr" )

  # Indicate the "true" control points based on true Bref, Fref.
  if ( gfx$annotate )
  {
    abline( v=omBtarg * lowerBoundMult, col=.HCRLBCOL, lty=.HCRLBLTY,  lwd=.HCRLBLWD )
    abline( v=omBtarg * upperBoundMult, col=.HCRUBCOL, lty=.HCRUBLTY,  lwd=.HCRUBLWD )
    abline( v=omBtarg,                  col=.BmsyCOL,  lty=.BmsyLTY,   lwd=.BmsyLWD )
    
    abline( h=Umsy,    col=.UmsyCOL, lty=.UmsyLTY, lwd=.UmsyLWD )
    abline( h=UlegMsy, col=.UlegCOL, lty=.UlegLTY, lwd=.UlegLWD )
  }
  
  # Lay down heat colors for HCRs applied by management procedure.
  colVec <- rev( heat.colors( n=length(Bref) ) )
  for ( i in 1:length(colVec) )
  {
    segments( 0,                   0, lowerBound[i],       0, col="gray", lwd=1 )
    segments( lowerBound[i],       0, upperBound[i], Fref[i], col="gray", lwd=1 )
    segments( upperBound[i], Fref[i],        usr[2], Fref[i], col="gray", lwd=1 )
  }
  
  points( projExpBio, Fprec, bg=colVec, cex=1.2, pch=21 )
  
  # Plot the "True" harvest control rule.
  segments(    0,      0,  omBtarg * lowerBoundMult,     0, lty=1, lwd=3 )
  segments( omBtarg * lowerBoundMult,     0,  omBtarg * upperBoundMult,  UlegMsy, lty=1, lwd=3 )
  segments( omBtarg * upperBoundMult, UlegMsy, usr[2],   UlegMsy, lty=1, lwd=3 )  

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  box()

  mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Estimated Stock Status (000s t)" )
  mtext( side=2, line=.OUTLINE, cex=.CEXLAB2, outer=TRUE, "Estimated Harvest Rate" )

  if ( gfx$doLegend  )
  {
    specs <- obj$mp$hcr$specs
    # Display what type of rule this puppy actually thinks it is.
    panLegend( 0.4, 0.5,
      legTxt=c( paste("Rule: ",ctlList$mp$hcr$hcrType),
                paste("Lower Mult: ", lowerBoundMult),
                paste("Upper Mult: ", upperBoundMult),
                paste("Rem Ref Base: ",ctlList$mp$hcr$remRefBase),
                paste("Rem Ref Source: ",ctlList$mp$hcr$remRefSource),
                paste("Status Base: ",ctlList$mp$hcr$statusBase),
                paste("Status Source: ",ctlList$mp$hcr$statusSource),
                paste("Umsy: ",round(Umsy,digits=3)),
                paste("UlegMsy: ", round(UlegMsy,digits=3)),
                paste("Lambda1: ",ctlList$mp$hcr$lambda1 ) ) )
  }
  
  return( invisible() )
}     # END function .plotPmodHCR


# .plotOmPhase  (plot the intended harvest control rule)
# Purpose:      Plot the harvest control rule indicating the Critical, Cautious,
#               and Healthy zones, limit reference point, upper stock reference,
#               removal reference and Bsmy.  This plot shows the pairs of F
#               and SSB that actually occurred (i.e.,realized).  In contrast,
#               the plotViewTarHCR plot shows the target harvest control rule
#               managers were trying to achieve.
# Parameters:   obj - the parameter list from simGUI, colorZones TRUE means use
#               colors that mimic DFO (2006).
# Returns:      NULL (invisibly).
# Source:       A.R. Kronlund
.plotOmPhase <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                colorZones=FALSE, doLegend=TRUE, showProj=FALSE,
                xLim=NULL, yLim=NULL ) )
{
  ctlList <- obj$ctlList

  B0    <- ctlList$opMod$B0
  Bmsy  <- obj$refPtList$ssbFmsy
  Umsy  <- obj$refPtList$Umsy
  
  nT    <- ctlList$opMod$nT
  tMP   <- ctlList$opMod$tMP
  
  # Extract relevant reference point bases:
  if ( ctlList$mp$hcr$statusBase == "statusBaseBmsy" ) Bref <- Bmsy
  if ( ctlList$mp$hcr$statusBase == "statusBaseB0" )   Bref <- B0

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  if ( !gfx$showProj )
  {
    idx <- c( 1:nT )

    Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  
    # Get the harvest rate on legals and sublegals.
    nCol    <- dim( obj$om$legalHR )[2]
    legalHR <- obj$om$legalHR[ iRep,c(2:nCol) ][idx]    
  
    # X-axis limits.
    if ( is.null(xLim) )
      xLim <- c( 0,max(Bt) )
   
    # Y-axis limits.
    if ( is.null(yLim) )
      yLim <- c(0, max(legalHR) )
  }
  else
  {
    idx <- c( tMP:nT )

    Bt <- obj$om$Bt[ iRep,(2:ncol(obj$om$Bt)) ][idx]
  
    # Get the harvest rate on legals and sublegals.
    nCol    <- dim( obj$om$legalHR )[2]
    legalHR <- obj$om$legalHR[ iRep,c(2:nCol) ][idx]    
    
    # X-axis limits.
    if ( is.null(xLim) )
      xLim <- range( Bt )
   
    # Y-axis limits.
    if ( is.null(yLim) )
      yLim <- range( legalHR )    
  }

  plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )

  # Stock Status Objectives.
  usr <- par( "usr" )
  # Colour the Critical, Cautious and Healthy Zones:
  # (Note these are done based on the boundaries of the Status Zones, not the HCR)
  if ( gfx$colorZones )
  {
  #  rect(         0, 0, zoneLimit, usr[4], border=NA, col=.CRITCOL )
  #  rect( zoneLimit, 0, zoneUpper, usr[4], border=NA, col=.CAUTCOL )
  #  rect( zoneUpper, 0,    usr[2], usr[4], border=NA, col=.HEALCOL )
  }

  abline( v=Bmsy, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
  abline( h=Umsy, col=.UmsyCOL, lty=.UmsyLTY, lwd=.UmsyLWD )

  points( Bmsy,Umsy, bg="white",     cex=2.0, col="darkgreen", pch=21, lwd=2 )
  points( Bmsy,Umsy, bg="darkgreen", cex=1.2, col="darkgreen", pch=21 )  

  # Plot point at tMP.
  iPos <- 1
  if ( gfx$showProj )
    iPos <- tMP
  points( Bt[iPos],legalHR[iPos], cex=3, bg="black", pch=3, lwd=2 )
  
  # Now plot the trajectory of legal Harvest Rate, but only for tMP forward.
  colVec <- rev( heat.colors( n=length( Bt ) ) )
  points( Bt, legalHR, cex=1.2, bg=colVec, pch=21 )
 
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  depletion <- seq( 0,B0,B0/10.0 )
  axis( side=3, at=depletion, cex.axis=.CEXAXIS, labels=depletion/B0 )
  axis( side=4, cex.axis=.CEXAXIS, labels=FALSE )
  box()

  mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Stock Status (SSB)" )
  mtext( side=2, line=.INLINE3, cex=.CEXLAB, "Removal Rate (Legal HR)" )
  mtext( side=3, line=.INLINE1, cex=.CEXLAB, "Depletion" )

  if ( gfx$annotate )
  {
    # Calculate number and proportion in each zone from tMP:nT.
    #nZone <- table( cut( Bt, breaks=c(0,zoneLimit,zoneUpper,max(Bt) ) ) )
    #pZone <- nZone / length(Bt)

    #zoneLabels <- c( paste( format(round(pZone[1],2),digits=2,nsmall=2)," Critical",sep=" " ),
    #            paste( format(round(pZone[2],2),digits=2,nsmall=2)," Cautious",sep=" " ),
    #            paste( format(round(pZone[3],2),digits=2,nsmall=2)," Healthy",sep=" " ) )

    iSymCol <- c( 1,trunc(length(idx)/3),trunc(length(idx)/3)*2,length(idx) )
    symTxt <- c( paste( "Year ",idx[iSymCol[1]],sep="" ),
                 paste( "Year ",idx[iSymCol[2]],sep="" ),
                 paste( "Year ",idx[iSymCol[3]],sep="" ),
                 paste( "Year ",idx[iSymCol[4]],sep="" ) )

    #panLegend( 0.7,0.4, legTxt=c( zoneLabels," ",symTxt ),
    #             col=c(1,1,1,0,1,1,1,1),
    #             pch=c(22,22,22,21,21,21,21,21), cex=0.9, pt.cex=2,
    #             bg="white", pt.bg=c( .CRITCOL,.CAUTCOL,.HEALCOL,"white",
    #             colVec[iSymCol]) )

    mtext( side=3, line=.INLINE4, cex=.CEXLAB, "Operating Model Phase" )
  }
  return( invisible() )
}     # END function .plotOmPhase


.plotPriors <- function( obj, gfx=list( annotate=TRUE, doLegend=TRUE,
                xLim=NULL, yLim=NULL ) )
{
  # ctlList is obj
  
  assessMethod <- obj$mp$assess$methodId
  if ( assessMethod==.PMOD )
  {
    muPriorMSY <- obj$mp$assess$spPmMsy
    sdPriorMSY <- obj$mp$assess$spPsdMsy
    
    msy <- seq( 0.1,3*muPriorMSY, length=100 )
    
    # SPC: (24-Nov-2010) switch to normal prior
    #dens <- dlnorm( msy,meanlog=log(muPriorMSY),sdlog=sdPriorMSY )
    dens <- dnorm( msy,mean=muPriorMSY,sd=sdPriorMSY )
    
    xLim <- gfx$xLim
    if ( is.null(xLim) )
      xLim <- range( msy )
    
    yLim <- gfx$yLim
    if ( is.null(yLim) )
      yLim <- range( dens )

    plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
    lines( msy, dens, lwd=2 )
    
    abline( v=muPriorMSY, lty=2 )
    
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "MSY" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Prior Density" )
    
    if( gfx$doLegend )
    {
      panLegend( 0.7,0.9,
        legTxt=c(paste("Mean",muPriorMSY),paste("SD",sdPriorMSY)),lty=c(2,NA) )
    }
    
    muPriorFmsy <- obj$mp$assess$spPmFmsy
    sdPriorFmsy <- obj$mp$assess$spPmFmsy
    
    Fmsy <- seq( 0.0,3*muPriorFmsy, length=100 )
    
    # SPC: (24-Nov-2010) switch to normal prior
    #dens <- dlnorm( Fmsy,meanlog=log(muPriorFmsy),sdlog=sdPriorFmsy )
    dens <- dnorm( Fmsy,mean=muPriorFmsy,sd=sdPriorFmsy )
    
    xLim <- gfx$xLim
    if ( is.null(xLim) )
      xLim <- range( Fmsy )
    
    yLim <- gfx$yLim
    if ( is.null(yLim) )
      yLim <- range( dens )

    plot( xLim,yLim, type="n", axes=FALSE, xlab="", ylab="" )
    lines( Fmsy, dens, lwd=2 )
    
    abline( v=muPriorFmsy, lty=2 )
    
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    box()
    
    mtext( side=1, line=.INLINE1, cex=.CEXLAB, "Fmsy" )
    mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Prior Density" )
    
    if( gfx$doLegend )
    {
      panLegend( 0.7,0.9,
        legTxt=c(paste("Mean",muPriorFmsy),paste("SD",sdPriorFmsy)),lty=c(2,NA) )
    }   
  }
  else
  {
    .plotStatus( "No prior plots available for this assessment method.\n" )
  }
}     # END function .plotPriors                


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

  colF0   <- .REFCOLF0
  colF01  <- .REFCOLF01
  colFcra <- .REFCOLFCRA
  colFmsy <- .REFCOLFMSY
  colFmax <- .REFCOLFMAX
  colF40  <- .REFCOLF40

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

#------------------------------------------------------------------------------#
#--  Plotting functions - assessment model                                   --#
#------------------------------------------------------------------------------#

#.plotBtFit  (plot observed biomass estimates vs. time)                    
# Purpose:   Plot all predicted biomass states. 
#            Fits for replicate irep from simulation scenario isim.
# Notes:     Labelled "Stepwise Fits" in View
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (17-Aug-09), Modified A.R. Kronlund (07-Jul-10)
.plotBtFit <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                        bygears=FALSE, doLegend=FALSE, xLim=NULL, yLim=NULL,
                        useYears=FALSE ), lab = TRUE )
{
  # What stock assessment method?

  ctlList  <- obj$ctlList
  useIndex <- eval( parse( text=ctlList$mp$data$useIndex ) )

  B0         <- ctlList$opMod$B0

  omBt       <- obj$om$SBt
  omBt       <- omBt[ iRep, (2:ncol(omBt)) ]
  retroExpBt <- obj$mp$assess$retroSpawnBt
  retroExpBt <- retroExpBt[ retroExpBt[,"iRep"]==iRep, ]
  assessBt   <- retroExpBt[ ,(2:ncol(retroExpBt)) ]
  legalB     <- obj$om$legalB[ iRep, (2:ncol(obj$om$legalB)) ]
  
  nT  <- obj$ctlList$opMod$nT
  tMP <- obj$ctlList$opMod$tMP
  
  # Get the stock indices by gear.
  It <- obj$mp$data$Itg[ iRep,, ]
  
  # Get the scaled stock indices by gear.
  ItgScaled <- obj$mp$assess$ItgScaled[ iRep,,]

  # Get operating model equilibrium values.
  equilBmsy <- obj$refPtList$ssbFmsy

  # X-axis limits.
  xLim <- gfx$xLim
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( 1,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
  {
    yLim <- c( 0, max(c(omBt,assessBt),na.rm=TRUE) )
  }
  
  nGear <- dim( It )[2]
  if ( gfx$bygears )
  {
    for ( i in 1:nGear )
    {
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      
      lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
      lines( c(1:nT), legalB, col=.BlegCOL, lty=.BlegLTY, lwd=.BlegLWD )      
      
      # Add lines for all predicted biomass states
      if ( is.element( i,useIndex ) )
      {
        for ( j in 1:nrow(retroExpBt) )
          lines( c(1:nT), retroExpBt[j,3:ncol(retroExpBt)], col=.BexpCOL, lty=1, lwd=1 )     
      }      
      points( c(1:nT), ItgScaled[,i], cex=.CEXSYM2, col="black", bg=.GearCOL[i], pch=.GearPCH[i] )

      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
      # abline( h=equilBmsy, col=.BmsyCOL, lwd=.BmsyLWD, lty=.BmsyLTY )

      panLab( 0.8, 0.9, cex=.CEXANNO2, obj$ctlList$opMod$gNames[i] )  
      
      .addXaxis( xLim=xLim, initYear=.INITYEAR, gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
      
      if ( gfx$doLegend )
      {   
        panLegend( 0.5,0.85, adj=0,
          legTxt=c( "SSB","Index","Retro Exp. Bt"), cex=.CEXLEG2,
          col=c( .BtCOL, .GearCOL[i], .BtEstCOL ),
          lty=c( .BtLTY,          NA, .BtEstLTY ),
          lwd=c( .BtLWD,          NA, .BtEstLWD ),
          pch=c(     NA, .GearPCH[i],        NA ), bty="n", bg="white" )
      }
    }
    if(lab)
    {
      mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Year" )
      mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=TRUE, "Biomass" )  
    }

    
  }
  else
  {
    plot( xLim, yLim, type="n", axes=FALSE, xlab="",ylab="" )
  
    lines( c(1:nT), omBt,   col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
    # lines( c(1:nT), legalB, col=.BlegCOL, lty=.BlegLTY, lwd=.BlegLWD )
  
    if ( gfx$doLegend )
    {   
      panLegend( 0.1,0.9, adj=0,
        legTxt=c( "SSB",".3B0","1st Fit","2nd Fit"), cex=.CEXLEG2,
        pt.bg=c(  "black","black","green","darkgreen"),
        pt.cex=c( NA,NA,NA,1.2,1.2),
        col=c( .BtCOL, "orange", .BtEstCOL, "black","black" ),
        lty=c( .BtLTY,.BmsyLTY, .BtEstLTY, 1, 1 ),
        lwd=c( .BtLWD,.BmsyLWD, .BtEstLWD, 2, 2 ),
        pch=c(         NA,     NA,      NA,          NA, 21, 21 ),
        bg="white", bty="n" )
    }
  
    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
    abline( h=0.3*B0, col="orange", lty=.BmsyLTY, lwd=.BmsyLWD )  
  
    if ( gfx$useYears )
    {
      .addXaxis( xLim=xLim, initYear=.INITYEAR, years = gfx$useYears )
      axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
      axis( side=4, labels=FALSE )      
    }
    else
    {
      axis( side=1, cex.axis=.CEXAXIS )
      axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
      axis( side=3, labels=FALSE )
      axis( side=4, labels=FALSE )
    }
    
    box()
    if(lab)
    {
      mtext( side=1, line=.OUTLINE, cex=.CEXLAB, outer=FALSE, "Year" )
      mtext( side=2, line=.OUTLINE, cex=.CEXLAB, outer=FALSE, "Biomass" )  
    }

    # Add lines for all predicted biomass states
    if ( !all( is.na(retroExpBt[,"tStep"] ) ) )    
    {
      for ( j in 1:nrow(retroExpBt) )
      {
        lines( c(1:nT), retroExpBt[j,3:ncol(retroExpBt)], col=.BexpCOL, lty=1, lwd=1 )
      }
    
      # Add a green dot for the first trace - safety check - search for last non NA.
      estBt <- retroExpBt[ 1,3:ncol(retroExpBt) ]
      lines( c(1:nT), estBt, col="green", lty=1, lwd=2 )
      idt <- max( c(1:nT)[ !is.na(estBt) ] )
      points( idt, estBt[idt], bg="green", pch=21, cex=1.2 )
    
      #  Add a darkgree dot for the second trace - safety check - search for last non NA.
      estBt <- retroExpBt[ 2,3:ncol(retroExpBt) ]
      lines( c(1:nT), estBt, col="darkgreen", lty=1, lwd=2 )
      idt <- max( c(1:nT)[ !is.na(estBt) ] )
      points( idt, estBt[idt], bg="darkgreen", pch=21, cex=1.2 )    
    }
      
    if ( gfx$annotate )
    {   
      #points( c(1:nT), It, pch=1, col="black" )
    }
  }   # Not by gear.
  
}     # END function .plotBtFit

#.plotUt  (plot legal and sub-legal harvest rate times series)                   
# Purpose:   Plot all predicted biomass states. 
#            Fits for replicate irep from simulation scenario isim.
# Notes:     Labelled "Stepwise Fits" in View
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
.plotUt <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                     bygears=FALSE, doLegend=FALSE, xLim=NULL, yLim=NULL,
                     useYears=FALSE ), lab = TRUE )
{
  nCol       <- dim( obj$om$legalHR )[2]
  legalHR    <- obj$om$spawnHR[ iRep,c(2:nCol) ]
  sublegalHR <- obj$om$sublegalHR[ iRep,c(2:nCol) ]
  tMP        <- obj$ctlList$opMod$tMP
  
  # ARK (12-Dec-10) Changed Umsy to legalHRFmsy.
  #Umsy <- obj$pars$Umsy
  Umsy <- obj$refPtList$legalHRFmsy
  
  nT <- length( legalHR )
  
  nGear <- obj$ctlList$opMod$nGear

  # X-axis limits.
  xLim <- gfx$xLim
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( 1,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  
  if ( gfx$bygears )
  {
    legalHRg <- matrix( NA, nrow=nGear,ncol=nT )
    sublegalHRg <- legalHRg
    for ( i in 1:nGear )
    {
      legalHRg[i,]    <- obj$om$Ctg[iRep,,i] / obj$om$legalB[ iRep,c(2:ncol(obj$om$legalB)) ]
      sublegalHRg[i,] <- obj$om$Dtg[iRep,,i] / obj$om$sublegalB[iRep,c(2:ncol(obj$om$sublegalB)) ]
    }
    
    if ( is.null(yLim) )
      yLim <- c( 0, max(legalHRg,sublegalHRg) )  
  
    for ( i in 1:nGear )
    {
      legalHRg    <- obj$om$Ctg[iRep,,i] / obj$om$legalB[ iRep,c(2:ncol(obj$om$legalB)) ]
      sublegalHRg <- obj$om$Dtg[iRep,,i] / obj$om$sublegalB[iRep,c(2:ncol(obj$om$sublegalB)) ]
    
      plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
      lines( c(1:nT),legalHRg, col=.LegUtgCOL, lty=.LegUtgLTY, lwd=.LegUtgLWD )
      lines( c(1:nT), sublegalHRg, col=.SlegUtgCOL, lty=.SlegUtgLTY, lwd=.SlegUtgLWD )      

      abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

      panLab( 0.1, 0.9, cex=.CEXANNO2, obj$ctlList$opMod$gNames[i] )  
      
      axis( side=1, cex.axis=.CEXAXIS2 )
      axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
      box()
      
      if ( gfx$doLegend )
      {
        panLegend( 0.7,0.95, legTxt=c("Legal"), cex=1.2,
          lty=c( .LegUtgLTY, .SlegUtgLTY ), lwd=c(.LegUtgLWD,.SlegUtgLWD ) )    
      }
    }
    mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )
    mtext( side=2, line=.OUTLINE2, cex=.CEXLAB2, outer=TRUE, "Harvest Rate" )
  }
  else
  {
    if ( is.null(yLim) )
      yLim <- c( 0, max(c(legalHR,sublegalHR),na.rm=TRUE) )

    plot( xLim, yLim, type="n", axes=FALSE, xlab="",ylab="" )
  
    lines( c(1:nT), legalHR,    col=.LegUtCOL,  lty=.LegUtLTY,  lwd=.LegUtLWD )
    lines( c(1:nT), sublegalHR, col=.SlegUtCOL, lty=.SlegUtLTY, lwd=.SlegUtLWD )
  
    if ( gfx$doLegend )
    {
      panLegend( 0.5,0.95, legTxt=c(expression(C[t]/SB[t]),expression(U[max])), bty = "n",
      lty=c( .LegUtLTY, .BmsyLTY ), lwd=c( .LegUtLWD, .BmsyLWD ),
      col = c("black", .BmsyCOL) )    
    }

    abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  

    targetHR <- obj$ctlList$mp$hcr$targHRHerring

    abline( h=targetHR, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )    
    #abline( h=equilBmsy, lty=2, col="black", lwd=2 )  

    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )

    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=4, labels=FALSE )
    
    box()
    if( lab )
    {
      mtext( side=1, line=.OUTLINE,  cex=.CEXLAB2, outer=TRUE, "Year" )
      mtext( side=2, line=3, cex=.CEXLAB2, outer=FALSE, "Harvest Rate" )
    }
    
    if ( gfx$annotate )
    {   
    }
  }   # Not by gear.
  
}     # END .plotUt function.

#.plotPmodParCors (plot correlation between Fmsy and Bmsy estimates)                   
# Purpose:      Plot annual parameter estimates of Fmsy vs. Bmsy from model
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09), Modified A.R. Kronlund (07-Jul-10)
.plotPmodEstCor <- function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                             doLegend=FALSE, xLim=NULL, yLim=NULL ) )
{
  par( mfrow=c(1,1) )

  estPars  <- obj$mp$assess$estPars
  idx <- estPars$iRep==iRep
  nT  <- max(estPars$tStep)
  
  # Extract annual estimates of leading parameters for replicate.
  estBmsy <- estPars$Bmsy[ idx ]
  estFmsy <- estPars$Fmsy[ idx ]
  estMSY  <- estPars$MSY[ idx ]
  
  #pairs( cbind( estBmsy, estFmsy, estMSY ) )
  #scan()

  # Get operating model equilibrium values.
  equilBmsy <- obj$pars$ssbFmsy
  equilFmsy <- obj$pars$Umsy
  equilMSY  <- obj$pars$yieldFmsy

  # Plot Fmsy vs. Bmsy estimates.
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( is.null(xLim) )
    xLim <- range( c(equilBmsy,estBmsy) )
  
  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
  {
    yLim <- range( c(equilFmsy,estFmsy) )
  }  
  
  plot( xLim, yLim, type="n",axes=FALSE,xlab="",ylab="" )
  
  abline( v=equilBmsy, lty=2 )
  abline( h=equilFmsy, lty=2 )  
  points( equilBmsy,equilFmsy, cex=2, bg="lightblue", pch=21 )
  points( equilBmsy,equilFmsy, cex=1.2, pch="+" )
  
  colVec <- rev( heat.colors( n=length(estFmsy) ) )  
  points( estBmsy,estFmsy, pch=21, bg=colVec )
    
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  mtext( side=1, line=3,   cex=.VIEWCEX, "Bmsy" )
  mtext( side=2, line=3.5, cex=.VIEWCEX, "Fmsy" )

  if ( gfx$annotate)
  {
#    mtext( side=3, line=0, cex=.VIEWTEX, outer=TRUE, "Reference Points" )
#    panLegend( 0.05,0.95, legTxt=c("MLE Estimate","2 SD error bar", "Equilibrium" ),
#               bg="white", lty=c(NA,.ERRBARLTY,2), pch=c(.ESTPCH, NA, NA),
#               col=c(.ESTCOL, .ERRBARCOL, "black"), lwd=c(1,.ERRBARLWD, 2))
  }
  return( invisible() )
}     # .plotPmodEstCors function

#.plotPmodEstPars   (plot annual parameter estimates from production model)                   
# Purpose:      Plot annual parameter estimates from stepwise model 
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09), Revised A.R. Kronlund (07-Jul-10)
.plotPmodEstPars <-function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                             doLegend=FALSE,
                             xLim=NULL, yLimB=NULL, yLimF=NULL, yLimC=NULL ) )
{
  par( mfrow=c(3,1) )

  estPars  <- obj$mp$assess$estPars
  idx <- estPars$iRep==iRep
  nT  <- max(estPars$tStep)
  
  # Extract annual estimates of leading parameters for replicate.
  estBmsy <- estPars$Bmsy[ idx ]
  estFmsy <- estPars$Fmsy[ idx ]
  estMSY  <- estPars$MSY[ idx ]

  # Extract upper and lower error bars (+ 2 sd)
  ubBmsy <- exp( log(estBmsy) + 2*(estPars$stdBmsy[idx]) )
  lbBmsy <- exp( log(estBmsy) - 2*(estPars$stdBmsy[idx]) ) 
  ubFmsy <- exp( log(estFmsy) + 2*(estPars$stdFmsy[idx]) )
  lbFmsy <- exp( log(estFmsy) - 2*(estPars$stdFmsy[idx]) )
  
  # Extract priors.
  runMSEpar <- .createList( obj$runMSEpar )
  muPriorMSY  <- runMSEpar$mp$assess$muPriorMSY
  muPriorFmsy <- runMSEpar$mp$assess$muPriorFmsy
    
  # Get operating model equilibrium values.
  equilBmsy <- obj$pars$ssbFmsy
  equilFmsy <- obj$pars$Umsy
  equilMSY  <- obj$pars$yieldFmsy

  # Plot Bmsy estimates.
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( gfx$showProj )
    xLim <- c( obj$pars$tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( obj$pars$tMP,nT )

  # Y-axis limits.
  yLimB <- gfx$yLimB
  if ( is.null(yLimB) )
  {
    yLimB <- range( c(equilBmsy,estBmsy) )
  }  
  
  plot( xLim, yLimB, type="n",axes=FALSE,xlab="",ylab="" )
  points( estPars$tStep[idx], estBmsy, pch=16, col=.ESTCOL)
    
  #  arrows((obj$pars$tMP:obj$pars$nT), BmsyLo, (obj$pars$tMP:obj$pars$nT), BmsyUp, 
  #    angle=0, col=.ERRBARCOL, lty=.ERRBARLTY, lwd=.ERRBARLWD ) 
    
  abline( h=equilBmsy, lty=2, col="black", lwd=2 )
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()
  mtext( side=2, line=3.5, cex=.VIEWCEX, "Bmsy" )

  if ( gfx$annotate)
  {
#    mtext( side=3, line=0, cex=.VIEWTEX, outer=TRUE, "Reference Points" )
#    panLegend( 0.05,0.95, legTxt=c("MLE Estimate","2 SD error bar", "Equilibrium" ),
#               bg="white", lty=c(NA,.ERRBARLTY,2), pch=c(.ESTPCH, NA, NA),
#               col=c(.ESTCOL, .ERRBARCOL, "black"), lwd=c(1,.ERRBARLWD, 2))
  }

  # Plot Fmsy estimates.

  # Y-axis limits.
  yLimF <- gfx$yLimF
  if ( is.null(yLimF) )
    yLimF <- range( c(muPriorFmsy,equilFmsy,estFmsy) )
    
  plot( xLim, yLimF, type="n",axes=FALSE,xlab="",ylab="" )
  points( estPars$tStep[idx], estFmsy, pch=16, col=.ESTCOL )
    
  #arrows(obj$pars$tMP:obj$pars$nT, FmsyLo, obj$pars$tMP:obj$pars$nT, FmsyUp, 
  #    angle=0, col=.ERRBARCOL, lty=.ERRBARLTY, lwd=.ERRBARLWD )
  
  abline( h=equilFmsy,lty=2, col="black", lwd=2 )
  abline( h=muPriorFmsy, lty=2, col="red", lwd=2 )
  
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()
 
  mtext( side=2, line=3.5,   cex=.VIEWCEX, "Fmsy" )
  
  if ( gfx$doLegend )
  {
    panLegend( 0.8, 0.35, legTxt=c("Equilibrum","Prior Mean"),col=c("black","red"),
      lty=c(2,2),lwd=c(2,2) )
  }

  # Plot MSY estimates.

  # Y-axis limits.
  yLimC <- gfx$yLimC
  if ( is.null(yLimC) )
    yLimC <- range( c(muPriorMSY,equilMSY,estMSY) )
    
  plot( xLim, yLimC, type="n",axes=FALSE,xlab="",ylab="" )
  points( estPars$tStep[idx], estMSY, pch=16, col=.ESTCOL )
    
  #arrows(obj$pars$tMP:obj$pars$nT, FmsyLo, obj$pars$tMP:obj$pars$nT, FmsyUp, 
  #    angle=0, col=.ERRBARCOL, lty=.ERRBARLTY, lwd=.ERRBARLWD )
  
  abline( h=equilMSY,   col="black", lty=2, lwd=2 )
  abline( h=muPriorMSY, col="red",   lty=2, lwd=2 )
  
  axis( side=1 )
  axis( side=2, las=.YAXISLAS )
  box()

  if ( gfx$doLegend )
  {
    panLegend( 0.8, 0.35, legTxt=c("Equilibrum","Prior Mean"),col=c("black","red"),
      lty=c(2,2),lwd=c(2,2) )
  }
 
  mtext( side=2, line=3.5, cex=.VIEWCEX, "MSY" )
  mtext( side=1, line=1,   cex=.VIEWCEX, outer=TRUE, "Year" )

  return( invisible() )
}     # .plotPmodEstPars function

#.plotHCRrefPtSeries (plot annual parameter estimates)                   
# Purpose:      Plot annual parameter estimates from stepwise model 
#                 fits for replicate irep from simulation scenario isim.
# Parameters:   Object with saved simulation results, sim and rep number 
#                 to be plotted, and plot specifications
# Returns:      NULL (invisibly).
# Source:       K.Holt (20-Aug-09), Revised A.R. Kronlund (07-Jul-10)
.plotHCRrefPtSeries <-function( obj, iSim=1, iRep=1, gfx=list( annotate=TRUE,
                                doLegend=FALSE, showProj=TRUE,
                                xLim=NULL, yLim=NULL, yLim2=NULL,
                                useYears=FALSE ) )
{
  guiInfo <- getWinVal( scope="L" )
  
  # Extract plot parameters.
  xLim       <- gfx$xLim
  yLim       <- gfx$yLim
  yLim2      <- gfx$yLim2

  # Recalc ref pts
  ctlList <- obj$ctlList
  opMod <- ctlList$opMod
  tmp <- calcRefPoints( as.ref(opMod) )
  refPts <- deref( tmp )
  ctlList$refPts <- refPts

  nT      <- ctlList$opMod$nT
  tMP     <- ctlList$opMod$tMP
 
  # Extract relevant reference point bases: SPC edited 4June
  B0      <- ctlList$opMod$B0
  Bmsy    <- obj$refPtList$ssbFmsy

  # Extract relevant reference point bases:
  if ( ctlList$mp$hcr$statusBase == "statusBaseBmsy" ) baseB <- Bmsy
  if ( ctlList$mp$hcr$statusBase == "statusBaseB0" )   baseB <- B0

  # ARK (12-Dec-10) Changed Umsy to legalHRFmsy.
  #equilFmsy     <- obj$pars$Umsy
  Umsy           <- obj$refPtList$Umsy
  legalHRFmsy    <- obj$refPtList$legalHRFmsy
  
  # Operating model quantities.
  Bt <- obj$om$FBt[ iRep,(2:ncol(obj$om$Bt)) ]
  Ct <- obj$om$Ct[ iRep,(2:ncol(obj$om$Ct)) ]
  Dt <- obj$om$Dt[ iRep,(2:ncol(obj$om$Dt)) ]
    
  legalHR <- (Ct + Dt) / Bt

  # Management procedure quantities.  
    
  lowerBoundMult <- ctlList$mp$hcr$lowerBoundMult     # Lower control pt. mult.
  upperBoundMult <- ctlList$mp$hcr$upperBoundMult     # Upper control pt. mult.

  # B reference, lower and upper control point series.
  Bref       <- obj$mp$hcr$Bref[ iRep,c(2:(nT+1)) ]
  lowerBound <- obj$mp$hcr$lowerBound[ iRep, c(2:(nT+1)) ]
  upperBound <- obj$mp$hcr$upperBound[ iRep, c(2:(nT+1)) ]
  
  # Projected model exploitable biomass.
  mpdPars <- obj$mp$assess$mpdPars
  projExpBio <- mpdPars[ mpdPars$iRep==iRep, "projExpBio" ]

  # Plot biomass view.
  
  # X-axis limits.
  xLim <- gfx$xLim
  if ( gfx$showProj )
    xLim <- c( tMP - 1,nT )
  else
    if ( is.null(xLim) )
      xLim <- c( tMP - 1,nT )

  # Y-axis limits.
  yLim <- gfx$yLim
  if ( is.null(yLim) )
  {
    yLim <- c(0, max( c(Bt[(tMP-1):nT],baseB,Bref,lowerBound,upperBound,projExpBio),na.rm=TRUE ) )
  }  
  
  plot( xLim, yLim, type="n",axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )
  
  # Indicate lRef, uRef and Bmsy and tMP.
  abline( h=baseB, col=.BmsyCOL, lty=.BmsyLTY, lwd=.BmsyLWD )
 
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )  
  
  lines( c(1:nT),         Bt, col=.BtCOL,   lty=.BtLTY,   lwd=.BtLWD )
  
  lines( c(tMP:nT), projExpBio, col=.BexpCOL, lty=.BexpLTY, lwd=.BexpLWD )
    
  points( c(1:nT), Bref,       cex=1.2, pch=21, bg="black" )
  points( c(1:nT), upperBound, cex=1.2, pch=21, bg="white" )
  points( c(1:nT), lowerBound, cex=1.2, pch=21, bg="lightblue" )
  
  if ( gfx$useYears )
  {
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )      
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=4, labels=FALSE )      
  }
  else
  {
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
  }
  
  box()
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Biomass (000s t)" )

  if ( gfx$doLegend )
  {
    panLegend( 0.5,0.95, cex=0.8,
               legTxt=c( "Exp SSB","ProjExp SSB","B0","UB","LB" ),
               col=c( .BtCOL,.BexpCOL,"black","black","black"),
               pt.bg=c(NA,NA,"black","white","lightblue" ),
               pt.cex=c(NA,NA,1.2,1.2,1.2),
               lty=c(.BtLTY, 1, NA, NA, NA),
               pch=c(NA,NA,21,21,21),
               lwd=c(.BtLWD, 2,NA,NA,NA), bg="white" )
  }

  # Removal rate view.

  # Extract the reference removal rate (estimated and unadjusted).
  idx  <- obj$mp$hcr$Fref[,"iRep"] == iRep
  Fref <- obj$mp$hcr$Fref[ idx, c(2:ncol(obj$mp$hcr$Fref)) ]  
  
  # Extract the OM legal harvest rate.
  # legalHR <- obj$om$legalHR[ iRep,c(2:ncol(obj$om$legalHR)) ]
  
  if ( is.null(yLim2) )
  {
    yLim2 <- c( 0, max( c(legalHRFmsy,Fref,legalHR), na.rm=T ) )
  }
  
  plot( xLim,yLim2, type="n",axes=FALSE,xlab="",ylab="" )
  
  abline( h=legalHRFmsy, col=.UlegCOL, lty=.UlegLTY, lwd=.UlegLWD )
  abline( h=Umsy, col=.UmsyCOL, lty=.UmsyLTY, lwd=.UmsyLWD )  
  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  # Plot the OM "actual" harvest rate on legal fish.
  lines( c(1:nT), legalHR, col=.UlegCOL, lty=1, lwd=2 )

  idx   <- obj$mp$hcr$precautionaryFt[ ,"iRep"] == iRep
  Fprec <- obj$mp$hcr$precautionaryFt[ idx, c(2:ncol(obj$mp$hcr$precautionaryFt)) ]
  
  lines( c(1:nT), Fprec, col="red", lty=1, lwd=1 )  
  
  # Now draw vertical segments bewteen F reference removal and F precautionary.
  segments( c(1:nT), Fref, c(1:nT), Fprec, col="darkgray", lwd=3 )
  
  # Plot the MP reference removal fishing mortality.  
  lines( c(1:nT), Fref, col="red", lty=1, lwd=2 )
  points( c(1:nT), Fprec, cex=1.2, pch=21, bg="white", col="red" )

  if ( gfx$useYears )
  {
    .addXaxis( xLim, initYear=.INITYEAR, years=gfx$useYears )     
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=4, labels=FALSE )      
  }
  else
  {
    axis( side=1, cex.axis=.CEXAXIS )
    axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
    axis( side=3, labels=FALSE )
    axis( side=4, labels=FALSE )
  }
  box()
    
  mtext( side=2, line=.INLINE2, cex=.CEXLAB, "Harvest Rate" )

  if ( gfx$doLegend )
  {
    panLegend( 0.5,0.95, cex=0.8,
               legTxt=c( "Legal HR","MP Fref","MP precF","UlegMSY", "Umsy" ),
               col=c( .UlegCOL,"red","red",.UlegCOL,.UmsyCOL ),
               pt.bg=c(NA,NA,"white",NA,NA),
               lty=c(1,1,1,.UlegLTY,.UmsyLTY),
               pch=c(NA,NA,21,NA,NA),
               lwd=c(2,2,1,2,.UlegLWD,.UmsyLWD), bg="white" )
  }
  
  mtext( side=1, line=.OUTLINE-1, cex=.CEXLAB, outer=TRUE, "Year" )

  return( invisible() )
}     # END function .plotHCRrefPtSeries

#------------------------------------------------------------------------------#
#--                   Performance Statistics Plots                           --#
#------------------------------------------------------------------------------#

# Plots for Performance GUI (guiPerf):                                         #
#                                                                              #

#------------------------------------------------------------------------------#
# Tulip plots (simulation envelope plots)                                      #
#------------------------------------------------------------------------------#

.addObjectives <- function( obj, failedReps=NULL, base="Bmsy",
                            targetMult, targetYear, targetProb )
{
  # Sreen out failed replicates.
  
  Bt <- obj$om$Bt
  
  if ( !is.null( failedReps ) )
  {
    isFailed <- is.element( Bt[,"iRep"], failedReps )
    nFail <- length(failedReps)
    Bt <- Bt[ !isFailed,c(2:ncol(Bt)) ]
  }
  else
    Bt <- Bt[ ,c(2:ncol(Bt)) ]

  tMP  <- obj$pars$tMP
   
  baseVal <- 0
  if ( base=="Bmsy" )
    baseVal <- obj$pars$ssbFmsy
  else if ( base=="B0" )
    baseVal <- obj$pars$B0
     
  # .calcStatsTarget works in terms Bt/baseVal with target targMult*baseVal.
  # e.g., if Bmsy=30 and targetMult=0.4, then targ=0.4*30=12.   
  tmp <- .calcStatsTarget( Bt,baseVal,tMP, targetMult,targetYear,targetProb )

  usr <- par( "usr" )
   
  # Objective 1: yearAtTargetProb (black dot) - the year where the probability is
  #              P that the Target Base Multiplier*Base will be first reached.

  # Horizontal line at target.
  abline( h=tmp$target/baseVal, lty=2 )
   
  # Vertical line at targetYear to x-axis.
  segments(tmp$yearAtTargetProb,usr[3],tmp$yearAtTargetProb,tmp$targetMult,lty=2)

  # A point at the year.
  points( tmp$yearAtTargetProb, tmp$target/baseVal, col=.TARGYEARCOL,cex=2,lwd=2,pch=1 )
  points( tmp$yearAtTargetProb, tmp$target/baseVal, col=.TARGYEARCOL,cex=1.1,lwd=2,pch=16 )
          
  # Objective 2: targAtYearProb (red dot) - the Base Multiplier where the probability
  #              is P in year Y that the Base Multiplier * Target will be exceeded.
  
  # A vertical line at targetYear.
  segments( tmp$targetYear,usr[3],tmp$targetYear,tmp$targetAtYearProb/baseVal,lty=2 )
   
  # A point at the target.
  points( tmp$targetYear, tmp$targetAtYearProb/baseVal, col=.TARGCOL,cex=2,lwd=2,pch=1 )
  points( tmp$targetYear, tmp$targetAtYearProb/baseVal, col=.TARGCOL,cex=1.1,lwd=2,pch=16 )
          
  # Objective 3: probAtTargYear (royal blue dot) - the probablity P of being
  #       equal to or greater than the Target Base Multiplier*Base in year Y.
  
  # A point at the intersection of year and target.
  points( tmp$targetYear,tmp$targetMult, col=.TARGPROBCOL,cex=2,lwd=2,pch=1 )
  points( tmp$targetYear,tmp$targetMult, col=.TARGPROBCOL,cex=1.1,lwd=2,pch=16 )
   
  return( tmp )
}     # .addObjectives function

# .plotTrend      (Exponential slope years t=t1,.,,(t1+delta) for determining
#                  stock trend over the period=delta.
# Purpose:         Displays fan plot of slopes and distribution of slopes.
# Parameters:      obj is the object containing the Rdata blob list.
# Returns:         NULL (invisibly)
.plotTrend <- function( obj, delta=10, multLrp=0.4, multUsr=0.8,
                        label=NULL, traces=NULL, failedReps=NULL,
                        gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
                        showProj=TRUE, xLim=NULL, yLim=NULL, useYears=FALSE ),... )
{
  ctlList <- obj$ctlList

  # Get the "true" spawning biomass from the operating model.
  Bt <- obj$om$Bt
  
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
  
  # Time indices (could use obj$par$nT).
  tMP   <- ctlList$opMod$tMP
  nT    <- ctlList$opMod$nT
  nReps <- ctlList$gui$nReps
  tVec  <- c(1:ncol(Bt))

  xLim <- gfx$xLim
  
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( (tMP - 1),nT )

  if ( is.null(yLim) )    
    yLim <- c( 0,max( Bt ) )
   
  # Build the plot area.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  if ( gfx$useYears )
  { 
    .addXaxis( xLim, initYear=.INITYEAR, years=useYears )

    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )
  }

  if ( gfx$doGrid )
  {
    abline( v=seq(xLim[1], xLim[2],  5), lwd=0.5, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],0.1), lwd=0.5, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
  
  if ( !is.null(label) )
    panLab( 0.5,0.9, cex=.CEXLAB, label )

  if ( gfx$annotate )
  {
    if ( !is.null(failedReps) )
      panLab( 0.5, 0.02, cex=0.8,
        paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )        
  }

  #browser()
  trend <- .calcStatsTrend( Bt, t1=tMP, delta=delta )
  
  # Scaled "risk" probability.  If we had draws from the posterior, then we'd
  # have to pick a "mean" SSB for t1, to compute pDecline, or a mean pDecline?
  # Let's assume we shall pick a mean pDecline.
  
  SSB    <- as.numeric( Bt[,unique(trend$t1) ] )
  target <- obj$refPtList$ssbFmsy
  trend$pDecline <- .calcStatsAccDecline( SSB, target,
    lowProb=0.05, hiProb=0.5, multLrp=multLrp, multUsr=multUsr  )

  cat( "\nhMSG (.plotTrend) Trend statistics...\n" )
  print( trend[1:min(10,length(Bt)),] )

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= (nReps-nFail) ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
    {
      lines( tVec, Bt[i,], col="gray" )
      t1 <- trend[i,"t1"]
      t2 <- trend[i,"t2"]
      y1 <- trend[i,"y1"]
      y2 <- trend[i,"yPrime"]
      segments( t1,y1,t2,y2, col="black" )
    }
    abline( v=t2, lty=2 )
    usr <- par( "usr" )
    if ( gfx$annotate )
    {
      text( t1, max(0,usr[3])+2, pos=4, paste( "t1=",t1,sep="" ) )
      text( t2, max(0,usr[3])+2, pos=4, paste( "t2=",t2,sep="" ) )    
    }
  }
  
  box()
  if (mfg[1]==mfg[3] )
    mtext( side=1, line=2.5, cex=.CEXLAB, "Year" )
  
  # Now plot the distribution of the slopes.
  xLim <- range( c(0,trend$beta) )

  hist( trend$beta, axes=FALSE, main="", xlab="", xlim=xLim, ylab="" )
  abline( v=0, lty=2 )
  abline( h=0, lty=3 )
  abline( v=mean(trend$beta ), lty=4, lwd=2 )
  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  box()
  if (mfg[1]==mfg[3] )
    mtext( side=1, line=2.5, cex=.CEXLAB, "Exponential Slope" )  
  
  nInc <- sum( trend$beta >= 0.0 )
  nDec <- length( trend$beta ) - nInc
  pObs <- nDec / length( trend$beta )
  pDec <- mean( trend$pDecline )
  
  if ( gfx$doLegend )
    panLegend( 0.6,0.95, legTxt=c( paste( 
      c("Stable/Inc ","Decreasing ","Obs. P(decline) ","P(decline) " ),
      c(nInc,nDec,pObs,round(pDec,digits=3)) ) ),
      bg="white", bty="n", cex=1.0 )
  
  return( invisible() )
}     # END function .plotTrend

# .plotTulipBmsy (tulip simulation envelope for SSB/Bmsy)
# Purpose:            Display tulip (envelope) SSB/Bmsy for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
.plotTulipBmsy <- function( obj, multLrp, multUsr,
                 label=NULL, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                 allQuants=TRUE, failedReps=NULL,
                 gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
                           showProj=TRUE, xLim=NULL, yLim=NULL,
                           useYears=FALSE ),... )
{
  ctlList <- obj$ctlList
  
  # Get the spawning stock depletion at MSY.
  Bmsy   <- obj$refPtList$ssbFmsy
  
  # Get the spawning biomass.
  Bt <- obj$om$Bt[ ,c(2:ncol(obj$om$Bt)) ]
  
  # Scale the spawning biomass to spawning Bmsy.
  scaledBt <- Bt / Bmsy

  # Time indices.
  tMP   <- ctlList$opMod$tMP
  nT    <- ctlList$opMod$nT
  nReps <- ctlList$gui$nReps

  tVec  <- c(1:ncol(scaledBt))

  xLim <- gfx$xLim
  
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( (tMP - 1),nT )

  if ( is.null(yLim) )    
    yLim <- c( 0,max( c(scaledBt,1.0) ) )
   
  # Build the plot area.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )

  if ( gfx$doGrid )
  {
    abline( v=seq(xLim[1], xLim[2],  5), lwd=0.5, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],0.1), lwd=0.5, col=.GRIDCOL )
  }

  quants <- apply( scaledBt, 2, quantile, probs=qProbs )

  # Plot the shaded envelope.
  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
    lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
  }
  
  abline( h=1.0, col=.MSYCOL, lty=.MSYLTY, lwd=.MSYLWD )
  abline( h=multLrp, col=.LrpCOL, lty=.LrpLTY, lwd=.LrpLWD )
  abline( h=multUsr, col=.UsrCOL, lty=.UsrLTY, lwd=.UsrLWD )
    
  if ( gfx$annotate )
  {
    #if ( !is.null(failedReps) )
    #  panLab( 0.5, 0.02, cex=0.8,
    #    paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )
    if ( !is.null( label ) )
      panLab( 0.5, 0.9, cex=.CEXLAB, label )                
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  nFail <- 0
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= (nReps-nFail) ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, scaledBt[i,] )
  }
  
  box()
  return( invisible() )
}     # END function .plotTulipBmsy

# .plotTulipBmsy (tulip simulation envelope for SSB/Bmsy)
# Purpose:            Display tulip (envelope) SSB/Bmsy for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
.plotTulipBmsyCmsy <- function( obj, multLrp, multUsr,
                 traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                 allQuants=TRUE, failedReps=NULL,
                 gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
                           showProj=TRUE, xLim=NULL, yLim=NULL ),... )
{
  runMSEpars <- .createList( obj$runMSEpar )

  # Get the spawning stock depletion at MSY.
  Bmsy   <- obj$pars$ssbFmsy
  
  # Get the spawning biomass.
  Bt <- obj$om$Bt
  
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
  
  # Scale the spawning biomass to spawning Bmsy.
  scaledBt <- Bt / Bmsy

  # Time indices (could use obj$par$nT).
  tMP   <- obj$pars$tMP
  nT    <- obj$pars$nT
  nReps <- obj$pars$nReps
  tVec  <- c(1:ncol(scaledBt))

  xLim <- gfx$xLim
  
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( (tMP - 1),nT )

  if ( is.null(yLim) )    
    yLim <- c( 0,max( c(scaledBt,1.0) ) )
   
  # Build the plot area.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )
  
  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  
    
  #if ( fixY )
  #{
  #  # Y-axis (left): panel is in the first column.
  #  if ( mfg[2]==1 )
  #    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  #  else
  #    axis( side=2, labels=FALSE )
      
  #  # Y-axis (right): panel is in the last column.
  #  if ( mfg[2]==mfg[4] )
  #    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  #  else
  #    axis( side=4, labels=FALSE )  
  #}
  #else
  yTicks <- round(seq( 0, yLim[2], length = 4 ),2)
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, at = yTicks )

  if ( gfx$doGrid )
  {
    abline( v=seq(xLim[1], xLim[2],  5), lwd=0.5, col=.COLGRID )
    abline( h=seq(yLim[1], yLim[2],0.1), lwd=0.5, col=.COLGRID )
  }

  quants <- apply( scaledBt, 2, quantile, probs=qProbs )

  # Plot the shaded envelope.
  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
    lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
  }

  if ( gfx$annotate )
  {
    abline( h=1.0, col=.COLDepMsy, lty=.LTYDepMsy, lwd=.LWDDepMsy )
    abline( h=multLrp, col=.COLDepLrp, lty=.LTYDepLrp, lwd=.LWDDepLrp )
    abline( h=multUsr, col=.COLDepUsr, lty=.LTYDepUsr, lwd=.LWDDepUsr )
    
    if ( !is.null(failedReps) )
      panLab( 0.5, 0.02, cex=0.8,
        paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )        
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= (nReps-nFail) ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, scaledBt[i,] )
  }
  
  box()
  return( invisible() )
}     # .plotTulipBmsyCmsy



# .plotTulipCatchMSY (tulip simulation envelope for catch)
# Purpose:        Display tulip (simulation envelope) catch for one simulation.
# Parameters:     obj is the object containing the Rdata blob list.
#                 Allow percentiles to be specified.
# Returns:        NULL (invisibly)
.plotTulipCatchMSY <- function( obj, label=NULL,
                     traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                     allQuants=TRUE, failedReps=NULL,
                     gfx=list( annotate=TRUE, doLegend=TRUE, doGrid=FALSE,
                               showProj=TRUE, xLim=NULL, yLim=NULL,
                               useYears=FALSE ),... )
{
  cax <- .TULCAX

  # Get the catch biomass.
  # iRep is not labelled, but is in first column.
  
  Ct <- obj$om$Ct
 
  # Determine which reps to remove.
  if ( is.null(failedReps) )
  {
    Ct <- Ct[ ,c(2:ncol(Ct)) ]
    nFail <- 0
  }
  else
  {
    isFailed <- is.element( Ct[,1], failedReps )
    nFail <- length(failedReps)
    Ct <- Ct[ !isFailed,c(2:ncol(Ct)) ]
  }  

  MSY <- obj$pars$landedFmsy
  
  # Rescale catch.
  Ct <- Ct / MSY

  # Time indices (could use obj$par$nT).
  tMP   <- obj$pars$tMP
  nT    <- obj$pars$nT
  nReps <- obj$pars$nReps
  tVec  <- c(1:ncol(Ct))

  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- c(1,nT)
  if ( gfx$showProj )
    xLim <- c( (tMP - 1),nT )

  if ( is.null(yLim) )    
    yLim <- c( 0,max(Ct) )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  if ( gfx$useYears )
  { 
    xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
    xSeq <- xPos - .INITYEAR + 1
    xLabs <- paste( xPos )

    # X-axis (bottom): panel is in the last row.
    if ( mfg[1]==mfg[3] )
      axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=xLabs )
    else
      axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
    # X-axis (top): panel is in the first row.
    if ( mfg[1]==1 )
      axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=xLabs )
    else
      axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  

    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )
  }
  else
  {
    xSeq <- seq( 0,nT,5 )
   
    # X-axis (bottom): panel is in the last row.
    if ( mfg[1]==mfg[3] )
      axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    else
      axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
    # X-axis (top): panel is in the first row.
    if ( mfg[1]==1 )
      axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
    else
      axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )
  }

  if ( gfx$doGrid )
  {
    abline( v=seq( xLim[1], xLim[2], 5), lwd=0.5, col=.COLGRID )
    abline( h=seq( yLim[1], yLim[2], 5), lwd=0.5, col=.COLGRID )
  }

  quants <- apply( Ct, 2, quantile, probs=qProbs )

  # Plot the extreme quantiles.
  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
    lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
  }
  
  usr <- par( "usr" )

  if ( !is.null(MSY) && gfx$annotate )
  {
    urs <- par( "usr" )
    points( usr[2], MSY, xpd=T, cex=1.4, pch=16, col="black" )
  }
  
  abline( h=1, lty=.LTYBmsy )
  
  if ( gfx$annotate )
  {
    if ( !is.null(failedReps) )
      panLab( 0.5, 0.02, cex=0.8,
        paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )
    if ( !is.null( label ) )
      panLab( 0.5, 0.9, cex=.CEXLAB, label )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )
        
  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= (nReps-nFail) ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Ct[i,] )
  }
  box()  
  return( invisible() )
}     # .plotTulipCatchMSY


# .plotTulipDepletion (tulip simulation envelope for depletion)
# Purpose:            Display tulip (envelope) depletion for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund
.plotTulipDepletion <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                                 xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                                 allQuants=FALSE,
                         gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
                                 showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ),... )                                 
{
  # Get the spawning biomass and number of replicates.
  Bt    <- obj$om$SBt[ , c(2:ncol(obj$om$SBt)) ]

  nReps <- nrow( Bt )

  opMod  <- obj$ctlList$opMod
  # tmp    <- calcRefPoints( as.ref(opMod) )
  # tmpRefPts <- deref( tmp )

  # obj$ctlList$refPts <- tmpRefPts

  # Extract B0 and Bmsy for depletion calculations.
  B0     <- obj$ctlList$opMod$B0
  Bmsy   <- obj$ctlList$refPts$ssbFmsy
  #browser()
  # Convert MSY and Bt to depletion scale.
  depMSY <- Bmsy / B0
  Dept   <- Bt   / B0
  #depMSY <- Bmsy
  #Dept   <- Bt

  if( !is.null(obj$ctlList$opMod$posteriorDraws) )
  {
    postDraws <- obj$ctlList$opMod$posteriorDraws
    SB0       <- obj$ctlList$opMod$mcmcPar[postDraws,"sbo"]
    for( repIdx in 1:nrow(Dept) )
      Dept[repIdx,] <- Bt[repIdx,]/SB0[repIdx]
  }

  # Time indices.
  tMP  <- obj$ctlList$opMod$tMP
  nT   <- obj$ctlList$opMod$nT
  tVec <- c(1:ncol(Dept))

  peakBiomassYear <- apply(X=Bt[,tMP:nT],MARGIN=1,FUN=function(x){which.max(x)})


  # Specify axis limits for plotting
  if( is.null(xLim) ) xLim <- gfx$xLim
  if( is.null(yLim) ) yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Dept[ ,tdx ], na.rm=TRUE )
  }
  
  # Build the plot area.
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( xLim[1],xLim[2],by = 5 )
  years <- seq(.INITYEAR,by = 1, length = nT)

  # X-axis (bottom): panel is in the last row.
  if( gfx$useYears ) 
    labs <- years[xSeq]
  else 
    labs <- xSeq
  
  axis(side = 1, at = xSeq, labels = labs, cex.axis=.CEXAXIS2 )
  

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    axis(side = 3, at = xSeq, labels = labs, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  # Y-axis (left).
  yTicks <- round(seq(from=0,to=yLim[2],length=6),2)
  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, at=yTicks, ... )
  #axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )

  axis( side=4, labels=FALSE,at=yTicks )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )


  quants <- apply( Dept, 2, quantile, probs=qProbs, na.rm = T )

  medBt <- apply(X=Dept,MARGIN=2,FUN=quantile,probs=0.5, na.rm = T)

  peakMedianBiomassYear <- which.max(medBt[tMP:nT])


  # Plot the shaded envelope.
  nQuants <- nrow( quants )

  # If final year has NAs, remove that year from tVec
  if(all(is.na(quants[,nT]))) tVec <- tVec[-length(tVec)]

  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,tVec],rev(quants[nQuants,tVec])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,tVec], lwd=2, lty=1 )
  if ( allQuants )
  {
    lines( tVec,quants[2,tVec], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,tVec], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }
  # lines for limit and upper ref pts for Atl Hal
  # Biomass reference lines
  if( .ISCAMFLAG )
  {
    # Blim
    depBlim <- .BlimHerring
    # USR (not sure what this is yet)
    depUSR <- .USRHerring
    # TRP (NCN objective 1)
    depTRP <- .TRPHerring

    abline( h = depBlim, lty = 2, col = "red", lwd = 2 )
    abline( h = depUSR, lty = 2, col = "darkgreen", lwd = 2 )

    if( gfx$doLegend )
      panLegend(  x = .3, y =.8, bty = "n",
                  legTxt = c( expression(.3*B[0]),
                              expression(.6*B[0])),
                  lty = 2, lwd = 2,
                  col = c("red","darkgreen")  )

  } else {
    abline( h=depMSY, lty="dashed", col="black")
    abline( h=0.4*depMSY, lty="dotted", col="black")  
  }
  
  

    if( .PLOT.MED.PEAKBIOMASS )
  {
    segments( x0=tMP+peakMedianBiomassYear-1,x1=tMP+peakMedianBiomassYear-1,
              y0=0, y1=max(medBt[tMP:nT]), lty="dotted",col="black" )

    segments( x0=tMP,x1=tMP+peakMedianBiomassYear-1,
              y0=max(medBt[tMP:nT]), y1=max(medBt[tMP:nT]), 
              lty="dotted",col="black" )
    panLab( x=(peakMedianBiomassYear+6)/nT , y=0.1, 
          txt=paste(.INITYEAR+tMP+peakMedianBiomassYear-1) )

  }

  box()

  usr <- par( "usr" )
  if ( !is.null(depMSY) && refPts )
  {
    urs <- par( "usr" )
    points( c(usr[1],usr[2]), c(depMSY,depMSY), xpd=T, bg=.BmsyBG, cex=.BmsyCEX, col=.BmsyCOL, pch=.BmsyPCH )
  }

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Dept[i,tVec] )
  }

  return( invisible() )
}     # END function .plotTulipDepletion

.plotTulipDepCat <- function( obj, allQuants=TRUE, annotate=FALSE,
                              yLimC=NULL, yLimD=NULL,
                              DepLab = "Depletion", colHeader = NULL,
                              ... )
{
  if( is.null(colHeader) )
    colHeader <- obj$ctlList$gui$mpLabel

  .plotTulipDepletion( obj, xLim=NULL, yLim=yLimD, ... )
  mtext(side = 3, text = colHeader, cex = .6, line = 3 )
  mfg <- par( "mfg" )
  if ( mfg[2]==1 )
    mtext( side=2, line=2.5, cex=1, expression(B[t]/B[0]) )
  .plotTulipCatch( obj, xLim=NULL, yLim = yLimC, ... )
  mfg <- par( "mfg" )  
  if( mfg[2]==1 ) 
    mtext( side=2, line=2.5, cex=1, "Catch (kt)" )  
}     # .plotTulipDepCat.

# .plotTulipF        (tulip simulation envelope for fishing mortality)
# Purpose:            Display tulip (envelope) F for one simulation.
# Parameters:         obj is the object containing the Rdata blob list.
#                     Allow percentiles to be specified.
# Returns:            NULL (invisibly)
# Source:             A.R. Kronlund
.plotTulipF <- function( obj, traces=NULL, qProbs=c(0.05,0.1,0.5,0.9,0.95),
                         xLim=NULL, yLim=NULL, annotate=TRUE, refPts=TRUE,
                         allQuants=TRUE,
                         gfx=list( annotate=TRUE, grids=FALSE, doLegend=TRUE,
                         showProj=FALSE, xLim=NULL, yLim=NULL, useYears=FALSE ),... )
{
  Ft    <- obj$om$Ft[ ,(1:ncol(obj$om$Ft)),, drop=FALSE ]
  Fmsy  <- obj$refPtList$Fmsy
  if( .USEMt )
  {
    Ft  <- obj$om$Mt[ ,2:ncol(obj$om$Mt), drop=FALSE ]
    Fmsy <- obj$ctlList$opMod$recM
  }
  if( .USERt )
  {
    Ft <- obj$om$Rt[ ,(2:ncol(obj$om$Rt)), drop=FALSE ]
  }
  
  nReps <- obj$ctlList$gui$nReps

  # Time indices.
  tMP   <- obj$ctlList$opMod$tMP
  nT    <- obj$ctlList$opMod$nT
  tVec  <- c(1:ncol(Ft))

  # Specify axis limits for plots
  xLim <- gfx$xLim
  yLim <- gfx$yLim
  
  # Set the x-axis limits.
  if ( is.null(xLim) )
    xLim <- c( 1, nT )
    
  if ( gfx$showProj )
    xLim <- c( tMP - 1, nT )

  # Set the y-axis limits.
  if ( is.null(yLim) )
  {
    tdx <- c( xLim[1]:xLim[2] )
    yLim <- range( Ft[ ,tdx ], na.rm=TRUE )
  }

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  mfg <- par( "mfg" )
  xSeq <- seq( 0,nT,5 )

  # X-axis (bottom): panel is in the last row.
  if ( mfg[1]==mfg[3] )
    # axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
    .addXaxis( xLim, initYear=.INITYEAR, side=1, years=gfx$useYears )
 # else
  #  #axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
   # .addXaxis( xLim, initYear=.INITYEAR, side=3, years=FALSE )

  # X-axis (top): panel is in the first row.
  if ( mfg[1]==1 )
    .addXaxis( xLim, initYear=.INITYEAR, side=3, years=gfx$useYears )
    #axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#  else
#    axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )

  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

  if ( gfx$grids )
  {
    abline( v=seq(xLim[1], xLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
    abline( h=seq(yLim[1], yLim[2],length=10), lty=.GRIDLTY, lwd=.GRIDLWD, col=.GRIDCOL )
  }
  
  # axis( side=4, labels=FALSE )

  abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

  quants <- apply( Ft, 2, quantile, probs=qProbs )

  nQuants <- nrow( quants )
  polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
            density=-1, col=.TULENVCOL, border=FALSE )

  # Plot the quantiles.
  lines( tVec,quants[3,], lwd=3, lty=3 )
  if ( allQuants )
  {
    lines( tVec,quants[2,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
    lines( tVec,quants[4,], col=.TULQCOL, lty=.TULQLTY, lwd=.TULQLWD )
  }

  usr <- par( "usr" )

  box()

  if ( !is.null(Fmsy) && refPts )
  {
    urs <- par( "usr" )
    points( c(usr[1],usr[2]), c(Fmsy,Fmsy), xpd=T, bg=.FmsyBG, col=.FmsyCOL, cex=.FmsyCEX, pch=.FmsyPCH )
  }

  # Check to ensure trace idx numbers do not exceed nReps.
  traces <- traces[ traces <= nReps ]
  if ( length(traces)==0 )
    traces <- 0
  if ( !is.null(traces) && traces!=0.0 )
  {
    for ( i in traces )
      lines( tVec, Ft[i,], col="black", lty=1, lwd=1 )
  }
  return( invisible() )
}     # END function .plotTulipF


# .plotTulipF <- function( obj, traces=iTraces, qProbs=quantVals, allQuants=TRUE,
#                  gfx=list( annotate=TRUE, doLegend=TRUE,
#                  grids=FALSE, showProj=FALSE, xLim=xLim,yLim=yLim ),... )                         
# {
#   #cax <- .TULCAX
  
#   # Get the Fishing Mortality.
#   Ft <- obj$om$legalHR[ ,(2:ncol(obj$om$legalHR)) ]
  
#   Fmsy <- obj$ctlList$refPts$Umsy

#   # Time indices (could use obj$par$nT).
#   tMP   <- obj$ctlList$opMod$tMP
#   nT    <- obj$ctlList$opMod$nT
#   nReps <- obj$ctlList$opMod$nReps
#   tVec  <- c(1:ncol(Ft))

#   xLim <- gfx$xLim
#   yLim <- gfx$yLim

#   # X-axis limits.
#   if ( is.null(xLim) )
#     xLim <- c(1,nT)
#   if ( gfx$showProj )
#     xLim <- c( (tMP - 1),nT )

#   if ( is.null(yLim) )    
#     yLim <- c( 0,max(Ft) )

#   plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

#   mfg <- par( "mfg" )
#   xSeq <- seq( 0,nT,5 )
   
#   # X-axis (bottom): panel is in the last row.
#   if ( mfg[1]==mfg[3] )
#     axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
#   else
#     axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
#   # X-axis (top): panel is in the first row.
#   if ( mfg[1]==1 )
#     axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#   else
#     axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  
    
#   #if ( fixY )
#   #{
#   #  # Y-axis (left): panel is in the first column.
#   #  if ( mfg[2]==1 )
#   #    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
#   #  else
#   #    axis( side=2, labels=FALSE )
      
#   #  # Y-axis (right): panel is in the last column.
#   #  if ( mfg[2]==mfg[4] )
#   #    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
#   #  else
#   #    axis( side=4, labels=FALSE )  
#   #}
#   #else
#     axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

#   if ( gfx$grids )
#   {
#     abline( v=seq( xLim[1], xLim[2],    5), lwd=0.5, col=.COLGRID )
#     abline( h=seq( yLim[1], yLim[2], 0.25), lwd=0.5, col=.COLGRID )
#   }

#   abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

#   quants <- apply( Ft, 2, quantile, probs=qProbs )

#   nQuants <- nrow( quants )
#   polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
#             density=-1, col=.TULCOL, border=FALSE )

#   # Plot the quantiles.
#   lines( tVec,quants[3,], lwd=2, lty=1 )
#   if ( allQuants )
#   {
#     lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
#     lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
#   }

#   usr <- par( "usr" )

#   if ( !is.null(Fmsy) && gfx$annotate )
#   {
#     urs <- par( "usr" )
#     points( usr[2], Fmsy, xpd=T, cex=1.4, pch=16, col="black" )
#   }

#   # Check to ensure trace idx numbers do not exceed nReps.
#   traces <- traces[ traces <= nReps ]
#   if ( length(traces)==0 )
#     traces <- 0
#   if ( !is.null(traces) && traces!=0.0 )
#   {
#     for ( i in traces )
#       lines( tVec, Ft[i,] )
#   }
#   box()  
#   return( invisible() )
# }     # .plotTulipHR

# .plotTulipF <- function( obj, gear=1, label=NULL,
#                   traces=iTraces, qProbs=quantVals, allQuants=TRUE,
#                   failedReps=NULL,
#                   gfx=list( annotate=TRUE, doLegend=TRUE,
#                   doGrid=FALSE, showProj=FALSE, xLim=xLim,yLim=yLim,
#                   useYears=FALSE ),... )                         
# {
#   cax <- .TULCAX
  
#   # Get the legal harvest rate.
#   legalHR <- obj$om$legalHR
  
#   # Determine which reps to remove.
#   if ( is.null(failedReps) )
#   {
#     legalHR <- legalHR[ ,c(2:ncol(legalHR)) ]
#     nFail <- 0
#   }
#   else
#   {
#     isFailed <- is.element( legalHR[,"iRep"], failedReps )
#     nFail <- length(failedReps)
#     legalHR <- legalHR[ !isFailed,c(2:ncol(legalHR)) ]
#   }  
  
#   # ARK (11-Dec-10) Changed Umsy to obj$pars$legalHRFmsy.
#   #legalHRmsy <- obj$pars$Umsy
#   legalHRmsy <- obj$pars$legalHRFmsy

#   # Time indices (could use obj$par$nT).
#   tMP   <- obj$pars$tMP
#   nT    <- obj$pars$nT
#   nReps <- obj$pars$nReps
#   tVec  <- c(1:ncol(legalHR))

#   xLim <- gfx$xLim
#   yLim <- gfx$yLim

#   # X-axis limits.
#   if ( is.null(xLim) )
#     xLim <- c(1,nT)
#   if ( gfx$showProj )
#     xLim <- c( (tMP - 1),nT )

#   if ( is.null(yLim) )    
#     yLim <- c( 0,max(legalHR) )

#   plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

#   mfg <- par( "mfg" )
#   if ( gfx$useYears )
#   { 
#     xPos <- seq( .INITYEAR,.INITYEAR+nT-1, 5 )
#     xSeq <- xPos - .INITYEAR + 1
#     xLabs <- paste( xPos )

#     # X-axis (bottom): panel is in the last row.
#     if ( mfg[1]==mfg[3] )
#       axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=xLabs )
#     else
#       axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
#     # X-axis (top): panel is in the first row.
#     if ( mfg[1]==1 )
#       axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=xLabs )
#     else
#       axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  

#     axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )
#   }
#   else
#   {
#     xSeq <- seq( 0,nT,5 )
   
#     # X-axis (bottom): panel is in the last row.
#     if ( mfg[1]==mfg[3] )
#       axis( side=1, at=xSeq, cex.axis=.CEXAXIS2 )
#     else
#       axis( side=1, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )
    
#     # X-axis (top): panel is in the first row.
#     if ( mfg[1]==1 )
#       axis( side=3, at=xSeq, cex.axis=.CEXAXIS2 )
#     else
#       axis( side=3, at=xSeq, cex.axis=.CEXAXIS2, labels=FALSE )  
#     axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS,... )
#   }
    
#   #if ( fixY )
#   #{
#   #  # Y-axis (left): panel is in the first column.
#   #  if ( mfg[2]==1 )
#   #    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
#   #  else
#   #    axis( side=2, labels=FALSE )
      
#   #  # Y-axis (right): panel is in the last column.
#   #  if ( mfg[2]==mfg[4] )
#   #    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
#   #  else
#   #    axis( side=4, labels=FALSE )  
#   #}
#   #else
#   #  axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )

#   if ( gfx$doGrid )
#   {
#     abline( v=seq( xLim[1], xLim[2],    5), lwd=0.5, col=.COLGRID )
#     abline( h=seq( yLim[1], yLim[2], 0.25), lwd=0.5, col=.COLGRID )
#   }

#   quants <- apply( legalHR, 2, quantile, probs=qProbs )

#   nQuants <- nrow( quants )
#   polygon ( x=c(tVec,rev(tVec)),y=c(quants[1,],rev(quants[nQuants,])),
#             density=-1, col=.TULCOL, border=FALSE )

#   # Plot the quantiles.
#   lines( tVec,quants[3,], lwd=2, lty=1 )
#   if ( allQuants )
#   {
#     lines( tVec,quants[2,], lwd=2, col=.TULQCOL, lty=1 )
#     lines( tVec,quants[4,], lwd=2, col=.TULQCOL, lty=1 )
#   }

#   if ( gfx$annotate )
#   {
#     if ( !is.null(failedReps) )
#       panLab( 0.5, 0.02, cex=0.8,
#         paste( "Removed",length(failedReps),"/",nReps," replicates." ) )
#     if ( !is.null( label ) )
#       panLab( 0.5, 0.9, cex=.CEXLAB, label )        
#   }

#   usr <- par( "usr" )

#   if ( !is.null(legalHRmsy) && gfx$annotate )
#   {
#     urs <- par( "usr" )
#     #points( usr[2], legalHRmsy, xpd=T, cex=1.4, pch=16, col="black" )
#     abline( h=legalHRmsy, lty=.LTYBmsy )
#   }
  
#   abline( v=tMP, col=.tMPCOL, lty=.tMPLTY, lwd=.tMPLWD )

#   # Check to ensure trace idx numbers do not exceed nReps.
#   traces <- traces[ traces <= (nReps-nFail) ]
#   if ( length(traces)==0 )
#     traces <- 0
#   if ( !is.null(traces) && traces!=0.0 )
#   {
#     for ( i in traces )
#       lines( tVec, legalHR[i,] )
#   }
#   box()  
#   return( invisible() )
# }     # .plotTulipHR


.plotFvsSSB <- function( obj, zones=list(zoneLimit=0.4,zoneUpper=0.8),
                         failedReps=NULL,
                         gfx=list( annotate=TRUE, doColors=TRUE, doLegend=TRUE,
                           doGrid=TRUE, showProj=TRUE, xLim=NULL,yLim=NULL ),... )
{
  cax  <- .TULCAX
  xLim <- gfx$xLim
  yLim <- gfx$yLim

  # Time indices (could use obj$par$nT).
  tMP   <- obj$pars$tMP
  nReps <- obj$pars$nReps
  nT    <- obj$pars$nT
  tProj <- c(tMP:nT)

  # Get the spawning biomass and harvest rate on legals.
  Bt      <- obj$om$Bt
  legalHR <- obj$om$legalHR
  
  # Determine which reps to remove.
  if ( is.null(failedReps) )
  {
    Bt <- Bt[ ,c(2:ncol(Bt)) ]
    legalHR <- legalHR[ ,c(2:ncol(legalHR)) ]
    nFail <- 0
  }
  else
  {
    isFailed <- is.element( Bt[,"iRep"], failedReps )
    nFail <- length(failedReps)
    Bt      <- Bt[ !isFailed,c(2:ncol(Bt)) ]
    legalHR <- legalHR[ !isFailed,c(2:ncol(legalHR)) ]
  }  
  
  # Subset the SSB and F from the projection period.
  Bt <- Bt[ ,tProj ]
  legalHR <- legalHR[ ,tProj ]

#  Ft <- Ft[ ,tProj ]
  
  B0   <- obj$pars$B0
  Bmsy <- obj$pars$ssbFmsy
  legUmsy <- obj$pars$legalHRFmsy
#  Fmsy <- obj$pars$Fmsy
  
  # X-axis limits.
  if ( is.null(xLim) )
    xLim <- range( c(0,Bt) )
    
  # Y-axis limits (for projection period).
  if ( is.null(yLim) )
#    yLim <- range( c(0,Ft) )
    yLim <- range( c(0,legalHR) )
  
  plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", ylab="" )
  
  # Stock status zone boundaries.
  zoneLimit <- zones$zoneLimit * Bmsy
  zoneUpper <- zones$zoneUpper * Bmsy

  # Harvest Control Rule reference points.
  #uRef <- upperRefMult * Bmsy
  #lRef <- limitRefMult * Bmsy
    
  # Get the plot region user coordinate limits.  
  usr <- par( "usr" )
    
  # Colour the Critical, Cautious and Healthy Zones.
  # Note these are done based on the boundaries of the Status Zones, not the HCR.
  if ( gfx$doColors )
  {
    rect(         0, 0, zoneLimit, usr[4], border=NA, col=.CRITCOL )
    rect( zoneLimit, 0, zoneUpper, usr[4], border=NA, col=.CAUTCOL )
    rect( zoneUpper, 0,    usr[2], usr[4], border=NA, col=.HEALCOL )
  }
  else
  {
    abline( v=zoneLimit, lty=.LTYDepLrp )
    abline( v=zoneUpper, lty=.LTYDepUsr )
  }
  
  #abline( v=lRef, lty=.HCRLRPLTY,  lwd=.HCRLRPLWD )
  #abline( v=uRef, lty=.HCRUSRLTY,  lwd=.HCRUSRLWD )
  
  for ( i in 1:nrow(Bt) )
    #points( Bt[i,],Ft[i,], pch=21, bg="lightgray", cex=0.6 )
     points( Bt[i,], legalHR[i,], pch=21, bg="lightgray", cex=0.6 )
  
  # Draw the removal rate in the Critical Zone.
  #segments(     0,       0,   lRef,      0, lty=1, lwd=3 )
  # Draw the removal rate in the Cautious Zone.
  #segments(   lRef,      0,   uRef, remRate, lty=1, lwd=3 )
  # Draw the removal rate in the Healthy Zone.
  #segments(   uRef, remRate, usr[2], remRate, lty=1, lwd=3 )

  mfg <- par( "mfg" )
   
  if ( mfg[1]==mfg[3] )
    axis( side=1, cex.axis=.CEXAXIS2 )
  else
    axis( side=1, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==1 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=2, labels=FALSE, las=.YAXISLAS )

  if ( mfg[1]==1 )
    axis( side=3, cex.axis=.CEXAXIS2 )
  else
    axis( side=3, cex.axis=.CEXAXIS2, labels=FALSE )

  if ( mfg[2]==mfg[4] )
    axis( side=4, cex.axis=.CEXAXIS2, las=.YAXISLAS, ... )
  else
    axis( side=4, labels=FALSE, las=.YAXISLAS )

  if ( gfx$doGrid )
  {
    abline( v=seq(xLim[1], xLim[2], 20), lwd=0.5, col="lightblue" )
    abline( h=seq(yLim[1], yLim[2],0.1), lwd=0.5, col="lightblue" )
  }
#  abline( h=Fmsy, lwd=1.5, lty=.REFLTYFMSY )
  abline( v=Bmsy, lwd=1.5, lty=.REFLTYBMSY )
  abline( h=legUmsy, lty=.LTYBmsy )
#  points( Bmsy,Fmsy, pch=21, bg="white", cex=1.5 )

  if ( gfx$annotate )
  {
    if ( !is.null(failedReps) )
      panLab( 0.5, 0.02, cex=0.8,
        paste( "Removed",length(failedReps)," of ",nReps," replicates." ) )   
  }

  if ( gfx$doLegend )
  {
    # Calculate number and proportion in each zone from tMP:nT.
    nZone <- table( cut( Bt, breaks=c(0,zoneLimit,zoneUpper,max(Bt) ) ) )
    pZone <- nZone / length(Bt)
      
    zoneLabels <- c( paste( format(round(pZone[1],2),digits=2,nsmall=2)," Critical",sep=" " ),
                     paste( format(round(pZone[2],2),digits=2,nsmall=2)," Cautious",sep=" " ),
                     paste( format(round(pZone[3],2),digits=2,nsmall=2)," Healthy",sep=" " ) )
    
    if ( gfx$doColor )
    {                 
      panLegend( 0.8,0.4, legTxt=zoneLabels,
                 col=c(1,1,1), pch=c(22,22,22), cex=0.9, pt.cex=2,
                 bg="white", pt.bg=c( .CRITCOL,.CAUTCOL,.HEALCOL ) )
    }
    else
    {
      panLegend( 0.75,0.3, legTxt=zoneLabels, cex=.CEXLAB, bg="white" )
    }                                  
  }

  usr <- par( "usr" )

  box()  
  return( invisible() )
}

.plotQuantDep <- function( obj, period )
{
  # obj: matrix of average depletion values for each replicate by period.
  # period: period definitions.
  
  cax <- .TULCAX

  Dept <- obj[ ,c(8:ncol(obj)) ]

  # If not provided, get the x-axis and y-axis ranges.
  xLim <- c( 1,length(unique(obj$simLabel)) )
  yLim <- c( 0,max(Dept) )

  for ( i in 1:nrow(period) )
  {
     plot( xLim, yLim, type="n", axes=FALSE, xaxs="i", xlab="", ylab="" )
     
     # Extract period data.
     # Plot quantiles by simulation.
     # The zoneLimit, zoneUpper will have to be computed on the depletion
     # scale when repResults is filled, zoneLimit / B0, Bmsy/B0, etc.
     # Then gray boxes at user quantiles plus whiskers at 0.05, 0.95, 0.5.
     # Lay down colors first.
  

     # Get the plot region user coordinate limits.  
     #usr <- par( "usr" )
    
     # Colour the Critical, Cautious and Healthy Zones.
     # Note these are done based on the boundaries of the Status Zones, not the HCR.
     #rect(         0, 0, zoneLimit, usr[4], border=NA, col=.CRITCOL )
     #rect( zoneLimit, 0, zoneUpper, usr[4], border=NA, col=.CAUTCOL )
     #rect( zoneUpper, 0,    usr[2], usr[4], border=NA, col=.HEALCOL )
  }     
}

.plotQboxDep <- function( obj, period, quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                          gfx=list( doLegend=TRUE ) )
{
  # Find the columns named RepX, where X is an integer.
  repNames <- substring(names(obj),1,3)=="Rep"
  
  xLim <- c( (1-0.5),(length(unique(obj$Simulation) )+0.5) )
  
  yLim <- c( 0, max( obj$DepMSY,unlist(obj[,repNames]) ) )
     
  #par( oma=c(2,3,2,2), mar=c(3,2,2,2), mfrow=c(nrow(period),1) )
    
  nReps <- ncol( obj[,repNames] )
     
  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    tmp <- obj[ obj$Period==period$Period[i], ]
    t1  <- period$Year1[ i ]
    t2  <- period$Year2[ i ]
    tdx <- c( t1:t2 )
    
    plot( xLim,yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="" )
  
    # Get the plot region user coordinate limits.  
    usr <- par( "usr" )

    # Loop over the simulations.
    for ( j in 1:nrow(tmp) )
    {
      DepMSY    <- tmp$Bmsy[ j ] / tmp$B0[ j ]
      zoneLimit <- tmp$zoneLimit[ j ] * tmp$Bmsy[ j ] / tmp$B0[ j ]
      zoneUpper <- tmp$zoneUpper[ j ] * tmp$Bmsy[ j ] / tmp$B0[ j ]
         
      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5
         
      # Colour the Critical, Cautious and Healthy Zones.
      # Note these are done based on the Status Zones, not the HCR ref points.
      colorZones <- TRUE
      if ( colorZones )
      {
        rect( x1,         0, x2, zoneLimit, border=NA, col=.CRITCOL )
        rect( x1, zoneLimit, x2, zoneUpper, border=NA, col=.CAUTCOL )
        rect( x1, zoneUpper, x2,    usr[4], border=NA, col=.HEALCOL )
      }
         
      yVals <- as.numeric( tmp[ j,repNames ] )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25
      quantVals <- quantile( yVals, probs=quantProbs )
      rect( j-delta, quantVals[2], j+delta,quantVals[4], border="gray" )
      medVal <- median(yVals)
      segments( j-delta, medVal, j+delta, medVal, col="gray", lty=1, lwd=2 )
      
      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=1.5 )
               
      points( j, DepMSY, pch=21, bg="red", cex=2.0 )
         
      mtext( side=3, line=1, cex=1.0,
        paste( tmp$Period[j]," (",tmp$t1[j],",",tmp$t2[j],")", sep="" ) )
         
    }     # Loop j over simulations.
       
    axis( side=1, at=c(1:nrow(tmp)), labels=tmp$Simulation )
    axis( side=2, las=2, cex.axis=1 )
    axis( side=3, labels=F )
    axis( side=4, las=2, cex.axis=1 ) 
    mtext( side=2, line=3, cex=1.2, outer=TRUE, "Depletion" )

    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.85,0.2, legTxt=c("True","Median(Avg Dep)"),
        pch=c(21,21), pt.bg=c("red","white"), pt.cex=c(2,1.5), bg="white" )
    }
    
  }     # Loop i over periods.     
}     # .plotQboxDep


.plotQboxSSB <- function( obj, period, quantProbs=c(0.05,0.1,0.5,0.9,0.95),
                          gfx=list( doLegend=TRUE ) )
{
  # Find the columns named RepX, where X is an integer.
  repNames <- substring(names(obj),1,3)=="Rep"
  
  xLim <- c( (1-0.5),(length(unique(obj$Simulation) )+0.5) )
  
  yLim <- c( 0, max( obj$ScaledBmsy,unlist(obj[,repNames]) ) )
  
  nReps <- ncol( obj[,repNames] )     
  
  # Loop over the periods.
  for ( i in 1:nrow(period) )
  {
    tmp <- obj[ obj$Period==period$Period[i], ]
    t1  <- period$Year1[ i ]
    t2  <- period$Year2[ i ]
    tdx <- c( t1:t2 )
    
    plot( xLim,yLim, type="n", axes=FALSE, xlab="", xaxs="i", ylab="" )
  
    # Get the plot region user coordinate limits.  
    usr <- par( "usr" )

    for ( j in 1:nrow(tmp) )
    {
      ScaledBmsy <- 1
      zoneLimit <- tmp$zoneLimit[ j ]
      zoneUpper <- tmp$zoneUpper[ j ]
         
      # Each zone has a width of 1 unit: (0.5,1.5, 1.5,2.5, etc... ).
      x1 <- j - 0.5
      x2 <- j + 0.5
         
      # Colour the Critical, Cautious and Healthy Zones.
      # Note these are done based on the Status Zones, not the HCR ref points.
      colorZones <- TRUE
      if ( colorZones )
      {
        rect( x1,         0, x2, zoneLimit, border=NA, col=.CRITCOL )
        rect( x1, zoneLimit, x2, zoneUpper, border=NA, col=.CAUTCOL )
        rect( x1, zoneUpper, x2,    usr[4], border=NA, col=.HEALCOL )
      }
         
      yVals <- as.numeric( tmp[ j, c(11:ncol(tmp) ) ] )

      # Now lay down the median, 25th, and 75th quantiles.
      delta <- 0.25
      quantVals <- quantile( yVals, probs=quantProbs )
      rect( j-delta, quantVals[2], j+delta,quantVals[4], border="gray" )
      medVal <- median(yVals)
      segments( j-delta, medVal, j+delta, medVal, col="gray", lty=1, lwd=2 )
         
      points( (j+jitter( rep(0,length(yVals)), factor=8 )), yVals,
               pch=21, bg="white", cex=1.5 )
               
      points( j, ScaledBmsy, pch=21, bg="red", cex=2.0 )
         
      mtext( side=3, line=1, cex=1.0,
        paste( tmp$Period[j]," (",tmp$t1[j],",",tmp$t2[j],")", sep="" ) )
         
    }     # Loop j over simulations.
       
    axis( side=1, at=c(1:nrow(tmp)), labels=tmp$Simulation )
    axis( side=2, las=2, cex.axis=1 )
    axis( side=3, labels=F )
    axis( side=4, las=2, cex.axis=1 ) 
    mtext( side=2, line=3, cex=1.2, outer=TRUE, "SSB/Bmsy" )

    box()
    
    if ( gfx$doLegend )
    {
      panLegend( 0.8,0.2, legTxt=c("True","Median(Avg SSB/Bmsy)"),
        pch=c(21,21), pt.bg=c("red","white"), pt.cex=c(2,1.5), bg="white" )
    }    
    
  }     # Loop i over periods.     
}     # .plotQboxSSB



.estParPairs <- function( obj, iRep=1, label=NULL, annotate=TRUE )
{
  panel.estPars <- function( x,y,z=stats,... )
  {
    xMean <- mean( x,na.rm=T )
    yMean <- mean( y,na.rm=T )
    points( x,y,pch=16,cex=0.6,col="darkgray" )
    abline( h=yMean,v=xMean,col="blue",lty=3 )

    if ( !is.null(stats) )
    {
      # This is logic to figure out what "pair" is being plotted.
      # The modal estimates are the first row of the mcmcObj.
      # The par()$mfg calls finds the current row and column indices of
      # the panel being plotted.

      xStat <- z[ par()$mfg[2] ]
      yStat <- z[ par()$mfg[1] ]
      points( xStat,yStat, pch=3, cex=3, lwd=2 )
    }
    
    # Now mark the first and last points (green and red?).
    #idx <- seq( 1,length(x),100 )
    #points( x[idx],y[idx], bg="white", cex=1.2, pch=21 )
    #idx <- seq( 100,length(x),100 )
    #points( x[idx], y[idx], bg="red", cex=1.2, pch=21 )
    
    points( xMean,yMean, bg="white", pch=21,cex=2 )    
  }

  panel.hist <- function( x,... )
  {
    # Histograms for diagonal of pairs plot (from PBS Modelling CCA).
    usr <- par("usr")
    on.exit( par(usr) )
    h <- hist( x, breaks="Sturges", plot=FALSE )
    #h <- hist( x, breaks=15, plot=FALSE )
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

  estPars <- obj$mp$assess$estPars
  #estPars <- estPars[ estPars$iRep==iRep, ]
  # Removed Bmsy as per SPC 13-Dec-10.
  
  estPars <- estPars[ c("Fmsy","MSY") ]
  names(estPars) <- c( "Umsy","MSY" )
  
  # Remove B_T as per SPC 13-Dec-10.
  #stepBt <- obj$mp$assess$stepBt
  # Get the last non-missing element of each row.
  #idx <- stepBt[,"tStep"] + 2
  #BT <- rep( NA, length( idx ) )
  #for ( i in 1:length(BT) )
  #  BT[i] <- stepBt[ i,idx[i] ]
    
  #estPars <- cbind( estPars, BT )
  # names( estPars ) <- c( names( estPars ), "B_T" )

  nPars <- ncol( estPars )

  if ( nPars==0 )
  {
    cat( "\nMSG (.estParPairs) No parameters supplied.\n" )
    return()
  }

  #stats <- c( obj$pars$ssbFmsy, obj$pars$legalHRFmsy,obj$pars$landedFmsy )
  stats <- c( obj$pars$legalHRFmsy,obj$pars$landedFmsy )   
  pairs( estPars, panel=panel.estPars, diag.panel=panel.hist, gap=0, cex.axis=1.8 )

  if ( !is.null(label) )
    mtext( side=3, adj=0, line=-0.5, cex=.CEXLAB, outer=T, label )
}     # estParPairs function
