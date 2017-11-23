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
    mfRow <- c(1,1)
    # HACK: ARK 25-Nov-09
    if ( nGear==2 )
      mfRow <- c(2,1)
    else if ( nGear==3 )
      mfRow <- c(3,1)
    else if ( nGear > 3 )
      mfRow <- c(2,2)
  
    par( oma=.OMA, mar=.MAR, mfrow=mfRow )
    
    for ( g in 1:nGear  )
    {
      plot( xRange,yRange,type="n",axes=F,xlab="",ylab="" )
      lines( len,Plg[,g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )
      
      #if ( gfx$annotate )
        abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )
    
      if ( gfx$annotate )
        panLab( 0.8,0.90, adj=0, cex=1.2,
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
      lines( len,Plg[,g], col=.COLPlg[g], lty=.LTYPlg[g], lwd=.LWDPlg[g] )

    if ( gfx$annotate )
      abline( v=sizeLim, col=.COLSIZE, lty=.LTYSIZE, lwd=.LWDSIZE )
    
    axis( side=1, cex.axis=.CEXAXIS2 )
    axis( side=2, cex.axis=.CEXAXIS2, las=.YAXISLAS )
    box()
      
    if ( gfx$doLegend )
    {
      tmp <- paste( obj$gNames, "(dg =",round( obj$dg, digits=3 ),")" )
      panLegend( 0.6,0.95, legTxt=tmp, bg="white",
                 col=.COLPlg, lty=.LTYPlg, lwd=.LWDPlg, pt.cex=1.2 )    
    }
  }

  mtext( side=1, line=.OUTLINE1, cex=.CEXLAB, outer=TRUE, "Length" )
  mtext( side=2, line=.OUTLINE2, cex=.CEXLAB, outer=TRUE, "Proportion Discarded at Length" )
}


# CIC talk - June 30, 2010.
result <- list()
result$dg    <- 1
result$nGear <- 1
result$L50Dg <- 57
result$L95Dg <- 50
result$sizeLim <- 55
result$Lal <- 30

graphics.off()

# CIC talk - Sept 21, 2010 "Perfect".
result <- list()
result$dg    <- 1
result$nGear <- 1
result$L50Dg <- 53
result$L95Dg <- 50
result$sizeLim <- 55
result$Lal <- 30

.plotPlg( result, gfx=list( autolayout=FALSE, annotate=FALSE, bygear=TRUE,
          doLegend=FALSE, xLim=c(30,80), yLim=c(0.0,1.0) ) )
          
dev.new()
          
# CIC talk - Sept 21, 2010 "Optimist".
result <- list()
result$dg    <- 1
result$nGear <- 1
result$L50Dg <- 59
result$L95Dg <- 54.5
result$sizeLim <- 55
result$Lal <- 30

.plotPlg( result, gfx=list( autolayout=FALSE, annotate=FALSE, bygear=TRUE,
          doLegend=FALSE, xLim=c(30,80), yLim=c(0.0,1.0) ) )
