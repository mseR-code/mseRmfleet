# Function to plot DFO harvest policy in 3 panels:
# (a) Objectives
# (b) Harvest control rule
# (c) Risk probabilities.

.plotObjectives <- function( B0=100, target=30, lrpMult=0.4, usrMult=0.8,
                             lowProb=0.05, hiProb=0.5,
                             xLim=NULL, yLim=NULL )
{
  if ( is.null(xLim) )
    xLim <- c( 0,B0 )
    
  if ( is.null(yLim) )
    yLim <- c(0,1)
  
  LRP <- lrpMult * target
  USR <- usrMult * target
  
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  usr <- par( "usr" )
  
  rect( usr[1],usr[3],LRP, usr[4], border=NA, col="gray80" )
  rect( LRP,usr[1],USR,    usr[4], border=NA, col="gray90" )
  rect( USR,usr[3],usr[2], usr[4], border=NA, col="gray100" )
  
  #rect( usr[1],usr[3],LRP, usr[4], border=NA, col="pink" )
  #rect( LRP,usr[1],USR,    usr[4], border=NA, col="yellow" )
  #rect( USR,usr[3],usr[2], usr[4], border=NA, col="green" ) 
  
  abline( v=LRP, lty=2 )
  abline( v=USR, lty=2 )
  abline( v=target, lty=5, lwd=2 )
  
  #text( LRP,   0.8, cex=1.4, "LRP" )
  #text( USR,   0.8, cex=1.4, "USR" )
  #text( target,0.8, cex=1.4, "Target" )
  
  text( LRP/2, 0.9,              cex=1.5, "Critical" )
  text( LRP+(USR-LRP)/2, 0.9,    cex=1.5, "Cautious" )
  text( USR+(xLim[2]-USR)/2,0.9, cex=1.5, "Healthy" )
  
  LRP <- lrpMult * target
  USR <- usrMult * target
  SSB <- seq( 0,B0,1 )
    
  #abline( v=LRP, lty=2 )
  #abline( v=USR, lty=2 )
  #abline( v=target, lty=4 )

  # Scaled "risk" probability.
  pDecline <- lowProb + (hiProb-lowProb) * (SSB-LRP)/(target-LRP)
  
  pDecline <- ifelse( SSB < LRP, 0.05, pDecline )
  pDecline <- ifelse( SSB > target, 0.5,  pDecline )
  
  lines( SSB, pDecline, lty=2, lwd=4, col="blue" )
  
  #text( (USR-LRP)/2+LRP+2, 0.35, "Acceptable Probability of Decline", cex=1.3, srt=40, col="red" )
  
  #points( LRP, lowProb, pch=16 )
  #points( target,0.5,   pch=16 )
  
  #text( LRP,    lowProb, cex=1.5, pos=1, offset=0.5,
  #      paste( "(P=",lowProb,")" ) )
  #text( target, hiProb,  cex=1.5, pos=2, offset=1,
  #      paste( "(P=",hiProb,")",sep="" ) )
  
  pUSR <- lowProb + (hiProb-lowProb) * ( USR-LRP)/(target-LRP)
  #text( USR,    pUSR,    cex=1.5, pos=2, offset=1,
  #      paste( "(P=",pUSR,")",sep="" ) )  
  
  axis( side=1, cex=1.0, labels=FALSE )
  axis( side=2, cex.axis=1.4, las=2 )
  box()
  
  mtext( side=1, line=1.0, cex=1.4, "True Stock Status" )
  mtext( side=2, line=4,   cex=1.4, "Probability"  )
  mtext (side=3, line=0.5, cex=1.6, "Objectives" )
}

.plotHCR <- function( B0=100, target=30, lbMult=0.4, ubMult=0.6, remRate=0.6,
                      xLim=NULL, yLim=NULL )
{
  if ( is.null(xLim) )
    xLim <- c( 0,B0 )

  if ( is.null(yLim) )
    yLim <- c(0,1)

  lb <- lbMult * target
  ub <- ubMult * target

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )
  
  segments( 0,0, lb,0,       lty=1, lwd=2 )
  segments( lb,0,ub,remRate, lty=1, lwd=2 )
  segments( ub, remRate, xLim[2], remRate, lty=1, lwd=2 )

  abline( v=lb, lty=2 )
  abline( v=ub, lty=2 )
  abline( v=target, lty=4 )

  text( lb,     0.95, cex=1.4, "LB" )
  text( ub,     0.95, cex=1.4, "UB" )
  text( target, 0.95, cex=1.4, "Estimated\n Target" )

  axis( side=1, cex=1.0, labels=FALSE )
  axis( side=2, cex=1.0, labels=FALSE, las=2 )
  box()

  mtext( side=1, line=1.0, cex=1.4, "Estimated Stock Status" )
  mtext( side=2, line=4, cex=1.4, "Estimated Removal Rate" )
  mtext( side=3, line=0.5, cex=1.6, "Harvest Control Rule" )
}

.plotPrDecline <- function( B0=100, target=30, lrpMult=0.4, usrMult=0.8,
                           lowProb=0.05, hiProb=0.5, xLim=NULL, yLim=NULL )
{
  if ( is.null(xLim) )
    xLim <- c(0,B0 )
  if ( is.null(yLim) )
    yLim <- c(0,1)

  LRP <- lrpMult * target
  USR <- usrMult * target
  SSB <- seq( 0,B0,1 )
    
  plot( xLim, yLim, type="n", axes=FALSE, xlab="", ylab="" )

  abline( v=LRP, lty=2 )
  abline( v=USR, lty=2 )
  abline( v=target, lty=4 )

  # Scaled "risk" probability.
  pDecline <- lowProb + (hiProb-lowProb) * (SSB-LRP)/(target-LRP)
  
  pDecline <- ifelse( SSB < LRP, 0.05, pDecline )
  pDecline <- ifelse( SSB > target, 0.5,  pDecline )
  
  lines( SSB, pDecline, lty=1, lwd=2 )
  
  #points( LRP, lowProb, pch=16 )
  #points( target,0.5,   pch=16 )
  
  text( LRP,    lowProb, cex=1.75, pos=1, offset=0.5,
        paste( "(LRP, P=",lowProb,")" ) )
  text( target, hiProb,  cex=1.75, pos=2, offset=1,
        paste( "(Target, P=",hiProb,")",sep="" ) )
  
  pUSR <- lowProb + (hiProb-lowProb) * ( USR-LRP)/(target-LRP)
  text( USR,    pUSR,    cex=1.75, pos=2, offset=1,
        paste( "(USR, P=",pUSR,")",sep="" ) )
  
  axis( side=1, cex.axis=1.2, labels=FALSE )
  axis( side=2, cex.axis=1.6, las=2 )
  box()
  
  mtext( side=1, line=1.5, cex=1.2, "Stock Status" )
  mtext( side=2, line=4,   cex=1.2, "Acceptable P(decline)"  )
}

.plotMgmt <- function()
{
  par( oma=c(1,1,1,1), mar=c(3,5,2,1), mfrow=c(1,1) )
  
  .plotObjectives( B0=100, target=30, lrpMult=0.4, usrMult=0.8, xLim=c(0,60) )
  scan()
  
#  .plotPrDecline( B0=100, target=30, lrpMult=0.4, usrMult=0.8, lowProb=0.05,
#                 hiProb=0.5, xLim=c(0,60), yLim=c(0,0.55) )
  
  .plotHCR( B0=100, target=35, lbMult=0.4, ubMult=0.6, remRate=0.6, xLim=c(0,60) )

  par( mfrow=c(1,1) )
}

.plotMgmt()