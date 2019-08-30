# Start by loading blob from an RData file
source("mseRtools.r")

# Load blob
load("./mseRproject/sim30082019082807/sim30082019082807.Rdata")

library(scales)

Bt <- blob$om$Bt[2,2:55]

repFile <- blob$ctlList$opMod$repFile
fitBt <- repFile$SSBt

RE <- (Bt - fitBt)/fitBt

years <-  1965:2018

vertLines <- seq( 1970, 2018, by = 10)

# Plot fit
par(mfrow = c(2,1), oma = c(3,3,4,1), mar= c(0,1,0,1) )
plot( x = c(1965,2018), y = c(0,max(Bt,fitBt)),
      type= "n", xlab = "", ylab = "", las = 1, axes =F )
  abline(v = vertLines, lty =4, lwd = .5, col = "grey70")
  axis( side = 2, las =1)
  box( lwd = 1)
  mtext( side = 2, text= "Biomass (kt)" , line = 3)
  lines( x = years, y = Bt, col = "red", lwd = 2)
  lines( x = years, y = fitBt, col = "black", lwd = 2, lty = 2 )
  legend( "topright", bty= "n",
          lty = c(1,2), lwd = c(2,2),
          col = c("red","black"),
          legend = c("OM Reconstruction","ADMB rep file"))

plot( x = range(years), y = c(-5,5),
      type=  "n", xlab = "", ylab = "", las =1, axes = F )
  abline(v = vertLines, lty =4, lwd = .5, col = "grey70")
  axis( side = 1 )
  axis( side = 2, las = 1)
  box( lwd = 1 )
  mtext(side = 2, text = "Relative Error (%)", line = 3)
  abline(h = 0, lty = 2, lwd = .8)
  points( x = years, y = RE*100, cex = .8, pch = 4) 


# plot( x = c(2017,2032), y = c(0,max(Bt1,Bt2)), 
#       xlab = "", ylab = "", type = "n" )
#     polygon(  x = c(1951:2032,2032:1951),
#               y = c(Bt1Quants[1,],rev(Bt1Quants[3,]) ),
#               border = NA, col = alpha("grey40",alpha=.6) )
#     lines(x = 1951:2032, y = Bt1Quants[2,], lwd = 2, lty = 1)
#     polygon(  x = c(1951:2032,2032:1951),
#               y = c(Bt2Quants[1,],rev(Bt2Quants[3,]) ),
#               border = NA, col = alpha("red",alpha=.3) )
#     lines(x = 1951:2032, y = Bt2Quants[2,], lwd = 2, col = "red", lty = 1)
#     for(tIdx in 1:length(traces))
#     {
#       lines(x = 1951:2032, y = Bt1[traces[tIdx],], lwd = .8, col = "black", lty = tIdx + 3)
#       lines(x = 1951:2032, y = Bt2[traces[tIdx],], lwd = .8, col = "red", lty = tIdx + 3)
#     }
#     abline(v = 2018, lty = 3, lwd = .8 )
#     panLegend(  x = 0.05, y = 0.95, 
#                 legTxt = c( blob1$ctlList$gui$mpLabel,
#                             blob2$ctlList$gui$mpLabel),
#                 pch = 15, 
#                 col = c(alpha("grey40",0.6),alpha("red",0.3)), 
#                 bty = "n", cex = 2 )

# plot( x = c(2017,2032), y = c(0,2.8), 
#       xlab = "", ylab = "", type = "n" )
#     polygon(  x = c(1951:2032,2032:1951),
#               y = c(Ct1Quants[1,],rev(Ct1Quants[3,]) ),
#               border = NA, col = alpha("grey40",alpha=.6) )
#     lines(x = 1951:2032, y = Ct1Quants[2,], lwd = 2, lty = 1)
#     polygon(  x = c(1951:2032,2032:1951),
#               y = c(Ct2Quants[1,],rev(Ct2Quants[3,]) ),
#               border = NA, col = alpha("red",alpha=.3) )
#     lines(x = 1951:2032, y = Ct2Quants[2,], lwd = 2, lty = 1, col = "red" )
#      for(tIdx in 1:length(traces))
#     {
#       lines(x = 1951:2032, y = Ct1[traces[tIdx],], lwd = .8, col = "black", lty = tIdx + 3)
#       lines(x = 1951:2032, y = Ct2[traces[tIdx],], lwd = .8, col = "red", lty = tIdx + 3)
#     }
   
#     abline( v = 2018, lty = 3, lwd = .8 )

