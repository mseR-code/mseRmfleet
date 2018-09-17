# Start by loading blob from an RData file


# Load blob
load("./mseRproject/WCVI_slowUp_HR.1_cap2_Jul18/sim1406201806180116/sim1406201806180116.Rdata")
blob1 <- blob

load("./mseRproject/WCVI_slowUp_HR.1_cap2_Jul18/sim180720180023058/sim180720180023058.Rdata")
blob2 <- blob


library(scales)

Bt1 <- blob1$om$Bt[,2:83]
Bt2 <- blob2$om$Bt[,2:83]

Ct1 <- blob1$om$Ct[,2:83]
Ct2 <- blob2$om$Ct[,2:83]

# pull traces
traces <- sample(1:100, size= 3)


Bt1Quants <- apply(X = Bt1, FUN = quantile, probs = c(0.025, 0.5, 0.975), MARGIN = 2)
Bt2Quants <- apply(X = Bt2, FUN = quantile, probs = c(0.025, 0.5, 0.975), MARGIN = 2)

Ct1Quants <- apply(X = Ct1, FUN = quantile, probs = c(0.025, 0.5, 0.975), MARGIN = 2)
Ct2Quants <- apply(X = Ct2, FUN = quantile, probs = c(0.025, 0.5, 0.975), MARGIN = 2)



par(mfrow = c(2,1), oma = c(3,3,1,1), mar= c(1,1,1,1) )
plot( x = c(2017,2032), y = c(0,max(Bt1,Bt2)), 
      xlab = "", ylab = "", type = "n" )
    polygon(  x = c(1951:2032,2032:1951),
              y = c(Bt1Quants[1,],rev(Bt1Quants[3,]) ),
              border = NA, col = alpha("grey40",alpha=.6) )
    lines(x = 1951:2032, y = Bt1Quants[2,], lwd = 2, lty = 1)
    polygon(  x = c(1951:2032,2032:1951),
              y = c(Bt2Quants[1,],rev(Bt2Quants[3,]) ),
              border = NA, col = alpha("red",alpha=.3) )
    lines(x = 1951:2032, y = Bt2Quants[2,], lwd = 2, col = "red", lty = 1)
    for(tIdx in 1:length(traces))
    {
      lines(x = 1951:2032, y = Bt1[traces[tIdx],], lwd = .8, col = "black", lty = tIdx + 3)
      lines(x = 1951:2032, y = Bt2[traces[tIdx],], lwd = .8, col = "red", lty = tIdx + 3)
    }
    abline(v = 2018, lty = 3, lwd = .8 )
    panLegend(  x = 0.05, y = 0.95, 
                legTxt = c( blob1$ctlList$gui$mpLabel,
                            blob2$ctlList$gui$mpLabel),
                pch = 15, 
                col = c(alpha("grey40",0.6),alpha("red",0.3)), 
                bty = "n", cex = 2 )

plot( x = c(2017,2032), y = c(0,2.8), 
      xlab = "", ylab = "", type = "n" )
    polygon(  x = c(1951:2032,2032:1951),
              y = c(Ct1Quants[1,],rev(Ct1Quants[3,]) ),
              border = NA, col = alpha("grey40",alpha=.6) )
    lines(x = 1951:2032, y = Ct1Quants[2,], lwd = 2, lty = 1)
    polygon(  x = c(1951:2032,2032:1951),
              y = c(Ct2Quants[1,],rev(Ct2Quants[3,]) ),
              border = NA, col = alpha("red",alpha=.3) )
    lines(x = 1951:2032, y = Ct2Quants[2,], lwd = 2, lty = 1, col = "red" )
     for(tIdx in 1:length(traces))
    {
      lines(x = 1951:2032, y = Ct1[traces[tIdx],], lwd = .8, col = "black", lty = tIdx + 3)
      lines(x = 1951:2032, y = Ct2[traces[tIdx],], lwd = .8, col = "red", lty = tIdx + 3)
    }
   
    abline( v = 2018, lty = 3, lwd = .8 )

