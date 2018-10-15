# Start by loading blob from an RData file
source("mseRtools.r")

# Load blob
library(scales)

plotOMAMcomp <- function(sim = "sim15102018142711")
{
  # load blob 
  blobFile <- paste(sim,".Rdata",sep = "")
  blobPath <- file.path(".","mseRproject",sim,blobFile)

  load(blobPath)

  Bt <- blob$om$Bt[1,2:55]

  repFile <- lisread(blob$ctlList$opMod$repFileName)
  fitBt <- repFile$SSBt

  RE <- (Bt - fitBt)/fitBt

  years <-  1965:2018

  vertLines <- seq( 1970, 2018, by = 10)

  # Plot fit
  par(mfrow = c(2,1), oma = c(3,3,1,1), mar= c(0,1,0,1) )
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

  plot( x = range(years), y = c(-1,1),
        type=  "n", xlab = "", ylab = "", las =1, axes = F )
    abline(v = vertLines, lty =4, lwd = .5, col = "grey70")
    axis( side = 1 )
    axis( side = 2, las = 1)
    box( lwd = 1 )
    mtext(side = 2, text = "Relative Error (%)", line = 3)
    abline(h = 0, lty = 2, lwd = .8)
    points( x = years, y = RE*100, cex = .8, pch = 4) 

}

# age 2
plotOMAMcomp("sim15102018081550")

# age3
plotOMAMcomp("sim15102018142711")