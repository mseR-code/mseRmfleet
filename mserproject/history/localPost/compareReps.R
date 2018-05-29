# compare ISCAM-pbs and ISCAM_ageM rep files

library(RColorBrewer)
library(scales)

source("../../../read.admb.R")
source("../../../mseRtools.r")

PBSrep.SOG <- read.rep("../iscamSOG2017.rep")
ageMrep.SOG <- read.rep("SOG/tvm/SOG_iscam_tvM.rep")

SOG.sbt1 <- PBSrep.SOG$sbt
SOG.sbt2 <- ageMrep.SOG$sbt

PBSrep.WCVI <- read.rep("../iscamWCVI2017.rep")
ageMrep.WCVI <- read.rep("WCVI/tvm/WCVI_iscam_tvM.rep")

WCVI.sbt1 <- PBSrep.WCVI$sbt
WCVI.sbt2 <- ageMrep.WCVI$sbt

pbs.postSB.WCVI <- read.csv( file = "../posterior/wcvi/mcmc/iscam_sbt_mcmc.csv", 
                                  header = TRUE, stringsAsFactors = FALSE )

pbs.postSB.SOG <- read.csv( file = "../posterior/sog/mcmc/iscam_sbt_mcmc.csv", 
                                  header = TRUE, stringsAsFactors = FALSE )

ageM.postSB.WCVI <- read.csv(  file = "WCVI/tvM/mcmcSBt.csv", header = TRUE,
                              stringsAsFactors = FALSE )

ageM.postSB.SOG <- read.csv(  file = "SOG/tvM/mcmcSBt.csv", header = TRUE,
                              stringsAsFactors = FALSE )

pbs.postSB.WCVI <- apply(  X = pbs.postSB.WCVI, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )

pbs.postSB.SOG <- apply(  X = pbs.postSB.SOG, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )

ageM.postSB.WCVI <- apply(  X = ageM.postSB.WCVI, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )
ageM.postSB.SOG <- apply(  X = ageM.postSB.SOG, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )

browser()

yrs <- 1951:2017

pdf( file = "compareMLEbio.pdf" )

par(mfrow = c(2,1), mar = c(2,2,2,2), oma =c(3,3,1,1))

plot( x = c(1951,2017), y = range(pbs.postSB.SOG[,1:67],ageM.postSB.SOG), 
      type = "n", lwd = 2, las = 1, xlab = "Year" )
  polygon( x = c(yrs,rev(yrs)), y = c(pbs.postSB.SOG[1,1:67],rev(pbs.postSB.SOG[3,1:67])),
            border = NA, col = "grey70" )
  polygon( x = c(yrs,rev(yrs)), y = c(ageM.postSB.SOG[1,],rev(ageM.postSB.SOG[3,])),
            border = NA, col = alpha(colour = "red", alpha = .3) )
  lines( x = yrs, y = pbs.postSB.SOG[2,1:67], lwd = 2 )
  lines( x = yrs, y = ageM.postSB.SOG[2,], lwd = 2, lty = 2, col = "red" )
  panLab( x = 0.1, y = 0.8, txt = "SOG" )
  panLegend(  x = 0.6, y = 0.9, bty = "n",
              legTxt = c("ISCAM-pbs", "ISCAM-LFR" ),
              pch = c(22,22), pt.cex = 2, pt.lwd = c(-1,-1),
              pt.bg = alpha(c("black","red"),.3),
              lwd = c(2,2), lty = c(1,2), col = c("black","red"),
              border = NA )


plot( x = c(1951,2017), y = range(pbs.postSB.WCVI[,1:67],ageM.postSB.WCVI), 
      type = "n", lwd = 2, las = 1, xlab = "Year" )
  polygon( x = c(yrs,rev(yrs)), y = c(pbs.postSB.WCVI[1,1:67],rev(pbs.postSB.WCVI[3,1:67])),
            border = NA, col = "grey70" )
  polygon( x = c(yrs,rev(yrs)), y = c(ageM.postSB.WCVI[1,],rev(ageM.postSB.WCVI[3,])),
            border = NA, col = alpha(colour = "red", alpha = .3) )
  lines( x = yrs, y = pbs.postSB.WCVI[2,1:67], lwd = 2 )
  lines( x = yrs, y = ageM.postSB.WCVI[2,], lwd = 2, lty = 2, col = "red" )
  panLab( x = 0.1, y = 0.8, txt = "WCVI" )
  panLegend(  x = 0.6, y = 0.8, bty = "n",
              legTxt = c("ISCAM-pbs", "ISCAM-LFR" ),
              lwd = c(2,2), lty = c(1,2), col = c("black","red") )

dev.off()

pdf( file = "comparePosteriorMt.pdf" )

pbs.postM.WCVI <- read.csv( file = "../posterior/wcvi/mcmc/iscam_m_mcmc.csv", 
                                  header = TRUE, stringsAsFactors = FALSE )

pbs.postM.SOG <- read.csv( file = "../posterior/sog/mcmc/iscam_m_mcmc.csv", 
                                  header = TRUE, stringsAsFactors = FALSE )

ageM.postM.WCVI <- read.csv(  file = "WCVI/tvM/mcmcMt.csv", header = TRUE,
                              stringsAsFactors = FALSE )

ageM.postM.SOG <- read.csv(  file = "SOG/tvM/mcmcMt.csv", header = TRUE,
                              stringsAsFactors = FALSE )

pbs.postM.WCVI <- apply(  X = pbs.postM.WCVI, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )

pbs.postM.SOG <- apply(  X = pbs.postM.SOG, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )

ageM.postM.WCVI <- apply(  X = ageM.postM.WCVI, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )
ageM.postM.SOG <- apply(  X = ageM.postM.SOG, FUN = quantile,
                          MARGIN = 2, probs = c( 0.025, 0.5, 0.975) )


par(mfrow = c(2,1), mar = c(2,2,2,2), oma= c(3,3,1,1) )
plot(x = range(yrs), y = range(pbs.postM.SOG, ageM.postM.SOG), type = "n",
      xlab = "Year", ylab = expression(M[t]), las = 1 )
  polygon( x = c(yrs,rev(yrs)), y = c(pbs.postM.SOG[1,],rev(pbs.postM.SOG[3,])),
            border = NA, col = "grey70" )
  polygon( x = c(yrs,rev(yrs)), y = c(ageM.postM.SOG[1,],rev(ageM.postM.SOG[3,])),
            border = NA, col = alpha(colour = "red", alpha = .3) )
  lines( x = yrs, y = pbs.postM.SOG[2,], lwd = 2 )
  lines( x = yrs, y = ageM.postM.SOG[2,], lwd = 2, lty = 2, col = "red" )
  panLab( x = 0.1, y = 0.8, txt = "SOG" )
  panLegend(  x = 0.6, y = 0.9, bty = "n",
              legTxt = c("ISCAM-pbs", "ISCAM-LFR" ),
              pch = c(22,22), pt.cex = 2, pt.lwd = c(-1,-1),
              pt.bg = alpha(c("black","red"),.3),
              lwd = c(2,2), lty = c(1,2), col = c("black","red"),
              border = NA )

plot(x = range(yrs), y = range(pbs.postM.WCVI, ageM.postM.WCVI), type = "n",
      xlab = "Year", ylab = expression(M[t]), las = 1 )
  polygon( x = c(yrs,rev(yrs)), y = c(pbs.postM.WCVI[1,],rev(pbs.postM.WCVI[3,])),
            border = NA, col = "grey70" )
  polygon( x = c(yrs,rev(yrs)), y = c(ageM.postM.WCVI[1,],rev(ageM.postM.WCVI[3,])),
            border = NA, col = alpha(colour = "red", alpha = .3) )
  lines( x = yrs, y = pbs.postM.WCVI[2,], lwd = 2 )
  lines( x = yrs, y = ageM.postM.WCVI[2,], lwd = 2, lty = 2, col = "red" )
  panLab( x = 0.1, y = 0.8, txt = "WCVI" )

dev.off()