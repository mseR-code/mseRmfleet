# Start by loading blob from an RData file
# Load blob
load("./mseRproject/sim050620181111177/sim050620181111177.Rdata")

Bt              <- blob$om$SBt
Bt              <- Bt[,2:ncol(Bt)]
posteriorDraws  <- blob$ctlList$opMod$posteriorDraws
SBtMCMCpath     <- file.path(blob$ctlList$opMod$posteriorSamples,"mcmcSBt.csv")
SBtMCMC         <- read.csv( SBtMCMCpath, header = T )[posteriorDraws,]

MREbt <- as.matrix((Bt - SBtMCMC)/SBtMCMC)

MREbt_Dist <- apply(  X = MREbt, FUN = quantile, MARGIN = 2,
                      probs = c(0.1, 0.5, 0.9) )

plot( x = c(1951,2017), y = c(-1,1), 
      xlab = "", ylab = "", type = "n" )
  polygon(  x = c(1951:2017,2017:1951),
            y = c(MREbt_Dist[1,],rev(MREbt_Dist[3,]) ),
            border = NA, col = "grey40" )
  lines( x = 1951:2017, y = MREbt_Dist[2,], lwd = 2 )
  


# load packages
library(RColorBrewer)

# Blob should contain the rep file for the assessment Bt
# SOGrep  <- SOGblob$ctlList$opMod$repFile
# SOGom 	<- SOGblob$om
# SOGam 	<- SOGblob$assess

WCVIrep   <- blob$ctlList$opMod$repFile
WCVIom    <- blob$om


# Model dimensions
tMP <- blob$ctlList$opMod$tMP
nT 	<- blob$ctlList$opMod$nT

# Pull out AM and OM Bt
# SOGamBt <- SOGrep$sbt[1:(tMP - 1)]
# SOGomBt <- SOGom$Bt[1,2:(tMP)] # offset by 1 for rep label

# WCVIamBt  <- WCVIrep$sbt[1:(tMP - 1)]
# WCVIomBt  <- WCVIom$Bt[1,2:(tMP)] # offset by 1 for rep label
# WCVIomBtF <- ISCAMF_om$Bt[1,2:(tMP)] # offset by 1 for rep label
# WCVIomBtS <- sel_om$Bt[1,2:tMP]
# WCVIomBtf <- fix_om$Bt[1,2:tMP]

# WCVIamFt  <- WCVIrep$ft[1:3,1:(tMP - 1)]
# WCVIomFt  <- WCVIom$Ftg[1,1:(tMP-1),1:3] 
# WCVIomFtF <- ISCAMF_om$Ftg[1,1:(tMP-1),1:3] 
# WCVIomFtS <- sel_om$Ftg[1,1:(tMP-1),1:3] 
# WCVIomFtf <- fix_om$Ftg[1,1:(tMP-1),1:3] 


# # Calculate total fishing mortality at age?


# # Plot colours
# plotCols <- brewer.pal(n=3, name = "Dark2" )

# amCol <- plotCols[1]
# # omCol <- NA #plotCols[2]
# # fCol <- NA #plotCols[3]
# # sCol <- plotCols[4]
# fixCol <- plotCols[2]

# years <- WCVIrep$yr


# # plot ( 	x = 1, y=1, xlim = range(years), ylim = c(0,max(omBt, amBt)), 
# #         type = "n", xlab = "Year", 
# # 				ylab = "", las = 1, axes = F )
# # 	axis( side = 1 )
# # 	axis( side = 2 )
# # 	lines( x = years, y = SOGamBt, col = amCol, lwd = 3 )
# # 	lines( x = years, y = SOGomBt, col = omCol, lwd = 3 )
# #   panLegend(  x = 0.1, y = 0.95, legTxt = c("ISCAM 2015", "OM" ),
# #               col = c( amCol, omCol ), lwd = 3, bty = "n")

# par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(2,3,1,1))

# plot (  x = 1, y=1, xlim = range(years), ylim = c(0,max(WCVIomBt, WCVIamBt)), 
#         type = "n", xlab = "", 
#         ylab = "", las = 1, axes = T )
#   lines( x = years, y = WCVIamBt, col = amCol, lwd = 3 )
#   # lines( x = years, y = WCVIomBt, col = omCol, lwd = 3 )
#   # lines( x = years, y = WCVIomBtF, col = fCol, lwd = 3 )
#   # lines( x = years, y = WCVIomBtS, col = sCol, lwd = 3 )
#   lines( x = years, y = WCVIomBtf, col = fixCol, lwd = 3 )
#   panLegend( x = 0.1, y = 0.9,
#               legTxt = c("ISCAM", "mseR"),
#               col = c(amCol,fixCol),
#               lwd = 3, bty = "n" )
#   mtext( side = 2, text = "WCVI biomass (kt)", line = 2 )

# gears <- c("Reduction/F+B","Seine-Roe","Gillnet")

# for( g in 1:3 )
# {
#   plot (  x = 1, y=1, xlim = range(years), 
#           ylim = c(0,max(WCVIomFt[,g], WCVIamFt[,g], WCVIomFtF[,g])), 
#           type = "n", xlab = "Year", 
#           ylab = "WCVI biomass (kt)", las = 1, axes = T )
#   lines( x = years, y = WCVIamFt[g,], col = amCol, lwd = 3 )
#   # lines( x = years, y = WCVIomFt[,g], col = omCol, lwd = 3 )
#   # lines( x = years, y = WCVIomFtF[,g], col = fCol, lwd = 3 )
#   # lines( x = years, y = WCVIomFtS[,g], col = sCol, lwd = 3 )
#   lines( x = years, y = WCVIomFtf[,g], col = fixCol, lwd = 3 )
#   panLab( x=.8, y = .7, txt = gears[g])
# }
#   mtext( side = 1, text = "Year", outer = T )