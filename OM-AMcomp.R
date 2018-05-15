# Start by loading blob from an RData file
# Load SOG
load("./mseRproject/sim15052018123526/sim15052018123526.Rdata")
SOGblob <- blob
# # Load ISCAM F condition
# load("./mserproject/sim02042018143843/sim02042018143843.RData")
# SOGblob_iscamF <- blob

# # Newblob
# load("./mserproject/sim09042018130105/sim09042018130105.RData")
# SOGblob_new <- blob

# # Fixed Sel
# load("./mserproject/sim09042018130630/sim09042018130630.RData")
# SOGblob_fix <- blob


# load packages
library(RColorBrewer)

# Blob should contain the rep file for the assessment Bt
# SOGrep  <- SOGblob$ctlList$opMod$repFile
# SOGom 	<- SOGblob$om
# SOGam 	<- SOGblob$assess

SOGrep   <- SOGblob$ctlList$opMod$repFile
SOGom    <- SOGblob$om
# sel_om    <- SOGblob_new$om
# fix_om    <- SOGblob_fix$om


# Model dimensions
tMP <- blob$ctlList$opMod$tMP
nT 	<- blob$ctlList$opMod$nT

# Pull out AM and OM Bt
# SOGamBt <- SOGrep$sbt[1:(tMP - 1)]
# SOGomBt <- SOGom$Bt[1,2:(tMP)] # offset by 1 for rep label

SOGamBt  <- SOGrep$sbt[1:(tMP - 1)]
SOGomBt  <- SOGom$SBt[1,2:(tMP)] # offset by 1 for rep label
# # SOGomBtF <- ISCAMF_om$Bt[1,2:(tMP)] # offset by 1 for rep label
# SOGomBtS <- sel_om$Bt[1,2:tMP]
# SOGomBtf <- fix_om$Bt[1,2:tMP]

SOGamFt  <- SOGrep$ft[1:3,1:(tMP - 1)]
SOGomFt  <- SOGom$Ftg[1,1:(tMP-1),1:3] 
# SOGomFtF <- ISCAMF_om$Ftg[1,1:(tMP-1),1:3] 
# SOGomFtS <- sel_om$Ftg[1,1:(tMP-1),1:3] 
# SOGomFtf <- fix_om$Ftg[1,1:(tMP-1),1:3] 


# Calculate total fishing mortality at age?


# Plot colours
plotCols <- brewer.pal(n=3, name = "Dark2" )

amCol <- plotCols[1]
omCol <- plotCols[2]
# fCol <- NA #plotCols[3]
# sCol <- plotCols[4]
# fixCol <- plotCols[2]

years <- SOGrep$yr


par(mfrow = c(4,1), mar = c(1,1,1,1), oma = c(2,3,1,1))

plot (  x = 1, y=1, xlim = range(years), ylim = c(0,max(SOGomBt, SOGamBt)), 
        type = "n", xlab = "", 
        ylab = "", las = 1, axes = T )
  lines( x = years, y = SOGamBt, col = amCol, lwd = 3 )
  lines( x = years, y = SOGomBt, col = omCol, lwd = 3 )
  panLegend( x = 0.1, y = 0.9,
              legTxt = c("ISCAM", "mseR"),
              col = c(amCol,fixCol),
              lwd = 3, bty = "n" )
  mtext( side = 2, text = "SOG biomass (kt)", line = 2 )

gears <- c("Reduction/F+B","Seine-Roe","Gillnet")

for( g in 1:3 )
{
  plot (  x = 1, y=1, xlim = range(years), 
          ylim = c(0,max(SOGomFt[,g], SOGamFt[,g])), 
          type = "n", xlab = "Year", 
          ylab = "SOG biomass (kt)", las = 1, axes = T )
  lines( x = years, y = SOGamFt[g,], col = amCol, lwd = 3 )
  lines( x = years, y = SOGomFt[,g], col = omCol, lwd = 3 )
  panLab( x=.8, y = .7, txt = gears[g])
}
  mtext( side = 1, text = "Year", outer = T )