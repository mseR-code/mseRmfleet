# Start by loading blob from an RData file
# Load WCVI
load("./mserproject/sim280920171116271/sim280920171116271.RData")
WCVIblob <- blob
# Load SOG
load("./mserproject/sim280920171301123/sim280920171301123.RData")
SOGblob <- blob

# load packages
library(RColorBrewer)

# Blob should contain the rep file for the assessment Bt
SOGrep  <- SOGblob$ctlList$opMod$repFile
SOGom 	<- SOGblob$om
SOGam 	<- SOGblob$assess

WCVIrep   <- WCVIblob$ctlList$opMod$repFile
WCVIom    <- WCVIblob$om
WCVIam    <- WCVIblob$assess

# Model dimensions
tMP <- blob$ctlList$opMod$tMP
nT 	<- blob$ctlList$opMod$nT

# Pull out AM and OM Bt
SOGamBt <- SOGrep$sbt[1:(tMP - 1)]
SOGomBt <- SOGom$Bt[1,2:(tMP)] # offset by 1 for rep label

WCVIamBt <- WCVIrep$sbt[1:(tMP - 1)]
WCVIomBt <- WCVIom$Bt[1,2:(tMP)] # offset by 1 for rep label

# Plot colours
plotCols <- brewer.pal(n=3, name = "Dark2" )

amCol <- plotCols[1]
omCol <- plotCols[2]

years <- rep$yr

par(mfrow = c(2,1), mar = c(1,1,1,1), oma = c(2,3,1,1))
plot ( 	x = 1, y=1, xlim = range(years), ylim = c(0,max(omBt, amBt)), 
        type = "n", xlab = "Year", 
				ylab = "", las = 1, axes = F )
	axis( side = 1 )
	axis( side = 2 )
	lines( x = years, y = SOGamBt, col = amCol, lwd = 3 )
	lines( x = years, y = SOGomBt, col = omCol, lwd = 3 )
  panLegend(  x = 0.1, y = 0.95, legTxt = c("ISCAM 2015", "OM" ),
              col = c( amCol, omCol ), lwd = 3, bty = "n")

plot (  x = 1, y=1, xlim = range(years), ylim = c(0,max(omBt, amBt)), 
        type = "n", xlab = "", 
        ylab = "", las = 1, axes = F )
  axis( side = 1 )
  axis( side = 2 )
  lines( x = years, y = WCVIamBt, col = amCol, lwd = 3 )
  lines( x = years, y = WCVIomBt, col = omCol, lwd = 3 )

mtext(side = 2, outer = T, text = "Biomass (kt)", line = 2 )