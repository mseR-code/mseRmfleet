library(RColorBrewer)

folder <- file.path("mserproject","sim24072018110543")

repFile <- file.path(folder,"iscam_ageM.rep")
blobFile <- file.path(folder,"sim24072018110543.RData")

source("mseRtools.r")
source("read.admb.R")
rep <- read.rep(repFile)
load(blobFile)

It    <- rep$it
Ityrs <- rep$iyr
sbt   <- rep$sbt
q     <- rep$q

Bt <- blob$om$Bt[1,2:83]

ageData <- rep$A
ageFit  <- rep$Ahat

pointCols <- brewer.pal(n = 4, "Dark2")

# First, plot the CPUE index
pdf("Itplot.pdf", width = 11, height = 4 )
par()
plot( x = range(Ityrs, na.rm = T), y = c(0,max(It,sbt,na.rm=T)), type= "n", 
      xlab = "Year", ylab = "Spawn Index (kt)", las =1 )
  points(x = Ityrs[1,], y = It[1,]/q[1], pch = 16, cex = 1.2, col = pointCols[1] )
  points(x = Ityrs[2,], y = It[2,], pch = 16, cex = 1.2, col = pointCols[2] )
  lines( x = 1951:2032, y = sbt, lty = 3, lwd = .8)
  # lines( x = 1951:2032, y = Bt, lty = 1, lwd = 2, col = "red")
  abline(v = 2017.5, lty = 2, lwd = .8)
  panLegend( x = 0.6, y = 0.97,
              legTxt = c( "Surface Survey",
                          "Dive Survey",
                          # "OM",
                          "AM MLE" ),
              pch = c(16,16,NA), 
              cex = 1.2, 
              col = c(pointCols[1:2],"black"),
              lty = c(NA,NA,3),
              lwd = c(NA,NA,.8),
              bty = "n" )
  mtext( side = 3, text = "WCVI")
dev.off()

# Now plot age observations
# Loop over the three fleets
gears <- 1:3
gearNames <- c("Reduction","Seine-Roe","Gillnet")

for( gIdx in gears )
{
  obsIndices  <- which(ageData[,2] == gIdx)
  obsYears    <- ageData[obsIndices,1]
  nObs        <- length(obsIndices)

  gearAgeFit  <- ageFit[ageFit[,2] == gIdx,]
  
  # Set up a plotting window with five columns
  nRows <- ceiling(nObs/5)

  fileName <- paste(gearNames[gIdx],"ageComps.pdf", sep = "")

  pdf( fileName, width = 11, height = 1.5 * nRows )

  par( mfrow = c(nRows,5), mar = c(1,2,1,1), oma = c(2,2,3,0) )
  for( yrIdx in 1:nObs)
  { 
    # browser()
    yr <- obsYears[yrIdx]
    obsIdx <- obsIndices[yrIdx]

    subAgeData  <- ageData[obsIdx,]

    subAgeFit   <- gearAgeFit[gearAgeFit[,1] == yr ]

    plot( x = c(2,10), y = c(0,1.1*max(subAgeData[3:11],subAgeFit[3:11],na.rm = T)), 
          type = "n", xlab = "", ylab = "", las = 1 )
      panLab( x = 0.12, y = 0.9, txt = yr )
      rect( xleft = 2:10-0.2, xright = 2:10+0.2,
            ybottom = 0, ytop = subAgeData[3:11], border = NA, col = "grey70" )
      # lines(x = 2:10, y = subAgeFit[,3:11] )
      points(x = 2:10, y = subAgeFit[3:11], pch = 16, col = pointCols[4], cex = 1.5 )
  }
  mtext( side = 3, text = gearNames[gIdx], cex = 2, outer =T)
  dev.off()
}