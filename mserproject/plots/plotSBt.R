# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")

gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
          showProj=TRUE, xLim=NULL, yLim=NULL, useYears=TRUE )

# List sims
sims <- list.files("../")
sims <- sims[grepl("sim",sims)]

readInfoFile <- function( sim )
{
  infoPath <- file.path("..",sim,paste(sim, ".info", sep = "") ) 
  info <- lisread(infoPath)
  info.df <- as.data.frame(info)
  info.df$simLabel <- sim

  info.df
}

# Read in info files, sort by  scenarios
info.df <- lapply( X = sims, FUN = readInfoFile )
info.df <- do.call( "rbind", info.df )

simID <- info.df$simLabel[10]
simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
load(simPath)

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

SBt <- blob$om$SBt[1,2:(nT+1)]
Bt  <- blob$om$Bt[1,2:(nT+1)]

Mt <- blob$om$Mt[1,2:(nT+1)]

if( !is.null(blob$om$FBt) )
{
  FBt <- blob$om$FBt[1,2:(nT+1)]
} else 
  FBt <- Bt * exp(-Mt)

colPal <- brewer.pal(3, "Dark2")

dev.new()
plot( x = yrs, y = Bt, type = "l", xlab = "",
      ylab = "SSB (kt)", lwd = 3, ylim = c(0,max(Bt)),
      col = colPal[1] )
  lines( yrs, SBt, col = colPal[2], lwd = 3 )
  lines( yrs, FBt, col = colPal[3], lwd = 3 )
  panLegend(  x = .6, y = .9, bty = "n",
              col = colPal,
              legTxt = c("Start of Year SSB",
                        "Post M, pre F SSB",
                        "Post M+F SSB"),
              lwd = 3 )

dev.new()
par(mfcol = c(2,2), mar = c(1,1.5,1,1.5), oma = c(3,3,1,1))
.plotTulipDepCat(blob, gfx = gfx, yLimD = c(0,1), yLimC = c(0,10), refPts = F )

.FBt_Perf <<- FALSE
source("../../mseRoptions.R")
gfx$doLegend <- FALSE
.plotTulipDepCat(blob, gfx = gfx, yLimD = c(0,1), yLimC = c(0,10), refPts = F )
