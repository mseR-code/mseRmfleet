# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)
require(dplyr)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")
source("../../mseRstats.R")

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

qProbs <- c( 0.025, 0.25, 0.5, 0.75, 0.975 )
short <- c(68,73)
med   <- c(74,86)
long  <- c(87,92)

# Read in info files, sort by  scenarios
info.df <- lapply( X = sims, FUN = readInfoFile )
info.df <- do.call( "rbind", info.df )

scenarios <- unique( info.df$scenarioLabel )
MPs       <- unique( info.df$mpLabel )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

for( scenIdx in 1:length(scenarios) )
{
  scen <- scenarios[scenIdx]

  clevelandPlots  <- paste(scen, "Cleveland.pdf", sep = "" )

  pdf( file = clevelandPlots, width = 18, height = 6 )
  par( mfcol = c(2,9), mar = c(1,1.5,1,1.5), oma = c(3,3,4,1))

  for( mpIdx in 1:length(MPs) )
  {
    mp <- MPs[mpIdx]
    info.df.sub <-  info.df %>%
                    filter( scenarioLabel == scen,
                            mpLabel == mp )

    simID     <- info.df.sub$simLabel
    simPath  <- file.path("..",simID,paste(simID, ".RData", sep = "") )
    load(simPath)

    if(mpIdx == 1) gfx$doLegend <- TRUE
    else gfx$doLegend <- FALSE

    .plotTulipDepCat( blob, gfx = gfx, yLimD = c(0,1), yLimC = c(0,10),
                      refPts = FALSE )
  }

  mtext( side = 3, outer = T, text = scen, cex = 1.3, line = 2.5)

  dev.off()

}


 # Now do cleveland plots
  pdf( file = clevelandPlots, width = 5, height = 5 )
  
  blobList <- vector(length = length(MPs), mode = "list" )
  names(blobList) <- MPs
  for( mpIdx in 1:length(MPs) )
  {
    mp <- MPs[mpIdx]
    info.df.sub <-  info.df %>%
                    filter( scenarioLabel == scen,
                            mpLabel == mp )

    simID     <- info.df.sub$simLabel
    simPath  <- file.path("..",simID,paste(simID, ".RData", sep = "") )
    load(simPath)

    blobList[[mpIdx]] <- blob

  }

  # First, plot short time period depletion plots
  par( mfcol = c(3,3), mar = c(1,1,1,1), oma = c(3,3,3,1) )
  plot( x = c(0,2), y = c(1,length(MPs)), type = "n", axes =F )
    axis( side = 1, labels = c(0, 1, 2) )
    abline(v = 1, lty = 2, lwd = .8 )
