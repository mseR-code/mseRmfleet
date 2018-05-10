# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)
require(dplyr)
require(ref)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")
source("../../mseRstats.R")
source("../../mseRrefPoints.R")

MPs <- c( "NoFish",
          "minE18.8_HR.2",
          "minE18.8_HR.1",
          "minE18.8_HR.2_cap5",
          "minE18.8_HR.1_cap5",
          "minE.5B0_HR.2",
          "minE.5B0_HR.1",
          "minE.5B0_HR.2_cap5",
          "minE.5B0_HR.1_cap5",
          "HS30-60_HR.2",
          "HS30-60_HR.1",
          "HS30-60_HR.2_cap5",
          "HS30-60_HR.1_cap5",
          "PerfectInfo_minE18.8_HR.2",
          "PerfectInfo_minE18.8_HR.1",
          "PerfectInfo_minE18.8_HR.2_cap5",
          "PerfectInfo_minE18.8_HR.1_cap5",
          "PerfectInfo_minE.5B0_HR.2",
          "PerfectInfo_minE.5B0_HR.1",
          "PerfectInfo_minE.5B0_HR.2_cap5",
          "PerfectInfo_minE.5B0_HR.1_cap5",
          "PerfectInfo_HS30-60_HR.2",
          "PerfectInfo_HS30-60_HR.1",
          "PerfectInfo_HS30-60_HR.2_cap5",
          "PerfectInfo_HS30-60_HR.1_cap5")

# MPs <- MPs2

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
info.df <- do.call( "rbind", info.df ) %>%
            arrange(scenarioLabel,mpLabel)

scenList <- unique( info.df$scenarioLabel )
# MPs       <- unique( info.df$mpLabel )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

# plotDepCatchMultiPanels <- function( MPnames = MPs, plotNameRoot = "DepCatch",
#                                       scenarios = scenList )
# {

# }

for( scenIdx in 1:length(scenarios) )
{
  scen <- scenarios[scenIdx]

  depCatchPlot    <- paste(scen, "bestMPs_DepCatch.pdf", sep = "" )
  depCatch_noFish <- paste(depCatchPlot,"_noFish.pdf", sep = "" )

  noFishID <- info.df[  which(info.df$mpLabel == "NoFish" & info.df$scenarioLabel == scen)[1],
                        "simLabel"]
  noFishPath  <- file.path("..",noFishID,paste(noFishID, ".RData", sep = "") )

  if(!is.na(noFishID))
  {
    load(noFishPath)
    noFishBlob <- blob
  }

  mpList <- vector( mode = "list", length = length(MPs) - 1 )
  mpListIdx <- 1

  pdf( file = depCatchPlot, width = length(MPs)*2, height = 6 )
  par( mfcol = c(2,length(MPs)), mar = c(1,1.5,1,1.5), oma = c(3,3,4,1))

  for( mpIdx in 1:length(MPs) )
  {
    mp <- MPs[mpIdx]
    info.df.sub <-  info.df %>%
                    filter( scenarioLabel == scen,
                            mpLabel == mp )

    simID     <- info.df.sub[1,]$simLabel
    if(is.na(simID)) next

    simPath  <- file.path("..",simID,paste(simID, ".RData", sep = "") )
    load(simPath)

    if(mpIdx == 1) gfx$doLegend <- TRUE
    else gfx$doLegend <- FALSE

    .plotTulipDepCat( blob, gfx = gfx, yLimD = c(0,1), yLimC = c(0,10),
                      refPts = FALSE )

    # Now rescale blob$Bt if
    if( mp != "NoFish" )
    {
      blob$om$SBt <- blob$om$SBt / noFishBlob$om$SBt
      blob$ctlList$opMod$B0 <- 1

      mpList[[mpListIdx]] <- blob
      names(mpList)[mpListIdx] <- mp
      mpListIdx <- mpListIdx + 1
    }
  }

  mtext( side = 3, outer = T, text = scen, cex = 1.3, line = 2.5)

  dev.off()

  if(is.na(noFishID)) next
  pdf( file = depCatch_noFish, width = (length(MPs)-1)*2, height = 6 )
  par( mfcol = c(2,length(MPs)-1), mar = c(1,1.5,1,1.5), oma = c(3,3,4,1))

  for( idx in 1:length(mpList) )
  {
    if(idx == 1) gfx$doLegend <- TRUE
    else gfx$doLegend <- FALSE

    if( is.null(mpList[[idx]]) ) next

    .plotTulipDepCat( mpList[[idx]], gfx = gfx, yLimD = c(0,1), yLimC = c(0,10),
                      refPts = FALSE, DepLab = expression(SSB / SSB[NoFish]) )
  }
  dev.off()
}


