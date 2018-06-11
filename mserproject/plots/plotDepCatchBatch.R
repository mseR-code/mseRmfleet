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
source("../../mseRrefPoints.R")
source("plotBioFunctions.R")

MPs <- c( "NoFish",
          "minE21.2_HR.2",
          "minE21.2_HR.1",
          "minE21.2_HR.1_cap30",
          "minE.5B0_HR.2",
          "minE.5B0_HR.1",
          "minE.5B0_HR.1_cap30",
          "HS30-60_HR.2",
          "HS30-60_HR.1",
          "HS30-60_HR.1_cap30" )

MPs_PI <- c(  "PerfectInfo_minE21.2_HR.2",
              "PerfectInfo_minE21.2_HR.1",
              "PerfectInfo_minE21.2_HR.1_cap30",
              "PerfectInfo_minE.5B0_HR.2",
              "PerfectInfo_minE.5B0_HR.1",
              "PerfectInfo_minE.5B0_HR.1_cap30",
              "PerfectInfo_HS30-60_HR.2",
              "PerfectInfo_HS30-60_HR.1",
              "PerfectInfo_HS30-60_HR.1_cap30" )

currMPs <- c( "NoFish",
              "minE21.2_HR.2",
              "minE21.2_HR.1",
              "minE21.2_HR.1_cap30" )

bestMPs <- c( "NoFish",
              "minE.5B0_HR.1_cap30",
              "minE.5B0_HR.1",
              "HS30-60_HR.1_cap30" )



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

# now plot the depCatch multi panels we want
plotDepCatchMultiPanels(  MPnames = MPs, plotNameRoot = "allMPs_DepCatch",
                          scenarios = scenList, df = info.df, gfx = gfx)

plotDepCatchMultiPanels(  MPnames = currMPs, plotNameRoot = "currMPs_DepCatch",
                          scenarios = scenList, df = info.df, gfx = gfx)

plotDepCatchMultiPanels(  MPnames = bestMPs, 
                          plotNameRoot = "bestMPs_DepCatch",
                          scenarios = scenList, df = info.df, gfx = gfx)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_minE.5B0], 
#                           plotNameRoot = "minE.5B0_bestMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_.HS30.60], 
#                           plotNameRoot = "HS30-60_bestMPs",
#                           scenarios = scenList, df = info.df)


# plotDepCatchMultiPanels(  MPnames = MPs[base3HCRs], 
#                           plotNameRoot = "baseHCRs",
#                           scenarios = scenList, df = info.df)
