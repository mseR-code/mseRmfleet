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
source("plotBioFunctions.R")

MPs <- c( "minE18.8_HR.2",
          "minE18.8_HR.1",
          "minE18.8_HR.1_cap2",
          "minE.5B0_HR.2",
          "minE.5B0_HR.1",
          "minE.5B0_HR.1_cap2",
          "HS30-60_HR.2",
          "HS30-60_HR.1",
          "HS30-60_HR.1_cap2",
          "NoFish" )


MPs_PI <- c(  "PerfectInfo_minE18.8_HR.2",
              "PerfectInfo_minE18.8_HR.1",
              "PerfectInfo_minE18.8_HR.1_cap2",
              "PerfectInfo_minE.5B0_HR.2",
              "PerfectInfo_minE.5B0_HR.1",
              "PerfectInfo_minE.5B0_HR.1_cap2",
              "PerfectInfo_HS30-60_HR.2",
              "PerfectInfo_HS30-60_HR.1",
              "PerfectInfo_HS30-60_HR.1_cap2" )

currMPs <- c( "minE18.8_HR.2",
              "minE18.8_HR.1",
              "minE18.8_HR.1_cap2",
              "NoFish" )

bestMP <- c(  "minE.5B0_HR.1_cap2",
              "minE18.8_HR.1_cap2",
              "HS30-60_HR.1_cap2",
              "NoFish" )

# checkMPs <- c(  "NoFish",
#                 "minE.5B0_HR.1",
#                 "minE.5B0_HR.1_cap5")


# whatWeLike_minE18.8 <- 3:5
# whatWeLike_minE.5B0 <- 6:9
# whatWeLike_.HS30.60 <- 11:13

# base3HCRs <- c(2,6,10)



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

# scenList <- c( "WCVI_DIM", "WCVI_DDM" )
# MPs       <- unique( info.df$mpLabel )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

# now plot the depCatch multi panels we want
# plotDepCatchMultiPanels(  MPnames = MPs, plotNameRoot = "DepCatch",
#                           scenarios = scenList, df = info.df)

plotDepCatchMultiPanels(  MPnames = MPs, plotNameRoot = "allMPs",
                          scenarios = scenList, df = info.df,
                          gfx = gfx )

plotDepCatchMultiPanels(  MPnames = currMPs, plotNameRoot = "currMPs",
                          scenarios = scenList, df = info.df,
                          gfx = gfx )

plotDepCatchMultiPanels(  MPnames = bestMP, plotNameRoot = "bestMP",
                          scenarios = scenList, df = info.df,
                          gfx = gfx )

# plotDepCatchMultiPanels(  MPnames = checkMPs, plotNameRoot = "checkMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = currMP_proxy, plotNameRoot = "currMP_proxy",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_minE18.8], 
#                           plotNameRoot = "minE18.8_bestMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_minE.5B0], 
#                           plotNameRoot = "minE.5B0_bestMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_.HS30.60], 
#                           plotNameRoot = "HS30-60_bestMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[base3HCRs], 
#                           plotNameRoot = "baseHCRs",
#                           scenarios = scenList, df = info.df)
