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

MPs <- c("currMP")



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
short <- c(55,66)
med   <- c(67,77)
long  <- c(78,89)

# Read in info files, sort by  scenarios
info.df <- lapply( X = sims, FUN = readInfoFile )
info.df <- do.call( "rbind", info.df ) %>%
            arrange(scenarioLabel,mpLabel)

scenList <- unique( info.df$scenarioLabel )
scenList <- "sableOpMod_MLE_110619"

# scenList <- c( "WCVI_DIM", "WCVI_DDM" )
# MPs       <- unique( info.df$mpLabel )

yrs <- seq(1965,by = 1, length = 89)
nT <- 89

# now plot the depCatch multi panels we want
plotDepCatchHRMultiPanel( simFolder = "../",
                          mps = c("currMP","ddMP"), 
                          traces = 3,
                          scenario = c("sableOpMod_MLE_110619"),
                          years = 1965:2054,
                          saveFile = FALSE,
                          yLimD = c(0,1.5),
                          yLimC = c(0,10),
                          yLimU = c(0,0.1) )



# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_minE.5B0], 
#                           plotNameRoot = "minE.5B0_bestMPs",
#                           scenarios = scenList, df = info.df)

# plotDepCatchMultiPanels(  MPnames = MPs[whatWeLike_.HS30.60], 
#                           plotNameRoot = "HS30-60_bestMPs",
#                           scenarios = scenList, df = info.df)


# plotDepCatchMultiPanels(  MPnames = MPs[base3HCRs], 
#                           plotNameRoot = "baseHCRs",
#                           scenarios = scenList, df = info.df)
