# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)
library(DirichletReg)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")

source("plotBioFunctions.R")

MPnames <- c( "NoFish",
              "minE18.8_HR.2",
              "minE18.8_HR.2_cap5",
              "minE18.8_HR.1",
              "minE18.8_HR.1_cap5",
              "minE.5B0_HR.2",
              "minE.5B0_HR.2_cap5",
              "minE.5B0_HR.1",
              "minE.5B0_HR.1_cap5",
              "HS30-60_HR.2",
              "HS30-60_HR.2_cap5",
              "HS30-60_HR.1",
              "HS30-60_HR.1_cap5" )

rep <- 25

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
info.df$scenarioLabel <- as.character(info.df$scenarioLabel)
info.df$mpLabel <- as.character(info.df$mpLabel)

scenarios <- unique(info.df$scenarioLabel)
# MPnames   <-unique(info.df$mpLabel)

# calcStatsPFMAProp_DR()
statAreaPropStats <- read.csv(  "./statAreaBiomass/statAreaPropTable.csv",
                                header = TRUE, stringsAsFactors = FALSE )


# Now plot Cleveland plots
# Show stat Areas >25% separately but on same plot


plotComparativeScenarioStatAreaProps( scenList = scenarios[length(scenarios):1],
                                      Periods = "2Gen",
                                      periodLabels = "2Gen",
                                      MPs = MPnames,
                                      hlIdx = NULL,
                                      fileName = "./statAreaBiomass/statAreaSeparate_Cleveland",
                                      midLine = .75,
                                      xLim = c(0,1),
                                      hzLine = NA,
                                      stats = statAreaPropStats,
                                      xlabel = expression(paste("P( ", B[Area]/B[WCVI] > .25, " )") ) )


# Now show probability of all >25% at the same time.
plotComparativeScenarioObjs(  scenList = scenarios[length(scenarios):1],
                              Periods = "2Gen",
                              periodLabels = "2Gen",
                              MPs = MPnames,
                              hlIdx = NULL,
                              fileName = "./statAreaBiomass/statAreaAll_Cleveland",
                              statName = "ProbStatAreaAll",
                              midLine = .75,
                              xLim = c(0,1),
                              hzLine = NA,
                              stats = statAreaPropStats,
                              quantiles = TRUE,
                              xlabel = expression(paste("P( ", B[All]/B[WCVI] > .25, " )") ) )



pdf( file = "Bt_PFMAs_RW.pdf" )
plotPFMABt_RW(rep = 3,simNum  = 14, seedVal = 111, logPropSD =.2)
dev.off()

pdf( file = "Bt_PFMAs_DRlinear.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "linear")
dev.off()

pdf( file = "Bt_PFMAs_DRquadratic.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "quadratic")
dev.off()

pdf( file = "Bt_PFMAs_DRcubic.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "cubic")
dev.off()

pdf( file = "BtEnv_PFMAs_DRlinear.pdf" )
plotPFMABtEnv_DR(simNum = 14,  seedVal = 111, DR = "linear")
dev.off()

pdf( file = "BtEnv_PFMAs_DRquadratic.pdf" )
plotPFMABtEnv_DR(simNum = 14,  seedVal = 111, DR = "quadratic")
dev.off()

pdf( file = "BtEnv_PFMAs_RW.pdf" )
plotPFMABtEnv_RW(simNum = 14, seedVal = 111, logPropSD = .2)
dev.off()

pdf( file = "propEnv_PFMAs_DRlinear.pdf" )
plotPFMAProp_DR(simNum = 14, seedVal = 111, DR = "linear")
dev.off()

pdf( file = "propEnv_PFMAs_RW.pdf" )
plotPFMAProp_RW(simNum = 14, seedVal = 111, logPropSD = .2)
dev.off()