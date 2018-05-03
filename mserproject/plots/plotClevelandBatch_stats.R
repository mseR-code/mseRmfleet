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

source("plotBioFunctions.R")


MPs <- c( "NoFish",
          "currMP",
          "FC18.8_HR0.1",
          "RC.5B0_HR0.2",
          "RC.5B0_HR0.1",
          "RC.25B0_HR0.2",
          "RC.25B0_HR0.1",
          "ccRule",
          "PerfectInfo_currMP",
          "PerfectInfo_FC18.8_HR0.1",
          "PerfectInfo_RC.5B0_HR0.2",
          "PerfectInfo_RC.5B0_HR0.1",
          "PerfectInfo_RC.25B0_HR0.2",
          "PerfectInfo_RC.25B0_HR0.1")

whatWeLike <- c(3,4,5,7)
hlIdx <- whatWeLike


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
short <- c(68,79)
med   <- c(80,91)
# long  <- c(86,92)

stats <-  read.csv(  "../statistics/mseRstatistics_perfTable1.csv", header = T,
                    stringsAsFactors = FALSE ) %>%
          mutate( deltaPdecline = pDecline - obsPdecline )


scenarios <- unique( stats$Scenario )
# MPs       <- unique( stats$Procedure )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

Periods <- c("Short","Med")
perLabel <- c("2 Generations", "4 Generations")


for( scenIdx in 1:length(scenarios) )
{
  scen <- scenarios[scenIdx]

  clevelandPlots  <- paste(scen, "Cleveland_HL.pdf", sep = "" )

  pdf( file = clevelandPlots, width = 12, height = 8 )
  
  # First, plot short time period depletion plots
  par( mfrow = c(length(Periods),4), mar = c(1,1,1,1), oma = c(3,12,4,3) )
  
  # Final Depletion
  for( pIdx in 1:2 )
  {
    period <- Periods[pIdx]
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AvgDep",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = .3,
                            mpLabs = T,
                            MPorder = MPs,
                            hzLine = 6.5 )
    
    if( pIdx == 2 )
      mtext( side = 1, text = "Average Depletion", line = 2 )

    # Avg Catch
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AvgCatch",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = NA,
                            mpLabs = F,
                            hzLine = 6.5 )
    if( pIdx == 2)
      mtext( side = 1, text = "Average Catch (kt)", line = 2 )

    # AAV
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AAV",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = NA,
                            mpLabs = F,
                            hzLine = 6.5 )
    if( pIdx == 2)
      mtext( side = 1, text = "Average Annual Variation (%)", line = 2 )

    # Prop years closed
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "PropClosure",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = NA,
                            mpLabs = F,
                            hzLine = 6.5 )
    if( pIdx == 2)
      mtext( side = 1, text = "Prob TAC < 500t (%)", line = 2 )

    mtext( side = 4, text = perLabel[pIdx], line = 2)
  }

  mtext( side = 3, outer = T, text = scen )

  dev.off()

  pdf( file = paste(scen, "objectivePerfCleveland_HL.pdf"), width = 12, height = 8 )
  # First, plot short time period depletion plots
  par( mfrow = c(length(Periods),3), mar = c(2,1,2,1), oma = c(3,12,4,3) )

  for( pIdx in 1:length(Periods) )
  {

    period <- Periods[pIdx]
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "ProbGt.3B0",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = c(.9,.95),
                            xLim = c(0,1),
                            mpLabs = T,
                            hzLine = 6.5 )
    if( pIdx == 2 )
      mtext( side = 1, text = expression(paste("P( ", B[t] > .3*B[0], " )") ), line = 3 )

    plotScenarioClevelands( scenarioName = scen, 
                            statName = "deltaPdecline",
                            quantiles = FALSE,
                            timePeriod = period,
                            statsTable = stats,
                            midLine = c(0),
                            xLim = c(-1,1),
                            mpLabs = F,
                            hzLine = 6.5 )
    
    if( pIdx == 2 )
      mtext( side = 1, text = "Objective 2 Performance", line = 3)  

    plotScenarioClevelands( scenarioName = scen, 
                            statName = "ProbGt.75B0",
                            timePeriod = period,
                            statsTable = stats,
                            midLine = c(.5,.75),
                            xLim = c(0,1),
                            mpLabs = F,
                            hzLine = 6.5 )
    if( pIdx == 2 )
      mtext( side = 1, text = expression(paste("P( ", B[t] > .75*B[0], " )") ), line = 3 )

    mtext( side = 4, text = perLabel[pIdx], line = 2 )
  }

  


  mtext( side = 3, outer = T, text = scen, cex = 2, line = 2 )

  dev.off()
}


