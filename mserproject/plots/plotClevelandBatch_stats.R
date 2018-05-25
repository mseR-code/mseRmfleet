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


MPnames <- c( "NoFish",
              "minE18.8_HR.2",
              "minE18.8_HR.2_cap30",
              "minE18.8_HR.1",
              "minE18.8_HR.1_cap30",
              "minE.5B0_HR.2",
              "minE.5B0_HR.2_cap30",
              "minE.5B0_HR.1",
              "minE.5B0_HR.1_cap30",
              "HS30-60_HR.2",
              "HS30-60_HR.2_cap30",
              "HS30-60_HR.1",
              "HS30-60_HR.1_cap30" )
              # "PerfectInfo_minE18.8_HR.2",
              # "PerfectInfo_minE18.8_HR.1",
              # "PerfectInfo_minE18.8_HR.2_cap5",
              # "PerfectInfo_minE18.8_HR.1_cap5",
              # "PerfectInfo_minE.5B0_HR.2",
              # "PerfectInfo_minE.5B0_HR.1",
              # "PerfectInfo_minE.5B0_HR.2_cap5",
              # "PerfectInfo_minE.5B0_HR.1_cap5",
              # "PerfectInfo_HS30-60_HR.2",
              # "PerfectInfo_HS30-60_HR.1",
              # "PerfectInfo_HS30-60_HR.2_cap5",
              # "PerfectInfo_HS30-60_HR.1_cap5")

whatWeLike <- c(3:5,6:9,11:13)
hlIdx <- NULL


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

statTable <-  read.csv(  "../statistics/mseRstatistics_perfTable1.csv", header = T,
                          stringsAsFactors = FALSE ) %>%
                mutate( deltaPdecline = pDecline - obsPdecline )


scenarios <- unique( statTable$Scenario )
# MPs       <- unique( stats$Procedure )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

timePeriods <- c("Short","Med")
perLabel <- c("3 Generations", "4 Generations")



for( scenIdx in 1:length(scenarios) )
{
    plotClevelands( scen = scenarios[scenIdx], 
                    Periods = timePeriods,
                    periodLabels = perLabel,
                    MPs = MPnames,
                    hlIdx = NULL,
                    fileName = "Cleveland",
                    stats = statTable )

    plotClevelands( scen = scenarios[scenIdx], 
                    Periods = timePeriods,
                    periodLabels = perLabel,
                    MPs = MPnames,
                    hlIdx = whatWeLike,
                    fileName = "ClevelandHL",
                    stats = statTable )
}



plotComparativeScenarioObjs( fileName = "Objective1_LRP", statName = "ProbGt.3B0",
                              xlabel = expression(paste("P( ", B[t] > .3*B[0], " )") ) )

plotComparativeScenarioObjs( fileName = "Objective1_LRP_HL", statName = "ProbGt.3B0",
                              xlabel = expression(paste("P( ", B[t] > .3*B[0], " )") ),
                              hlIdx = whatWeLike )

plotComparativeScenarioObjs( fileName = "Objective2_USR", statName = "ProbGt.6B0",
                              xlabel = expression(paste("P( ", B[t] > .6*B[0], " )") ),
                              midLine = .5, Periods = c("Short") )

plotComparativeScenarioObjs( fileName = "Objective2_USR_HL", statName = "ProbGt.6B0",
                              xlabel = expression(paste("P( ", B[t] > .6*B[0], " )") ),
                              hlIdx = whatWeLike, midLine = .5, Periods = c("Short") )

plotComparativeScenarioObjs( fileName = "NCNGoal1_USR", statName = "ProbGt.75B0",
                              xlabel = expression(paste("P( ", B[t] > .75*B[0], " )") ),
                              midLine = .75, Periods = c("Short","Med")  )

plotComparativeScenarioObjs( fileName = "NCNGoal1_USR_HL", statName = "ProbGt.75B0",
                              xlabel = expression(paste("P( ", B[t] > .75*B[0], " )") ),
                              hlIdx = whatWeLike, midLine = .75, Periods = c("Short","Med") )

plotComparativeScenarioObjs( fileName = "NCNGoal2", statName = "ProbNCNGoal2",
                              xlabel = expression(paste("P( ", B[t] > .75*B[0], " )") ),
                              midLine = .75, Periods = c("Short")  )

plotComparativeScenarioObjs( fileName = "NCNGoal2_HL", statName = "ProbNCNGoal2",
                              xlabel = expression(paste("P( ", B[t] > .75*B[0], " )") ),
                              hlIdx = whatWeLike, midLine = .75, Periods = c("Short") )

plotComparativeScenarioObjs( fileName = "Objective2_USR_B0", statName = "ProbGtB0",
                              xlabel = expression(paste("P( ", B[t] > B[0], " )") ),
                              midLine = .5, Periods = c("Short") )

plotComparativeScenarioObjs( fileName = "Objective2_USR_B0_HL", statName = "ProbGtB0",
                              xlabel = expression(paste("P( ", B[t] > B[0], " )") ),
                              hlIdx = whatWeLike, midLine = .5, Periods = c("Short") )
