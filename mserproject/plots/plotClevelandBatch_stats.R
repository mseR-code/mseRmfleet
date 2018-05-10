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

whatWeLike <- c(3,4,5,7)
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


