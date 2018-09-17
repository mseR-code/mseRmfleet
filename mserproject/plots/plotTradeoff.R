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
                mutate( deltaPdecline = pDecline - obsPdecline ) %>%
                filter( !grepl("PerfectInfo",Procedure))


scenarios <- unique( statTable$Scenario )
# MPs       <- unique( stats$Procedure )

periods <- unique(statTable$Period)
perLabels <- c("2Gen", "3Gen", "4Gen")

yrs <- seq(1951,by = 1, length = 92)
nT <- 92

for( scenIdx in 1:length(scenarios) )
{
  scenario <- scenarios[scenIdx]
  for( pIdx in 1:length(periods))
  {
    period <- periods[pIdx]
    perLabel <- perLabels[pIdx]

    fileName <- paste(scenario,"_",perLabel,".pdf",sep = "")
    filePath <- file.path("./tradeoffs",fileName)
    pdf(file = filePath, width = 8, height = 8)

    plotPerfStatsTradeoff(  stats = statTable, 
                            period = period, 
                            scenario = scenario,
                            xStat = "medProbGt.75NoFish", 
                            xLab = expression(P( B[t] > .75*B[NoFish] ) ),
                            xLim = c(0,1),
                            yStat = "medAvgCatch",
                            yLab = "Average Catch (kt)",
                            yLim = c(2,6),
                            pchTypes = c(0,1,2),
                            vline = .75,
                            hline = NA  )

    dev.off()


  }
}



