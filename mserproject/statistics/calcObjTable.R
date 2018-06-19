# Functions to summarise the performance table
# for each MP/scenario combo, and output to
# an objectives table

library(dplyr)

# source mseR functions
source("./mseRtools.R")
source("./mseRoptions.R")
source("./mseRglobals.R")

# First, load the perf table
perfTable <- read.csv(  "./mserproject/statistics/mseRstatistics_perfTable1.csv", header =T, 
                        stringsAsFactors = FALSE)

# Get scenario/MP labels
# scenarios <- unique( perfTable$Scenario )
# MPs       <- unique( perfTable$Procedure )

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

scenarios <- c( "WCVI_DDM", "WCVI_DIM", "WCVI_conM" )

Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 11 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbBtGt.3B0",
                          "ProbBtGt.6B0",
                          "NCN1_ProbGt.75B0",
                          "NCN2_ProbGtB90s",
                          "medAAV",
                          "medAveCatch",
                          "probClosure",
                          "ProbBtGtBave",
                          "ProbBtGtBave-prod" )



objTable <- as.data.frame(objTable)
tabRow <- 0
for( sIdx in 1:length(scenarios) )
  for( mIdx in 1:length(MPs) )
  {
    # increment table row
    tabRow    <- tabRow + 1

    # save scen/mp names
    scenario  <- scenarios[sIdx]
    mp        <- MPs[mIdx]

    subPerf <-  perfTable %>%
                filter( Scenario == scenario,
                        Procedure == mp )

    objTable[tabRow,c("Scenario","MP")] <- c(scenario,mp)
    
    objTable[tabRow,"ProbBtGt.3B0"] <- subPerf[subPerf$Period == "Long", "totProbBtGt.3B0" ]
    objTable[tabRow,"ProbBtGt.6B0"] <- subPerf[subPerf$Period == "Long", "totProbBtGt.6B0" ]
    objTable[tabRow,"medAAV"] <- subPerf[subPerf$Period == "Long", "medAAV" ]
    objTable[tabRow,"medAveCatch"] <- subPerf[subPerf$Period == "Long", "medAvgCatch" ]
    objTable[tabRow,"probClosure"] <- subPerf[subPerf$Period == "Long", "meanProbClosure" ]
    objTable[tabRow,"NCN1_ProbGt.75B0"] <- subPerf[subPerf$Period == "Long", "totProbGt.75B0" ]
    objTable[tabRow,"NCN2_ProbGtB90s"] <- subPerf[subPerf$Period == "Med", "totProbGtB90s" ]
    objTable[tabRow,"ProbBtGtBave"] <- subPerf[subPerf$Period == "Long", "totProbBtGtBave" ]
    objTable[tabRow,"ProbBtGtBave-prod"] <- subPerf[subPerf$Period == "Long", "totProbBtGtBave.prod" ]
    
  }


write.csv( objTable, file = "./mserproject/statistics/WCVI_HerringObjectiveTable.csv")
