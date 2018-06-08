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
scenarios <- unique( perfTable$Scenario )
# MPs       <- unique( perfTable$Procedure )

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

Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 16 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbGt.3B0",
                          "minProbBtGt.3B0",
                          "totProbBtGt.3B0",
                          "ProbGt.6B0",
                          "minProbBtGt.6B0",
                          "totProbBtGt.6B0",
                          "medAveCatch",
                          "medAAV",
                          "ProbGtBave",
                          "minProbBtGtBave",
                          "totProbBtGtBave",
                          "ProbGtBave-prod",
                          "minProbBtGtBave-prod",
                          "totProbBtGtBave-prod" )


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

    if(nrow(subPerf) == 0) next

    if(nrow(subPerf) > 3 ) browser()
    
    objTable[tabRow,"ProbGt.3B0"] <- subPerf[subPerf$Period == "Med", "medProbGt.3B0" ]
    objTable[tabRow,"minProbBtGt.3B0"] <- subPerf[subPerf$Period == "Med", "minProbBtGt.3B0" ]
    objTable[tabRow,"totProbBtGt.3B0"] <- subPerf[subPerf$Period == "Med", "totProbBtGt.3B0" ]
    objTable[tabRow,"ProbGt.6B0"] <- subPerf[subPerf$Period == "Med", "medProbGt.6B0" ]
    objTable[tabRow,"minProbBtGt.6B0"] <- subPerf[subPerf$Period == "Med", "minProbBtGt.6B0" ]
    objTable[tabRow,"totProbBtGt.6B0"] <- subPerf[subPerf$Period == "Med", "totProbBtGt.6B0" ]
    objTable[tabRow,"ProbGtBave"] <- subPerf[subPerf$Period == "Med", "medProbGtBave" ]
    objTable[tabRow,"minProbBtGtBave"] <- subPerf[subPerf$Period == "Med", "minProbBtGtBave" ]
    objTable[tabRow,"totProbBtGtBave"] <- subPerf[subPerf$Period == "Med", "totProbBtGtBave" ]
    objTable[tabRow,"ProbGtBave-prod"] <- subPerf[subPerf$Period == "Med", "medProbGtBave.prod" ]
    objTable[tabRow,"minProbBtGtBave-prod"] <- subPerf[subPerf$Period == "Med", "minProbBtGtBave.prod" ]
    objTable[tabRow,"totProbBtGtBave-prod"] <- subPerf[subPerf$Period == "Med", "totProbBtGtBave.prod" ]
    objTable[tabRow,"medAveCatch"] <- subPerf[subPerf$Period == "Med", "medAvgCatch" ]
    objTable[tabRow,"medAAV"] <- subPerf[subPerf$Period == "Med", "medAAV" ]

  }


write.csv( objTable, file = "./mserproject/statistics/SOG_HerringObjectiveTable.csv")
