# Functions to summarise the performance table
# for each MP/scenario combo, and output to
# an objectives table

library(dplyr)

# source mseR functions
source("../../mseRtools.R")
source("../../mseRoptions.R")
source("../../mseRglobals.R")

# First, load the perf table
perfTable <- read.csv(  "mseRstatistics_perfTable1.csv", header =T, 
                        stringsAsFactors = FALSE)

# Get scenario/MP labels
# scenarios <- unique( perfTable$Scenario )
# MPs       <- unique( perfTable$Procedure )

MPs <- c( "NoFish",
          "minE18.8_HR.2",
          "minE18.8_HR.1",
          "minE18.8_HR.1_cap2",
          "minE.5B0_HR.2",
          "minE.5B0_HR.1",
          "minE.5B0_HR.1_cap2",
          "HS30-60_HR.2",
          "HS30-60_HR.1",
          "HS30-60_HR.1_cap2" )

scenarios <- c( "WCVI_DIM", "WCVI_DDM" )

Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 13 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbGt.3B0",
                          "minProbBtGt.3B0",
                          "ProbGt.6B0",
                          "medAveCatch",
                          "medAAV",
                          "NCN1_ProbGt.75B0",
                          "NCN1_ProbGt.75NoFish",
                          "NCN2_ProbGt.76B0_2Gen",
                          "NCN2_ProbGt.76NoFish_2Gen",
                          "ProbGtSBave",
                          "ProbGtSBave-prod" )



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
    
    objTable[tabRow,"ProbGt.3B0"] <- subPerf[subPerf$Period == "Med", "medProbGt.3B0" ]
    objTable[tabRow,"minProbBtGt.3B0"] <- subPerf[subPerf$Period == "Med", "minProbBtGt.3B0" ]
    objTable[tabRow,"ProbGt.6B0"] <- subPerf[subPerf$Period == "Med", "medProbGt.6B0" ]
    objTable[tabRow,"medAveCatch"] <- subPerf[subPerf$Period == "Med", "medAvgCatch" ]
    objTable[tabRow,"medAAV"] <- subPerf[subPerf$Period == "Med", "medAAV" ]
    objTable[tabRow,"NCN1_ProbGt.75B0"] <- subPerf[subPerf$Period == "Med", "medProbGt.75B0" ]
    objTable[tabRow,"NCN1_ProbGt.75NoFish"] <- subPerf[subPerf$Period == "Med", "medProbGt.75NoFish" ]
    objTable[tabRow,"NCN2_ProbGt.76B0_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2" ]
    objTable[tabRow,"NCN2_ProbGt.76NoFish_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2NoFish" ]
    objTable[tabRow,"ProbGtSBave"] <- subPerf[subPerf$Period == "Med", "medProbGtLTA" ]
    objTable[tabRow,"ProbGtSBave-prod"] <- subPerf[subPerf$Period == "Med", "medProbGtrefB" ]
    
  }


write.csv( objTable, file = "WCVI_HerringObjectiveTable.csv")
