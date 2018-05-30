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
scenarios <- unique( perfTable$Scenario )
# MPs       <- unique( perfTable$Procedure )

MPs <- c( "NoFish",
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

Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 16 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbGt.3B0_3Gen",
                          "ProbGt.3B0_4Gen",
                          "ProbGt.6B0_3Gen",
                          "ProbGt.6B0_4Gen",
                          "medAveCatch_3Gen",
                          "medAAV_3Gen",
                          "NCN1_ProbGt.75B0_3Gen",
                          "NCN1_ProbGt.75B0_4Gen",
                          "NCN1_ProbGt.75NoFish_3Gen",
                          "NCN1_ProbGt.75NoFish_4Gen",
                          "NCN2_ProbGt.76B0_2Gen",
                          "NCN2_ProbGt.76NoFish_2Gen",
                          "ProbGtSBave_3Gen",
                          "ProbGtSBave-prod_3Gen" )



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
    
    objTable[tabRow,"ProbGt.3B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.3B0" ]
    objTable[tabRow,"ProbGt.3B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.3B0" ]
    objTable[tabRow,"ProbGt.6B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.6B0" ]
    objTable[tabRow,"ProbGt.6B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.6B0" ]
    objTable[tabRow,"medAveCatch_3Gen"] <- subPerf[subPerf$Period == "Med", "medAvgCatch" ]
    objTable[tabRow,"medAAV_3Gen"] <- subPerf[subPerf$Period == "Med", "medAAV" ]
    objTable[tabRow,"NCN1_ProbGt.75B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.75B0" ]
    objTable[tabRow,"NCN1_ProbGt.75B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.75B0" ]
    objTable[tabRow,"NCN1_ProbGt.75NoFish_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.75NoFish" ]
    objTable[tabRow,"NCN1_ProbGt.75NoFish_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.75NoFish" ]
    objTable[tabRow,"NCN2_ProbGt.76B0_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2" ]
    objTable[tabRow,"NCN2_ProbGt.76NoFish_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2NoFish" ]
    objTable[tabRow,"ProbGtSBave_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGtLTA" ]
    objTable[tabRow,"ProbGtSBave-prod_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGtrefB" ]
  }


write.csv( objTable, file = "WCVI_HerringObjectiveTable.csv")