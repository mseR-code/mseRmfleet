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
MPs       <- unique( perfTable$Procedure )
Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 19 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbGt.3B0_3Gen",
                          "ProbGt.3B0_4Gen",
                          "ProbGt.6B0_2Gen",
                          "ProbGt.6B0_end2Gen",
                          "medAveCatch_3Gen",
                          "medAAV_3Gen",
                          "NCN1_ProbGt.75B0_3Gen",
                          "NCN1_ProbGt.75B0_end3Gen",
                          "NCN1_ProbGt.75B0_4Gen",
                          "NCN1_ProbGt.75B0_end4Gen",
                          "NCN1_ProbGt.75NoFish_3Gen",
                          "NCN1_ProbGt.75NoFish_4Gen",
                          "NCN2_ProbGt.76B0_2Gen",
                          "NCN2_ProbGt.76B0_end2Gen",
                          "NCN2_ProbGt.76NoFish_2Gen",
                          "ProbGtB0_2Gen",
                          "ProbGtB0_end2Gen")



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

    
    
    objTable[tabRow,"ProbGt.3B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.3B0" ]
    objTable[tabRow,"ProbGt.3B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.3B0" ]
    objTable[tabRow,"ProbGt.6B0_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGt.6B0" ]
    # objTable[tabRow,"ProbGt.6B0_end2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGt.6B0end" ]
    objTable[tabRow,"medAveCatch_3Gen"] <- subPerf[subPerf$Period == "Med", "medAvgCatch" ]
    objTable[tabRow,"medAAV_3Gen"] <- subPerf[subPerf$Period == "Med", "medAAV" ]
    # objTable[tabRow,"NCN1_ProbGt.75B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.75B0" ]
    # objTable[tabRow,"NCN1_ProbGt.75B0_end3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.75B0end" ]
    # objTable[tabRow,"NCN1_ProbGt.75B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.75B0" ]
    # objTable[tabRow,"NCN1_ProbGt.75B0_end4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.75B0end" ]
    # objTable[tabRow,"NCN1_ProbGt.75NoFish_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.75NoFish" ]
    # objTable[tabRow,"NCN1_ProbGt.75NoFish_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.75NoFish" ]
    # objTable[tabRow,"NCN2_ProbGt.76B0_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2" ]
    # objTable[tabRow,"NCN2_ProbGt.76B0_end2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2end" ]
    # objTable[tabRow,"NCN2_ProbGt.76NoFish_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbNCNGoal2NoFish" ]
    objTable[tabRow,"ProbGtB0_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGtB0" ]
    # objTable[tabRow,"ProbGtB0_end2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGtB0end" ]
  }


write.csv( objTable, file = "HerringObjectiveTable.csv")
