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

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 15 )
colnames(objTable) <- c(  "Scenario","MP",
                          "ProbGt.3B0_3Gen",
                          "ProbGt.3B0_4Gen",
                          "ProbGt.6B0_2Gen",
                          "ProbGt.6B0_3Gen",
                          "ProbGt.6B0_4Gen",
                          "ProbGtLTA_2Gen",
                          "ProbGtLTA_3Gen",
                          "ProbGtLTA_4Gen",
                          "ProbGtrefB0_2Gen",
                          "ProbGtrefB0_3Gen",
                          "ProbGtrefB0_4Gen",
                          "medAveCatch_3Gen",
                          "medAAV_3Gen")



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
    objTable[tabRow,"ProbGt.6B0_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGt.6B0" ]
    objTable[tabRow,"ProbGt.6B0_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGt.6B0" ]
    objTable[tabRow,"ProbGtLTA_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGtLTA" ]
    objTable[tabRow,"ProbGtLTA_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGtLTA" ]
    objTable[tabRow,"ProbGtLTA_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGtLTA" ]
    objTable[tabRow,"ProbGtrefB_2Gen"] <- subPerf[subPerf$Period == "Short", "medProbGtrefB" ]
    objTable[tabRow,"ProbGtrefB_3Gen"] <- subPerf[subPerf$Period == "Med", "medProbGtrefB" ]
    objTable[tabRow,"ProbGtrefB_4Gen"] <- subPerf[subPerf$Period == "Long", "medProbGtrefB" ]
    objTable[tabRow,"medAveCatch_3Gen"] <- subPerf[subPerf$Period == "Med", "medAvgCatch" ]
    objTable[tabRow,"medAAV_3Gen"] <- subPerf[subPerf$Period == "Med", "medAAV" ]

  }


write.csv( objTable, file = "HerringObjectiveTable.csv")
