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
MPs       <- unique( perfTable$Procedure )
MPS       <- MPs[order(MPs)]

# MPs <- c( "minE18.8_HR.1_slowUp2",
#           "minE18.8_HR.1_slowUp3",
#           "minE18.8_HR.1_slowUp4",
#           "minE18.8_HR.1_slowUp5"  )



Periods   <- unique( perfTable$Period )

PerfectInfo <- FALSE
if( !PerfectInfo ) MPs <- MPs[!grepl("PerfectInfo",MPs)]

objTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 13 )
colnames(objTable) <- c(  "Scenario","MP","Label",
                          "ProbBtGt.3B0",
                          "ProbBtGt.4B0",
                          "ProbBtGt.6B0",
                          "ProbBtGtBave",
                          "ProbBtGtBave-prod",
                          "NCN1_ProbGt.75B0",
                          "NCN2_ProbGtB90s",
                          "medAAV",
                          "medAveCatch",
                          "probClosure" )



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

    # browser()

    if(nrow(subPerf) == 0) next

    objTable[tabRow,c("Scenario","MP","Label")] <- c(scenario,mIdx,mp)
    
    objTable[tabRow,"ProbBtGt.3B0"] <- round(subPerf[subPerf$Period == "Long", "totProbBtGt.3B0" ],2)
    objTable[tabRow,"ProbBtGt.4B0"] <- round(subPerf[subPerf$Period == "Long", "totProbBtGt.4B0" ],2)
    objTable[tabRow,"ProbBtGt.6B0"] <- round(subPerf[subPerf$Period == "Long", "totProbBtGt.6B0" ],2)
    objTable[tabRow,"medAAV"] <- round(subPerf[subPerf$Period == "Long", "medAAV" ],2)
    objTable[tabRow,"medAveCatch"] <- round(subPerf[subPerf$Period == "Long", "medAvgCatch" ],2)
    objTable[tabRow,"probClosure"] <- round(subPerf[subPerf$Period == "Long", "meanProbClosure" ],2)
    objTable[tabRow,"NCN1_ProbGt.75B0"] <- round(subPerf[subPerf$Period == "Long", "totProbGt.75B0" ],2)
    objTable[tabRow,"NCN2_ProbGtB90s"] <- round(subPerf[subPerf$Period == "Med", "totProbGtB90s" ],2)
    objTable[tabRow,"ProbBtGtBave"] <- round(subPerf[subPerf$Period == "Long", "totProbBtGtBave" ],2)
    objTable[tabRow,"ProbBtGtBave-prod"] <- round(subPerf[subPerf$Period == "Long", "totProbBtGtBave.prod" ],2)
    
  }


write.csv( objTable, file = "./mserproject/statistics/WCVI_HerringObjectiveTable.csv")
