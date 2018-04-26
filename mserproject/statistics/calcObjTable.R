# Functions to summarise the performance table
# for each MP/scenario combo, and output to
# an objectives table

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

probDeclineTable <- matrix(NA, nrow = length(scenarios) * length(MPs), ncol = 8 )
colnames(probDeclineTable) <- c( "Scenario","MP",
                              "AccProb.Short","ObsProb.Short",
                              "AccProb.Med","ObsProb.Med",
                              "AccProb.Long","ObsProb.Long" )

probDeclineTable <- as.data.frame(probDeclineTable)
tabRow <- 0
for( sIdx in 1:length(scenarios) )
  for( mIdx in 1:length(MPs) )
  {
    # increment table row
    tabRow    <- tabRow + 1

    # save scen/mp names
    scenario  <- scenarios[sIdx]
    mp        <- MPs[mIdx]

    probDeclineTable[tabRow,c("Scenario","MP")] <- c(scenario,mp)
    
    for( period in c("Short","Med","Long") )
    {
      periodTable <-  perfTable %>%
                      filter( Scenario == scenario,
                              Procedure == mp,
                              Period == period )  

      colNames <- paste(c("AccProb","ObsProb"),".",period,sep = "")
      probDeclineTable[tabRow,colNames] <- periodTable[ ,c("pDecline","obsPdecline") ]
    }
    
  }


write.csv( probDeclineTable, file = "ProbDeclineTable.csv")