
# We need to expand.grid a few different
# options


# This grid is for options when cap is >0
JuveCapOptions <- list( methodId = c( SP = "3", DD = "4"),
                        jdProp = c( cap.5 = "0.5", cap1.0 = "1.0", cap1.5 = "1.5"),
                        jdAlloc = c(rctAl = "c(.23,.18,.59,0,0)", hstAl = "c(.30,.37,.33,0,0)"),
                        jdAmort = c( am5 = "5", am10 = "10") )

juveCapLabels <- lapply( X = JuveCapOptions, FUN = names )
names( juveCapLabels ) <- names(JuveCapOptions)


jcOptionBatchStems <- c(  methodId = "mp$assess$methodId",
                          jdProp = "mp$hcr$juveCapProp",
                          jdAlloc = "mp$hcr$juveCapAlloc",
                          jdAmort = 'mp$hcr$juveDiscAmortization')

# This grid is for other combinatorial options. When cap > 0, then
# prev grid is inclued in batch control file
otherOptions <- list( methodId = c( SP = "3", DD = "4"),
                      jdProp = c( noCap = "NULL", frt = "0" ),
                      jdAlloc = c( rctAl = "c(.23,.18,.59,0,0)"),
                      jdAmort = c( am5 = "5" ) )

AMlabels <- lapply( X = otherOptions, FUN = names )
names( AMlabels ) <- names(otherOptions)

otherOptionBatchStems <- c( methodId = "mp$assess$methodId",
                            cap = 'mp$hcr$juveCapProp' )

# Expand.grid levels and labels
jcGrid <- expand.grid(JuveCapOptions)
otherOptionsGrid <- expand.grid(otherOptions)

levelsGrid <- rbind( otherOptionsGrid, jcGrid )
levelsGrid <- dplyr::mutate_all(levelsGrid,as.character)

jcNameGrid <- expand.grid(juveCapLabels)
amNameGrid <- expand.grid(AMlabels)

labelsGrid <- rbind( amNameGrid, jcNameGrid )
labelsGrid <- dplyr::mutate_all(labelsGrid,as.character)






# makeInitCondDesign()
# Creates the .bch file for the initial conditions experiment,
# without the MP section underneath.
makeBatchDesign <- function (   scenarioGrid = NULL,
                                scenarioStems = NULL,
                                mpGrid = levelsGrid,
                                mpLabels = labelsGrid,
                                mpStems = jcOptionBatchStems,
                                mpLists = list(JuveCapOptions, otherOptions),
                                bchName = "juveCaps" )
{
  # First, recover the number of factors and Scenarios/MPs
  # Now start making the batch file for the simulation experiment
  outFile <- paste( bchName, ".bch", sep = "")
  cat(  "# Batch Control File, created ", date(), " by makeBatchDesign() \n", 
        file = outFile, append = F, sep = "" )
  cat( "parameter value\n", sep = "", append = T, file = outFile)
  cat( "#\n", file = outFile, append = T )
  cat( "# Scenarios \n", file = outFile, append = T )
  cat( "#\n", file = outFile, append = T )

  if( !is.null(scenarioGrid) )
  {
    message("SCENARIO GRID UNDER CONSTRUCTION")
    # nScenfactors  <- ncol(scenarioGrid)
    # nScens        <- nrow(scenarioGrid)  

    # for( scenIdx in 1:nScens )
    # {
    #   scenLabel <- ""
    #   repDiff <- combos[rIdx,"nDiff"]
    #   repBase <- totS - repDiff
    #   for( fIdx in 1:ncol(combos) )
    #   {
    #     scenLabel <- paste( scenLabel, colnames(combos)[fIdx], combos[rIdx,fIdx],sep = "" )
    #     if( fIdx < ncol(combos) ) scenLabel <- paste( scenLabel, "_", sep = "" )
    #   }
    #   cat( "# Scenario ", rIdx, " : ", scenLabel, "\n", file = outFile, append = T, sep = "" )
    #   cat( "#\n", file = outFile, append = T )
    #   cat(  "scenario$scenario", rIdx, "$ctrl$scenarioName '", scenLabel, "'\n", 
    #         sep = "", append = T, file = outFile )  
    #   cat(  "scenario$scenario", rIdx, "$opMod$nS ", combos[rIdx,"nS"], "\n", 
    #         sep = "", append = T, file = outFile )  
    #   cat(  "scenario$scenario", rIdx, "$opMod$surveyFreq ", combos[rIdx,"survFreq"], "\n", 
    #         sep = "", append = T, file = outFile )  
    #   cat(  "scenario$scenario", rIdx, "$opMod$sYear c(rep(", combos[rIdx,"sYear"], 
    #         ",", repDiff, "),rep(1988,",repBase, ")),\n", sep = "", append = T, file = outFile )
    #   cat(  "scenario$scenario", rIdx, "$opMod$initDep c(rep(", combos[rIdx,"initDep"], 
    #         ",", repDiff, "),rep(1,",repBase, ")),\n", sep = "", append = T, file = outFile )
    #   cat(  "scenario$scenario", rIdx, "$assess$initBioCode c(rep(1,",repDiff,"),rep(0,",repBase, ")),\n", sep = "", append = T, file = outFile )

    #   cat( "#\n", file = outFile, append = T )
    # }
  }

  cat( "#\n", file = outFile, append = T )
  cat( "# Management Procedures \n", file = outFile, append = T )
  cat( "#\n", file = outFile, append = T )
  # cat( "# MP 1 : baseAM \n", append = T, file = outFile )
  # cat( "# \n", append = T, file = outFile )
  # cat( "mp$mp1$ctrl$mpLabel 'baseAM' \n", append = T, file = outFile )
  # cat( "# \n", append = T, file = outFile )
  # cat( "# File Ends <not run>\n", append = T, file = outFile)

  if(!is.null(mpGrid))
  {

    nMPfactors  <- ncol(mpGrid)
    nMPs        <- nrow(mpGrid)  
    
    # This will loop over the design matrix and create the scenario entry in the 
    # batch control file
    for( mpIdx in 1:nMPs )
    {
      mpLabel <- paste(mpLabels[mpIdx,], collapse = "_")
      
      cat( "# MP ", mpIdx, " : ", mpLabel, "\n", file = outFile, append = T, sep = "" )
      cat( "#\n", file = outFile, append = T )
      cat(  "mp$mp", mpIdx, "$gui$mpLabel '", mpLabel, "'\n", 
            sep = "", append = T, file = outFile )  
      for( fIdx in 1:nMPfactors)
      {
        factorID <- colnames(mpGrid)[fIdx]
        cat(  "mp$mp", mpIdx,"$", mpStems[factorID]," ", mpGrid[mpIdx,factorID], "\n", 
              sep = "", append = T, file = outFile )    
      }

      cat( "#\n", file = outFile, append = T )
    }
  }

}





