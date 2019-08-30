# multiBatchScript.R

# A wrapper script to run multiple parallel batches
# with limited user input - likely worth
# creating two versions of this for the two 
# Mac Pros

library(parallel)
source("mseR.r")
source("parBatchFuns.R")

batchControlFiles <- c( "juveCaps.bch")
                        #"mseRbatchJob_CCAM2_tuned3.bch",
                        # "mseRbatchJob_PRDAM2_tuned2.bch")

baseControlFiles  <- c( "simCtlFile.txt")
                        #"simCtlFile.txt",
                        # "simCtlFile.txt")

saveDirName       <- c( "juveCaps_2AMs")
                        #"CCAM2_allM_tuned3",
                        # "PRDAM2_allM_tuned2")


currMP  <- c( NULL, NULL, NULL ) 

nSims   <- c( 58 )
nCores  <- c( 23 )




# Create a directory to hold completed mseR batch
# jobs
if(!dir.exists("../mseR_completedBatch"))
  dir.create("../mseR_completedBatch")

# Count batch jobs
nBatchJobs <- length( batchControlFiles )

for( bIdx in 1:nBatchJobs )
{
  message(  "Starting parallel batch for ",
            batchControlFiles[bIdx], ".\n", sep = "")
  # Make the batch
  makeBatch(  baseCtlFile = baseControlFiles[bIdx],
              batchCtlFile = batchControlFiles[bIdx])


  # Run the batch in parallel - new 
  # format should stop errors from
  # crashing runParBatch
  tryPar <- try(runParBatch(  nSims = nSims[bIdx], 
                              nCores = nCores[bIdx] ) )

  if( class( tryPar ) == "try-error" )
  {
    # add copy successful runs code here
    warning("Error in Par batch, blerch.\n")
    break
  } 

  # Create batch report
  renderBatchReport( plotMPs = currMP[bIdx] )

  # Now copy mseRproject folder to an outside location so
  # it doesn't grow the size of the copied WDs, and then clear
  # the sims
  dateStamp     <- as.character(Sys.Date())
  batchDirName  <- paste( saveDirName[bIdx], dateStamp, sep = "_" )

  destFolder <- file.path("..","mseR_completedBatch",batchDirName)

  dir.create( destFolder )

  projFolderContents <- list.files( "./mseRproject")

  # Only copy the batch folder, statistics folder
  # reports folder, and sim folders
  batchContents <- projFolderContents[grepl(x = projFolderContents, pattern = "batch")]
  simContents   <- projFolderContents[grepl(x = projFolderContents, pattern = "sim")]
  statsContents <- projFolderContents[grepl(x = projFolderContents, pattern = "statistics")]
  rptContents   <- projFolderContents[grepl(x = projFolderContents, pattern = "reports")]


  copyFiles <- union( union(  union(  batchContents, 
                                      simContents ),
                              statsContents ), 
                      rptContents )
  for( fIdx in 1:length(copyFiles) )
  {
    source <- file.path("./mseRproject",copyFiles[fIdx])
    
    system( paste( "cp -r", source, destFolder ) )  

  }

  # Removing sims from proj Folder
  for( fIdx in 1:length(simContents) )
  {
    dirName <- file.path("./mseRproject",simContents[fIdx])
    
    system( paste( "rm -r", dirName ) )  

  }

  message(  "Parallel batch complete for ",
            batchControlFiles[bIdx],
            " and mseRproject folder copied and tidied up for next batch.\n",sep = "")
}

  message(  "All batch jobs complete!\n" )

