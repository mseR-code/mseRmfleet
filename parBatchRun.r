library(parallel)
# set number of batch files to use
# nBatchFiles <- 30
# batchFiles <- 1:8
batchFiles <- c(1:12)
nBatchFiles <- length(batchFiles)
batchFolderNames <- paste("mseRBat",batchFiles,sep = "")

# Calls runSimEst in parallel inside a separate copy of the
# working directory, allowing for parallel system calls
doBatchRun <- function( batchFolderName )
{
  require(tools)
  cat("Running batchjob:",batchFolderName,"\n")

  # Set WD to the batch directory and assign the correct
  # control file to the global environment
  setwd(batchFolderName)

  batchCtlFile <- file.path( batchFolderName, "simCtlFile.txt" )
  # source mseR
  source("mser.r")

  # runMSE with the batch file
  # add random delay to offset simFolder names
  runMSE( ctlFile = batchCtlFile, saveBlob = TRUE )
}


# Load batch design
desPath <- file.path(getwd(),"mseRproject","batch","mseRbatch.design")
batchDesign <- read.csv(desPath, header=TRUE, skip=1,stringsAsFactors=FALSE)
batchParFile <- file.path( getwd(),"mseRproject","batch",basename( batchDesign$parFile ) )

# Get list of current files (we don't want to unnecessary duplication)
wdContents <- list.files("./")

for (i in 1:length(batchFiles))
{
  bNum <- batchFiles[i]
  # Create duplicate directory for experiment
  dir.create(batchFolderNames[i])
  # Copy necessary contents (there are some efficiency gains to be made
  # here)
  file.copy ( from = wdContents,
              to = batchFolderNames[i],
              recursive = TRUE )
  # copy batch control in to the new batch folder as the base simctl
  simCtlFile <- file.path(getwd(),batchFolderNames[i],"simCtlFile.txt")
  file.copy (batchParFile[bNum],simCtlFile,overwrite=TRUE)
}

# Now set # of cores and make a cluster
nCores  <- min(nBatchFiles,detectCores()-1)
cl      <- makePSOCKcluster(nCores)
# Run parallel batch
cat ("Running ", nBatchFiles, " simulations in parallel on ",
      nCores, " cores.\n", sep = "" )
tBegin    <- proc.time()
startDate <- date()
# append main WD path to the front of the batchFolderNames
batchFolderNames <- file.path(getwd(),batchFolderNames)
# browser()

tmp     <- parLapplyLB(cl, X=batchFolderNames, fun=doBatchRun)
# tmp <-lapply(X=file.path(getwd(),batchFolderNames), FUN=doBatchRun)
stopCluster(cl)

require(stringr)
for (i in 6:8)
{
  # Find the sim output folder in the project file
  batchProjDir <- file.path(batchFolderNames[i],"mseRproject")
  dirContents <- list.dirs(batchProjDir, full.names=FALSE,
                                  recursive=FALSE)
  simFolder <- dirContents[grep(pattern="sim",x=dirContents)]
  simFolderAppend <- paste(simFolder,i,sep = "")

  # Source folder:
  simFolderDir     <- file.path(batchProjDir,simFolder)
  simFolderAppDir  <- file.path(batchProjDir,simFolderAppend)
  file.rename ( simFolderDir, simFolderAppDir )
  infoRData <- grep ( pattern = simFolder, x = list.files(simFolderAppDir ))
  infoRData <- list.files ( simFolderAppDir, full.names = FALSE)[infoRData]
  file.rename(file.path(simFolderAppDir,infoRData[1]),
              file.path(simFolderAppDir,paste(simFolderAppend,".info",sep = "")))
  file.rename(file.path(simFolderAppDir,infoRData[2]),
              file.path(simFolderAppDir,paste(simFolderAppend,".RData",sep = "")))
  source <- simFolderAppDir

  # Set up the destination
  destination <- file.path(getwd(),"mseRproject")

  cat(  "\n", "Moving simulation ",i," sim folder to: ","\n",
        destination,"\n", sep="")

  # Now copy the completed simulation
  file.copy(from=source,to=destination,recursive=TRUE)

  # cat("Removing folder ", batchFolderNames[i], "\n", sep="")
  # system(command=paste("rm -d -R ",batchFolderNames[i],sep=""))
  options(warn=1)
}
elapsed <- (proc.time() - tBegin)[ "elapsed" ]
cat( "\nMSG (.runBatchJob): Elapsed time for parallel batch = ",
  round(elapsed/60.0,digits=2)," minutes.\n" )