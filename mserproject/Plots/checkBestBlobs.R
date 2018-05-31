# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)
require(dplyr)
require(ref)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")
source("../../mseRstats.R")
source("../../mseRrefPoints.R")
source("plotBioFunctions.R")

# MPs <- c( "NoFish",
#           "minE18.8_HR.2",
#           "minE18.8_HR.2_cap5",
#           "minE18.8_HR.1",
#           "minE18.8_HR.1_cap5",
#           "minE.5B0_HR.2",
#           "minE.5B0_HR.2_cap5",
#           "minE.5B0_HR.1",
#           "minE.5B0_HR.1_cap5",
#           "HS30-60_HR.2",
#           "HS30-60_HR.2_cap5",
#           "HS30-60_HR.1",
#           "HS30-60_HR.1_cap5",
#           "PerfectInfo_minE18.8_HR.2",
#           "PerfectInfo_minE18.8_HR.2_cap5",
#           "PerfectInfo_minE18.8_HR.1",
#           "PerfectInfo_minE18.8_HR.1_cap5",
#           "PerfectInfo_minE.5B0_HR.2",
#           "PerfectInfo_minE.5B0_HR.2_cap5",
#           "PerfectInfo_minE.5B0_HR.1",
#           "PerfectInfo_minE.5B0_HR.1_cap5",
#           "PerfectInfo_HS30-60_HR.2",
#           "PerfectInfo_HS30-60_HR.2_cap5",
#           "PerfectInfo_HS30-60_HR.1",
#           "PerfectInfo_HS30-60_HR.1_cap5" )

# currMPs <- c( "NoFish",
#               "minE18.8_HR.2",
#               "PerfectInfo_minE18.8_HR.2",
#               "minE18.8_HR.2_cap5",
#               "PerfectInfo_minE18.8_HR.2_cap5" )

# bestMP <- c(  "NoFish",
#               "minE.5B0_HR.1_cap5")

checkMPs <- c(  "NoFish",
                "minE.5B0_HR.1",
                "minE.5B0_HR.1_cap5")



# MPs <- MPs2

gfx=list( annotate=TRUE, doLegend=TRUE, grids=FALSE,
          showProj=TRUE, xLim=NULL, yLim=NULL, useYears=TRUE )

# List sims
sims <- list.files("../")
sims <- sims[grepl("sim",sims)]

readInfoFile <- function( sim )
{
  infoPath <- file.path("..",sim,paste(sim, ".info", sep = "") ) 
  info <- lisread(infoPath)
  info.df <- as.data.frame(info)
  info.df$simLabel <- sim

  info.df
}

qProbs <- c( 0.025, 0.25, 0.5, 0.75, 0.975 )
short <- c(68,73)
med   <- c(74,86)
long  <- c(87,92)

# Read in info files, sort by  scenarios
info.df <- lapply( X = sims, FUN = readInfoFile )
info.df <- do.call( "rbind", info.df ) %>%
            arrange(scenarioLabel,mpLabel) %>%
            mutate( mpLabel = as.character(mpLabel),
                    scenarioLabel = as.character(scenarioLabel) )


scenList <- unique( info.df$scenarioLabel )
# MPs       <- unique( info.df$mpLabel )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92


scen <- scenList[1]

info.df <-  info.df %>%
            filter( scenarioLabel == scen,
                    mpLabel %in% checkMPs )

blobList <- vector( mode = "list", length = length(checkMPs) )
SBtList <- blobList

par( mfrow = c(3,2) )

for( i in 1:length(checkMPs) )
{
  sub.df <- info.df %>%
            filter( mpLabel == checkMPs[i])
  simID <- sub.df$simLabel
  blobPath <- file.path("..",simID,paste(simID,".RData",sep = "") )
  load(blobPath)

  blobList[[checkMPs[i]]] <- blob
  Dept <- blob$om$SBt[,2:93] / blob$ctlList$opMod$B0

  SBtList[[i]] <- blob$om$SBt[,2:93]

  DeptGt.75 <- Dept
  DeptGt.75[DeptGt.75 < .75] <- 0
  DeptGt.75[DeptGt.75 >= .75] <- 1

  tdx <- 68:82
  DeptGt.75 <- DeptGt.75[,tdx]

  ProbGt.75 <- apply( X = DeptGt.75, FUN = sum,
                      MARGIN = 1 ) / length(tdx)

  hist(ProbGt.75, breaks = "FD", xlab = "Prob Gt .75 B0", main = checkMPs[i])
    abline(v = median(ProbGt.75))
  plot(ecdf(ProbGt.75))
    abline(v = .05)
  

}

cols <- brewer.pal(n=3, "Dark2")

for( rep in 1:100 )
{
  par(mfcol = c(4,1), oma = c(4,4,3,1), mar = c(2,2,2,2) )
  plot( x = c(68,92), y = c(0,90), xlab = "time", ylab = "Bt", type = "n", las = 1  )
    abline(v = c(68,82), lty = 3)
    abline(h = 0.75 * blob$ctlList$opMod$B0, lty = 4)
  for( k in 2:3)
  {
    lines( x= 1:92, y = blobList[[checkMPs[k]]]$om$SBt[rep,2:93],
            col = cols[k], lwd = 2, lty = k-1 )
  }
  mtext( side = 2, text = "Bt", line = 2 )

  panLegend( x = .4, y = 0.9, legTxt = checkMPs[2:3], bty = "n",
             col = cols[2:3], lwd = 2, lty = c(1,2) )

  plot( x = c(68,92), y = c(0,20), xlab = "time", ylab = "Ct", type = "n", las = 1  )
    abline(v = c(68,82), lty = 3)
    abline(h = 5, lty = 4 )
  for( k in 2:3)
  {
    lines( x= 1:92, y = blobList[[checkMPs[k]]]$om$Ct[rep,2:93],
            col = cols[k], lwd = 2, lty = k-1 )
  }
  mtext( side = 2, text = "Ct", line = 2 )

  plot( x = c(68,92), y = c(0,1), xlab = "time", ylab = "Mt" , type = "n", las = 1 )
    abline(v = c(68,82), lty = 3)
  for( k in 2:3)
  {
    lines( x= 1:92, y = blobList[[checkMPs[k]]]$om$Mt[rep,2:93],
            col = cols[k], lwd = 2, lty = k-1 )
  }
  mtext( side = 2, text = "Mt", line = 2 )

  plot( x = c(68,92), y = c(0,3000), xlab = "time", ylab = "Rt" , type = "n", las = 1 )
    abline(v = c(68,82), lty = 3)
  for( k in 2:3)
  {
    lines( x= 1:92, y = blobList[[checkMPs[k]]]$om$Rt[rep,2:93],
            col = cols[k], lwd = 2, lty = k-1 )
  }
  mtext( side = 2, text = "Rt", line = 2 )

  mtext( side = 3, text = paste("Replicate ", rep, sep = "" ), outer = T )
  mtext( side = 1, outer = T, text= "Time", line =2 )

  readline(prompt="Press [enter] to continue")
}

# dev.new()
# plot( blobList[[2]]$om$Ct)



