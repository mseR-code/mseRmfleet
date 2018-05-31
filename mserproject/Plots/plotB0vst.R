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


MPs <- c( "minE18.8_HR.2" )

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
            arrange(scenarioLabel,mpLabel)

scenList <- unique( info.df$scenarioLabel )

yrs <- seq(1951,by = 1, length = 92)
nT <- 92
tMP <- 68

par( mfrow =c(2,length(MPs) ), mar =c(1,1,1,1), oma = c(3,3,1,1) )

for( scenIdx in 1:length(scenList) )
{
  scen <- scenList[scenIdx]
  for( mIdx in 1:length(MPs) )
  {
    mp <- MPs[mIdx]

    df.sub <-   info.df %>%
                filter( scenarioLabel == scen,
                        mpLabel == mp )

    simID     <- df.sub[1,]$simLabel
    if(is.na(simID)) next

    simPath  <- file.path("..",simID,paste(simID, ".RData", sep = "") )
    load(simPath)

    stamp <- paste( scen, mp, sep = ":")

    # arrange B0 estimates in a matrix
    B0estimates <- blob$mp$assess$mpdPars$SSB0.sbo
    SSB0mat <- matrix(blob$ctlList$opMod$B0, nrow = 100, ncol = nT )
    for( i in 1:100 )
    {
      SSB0mat[i,68:92] <- B0estimates[1:25 + (i-1)*25]
    }

    SSB0dist <- apply(  X = SSB0mat, FUN = quantile, probs = c(.025, .5, .975),
                        MARGIN = 2)

    traces <- sample(x = 1:100, size = 3 )

    plot( x = range(yrs), y = c(0,max(B0estimates) ), type = "n", 
          xlab = "", ylab = "", las = 1 )
      polygon(  x = c(yrs,rev(yrs)), y = c(SSB0dist[1,],rev(SSB0dist[3,])),
                border = NA, col = "grey70" )
      lines( x = yrs, y = SSB0dist[2,], lwd = 3 )
      for( t in traces)
        lines( x = yrs, y = SSB0mat[t,], lwd = .8)
      panLab( x = 0.3, y = 0.3, txt = stamp )

  }
  mtext( side = 1, text = "Year", outer =T, line = 2)
  mtext( side =2, text = expression(paste("Est. ", B[0] (kt), sep = "" ) ), line = 1.5, outer = T)
}