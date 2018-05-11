# External script to plot some mseR perf plots 
# and some cleveland plots

require(RColorBrewer)
library(DirichletReg)

# Leverages some mseR functions
source("../../mseRtools.R")
source("../../mseRglobals.R")
source("../../mseRoptions.R")
source("../../mseRplots.R")

source("plotBioFunctions.R")

rep <- 25

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

# Read in info files, sort by  scenarios
info.df <- lapply( X = sims, FUN = readInfoFile )
info.df <- do.call( "rbind", info.df )


pdf( file = "Bt_PFMAs_RW.pdf" )
plotPFMABt_RW(rep = 3,simNum  = 14, seedVal = 111, logPropSD =.2)
dev.off()

pdf( file = "Bt_PFMAs_DRlinear.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "linear")
dev.off()

pdf( file = "Bt_PFMAs_DRquadratic.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "quadratic")
dev.off()

pdf( file = "Bt_PFMAs_DRcubic.pdf" )
plotPFMABt_DR(simNum = 14, rep = 3, seedVal = 111, DR = "cubic")
dev.off()

pdf( file = "BtEnv_PFMAs_DRlinear.pdf" )
plotPFMABtEnv_DR(simNum = 14,  seedVal = 111, DR = "linear")
dev.off()

pdf( file = "BtEnv_PFMAs_DRquadratic.pdf" )
plotPFMABtEnv_DR(simNum = 14,  seedVal = 111, DR = "quadratic")
dev.off()

pdf( file = "BtEnv_PFMAs_RW.pdf" )
plotPFMABtEnv_RW(simNum = 14, seedVal = 111, logPropSD = .2)
dev.off()

pdf( file = "propEnv_PFMAs_DRlinear.pdf" )
plotPFMAProp_DR(simNum = 14, seedVal = 111, DR = "linear")
dev.off()

pdf( file = "propEnv_PFMAs_RW.pdf" )
plotPFMAProp_RW(simNum = 14, seedVal = 111, logPropSD = .2)
dev.off()