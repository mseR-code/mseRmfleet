# Plots multi-panel plots of BtFitMtUt, using mseRplots.R functions
# Best to use lapply() over the rep numbers

library(dplyr)
library(RColorBrewer)

source("../../mseRoptions.r")
source("../../mseRglobals.r")
source("../../mseRplots.r")
source("../../mseRtools.r")
source("plotBioFunctions.R")

reps <- 1:100


graphics.off()

X <- lapply(  X = reps, FUN = plotMPsMtBtFitUt,
              simFolder = "../slowUp_MCMC_Jul8",
              scenario = "WCVI_DIM",
              MPs = c("minE18.8_HR.2_slowUp2","minE18.8_HR.2_slowUp3","minE18.8_HR.2_slowUp4","minE18.8_HR.2_slowUp5"),
              saveFileRoot = "minE18.8_slowUp",
              saveFile = TRUE,
              yLimB = c(0,100),
              yLimM = c(0,1.3),
              yLimHR = c(0,1.0))

graphics.off()


X <- lapply(  X = reps, FUN = plotMPsMtBtFitUt,
              simFolder = "../slowUp_MCMC_Jul8",
              scenario = "WCVI_DDM",
              MPs = c("minE18.8_HR.2_slowUp2","minE18.8_HR.2_slowUp3","minE18.8_HR.2_slowUp4","minE18.8_HR.2_slowUp5"),
              saveFileRoot = "minE18.8_slowUp",
              saveFile = TRUE,
              yLimB = c(0,100),
              yLimM = c(0,1.3),
              yLimHR = c(0,1.0))

graphics.off()


X <- lapply(  X = reps, FUN = plotMPsMtBtFitUt,
              simFolder = "../slowUp_MCMC_Jul8",
              scenario = "WCVI_conM",
              MPs = c("minE18.8_HR.2_slowUp2","minE18.8_HR.2_slowUp3","minE18.8_HR.2_slowUp4","minE18.8_HR.2_slowUp5"),
              saveFileRoot = "minE18.8_slowUp",
              saveFile = TRUE,
              yLimB = c(0,100),
              yLimM = c(0,1.3),
              yLimHR = c(0,1.0))

graphics.off()


X <- lapply(  X = reps, FUN = plotMPsMtBtFitUt,
              simFolder = "../WCVI_allScenarios_MCMC",
              scenario = "WCVI_DIM",
              MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
              saveFileRoot = "minE18.8",
              saveFile = TRUE,
              yLimB = c(0,100),
              yLimM = c(0,1.3),
              yLimHR = c(0,1.0))

graphics.off()


X <-lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "WCVI_DDM",
        simFolder = "../WCVI_allScenarios_MCMC",
        MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
        saveFileRoot = "minE18.8",
        saveFile = TRUE,
        yLimB = c(0,100),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "WCVI_conM",
        simFolder = "../WCVI_allScenarios_MCMC",
        MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
        saveFileRoot = "minE18.8",
        saveFile = TRUE,
        yLimB = c(0,180),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "WCVI_DIM",
        simFolder = "../WCVI_allScenarios_MCMC",
        MPs = c("minE18.8_HR.1_cap2","minE.5B0_HR.1_cap2","HS30-60_HR.1_cap2"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,100),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "WCVI_DDM",
        simFolder = "../WCVI_allScenarios_MCMC",
        MPs = c("minE18.8_HR.1_cap2","minE.5B0_HR.1_cap2","HS30-60_HR.1_cap2"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,100),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "WCVI_conM",
        simFolder = "../WCVI_allScenarios_MCMC",
        MPs = c("minE18.8_HR.1_cap2","minE.5B0_HR.1_cap2","HS30-60_HR.1_cap2"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,180),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()


X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        simFolder = "../SOG_allScenarios_MCMC",
        scenario = "SOG_DIM",
        MPs = c("minE21.2_HR.2","minE21.2_HR.1","minE21.2_HR.1_cap30"),
        saveFileRoot = "minE21.2",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

X <- lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "SOG_DDM",
        simFolder = "../SOG_allScenarios_MCMC",
        MPs = c("minE21.2_HR.2","minE21.2_HR.1","minE21.2_HR.1_cap30"),
        saveFileRoot = "minE21.2",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "SOG_conM",
        simFolder = "../SOG_allScenarios_MCMC",
        MPs = c("minE21.2_HR.2","minE21.2_HR.1","minE21.2_HR.1_cap30"),
        saveFileRoot = "minE21.2",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "SOG_DIM",
        simFolder = "../SOG_allScenarios_MCMC",
        MPs = c("minE21.2_HR.1","minE.5B0_HR.1","HS30-60_HR.1"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "SOG_DDM",
        simFolder = "../SOG_allScenarios_MCMC",
        MPs = c("minE21.2_HR.1","minE.5B0_HR.1","HS30-60_HR.1"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

lapply( X = reps, FUN = plotMPsMtBtFitUt,
        scenario = "SOG_conM",
        simFolder = "../SOG_allScenarios_MCMC",
        MPs = c("minE21.2_HR.2","minE.5B0_HR.2","HS30-60_HR.2"),
        saveFileRoot = "diffHCRs",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0))

graphics.off()

lapply( X = reps, FUN = plotScenariosMtBtFitUt,
        simFolder = "../SOG_AM1MP_MCMC",
        scenarios = c("SOG_DDM","SOG_DIM","SOG_conM"),
        MP = c("AM1_minE21.2_HR.2"),
        saveFileRoot = "allScenarios",
        saveFile = TRUE,
        yLimB = c(0,250),
        yLimM = c(0,1.3),
        yLimHR = c(0,1.0) )

graphics.off()

plotMPsMtBtFitUt( iRep = 78,
                  scenario = "WCVI_DDM",
                  simFolder = "../WCVI_allScenarios_MCMC",
                  MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
                  saveFileRoot = "minE18.8",
                  saveFile = FALSE,
                  yLimB = c(0,120),
                  yLimM = c(0,1.3),
                  yLimHR = c(0,1.0))


plotMPsMtBtFitUt( iRep = 68,
                  scenario = "WCVI_conM",
                  simFolder = "../WCVI_allScenarios_MCMC",
                  MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
                  saveFileRoot = "minE18.8",
                  saveFile = FALSE,
                  yLimB = c(0,250),
                  yLimM = c(0,1.3),
                  yLimHR = c(0,1.0))


plotMPsMtBtFitUt( iRep = 73,
                  scenario = "SOG_conM",
                  simFolder = "../SOG_allScenarios_MCMC",
                  MPs = c("minE21.2_HR.2","minE.5B0_HR.2","HS30-60_HR.2"),
                  saveFileRoot = "diffHCRs",
                  saveFile = FALSE,
                  yLimB = c(0,250),
                  yLimM = c(0,1.3),
                  yLimHR = c(0,1.0))


# for( i in 1:100 )
# {
#   plotMPsMtBtFitUt( iRep = i,
#                     scenario = "WCVI_DDM",
#                     simFolder = "../WCVI_allScenarios_MCMC",
#                     MPs = c("minE18.8_HR.2","minE18.8_HR.1","minE18.8_HR.1_cap2"),
#                     saveFileRoot = "minE18.8",
#                     saveFile = FALSE,
#                     yLimB = c(0,200),
#                     yLimM = c(0,1.3),
#                     yLimHR = c(0,1.0))

#   mtext( side = 1, outer = T, at = c(0.8), text = paste("Rep ", i,"/100",sep = ""),
#           col = "grey60", line = 2 )
#   readline(prompt="Press [enter] to continue \n")
# }