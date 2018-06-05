# plot HCRs for WCVI in a 3x2 grid,
#             HR.2 | HR.1 
# minEfixed
# minErel
# HockeyStick

source("../../mseRtools.r")
source("plotBioFunctions.R")

par(mfrow = c(4,2), mar = c(2,2,2,2), oma =c(4,5,3,3) )

plotMinEscapementHCR( cutoff = 21.2, refHR = .2,
                      refB = 130.239, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
mtext( side = 3, text = " Reference HR = 0.2", line = 2 )

plotMinEscapementHCR( cutoff = 21.2, refHR = .1,
                      refB = 130.239, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
mtext( side = 3, text = "Reference HR = 0.1", line = 2 )
mtext( side = 4, text = "minE21.2", line = 2 )

plotMinEscapementHCR( cutoff = 18.8, refHR = .2,
                      refB = 45.6158, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )

plotMinEscapementHCR( cutoff = 18.8, refHR = .1,
                      refB = 45.6158, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
mtext( side = 4, text = "minE18.8", line = 2 )


plotMinEscapementHCR( cutoff = .5, refHR = .2,
                      refB = 45.6158, cutoffType = "relative",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )

plotMinEscapementHCR( cutoff = .5, refHR = .1,
                      refB = 45.6158, cutoffType = "relative",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
mtext( side = 4, text = "minE.5B0", line = 2 )

plotHockeyStickHCR( LRP = .3, USR = .6,
                    refHR = .2,
                    refB = 1,
                    yLim = c(0,.3),
                    xLab = "",
                    yLab = "" )

plotHockeyStickHCR( LRP = .3, USR = .6,
                    refHR = .1,
                    refB = 1,
                    yLim = c(0,.3),
                    xLab = "",
                    yLab = "" )
mtext( side = 4, text = "HS30-60", line = 2 )

mtext( side = 1, outer = T, text = expression( paste("Stock Status ", B[t] / B[0],sep = "") ), line = 2 )
mtext( side = 2, outer = T, text = expression( paste("Target HR ", C[t] / B[t],sep = "") ), line =2 )