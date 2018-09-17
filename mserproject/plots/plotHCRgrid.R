# plot HCRs for WCVI in a 3x2 grid,
#             HR.2 | HR.1 
# minEfixed
# minErel
# HockeyStick

source("../../mseRtools.r")
source("plotBioFunctions.R")

labX <- .8
labY <- .9

par(mfrow = c(4,2), mar = c(2,2,2,2), oma =c(4,5,3,3) )

plotMinEscapementHCR( cutoff = 21.2, refHR = .2,
                      refB = 130.239, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MP 1, SOG\n 21.2kt Min Escapement")
mtext( side = 3, text = " Reference HR = 0.2", line = 2 )

plotMinEscapementHCR( cutoff = 21.2, refHR = .1,
                      refB = 130.239, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MPs 2-3, SOG\n 21.2kt Min Escapement")
mtext( side = 3, text = "Reference HR = 0.1", line = 2 )


plotMinEscapementHCR( cutoff = 18.8, refHR = .2,
                      refB = 45.6158, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MP 1, WCVI\n 18.8kt Min Escapement")

plotMinEscapementHCR( cutoff = 18.8, refHR = .1,
                      refB = 45.6158, cutoffType = "absolute",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MPs 2-3, WCVI\n 18.8kt Min Escapement")



plotMinEscapementHCR( cutoff = .5, refHR = .2,
                      refB = 45.6158, cutoffType = "relative",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MP 4" )
panLab(x = labX, y = labY-.1, txt = expression(paste(.5*B[0], " Min Escapement", sep = "")) )

plotMinEscapementHCR( cutoff = .5, refHR = .1,
                      refB = 45.6158, cutoffType = "relative",
                      yLim = c(0,.3),
                      xLab = "",
                      yLab = "" )
panLab(x = labX, y = labY, txt = "MPs 5-6" )
panLab(x = labX, y = labY-.1, txt = expression(paste(.5*B[0], " Min Escapement", sep = "")) )

plotHockeyStickHCR( LRP = .3, USR = .6,
                    refHR = .2,
                    refB = 1,
                    yLim = c(0,.3),
                    xLab = "",
                    yLab = "" )
panLab(x = labX, y = labY, txt = "MP 7\n Hockey Stick Rule" )

plotHockeyStickHCR( LRP = .3, USR = .6,
                    refHR = .1,
                    refB = 1,
                    yLim = c(0,.3),
                    xLab = "",
                    yLab = "" )
panLab(x = labX, y = labY, txt = "MPs 8-9\nHockey Stick Rule" )
# mtext( side = 4, text = "Hockey Stick Rule", line = .5 )
# mtext( side = 4, text = expression(paste("LCP = ", .3*B[0], sep = "")), line = 2 )
# mtext( side = 4, text = expression(paste(" UCP = ", .6*B[0],sep = "")), line = 3.3, adj = 0.4 )

mtext( side = 1, outer = T, text = expression( paste("Stock Status ", B[t] / B[0],sep = "") ), line = 2 )
mtext( side = 2, outer = T, text = expression( paste("Target HR ", C[t] / B[t],sep = "") ), line =2 )