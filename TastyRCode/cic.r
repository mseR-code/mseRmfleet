# CIC talk.

result <- list()
result$dg    <- 1
result$nGear <- 1
result$L50Dg <- 57
result$L95Dg <- 50
result$sizeLim <- 55
result$Lal <- 30

.plotPlg( result, gfx=list( autolayout=FALSE, annotate=FALSE, bygear=TRUE,
          doLegend=FALSE, xLim=c(30,80), yLim=c(0.0,1.0) ) )