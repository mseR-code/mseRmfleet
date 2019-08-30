# Sablefish opMod 2015

require( gridExtra )
require( PBSmodelling )
require( RColorBrewer )
library( scales )
library( SDMTools )

source( "sableOpmod.r" )
source( "lisread.r" )
source( "mserTools.r" )
source( "sca_globals.r" )

repList <- lisread( "sableopmod.rep" )
ctlList <- lisread( "opModControl.ctl" )

pngHeight <- 768
pngWidth <- 1024
ptSize <- 18
ptSize2 <- 24

nT <- repList$nT

graphics.off()

# Observed data and model fits.

# Age data.
.OMA <- c( 3,3,3,2 ) 
.MAR <- c( 2,2,2,1 )

png( "plotObsAgeProp_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_m1, seriesNames="Trap: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_f1, seriesNames="Trap: Females", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_m2, seriesNames="Trawl: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_f2, seriesNames="Trawl: Females", gfx=list( bygears=TRUE ) )
dev.off()


png( "plotObsAgeProp_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_m3, seriesNames="Std: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_f3, seriesNames="Std: Females", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_m4, seriesNames="StRS: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeProp_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeBubbles( repList$ageObsProp_f4, seriesNames="StRS: Females", gfx=list( bygears=TRUE ) )
dev.off()

.OMA <- c( 3,3,2,1 )
.MAR <- c( 2,2,1,1 ) 

# Observed age frequency.
png( "plotObsAgeFreq_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_m1, seriesNames="Trap: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_f1, seriesNames="Trap: Females", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_m2, seriesNames="Trawl: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_f2, seriesNames="Trawl: Females", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_m3, seriesNames="Std: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_f3, seriesNames="Std: Females", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_m4, seriesNames="StRS: Males", gfx=list( bygears=TRUE ) )
dev.off()

png( "plotObsAgeFreq_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotCatAgeFreq( repList$ageObsProp_f4, seriesNames="StRS: Females", gfx=list( bygears=TRUE ) )
dev.off()

# Abundance indices.
.OMA <- c( 3,3,1,1 )
.MAR <- c( 2,2,1,1 )

png( "plotIndices.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR, mfrow=c(3,1) )
.plotIndices( repList, seriesNames=c("Trap","Std.","StRS"),
	          gfx=list( bygears=TRUE ) )
dev.off()

# Model catch by gear.
png( "plotModCatGear.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,5,1,1), mar=c(2,2,0.5,0.5), mfrow=c(5,1) )
.plotModCat( repList, seriesNames=c("Trap","Hook","Trawl","Std.","StRS"),
	         gfx=list(annotate=TRUE, bygears=TRUE) )
dev.off()

# Estimated stock indices.
png( "plotMLEindices.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,3,1,1), mar=c(2,2,1,1), mfrow=c(3,1) )
.plotMLEindices( repList, gfx=list(annotate=TRUE, bygears=TRUE) )
dev.off()

# Releases
png( "plotFitReleases.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,3,1,1), mar=c(2,2,1,1), mfrow=c(3,1) )
.plotMLEdiscards( repList, gfx=list( annotate=TRUE, bygears=TRUE ) )
dev.off()

# Releases
png( "plotObsReleases.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,3,1,1), mar=c(2,2,1,1), mfrow=c(3,1) )
.plotMLEdiscards( repList, 
                  gfx=list( annotate=TRUE, bygears=TRUE, xLim = c(42,nT) ), 
                  fit = FALSE )
dev.off()

# Model Catch.
png( "plotModCat.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(2,2,1,1), mar=c(2,2,1,1) )
.plotModCat( repList )
dev.off()


# Model age fits.
.OMA <- c( 3,4,1,0.5 )
.MAR <- c( 2,2,1,0 ) 
png( "plotFitAgeFreq_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_m1, repList$uCgta_m1, seriesNames="Trap: Males",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_f1, repList$uCgta_f1, seriesNames="Trap: Females",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_m2, repList$uCgta_m3, seriesNames="Trawl: Males",
  gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_f2, repList$uCgta_f3, seriesNames="Trawl: Females",
  gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()


png( "plotFitAgeFreq_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_m3, repList$uCgta_m4, seriesNames="Std: Males",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_f3, repList$uCgta_f4, seriesNames="Std: Females",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_m4, repList$uCgta_m5, seriesNames="StRS: Males",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

png( "plotFitAgeFreq_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageFreq( repList$ageObsProp_f4, repList$uCgta_f5, seriesNames="StRS: Females",
	gfx=list( annotate=TRUE, bygears=TRUE, average = FALSE ) )
dev.off()

# Plot age fits averaged over time, with all gears on the same plot
png( "plotFitAgeFreq_avg.png", width = pngWidth, height = pngHeight, pointsize = ptSize )
par( mfcol = c(4,2), oma=c(3,4,1,1), mar=c(2,2,1.5,1) )
# Plot males
# Trap
.plotMLEageFreq( repList$ageObsProp_m1, repList$uCgta_m1, seriesNames="Trap: Males",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# Trawl
.plotMLEageFreq( repList$ageObsProp_m2, repList$uCgta_m3, seriesNames="Trawl: Males",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# Std
.plotMLEageFreq( repList$ageObsProp_m3, repList$uCgta_m4, seriesNames="Std: Males",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# StRS
.plotMLEageFreq( repList$ageObsProp_m4, repList$uCgta_m5, seriesNames="StRS: Males",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )

# Plot females
# Trap
.plotMLEageFreq( repList$ageObsProp_f1, repList$uCgta_f1, seriesNames="Trap: Females",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# Trawl
.plotMLEageFreq( repList$ageObsProp_f2, repList$uCgta_f3, seriesNames="Trawl: Females",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# Std
.plotMLEageFreq( repList$ageObsProp_f3, repList$uCgta_f4, seriesNames="Std: Females",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
# StRS
.plotMLEageFreq( repList$ageObsProp_f4, repList$uCgta_f5, seriesNames="StRS: Females",
  gfx=list( annotate=TRUE, bygears=TRUE, average = TRUE ) )
dev.off()

# Plot average residuals - similar to above. Probably want the
# standard devs put on as well.
png( "plotAgeResids_avg.png", width = pngWidth, height = pngHeight, pointsize = ptSize )
par( mfcol = c(4,2), oma=c(3,4,1,1), mar=c(2,2,1.5,1) )
# Males
# Trap
.plotMLEageResids_Ave(  repList$ageObsProp_m1, repList$uCgta_m1, 
                        seriesNames="Trap: Males",
                        gfx=list( annotate=TRUE ) )
# Trawl
.plotMLEageResids_Ave(  repList$ageObsProp_m2, repList$uCgta_m3, 
                        seriesNames="Trawl: Males",
                        gfx=list( annotate=TRUE ) )

# Std
.plotMLEageResids_Ave(  repList$ageObsProp_m3, repList$uCgta_m4, 
                        seriesNames="Std: Males",
                        gfx=list( annotate=TRUE ) )
# StRS
.plotMLEageResids_Ave(  repList$ageObsProp_m4, repList$uCgta_m5, 
                        seriesNames="StRS: Males",
                        gfx=list( annotate=TRUE ) )

# Females
# Trap
.plotMLEageResids_Ave(  repList$ageObsProp_f1, repList$uCgta_f1, 
                        seriesNames="Trap: Females",
                        gfx=list( annotate=TRUE ) )
# Trawl
.plotMLEageResids_Ave(  repList$ageObsProp_f2, repList$uCgta_f3, 
                        seriesNames="Trawl: Females",
                        gfx=list( annotate=TRUE ) )
# Std
.plotMLEageResids_Ave(  repList$ageObsProp_f3, repList$uCgta_f4, 
                        seriesNames="Std: Females",
                        gfx=list( annotate=TRUE ) )
# StRS
.plotMLEageResids_Ave(  repList$ageObsProp_f4, repList$uCgta_f5, 
                        seriesNames="StRS: Females",
                        gfx=list( annotate=TRUE ) )
dev.off()

# Bubble plots for age residuals.
.OMA <- c( 3,3,2,1 ) 
.MAR <- c( 2,2,2,1 )

png( "plotAgeResids_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_m1, repList$uCgta_m1, seriesNames="Trap: Males",
	gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_f1, repList$uCgta_f1, seriesNames="Trap: Females",
	gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_m2, repList$uCgta_m3, seriesNames="Trawl: Males",
  gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_f2, repList$uCgta_f3, seriesNames="Trawl: Females",
  gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_m3, repList$uCgta_m4, seriesNames="Std: Males",
	gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_f3, repList$uCgta_f4, seriesNames="Std: Females",
	gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_m4, repList$uCgta_m5, seriesNames="StRS: Males",
	gfx=list( annotate=TRUE ) )
dev.off()

png( "plotAgeResids_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR )
.plotMLEageResids( repList$ageObsProp_f4, repList$uCgta_f5, seriesNames="StRS: Females",
	gfx=list( annotate=TRUE ) )
dev.off()

# Parameter estimates.
png( "plotEstimatesTable.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
.plotEstimatesTable( repList )
dev.off()

.makeStdErrTable( repList )
.makeEstimatesTable( repList )
.makeLikelihoodCptsTable( repList )
.makePriorCptsTable( repList )


# Fixed inputs.

.OMA <- c( 1,1,1,1 )
.MAR <- c( 3,4,1,1 )

# Weight~Length.

png( "plotFixedInputs.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR, mfrow=c(2,2) )

#png( "plotWgtLen.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=.OMA, mar=.MAR )
.plotWgtLen( ctlList, gfx=list( annotate=TRUE ) )
#dev.off()

# Length at age.
#png( "plotLenAtAge.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=.OMA, mar=.MAR )
.plotLenAtAge( repList )
#dev.off()

# Maturity at age.
#png( "plotMatAtAge.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotMatAtAge( repList )
#dev.off()

# Weight at age.
#png( "plotWgtAtAge.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=.OMA, mar=.MAR )
.plotWgtAtAge( repList )
dev.off()

# Recruitment plots.

# Recruitment Rt age-1

.OMA <- c( 1,1,1,1 )
.MAR <- c( 3,3,1,1 )
png( "plotMLErecAge1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
.plotMLErecAge1( repList, gfx=list(annotate=TRUE, doLegend=TRUE) )
dev.off()

# Recruitment deviations.
png( "plotMLErecDevs.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=.OMA, mar=.MAR, mfrow=c(1,1) )
.plotMLErecDevs( repList )
dev.off()

# State variables.
png( "plotMLEbiomass.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(2,2,1,1), mar=c(3,3,1,1), mfrow=c(1,1) )
.plotMLEbiomass( repList )
dev.off()

# Harvest rates.
png( "plotMLEharvestRate.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(1,1,1,1), mar=c(3,4,1,1), mfrow=c(1,1) )
.plotMLEharvestRate( repList, label=NULL, gfx=list( annotate=TRUE, bygears=TRUE,
                                                    doLegend=TRUE ) )
dev.off()

# Selectivity.
#    p <- .plotSelLen( admbPin, admbDat$ctlPars$domeSelectivity,
#            gfx=list( doAnnotate=estAnnotate, doBW=estBW, doFacet=estFacet,
#            doLegend=estLegend, doVerbose=estVerbose, xLim=NULL, yLim=NULL ) )

# Estimated selectivity at age.
png( "plotSalg.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#png( "plotSelAge_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,4,3,3), mar=c(2,2,0.5,1), mfrow=c(5,2) )
.plotSalg( repList$sel_gta_m1, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = TRUE ) )
#dev.off()
mtext( side = 3, text = "Males", line = 1.2, font = 2)

#png( "plotSelAge_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_f1, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE  ) )
mtext( side = 3, text = "Females", line = 1.2, font = 2)
mtext( side = 4, text = "Trap", line = 1.2, font = 2 )
#dev.off()

#png( "plotSelAge_m2.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_m2, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))

#dev.off()

#png( "plotSelAge_f2.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_f2, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))
mtext( side = 4, text = "Hook", line = 1.2, font = 2 )
#dev.off()

#png( "plotSelAge_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_m3, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))

#dev.off()

#png( "plotSelAge_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_f3, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))
#dev.off()
mtext( side = 4, text = "Trawl", line = 1.2, font = 2 )

#png( "plotSelAge_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_m4, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()

#png( "plotSelAge_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_f4, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "Std", line = 1.2, font = 2 )

#png( "plotSelAge_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_m5, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()

#png( "plotSelAge_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$sel_gta_f5, seriesName=NULL,
  gfx=list( annotate=TRUE, doLegend = FALSE ) )
mtext( side = 4, text = "StRs", line = 1.2, font = 2 )
dev.off()

# Estimated selectivity at age.
png( "plotSalgU.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#png( "plotSelAge_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,4,3,3), mar=c(2,2,0.5,1), mfrow=c(5,2) )
.plotSalg( repList$selU_gta_m1, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = TRUE ) )
#dev.off()
mtext( side = 3, text = "Males", line = 1.2, font = 2)

#png( "plotSelAge_f1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_f1, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE  ) )
mtext( side = 3, text = "Females", line = 1.2, font = 2)
mtext( side = 4, text = "Trap", line = 1.2, font = 2 )
#dev.off()

#png( "plotSelAge_m2.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_m2, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))

#dev.off()

#png( "plotSelAge_f2.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_f2, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))
mtext( side = 4, text = "Hook", line = 1.2, font = 2 )
#dev.off()

#png( "plotSelAge_m3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_m3, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))

#dev.off()

#png( "plotSelAge_f3.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_f3, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ))
#dev.off()
mtext( side = 4, text = "Trawl", line = 1.2, font = 2 )

#png( "plotSelAge_m4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_m4, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()

#png( "plotSelAge_f4.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_f4, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "Std", line = 1.2, font = 2 )

#png( "plotSelAge_m5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_m5, seriesName=NULL,
  gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()

#png( "plotSelAge_f5.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#par( oma=c(2,2,1,1), mar=c(3,3,1,1) )
.plotSalg( repList$selU_gta_f5, seriesName=NULL,
  gfx=list( annotate=TRUE, doLegend = FALSE ) )
mtext( side = 4, text = "StRs", line = 1.2, font = 2 )
dev.off()

png( "plotAgeErrorMtx.png", width = 8, height = 8,
      units = "in", res = 300 )
.plotAgeErrorMatrix( obj = repList )
dev.off()


# Estimated selectivity at age.
png( "plotSlg.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
#png( "plotSelAge_m1.png", width=pngWidth, height=pngHeight, pointsize=ptSize )
par( oma=c(3,4,3,3), mar=c(2,2,0.5,1), mfrow=c(5,2) )
.plotSlg( repList, gearNum = 1, sexNum = 1,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
mtext( side = 3, text = "Male", line = 1.2, font = 2)
.plotSlg( repList, gearNum = 1, sexNum = 2,
          gfx=list( annotate=FALSE, doLegend = TRUE ) )
mtext( side = 3, text = "Female", line = 1.2, font = 2)
#dev.off()
mtext( side = 4, text = "Trap", line = 1.2, font = 2)

.plotSlg( repList, gearNum = 2, sexNum = 1,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
.plotSlg( repList, gearNum = 2, sexNum = 2,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "Hook", line = 1.2, font = 2)

.plotSlg( repList, gearNum = 3, sexNum = 1,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
.plotSlg( repList, gearNum = 3, sexNum = 2,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "Trawl", line = 1.2, font = 2)

.plotSlg( repList, gearNum = 4, sexNum = 1,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
.plotSlg( repList, gearNum = 4, sexNum = 2,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "Std.", line = 1.2, font = 2)

.plotSlg( repList, gearNum = 5, sexNum = 1,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
.plotSlg( repList, gearNum = 5, sexNum = 2,
          gfx=list( annotate=FALSE, doLegend = FALSE ) )
#dev.off()
mtext( side = 4, text = "StRS", line = 1.2, font = 2)

dev.off()

png("plotRefCurvesF.png", width = 8.5, height = 11, units = "in",
      res = 300 )
.plotRefCurvesF(repList)
dev.off()

png("plotRefCurvesU.png", width = 8.5, height = 11, units = "in",
      res = 300 )
.plotRefCurvesU(repList)
dev.off()

