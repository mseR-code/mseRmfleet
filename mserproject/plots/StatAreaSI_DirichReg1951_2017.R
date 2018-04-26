
###############################################################################
# Dirichlet regression to model how proportion of SI in each PFMA area is related to total spawn index for all survey years (1951-2017)

# Q2. Is proportion of spawn index in each area related to the total biomass? 

# Interpretation of precision parameter:
# https://stats.stackexchange.com/questions/286488/interpretation-of-the-precision-model-in-dirichlet-regression

# Cumulative spawn maps
#http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/pages/project-eng.html

#http://www.pac.dfo-mpo.gc.ca/science/species-especes/pelagic-pelagique/herring-hareng/herspawn/wcvi_map-eng.html

###############################################################################

rm(list = ls()) 

library(dplyr)
library(tidyr)
library(RColorBrewer)


# --- Data prep ---

# read in spawn index by stat area
dat <- read.csv('../history/StatAreaSI.csv')

# assign 0 for NAs in PFMA 25
dat[is.na(dat$SI),]$SI <-0

# Summarize proportions 
si <- tidyr::spread(dat,StatArea,SI)
wcviSI <- rowSums(si[,2:4])/1000

# convert to proportions
props <- (si[,2:4])/rowSums(si[,2:4])
props$wcviSI <- wcviSI

###############################
# --- Pairwise Correlations ---
###############################
pair.dat <- cbind(wcviSI, si[,2:4]/1000)


# # # Check collinearity for predictor variables of interest
# pairs(pair.dat, cex=0.1, fig=TRUE)

# # scatterplots - baseplot
# library(car)
# scatterplotMatrix(pair.dat,pch=19,cex=.5,reg.line=F, lwd.smooth=1.25,
# spread=F,ellipse=T, col=c('gray60','#2957FF','#FF8000'), col.axis='gray50')

# ggpairs(pair.dat,
# 		lower = list(continuous = "smooth_loess"))

# quartz.save('figs/StatAreaSI_pairsplot.R')


###################################################
# --- Dirichlet regression - ALL DATA ---
###################################################

library(DirichletReg)
library(colorspace)

drDat <- DR_data(props[,1:3])
rowSums(drDat)
# DR_data normalizes proportions to add to 1 (e.g. row 4,30)

# -- ternary plot ---
plot(drDat, cex=1, a2d=list(colored=T, c.grid=T))
# quartz.save('figs/StatAreaSI_ternaryPlot.png', type='png')

# -- Composition plot vs total spawn index --
clrs <- c('#1b9e77', '#d95f02', '#7570b3')

quartz(w=8, h=6.6)
plot(x=wcviSI, y=props[,1], ylim=0:1, pch=21, bg=clrs[1],
     xlab="Observed Spawn Index (1000 t)", ylab="Proportion",
     main='All Survey Years (1951-2017)')
points(x=wcviSI, y=props[,2], pch=21, bg=clrs[2])
points(x=wcviSI, y=props[,3], pch=21, bg=clrs[3])


# legend
legend('topright', bty='n',
		legend=c('PFMA 23', 'PFMA 24', 'PFMA 25'),
		pt.bg=clrs, pch=21)
# quartz.save('figs/StatAreaSI_CompositionPlot.png', type='png')
graphics.off()


# --- Fit Dirichlet regression ----

# Fit Dirichlet regression model using common parametrizations, as in Hijazi (2003) and Hijazi and Jernigan (2009).

fit1 <- DirichReg(drDat ~ wcviSI, model='common')
fit1

# Fit 2nd and 3rd models using quadratic & cube terms for SI

fit2 <- DirichReg(drDat ~ wcviSI + I(wcviSI^2), model='common')
fit3 <- DirichReg(drDat ~ wcviSI + I(wcviSI^2) + I(wcviSI^3), model='common')

anova(fit1, fit2) # fit2 is noy significantly better
anova(fit2, fit3) # fit3 is significantly better

# null model
fit0 <- DirichReg(drDat ~ 1, model='common')

# Compare Log likelihoods and calc AICc
logLik(fit1); logLik(fit2); logLik(fit3)

# Create stats summary
stats <- data.frame(model = c(0,1,2,3),
		desc = c('intecept only', 'linear','quadratic', 'cubic'),
		df = c(fit0$npar,fit1$npar, fit2$npar, fit3$npar),
		logLik = c(logLik(fit0), logLik(fit1), logLik(fit2), logLik(fit3) ),
		AIC = c(AIC(fit0), AIC(fit1), AIC(fit2), AIC(fit3))
		)

# Compare AICc
stats$AICc<- c(AIC(fit0) + 2*fit0$npar*(fit0$npar+1)/(nrow(drDat)-fit0$npar-1),
			   AIC(fit1) + 2*fit1$npar*(fit1$npar+1)/(nrow(drDat)-fit1$npar-1),
			   AIC(fit2) + 2*fit2$npar*(fit2$npar+1)/(nrow(drDat)-fit2$npar-1),
			   AIC(fit3) + 2*fit3$npar*(fit3$npar+1)/(nrow(drDat)-fit3$npar-1)
			   )

write.csv(stats, 'tables/StatArea_DirchletRegFits_1951_2017.csv', row.names=F) 

# --- Generate plots with predictions ---

fitList <- list(fit1=fit1, fit2=fit2, fit3=fit3)

for (j in 1:length(fitList))
{
	index=j
	fit <- fitList[index][[1]]

	# Generated smoothed predictions curvers
	fitted(fit) # only provided preds for observed depth values
	oldDat <- data.frame(wcviSI=wcviSI)
	newDat <- data.frame(wcviSI=1:max(wcviSI))
	preds <- predict(fit, newdata=newDat, mu=T)
	rowSums(preds)


	quartz(w=8, h=6.6)
	par(mar=c(4,4,2,4), mgp=c(2.2,.8,0))
	plot(x=wcviSI, y=props[,1], ylim=0:1, pch=21, bg=clrs[1], las=1,
	     xlab="Observed Spawn Index (1000 t)", ylab="Proportion",
	     main=paste('All Survey Years (1951-2017) -',names(fitList[j]) ))
	     
	points(x=wcviSI, y=props[,2], pch=21, bg=clrs[2])
	points(x=wcviSI, y=props[,3], pch=21, bg=clrs[3])

	lines(x=newDat$wcviSI, y=preds[,1], col=clrs[1])
	lines(x=newDat$wcviSI, y=preds[,2], col=clrs[2])
	lines(x=newDat$wcviSI, y=preds[,3], col=clrs[3])

	# Estimate precisions parameter, sum of alpha coefficients

	# add second y-axes for precision (alpha0 = phi)
	alpha0 <- rowSums(fit$fitted$alpha)
	phi <- fit$fitted$phi # same as alpha0

	# gen predicted phi line
	predPhi <- predict(fit, newdata=newDat, mu=F, phi=T )
		
	par(new = T)
	plot (x=newDat$wcviSI, y=predPhi, ylim = c(0, max(predPhi)), col= "black",
		  type="l", lty=3,axes = F, xlab = NA, ylab =NA)
	# points(x=wcviSI, y=alpha0, pch=3)	  		
	axis(side = 4, las=1)
	mtext(side = 4, line = 2.5, 
		expression(paste("Precision (", phi, ")", sep = "")))

	legend("top", legend = c(expression(hat(mu[c] == hat(alpha)[c]/hat(alpha)[0])),
		expression(hat(phi) == hat(alpha)[0])), lty = c(1, 2), lwd = c(3, 2), bty = "n")

	legend('topleft', bty='n',
			legend=c('PFMA 23', 'PFMA 24', 'PFMA 25'),
			pt.bg=clrs, pch=21)

	filename <- paste('figs/StatArea_DR1951_2017_',names(fitList[j]),
					 '.png', sep='')
	quartz.save(filename, type='png')

}

graphics.off()


#--- Multipanel plot with all SI areas ---

	newDat <- data.frame(wcviSI=1:max(wcviSI))
	preds1 <- predict(fit1, newdata=newDat, mu=T)
	preds2 <- predict(fit2, newdata=newDat, mu=T)
	preds3 <- predict(fit3, newdata=newDat, mu=T)
	# rowSums(preds1);rowSums(preds2); rowSums(preds3)


	quartz(w=8, h=6.6)
	par(mfrow=c(2,2), mar=c(3,4,0.5,0), mgp=c(1.8,.5,0), tck=-0.03,
		oma=c(0,0,2,0))
	for (i in 1:ncol(drDat))
	{	
		plot(x=wcviSI, y=props[,i], ylim=0:1, pch=21, bg=clrs[i],
		type='p',las=1,
	     xlab="Observed Spawn Index (1000 t)", ylab="Proportion")
		lines(x=newDat$wcviSI, y=preds1[,i], col='black', lwd=1, lty=1)
		lines(x=newDat$wcviSI, y=preds2[,i], col='black', lwd=1, lty=2)
		lines(x=newDat$wcviSI, y=preds3[,i], col='black', lwd=1, lty=3)
		text(x=1,y=.95, adj=0, names(props)[i])
	}	
	# legend
	plot.new()
	legend('topleft', bty='n', title='SI areas',
			legend=c(names(props)[1:3]), pt.bg=clrs, pch=21)
	legend('topright', bty='n', title='Dirichlet Regression Model Fits',
			legend=c('linear','quadratic','cubic'),
			lty=c(1,2,3))
	mtext(outer=T, side=3, line=0,
		  text=paste('Stat Area DirichletRegModel - fits 1,2,3'))

	filename <- paste('figs/StatArea_DR1951_2017_mutipanelfits.pdf', sep='')
	quartz.save(filename, type='pdf')


