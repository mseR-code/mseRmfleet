# Functions for plotting OM outputs

plotMtBtFitUt <- function(  )
{
  gfx <- list( useYears = TRUE, doLegend = FALSE, annotate = FALSE,
               showProj = FALSE, bygears = FALSE, yLim = c(0,1) )

  par(mfrow = c(3,1), mar =c(2,2,2,2), oma = c(3,3,1,1) )

  .plotMt(blob, gfx = gfx )
  panLegend(  x = 0.3, y = 0.3, bty = "n",
              legTxt = c( "OM", "Last Fit", "First Fit"),
              col = c("purple","red","darkgreen" ),
              lwd = c(2,2,2))

  gfx <- list( useYears = TRUE, doLegend = TRUE, annotate = FALSE,
               showProj = FALSE, bygears = FALSE, yLim = c(0,200) )

  .plotBtFit(blob, gfx = gfx )

  gfx <- list( useYears = TRUE, doLegend = TRUE, annotate = FALSE,
               showProj = FALSE, bygears = FALSE )

  .plotUt(blob, gfx = gfx )  
}


plotMinEscapementHCR <- function( cutoff = .5, refHR = .2,
                                  refB = 45.6158, cutoffType = "relative",
                                  yLim = c(0,.3),
                                  xLab = "Stock Status (Bt/B0)",
                                  yLab = "Target Harvest Rate (Ct/Bt)" )
{
  if( cutoffType == "absolute" ) 
    minE <- cutoff/refB
  if( cutoffType == "relative" )
    minE <- cutoff

  if(is.null(yLim))
    yLim <- c(0,1.5*refHR)

  rampTop <- minE / (1 - refHR)

  curveX <- seq( minE, 1, length = 1000)
  curveY <- curveX
  for( x in 1:length(curveX) ) 
    curveY[x] <- min( (refHR * curveX[x])/curveX[x],(curveX[x]-minE)/curveX[x])

  plot( x = c(0,1), y = yLim, type = "n", xlab = "", ylab = "", las = 1 )
    lines( curveX, curveY, lwd  = 3 )
    segments( x0 =0, x1 = minE, y0 = 0, lwd = 3 )
    abline( v = c(minE, rampTop), lwd = .8, lty = 2)
    abline( h = refHR, lwd = .8, lty = 3 )
    mtext( side = 1, text = xLab, line = 2 )
    mtext( side = 2, text = yLab, line = 3 )

}

plotHockeyStickHCR <- function( LRP = .3, USR = .6,
                                refHR = .2,
                                refB = 45.6158,
                                yLim = c(0,.3),
                                xLab = expression(paste("Stock Status (",B[t]/B[0],")",sep = "")),
                                yLab = "Target Harvest Rate (Ct/Bt)" )
{
  if(is.null(yLim))
    yLim <- c(0,1.5*refHR)


  plot( x = c(0,refB), y = yLim, type = "n", xlab = "", ylab = "", las = 1 )
    segments( x0 = 0, x1 = LRP * refB, y0 = 0, lwd = 3 )
    segments( x0 = LRP*refB, x1 = USR*refB, y0 = 0, y1 = refHR, lwd = 3 )
    segments( x0 = USR*refB, x1 = refB, y0 = refHR, y1 = refHR, lwd = 3 )
    abline( v = c(USR*refB, LRP * refB), lwd = .8, lty = 2)
    abline( h = refHR, lty = 3, lwd = .8)
    mtext( side = 1, text = xLab, line = 2 )
    mtext( side = 2, text = yLab, line = 3 )

}

# Acceptable probability of decline ramp, which interpolates
# between a 5% prob at the LRP (.3) and a 25% prob at the NCN
# TRP (.75)
plotAccDeclineRamp <- function( lowThresh = .3, lowProb = .05,
                                targThresh = .75, targProb = .25,
                                main = "DFO LRP + NCN Goal 1" )
{
  plot( x = c(0,1), y = c(0,1), type = "n",
        xlab = "SSB/B0", ylab = "Acceptable Probability of Decline",
        main = main )
    segments( x0 = c(0,lowThresh,targThresh), 
              x1 = c(lowThresh,targThresh,1),
              y0 = c(lowProb,lowProb,targProb), 
              y1 = c(lowProb,targProb,.95) )
    abline( v = c(lowThresh,targThresh), lty = 2, col = c("red","darkgreen") )
    panLab( x = 0.1, y = 0.9,
            txt = "Critical" )
    panLab( x = 0.5, y = 0.9,
            txt = "Cautious/Healthy" )
}

# Tradeoff plot between average catch and
# proportion of years closed
plotFishingOppTradeoff <- function( simNum = 1,
                                    info = info.df,
                                    thresh = .5 )
{
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  # model dimensions, years
  tMP <- blob$ctlList$opMod$tMP
  nT  <- blob$ctlList$opMod$nT
  yrs <- seq(1951,by = 1, length = nT)

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  Ct <- blob$om$Ct[,1 + tMP:nT]

  Ct_notest <- Ct - sum(blob$ctlList$opMod$testFishery)

  # Compute average catch
  aveCatch <- apply(X = Ct, FUN = mean, MARGIN = 1)
  maxCatch <- apply(X = Ct, FUN = max, MARGIN = 1)
  minCatch <- apply(X = Ct, FUN = min, MARGIN = 1)
  # Now compute proportion of years open/closed
  countAboveThresh <- function(Ct, thresh)
  {
    Ct[Ct > thresh] <- 1
    Ct[Ct <= thresh ] <- 0
    above <- sum(Ct)
    above
  }

  calcAAV <- function( Ct )
  {
    diffCt    <- diff(Ct)
    absDiffCt <- abs( diffCt )

    # Shift differences by 1 to catch transition from previous period
    # to current period, i.e., sum(|C_t-C_t-1|) for t=t1,..,t2.  For example,
    # if the current year is 2008, you have to get the first catch difference
    # of 2008-2007 to respect C_t - C_t-1, i.e., when t=2008, t-1=2007.

    # Sum the absolute differences of the shifted absolute catch differences.
    sumAbsDiffDt <- sum(absDiffCt)

    # Sum the catch by replicate over the summary period specified by tdx.
    sumCatch <- sum(Ct)

    # Compute the AAV by replicate.
    AAV <- ifelse( sumCatch > 0.0, sumAbsDiffDt / sumCatch * 100.0, 0.0 )
  }

  propAbove    <- apply(X = Ct_notest, FUN = countAboveThresh, MARGIN = 1, thresh = thresh) / (nT - tMP + 1)
  propBelow  <- 1 - propAbove

  AAV         <- apply(X = Ct, FUN = calcAAV, MARGIN = 1)

  par(mfcol = c(1,2), mar = c(1,4,1,1), oma = c(5,1,3,1))
  plot(x = c(0,1), y = range(aveCatch), type=  "n",
        xlab = "",
        ylab = "Average catch in a replicate" )
    points( x = propAbove, y = aveCatch, col = "grey", pch = 16 )
  
  plot(x = range(AAV), y = range(aveCatch), type=  "n",
        xlab = "",
        ylab = "Average catch in a replicate" )
    points( x = AAV, y = aveCatch, col = "grey", pch = 16 )
  
  propLabel <- paste( "Prop'n of proj years with TAC above ", thresh, " kt", sep = "")
    
  mtext( side = 1, line =2, outer = T, adj = .25,
          text = propLabel )
  mtext( side = 1, line =2, outer = T, adj = .75,
          text = "AAV")

  mtext(  side = 1, adj = .9, line = 4, outer =T,
          text = paste(scenLabel,":",mpLabel, sep = ""),
          col = "grey")



}




# Function to plot projected biomass for the OM over the statArea sub-areas. 
# Proportions used for apportionment in the history are simply the proportion
# of spawn index in each PFMA, and projections use a random
# walk in the log-proportions.
plotPFMABt_RW <- function(  simNum = 1, rep = 1, info = info.df,
                            seedVal = 2324, logPropSD = 0.1 )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Convert history into a matrix (3xnt)
  areaSI <- matrix(NA, nrow = 3, ncol = nT)
  rownames(areaSI) <- c("statArea23","statArea24","statArea25")
  areaSIprops <- matrix(NA, nrow = 3, ncol = nT)
  rownames(areaSIprops) <- c("statArea23","statArea24","statArea25")

  areaSI[1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
  areaSI[2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
  areaSI[3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
  areaSIprops[1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
  areaSIprops[2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
  areaSIprops[3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25

  # Now, need to project. Start with a RW
  # First, draw noise - need 3 * (nT - tMP + 1)
  logPropErrs <- rnorm( 3*(nT - tMP + 1), mean = 0, sd = logPropSD)
  areaErrs <- matrix(NA, nrow = 3, ncol = nT)
  areaErrs[,tMP:nT] <- logPropErrs

  # Loop from tMP, add noise and renormalise proportions
  for( tIdx in tMP:nT )
  {
    areaSIprops[,tIdx] <- log(areaSIprops[,tIdx - 1]) + areaErrs[,tIdx]
    areaSIprops[,tIdx] <- exp(areaSIprops[,tIdx])
    areaSIprops[,tIdx] <- areaSIprops[,tIdx] / sum(areaSIprops[,tIdx])
  }

  SBt <- blob$om$SBt[rep,2:(nT+1)]
  Bt  <- blob$om$Bt[rep,2:(nT+1)]

  Mt <- blob$om$Mt[rep,2:(nT+1)]

  if( !is.null(blob$om$FBt) )
  {
    FBt <- blob$om$FBt[rep,2:(nT+1)]
  } else 
    FBt <- Bt * exp(-Mt)

  colPal <- c("black",(brewer.pal(3, "Dark2")))

  areaSBt <- areaSIprops
  for( aIdx in 1:3 )
    areaSBt[aIdx,] <- areaSIprops[aIdx,] * SBt




  par(mfrow = c(4,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )
  plot( x = range(yrs), y = range(SBt, na.rm =T), xlab = "", ylab = "", type = "n", las = 1 )
    lines( x = yrs, y = SBt, col = colPal[1], lwd = 2  )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = 18.8, lty = 3, lwd = 2 )
    panLab( x = 0.9, y = 0.8, txt = "WCVI")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 25")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 24")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea23 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 23")
    
  mtext( side = 1, outer = T, line = 2, adj = .7,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, line = 2, adj = .3,
          text = paste("Replicate ", rep,"/",nReps, sep = "" ),
          col = "grey60" )

  titleText <- paste( "Apportioned by RW", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)


  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Biomass (kt)" )


}

# Function to plot projected biomass for the OM over the statArea sub-areas. 
# Proportions used for apportionment in the history are taken from
# a Dirichlet regression of observed proportions on the biomass.
# Each year of the projection, the alpha values for that biomass
# level are predicted, and random proportions are drawn from
# a Dir distibution with those parameters.
plotPFMABt_DR <- function(  simNum = 1, rep = 1, info = info.df,
                            seedVal = 2324, logPropSD = 0.1,
                            DR = "linear" )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Load Dirichlet Regression
  load("DirichletRegressions.RData")
  model <- DirReg[[DR]]

  # Convert history into a matrix (3xnt)
  areaSI <- matrix(NA, nrow = 3, ncol = nT)
  rownames(areaSI) <- c("statArea23","statArea24","statArea25")
  areaSIprops <- matrix(NA, nrow = 3, ncol = nT)
  rownames(areaSIprops) <- c("statArea23","statArea24","statArea25")

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Get historical SIs and proportions
  areaSI[1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
  areaSI[2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
  areaSI[3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
  areaSIprops[1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
  areaSIprops[2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
  areaSIprops[3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25


  SBt <- blob$om$SBt[rep,2:(nT+1)]
  Bt  <- blob$om$Bt[rep,2:(nT+1)]

  Mt <- blob$om$Mt[rep,2:(nT+1)]

  if( !is.null(blob$om$FBt) )
  {
    FBt <- blob$om$FBt[rep,2:(nT+1)]
  } else 
    FBt <- Bt * exp(-Mt)

  colPal <- c("black",(brewer.pal(3, "Dark2")))

  # Get alpha values from prediction function
  # create a new data frame
  projSBt <- data.frame(wcviSI = SBt[tMP:(nT)])
  alpha <- predict( model, newdata= projSBt, 
                    mu=F, alpha = T )
  # Hack to get past old sims without SBt at t = nT
  if(nrow(alpha) < nT - tMP + 1) alpha <- rbind(alpha,alpha[1,])

  randomProps <- rdirichlet( n = nT - tMP + 1, alpha = alpha)
  for( aIdx in 1:3)
    areaSIprops[aIdx,tMP:nT] <- randomProps[,aIdx]

  areaSBt <- areaSIprops
  for( aIdx in 1:3 )
    areaSBt[aIdx,] <- areaSIprops[aIdx,] * SBt


  par(mfrow = c(4,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )
  plot( x = range(yrs), y = range(SBt, na.rm =T), xlab = "", ylab = "", type = "n", las = 1 )
    lines( x = yrs, y = SBt, col = colPal[1], lwd = 2  )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = 18.8, lty = 3, lwd = 2 )
    panLab( x = 0.9, y = 0.8, txt = "WCVI")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 25")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 24")

  plot( x = range(yrs), y = range(areaSBt, na.rm =T), 
        xlab = "", ylab = "statArea23 Biomass (kt)", type = "n", las = 1 )
    lines( x = yrs, y = areaSBt[1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 23")

  titleText <- paste( "Apportioned by ", DR, " Dirichlet Regression", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)
    
  mtext( side = 1, outer = T, line = 2, adj = .9,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, line = 2, adj = .1,
          text = paste("Replicate ", rep,"/",nReps, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Biomass (kt)" )


}

# Function to plot projected biomass for the OM over the PFMA sub-areas. 
# Proportions used for apportionment in the history are taken from
# a Dirichlet regression of observed proportions on the biomass.
# Each year of the projection, the alpha values for that biomass
# level are predicted, and random proportions are drawn from
# a Dir distibution with those parameters.
plotPFMABtEnv_DR <- function( simNum = 1, info = info.df,
                              seedVal = 2324,
                              DR = "linear" )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Load Dirichlet Regression
  load("DirichletRegressions.RData")
  model <- DirReg[[DR]]

  # Convert history into a matrix (3xnt)
  areaSI <- array(NA, dim = c(nReps,3,nT),
                      dimnames = list(  paste("rep",1:nReps, sep = ""),
                                        c("statArea23","statArea24","statArea25"),
                                        paste("t",1:nT, sep = "") ) )

  areaSIprops <- array( NA, dim = c(nReps,3,nT),
                            dimnames = list(  paste("rep",1:nReps, sep = ""),
                                              c("statArea23","statArea24","statArea25"),
                                              paste("t",1:nT,sep = "") ) )

  areaSBt <- areaSIprops

  SBt <- array( NA, dim = c(nReps, nT),
                    dimnames = list(  paste("rep",1:nReps, sep = ""),
                                      paste("t",1:nT) ) )

  Bt <- SBt
  FBt <- SBt

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Get historical SIs and proportions
  for( rIdx in 1:nReps )
  {
    areaSI[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
    areaSI[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
    areaSI[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
    areaSIprops[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
    areaSIprops[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
    areaSIprops[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25  

    # Pull historic biomasses
    SBt[rIdx,] <- blob$om$SBt[rIdx,2:(nT+1)]
    Bt[rIdx,] <- blob$om$Bt[rIdx,2:(nT+1)]

    Mt <- blob$om$Mt[rIdx,2:(nT+1)]

    if( !is.null(blob$om$FBt) )
    {
      FBt[rIdx,] <- blob$om$FBt[rIdx,2:(nT+1)]
    } else 
      FBt[rIdx,] <- Bt[rIdx,] * exp(-Mt)  

    # Get alpha values from prediction function
    # create a new data frame
    projSBt <- data.frame(wcviSI = SBt[rIdx,tMP:nT])
    alpha <- predict( model, newdata= projSBt, 
                      mu=F, alpha = T )
    # Hack to get past old sims without SBt at t = nT
    if(nrow(alpha) < nT - tMP + 1) alpha <- rbind(alpha,alpha[1,])

    randomProps <- rdirichlet( n = nT - tMP + 1, alpha = alpha)
    for( aIdx in 1:3)
    {
      areaSIprops[rIdx,aIdx,tMP:nT] <- randomProps[,aIdx]
      areaSBt[rIdx,aIdx,] <- areaSIprops[rIdx,aIdx,] * SBt[rIdx,]
    }

  }

  # Take quantiles
  areaSBt_quantiles <- apply( X = areaSBt, FUN = quantile,
                              probs = c(0.025, 0.5, 0.975 ),
                              MARGIN = c(2,3) )
  SBt_quantiles <- apply( X = SBt, FUN = quantile,
                          probs = c(0.025, 0.5, 0.975 ),
                          MARGIN = c(2) )
  


  colPal <- c("black",(brewer.pal(3, "Dark2")))


  par(mfrow = c(4,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )
  plot( x = range(yrs), y = range(SBt_quantiles, na.rm =T), xlab = "", ylab = "", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(SBt_quantiles[1,], rev(SBt_quantiles[3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = SBt_quantiles[2,], col = colPal[1], lwd = 2  )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = 18.8, lty = 3, lwd = 2 )
    panLab( x = 0.9, y = 0.8, txt = "WCVI")

  plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,3,], rev(areaSBt_quantiles[3,3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 25")

  plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,2,], rev(areaSBt_quantiles[3,2,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 24")

    plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea23 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,1,], rev(areaSBt_quantiles[3,1,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 23")

  titleText <- paste( "Apportioned by ", DR, " Dirichlet Regression", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)
    
  mtext( side = 1, outer = T, line = 2, adj = .9,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Biomass (kt)" )


}

# Function to plot projected biomass for the OM over the PFMA sub-areas. 
# Proportions used for apportionment in the history are taken from
# a Dirichlet regression of observed proportions on the biomass.
# Each year of the projection, the alpha values for that biomass
# level are predicted, and random proportions are drawn from
# a Dir distibution with those parameters.
plotPFMAProp_DR <- function(  simNum = 1, info = info.df,
                              seedVal = 2324,
                              DR = "linear" )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Load Dirichlet Regression
  load("DirichletRegressions.RData")
  model <- DirReg[[DR]]

  # Convert history into a matrix (3xnt)
  areaSI <- array(NA, dim = c(nReps,3,nT),
                      dimnames = list(  paste("rep",1:nReps, sep = ""),
                                        c("statArea23","statArea24","statArea25"),
                                        paste("t",1:nT, sep = "") ) )

  areaSIprops <- array( NA, dim = c(nReps,3,nT),
                            dimnames = list(  paste("rep",1:nReps, sep = ""),
                                              c("statArea23","statArea24","statArea25"),
                                              paste("t",1:nT,sep = "") ) )

  areaSBt <- areaSIprops

  SBt <- array( NA, dim = c(nReps, nT),
                    dimnames = list(  paste("rep",1:nReps, sep = ""),
                                      paste("t",1:nT) ) )

  Bt <- SBt
  FBt <- SBt

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Get historical SIs and proportions
  for( rIdx in 1:nReps )
  {
    areaSI[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
    areaSI[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
    areaSI[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
    areaSIprops[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
    areaSIprops[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
    areaSIprops[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25  

    # Pull historic biomasses
    SBt[rIdx,] <- blob$om$SBt[rIdx,2:(nT+1)]
    Bt[rIdx,] <- blob$om$Bt[rIdx,2:(nT+1)]

    Mt <- blob$om$Mt[rIdx,2:(nT+1)]

    if( !is.null(blob$om$FBt) )
    {
      FBt[rIdx,] <- blob$om$FBt[rIdx,2:(nT+1)]
    } else 
      FBt[rIdx,] <- Bt[rIdx,] * exp(-Mt)  

    # Get alpha values from prediction function
    # create a new data frame
    projSBt <- data.frame(wcviSI = SBt[rIdx,tMP:nT])
    alpha <- predict( model, newdata= projSBt, 
                      mu=F, alpha = T )
    # Hack to get past old sims without SBt at t = nT
    if(nrow(alpha) < nT - tMP + 1) alpha <- rbind(alpha,alpha[1,])

    randomProps <- rdirichlet( n = nT - tMP + 1, alpha = alpha)
    for( aIdx in 1:3)
    {
      areaSIprops[rIdx,aIdx,tMP:nT] <- randomProps[,aIdx]
      areaSBt[rIdx,aIdx,] <- areaSIprops[rIdx,aIdx,] * SBt[rIdx,]
    }

  }

  # Take quantiles
  areaSIprop_quantiles <- apply(  X = areaSIprops, FUN = quantile,
                                  probs = c(0.025, 0.5, 0.975 ),
                                  MARGIN = c(2,3) )
  


  colPal <- c("black",(brewer.pal(3, "Dark2")))


  par(mfrow = c(3,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )
  plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,3,], rev(areaSIprop_quantiles[3,3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .25, lty = 3, lwd =1)
    panLab( x = 0.9, y = 0.8, txt = "statArea 25")

  plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,2,], rev(areaSIprop_quantiles[3,2,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .25, lty = 3, lwd =1)
    panLab( x = 0.9, y = 0.8, txt = "statArea 24")

    plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea23 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,1,], rev(areaSIprop_quantiles[3,1,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .25, lty = 3, lwd =1)
    panLab( x = 0.9, y = 0.8, txt = "statArea 23")

  titleText <- paste( "Apportioned by ", DR, " Dirichlet Regression", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)
    
  mtext( side = 1, outer = T, line = 2, adj = .9,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Proportion of WCVI biomass", line = 1.5 )


}



# Function to plot projected biomass envelopes for the OM over the 
# PFMA sub-areas. Proportions used for apportionment in the projection are 
# a RW from the last observed spawn indices at tMP - 1.
plotPFMABtEnv_RW <- function( simNum = 1, info = info.df,
                              seedVal = 2324, logPropSD = 0.1 )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Convert history into a matrix (3xnt)
  areaSI <- array(NA, dim = c(nReps,3,nT),
                      dimnames = list(  paste("rep",1:nReps, sep = ""),
                                        c("statArea 23","statArea 24","statArea 25"),
                                        paste("t",1:nT, sep = "") ) )

  areaSIprops <- array( NA, dim = c(nReps,3,nT),
                            dimnames = list(  paste("rep",1:nReps, sep = ""),
                                              c("statArea 23","statArea 24","statArea 25"),
                                              paste("t",1:nT,sep = "") ) )

  areaSBt <- areaSIprops

  SBt <- array( NA, dim = c(nReps, nT),
                    dimnames = list(  paste("rep",1:nReps, sep = ""),
                                      paste("t",1:nT) ) )

  Bt <- SBt
  FBt <- SBt

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Get historical SIs and proportions
  for( rIdx in 1:nReps )
  {
    areaSI[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
    areaSI[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
    areaSI[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
    areaSIprops[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
    areaSIprops[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
    areaSIprops[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25  

    # Pull historic biomasses
    SBt[rIdx,] <- blob$om$SBt[rIdx,2:(nT+1)]
    Bt[rIdx,] <- blob$om$Bt[rIdx,2:(nT+1)]

    Mt <- blob$om$Mt[rIdx,2:(nT+1)]

    if( !is.null(blob$om$FBt) )
    {
      FBt[rIdx,] <- blob$om$FBt[rIdx,2:(nT+1)]
    } else 
      FBt[rIdx,] <- Bt[rIdx,] * exp(-Mt)  

    # Now, need to project. Start with a RW
    # First, draw noise - need 3 * (nT - tMP + 1)
    logPropErrs <- rnorm( 3*(nT - tMP + 1), mean = 0, sd = logPropSD)
    areaErrs <- matrix(NA, nrow = 3, ncol = nT)
    areaErrs[,tMP:nT] <- logPropErrs

    # Loop from tMP, add noise and renormalise proportions
    for( tIdx in tMP:nT )
    {
      areaSIprops[rIdx,,tIdx] <- log(areaSIprops[rIdx,,tIdx - 1]) + areaErrs[,tIdx]
      areaSIprops[rIdx,,tIdx] <- exp(areaSIprops[rIdx,,tIdx])
      areaSIprops[rIdx,,tIdx] <- areaSIprops[rIdx,,tIdx] / sum(areaSIprops[rIdx,,tIdx])
    }
    
    for( aIdx in 1:3)
    {
      areaSBt[rIdx,aIdx,] <- areaSIprops[rIdx,aIdx,] * SBt[rIdx,]
    }

  }

  # Take quantiles
  areaSBt_quantiles <- apply( X = areaSBt, FUN = quantile,
                              probs = c(0.025, 0.5, 0.975 ),
                              MARGIN = c(2,3) )
  SBt_quantiles <- apply( X = SBt, FUN = quantile,
                          probs = c(0.025, 0.5, 0.975 ),
                          MARGIN = c(2) )
  


  colPal <- c("black",(brewer.pal(3, "Dark2")))


  par(mfrow = c(4,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )
  plot( x = range(yrs), y = range(SBt_quantiles, na.rm =T), xlab = "", ylab = "", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(SBt_quantiles[1,], rev(SBt_quantiles[3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = SBt_quantiles[2,], col = colPal[1], lwd = 2  )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = 18.8, lty = 3, lwd = 2 )
    panLab( x = 0.9, y = 0.8, txt = "WCVI")

  plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea 24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,3,], rev(areaSBt_quantiles[3,3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea  25")

  plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea 24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,2,], rev(areaSBt_quantiles[3,2,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea  24")

    plot( x = range(yrs), y = range(areaSBt_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea 23 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSBt_quantiles[1,1,], rev(areaSBt_quantiles[3,1,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSBt_quantiles[2,1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    panLab( x = 0.9, y = 0.8, txt = "statArea  23")

  titleText <- paste( "Apportioned by RW", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)
    
  mtext( side = 1, outer = T, line = 2, adj = .9,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Biomass (kt)" )


}

# Function to plot projection biomass proportion envelopes for the OM over the 
# PFMA sub-areas. Proportions used for apportionment in the projection are 
# a RW from the last observed spawn indices at tMP - 1.
plotPFMAProp_RW <- function(  simNum = 1, info = info.df,
                              seedVal = 2324, logPropSD = 0.1 )
{
  set.seed(seedVal)
  simID <- info.df$simLabel[simNum]
  simPath <- file.path("..",simID,paste(simID, ".RData", sep = "") )
  load(simPath)

  yrs <- seq(1951,by = 1, length = 92)
  nT <- blob$ctlList$opMod$nT
  tMP <- blob$ctlList$opMod$tMP

  # scen/MP info
  scenLabel <- info.df$scenarioLabel[simNum]
  mpLabel   <- info.df$mpLabel[simNum]
  nReps     <- blob$ctlList$gui$nReps

  # Convert history into a matrix (3xnt)
  areaSI <- array(NA, dim = c(nReps,3,nT),
                      dimnames = list(  paste("rep",1:nReps, sep = ""),
                                        c("statArea23","statArea24","statArea25"),
                                        paste("t",1:nT, sep = "") ) )

  areaSIprops <- array( NA, dim = c(nReps,3,nT),
                            dimnames = list(  paste("rep",1:nReps, sep = ""),
                                              c("statArea23","statArea24","statArea25"),
                                              paste("t",1:nT,sep = "") ) )

  areaSBt <- areaSIprops

  SBt <- array( NA, dim = c(nReps, nT),
                    dimnames = list(  paste("rep",1:nReps, sep = ""),
                                      paste("t",1:nT) ) )

  Bt <- SBt
  FBt <- SBt

  # Load history
  statAreaSIprops <- read.csv("../history/statAreaSI_props.csv")
  statAreaSI <- read.csv("../history/statAreaSI.csv")

  # Get historical SIs and proportions
  for( rIdx in 1:nReps )
  {
    areaSI[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI23
    areaSI[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI24
    areaSI[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$SI25
    areaSIprops[rIdx,1,1:nrow(statAreaSIprops)] <- statAreaSIprops$p23
    areaSIprops[rIdx,2,1:nrow(statAreaSIprops)] <- statAreaSIprops$p24
    areaSIprops[rIdx,3,1:nrow(statAreaSIprops)] <- statAreaSIprops$p25  

    # Now, need to project. Start with a RW
    # First, draw noise - need 3 * (nT - tMP + 1)
    logPropErrs <- rnorm( 3*(nT - tMP + 1), mean = 0, sd = logPropSD)
    areaErrs <- matrix(NA, nrow = 3, ncol = nT)
    areaErrs[,tMP:nT] <- logPropErrs

    # Loop from tMP, add noise and renormalise proportions
    for( tIdx in tMP:nT )
    {
      areaSIprops[rIdx,,tIdx] <- log(areaSIprops[rIdx,,tIdx - 1]) + areaErrs[,tIdx]
      areaSIprops[rIdx,,tIdx] <- exp(areaSIprops[rIdx,,tIdx])
      areaSIprops[rIdx,,tIdx] <- areaSIprops[rIdx,,tIdx] / sum(areaSIprops[rIdx,,tIdx])
    }

  }

  # Take quantiles
  areaSIprop_quantiles <- apply( X = areaSIprops, FUN = quantile,
                              probs = c(0.025, 0.5, 0.975 ),
                              MARGIN = c(2,3) )
  


  colPal <- c("black",(brewer.pal(3, "Dark2")))


  par(mfrow = c(3,1), mar = c(2,2,2,2), oma = c(3,3,3,1) )

  plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,3,], rev(areaSIprop_quantiles[3,3,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,3,], col = colPal[4], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .2, lty = 3 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 25")

  plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea24 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,2,], rev(areaSIprop_quantiles[3,2,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,2,], col = colPal[3], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .2, lty = 3 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 24")

    plot( x = range(yrs), y = range(areaSIprop_quantiles, na.rm =T), 
        xlab = "", ylab = "statArea23 Biomass (kt)", type = "n", las = 1 )
    polygon(  x = c(yrs,rev(yrs)),
              y = c(areaSIprop_quantiles[1,1,], rev(areaSIprop_quantiles[3,1,])),
              border = NA, col = "grey70" )
    lines( x = yrs, y = areaSIprop_quantiles[2,1,], col = colPal[2], lwd = 2 )
    abline( v = yrs[tMP], lty = 2 )
    abline( h = .2, lty = 3 )
    panLab( x = 0.9, y = 0.8, txt = "statArea 23")

  titleText <- paste( "Apportioned by RW", sep = "")

  mtext( side = 3, text = titleText, cex = 1.5, outer = T)
    
  mtext( side = 1, outer = T, line = 2, adj = .9,
          text = paste(scenLabel, ":",mpLabel, sep = "" ),
          col = "grey60" )

  mtext( side = 1, outer = T, text = "Year" )
  mtext( side = 2, outer = T, text = "Proportion of WCVI biomass", line = 1.5 )


}

plotClevelands <- function( scen = scenarios[1], 
                            Periods = timePeriods,
                            periodLabels = perLabel,
                            MPs = MPnames,
                            hlIdx = NULL,
                            fileName = "Cleveland",
                            stats = statTable,
                            hzLine = NA )
{
  plotFileName <- paste( scen, fileName, ".pdf", sep = "" )

  pdf( file = plotFileName, width = 12, height = 8 )
  
  # First, plot short time period depletion plots
  par( mfrow = c(1,4), mar = c(1,1,1,1), oma = c(3,12,4,3) )
  
  # Final Depletion
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AvgDep",
                            timePeriods = Periods,
                            statsTable = stats,
                            midLine = .3,
                            mpLabs = T,
                            MPorder = MPs,
                            hzLine = hzLine,
                            highLightIdx = hlIdx )
    mtext( side = 1, text = "Average Depletion", line = 2 )

    # Avg Catch
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AvgCatch",
                            timePeriods = Periods,
                            statsTable = stats,
                            midLine = NA,
                            MPorder = MPs,
                            mpLabs = F,
                            hzLine = hzLine,
                            highLightIdx = hlIdx )
    mtext( side = 1, text = "Average Catch (kt)", line = 2 )

    # AAV
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "AAV",
                            timePeriods = Periods,
                            statsTable = stats,
                            midLine = 25,
                            MPorder = MPs,
                            mpLabs = F,
                            hzLine = hzLine,
                            highLightIdx = hlIdx,
                            xLim = c(0,200) )
    mtext( side = 1, text = "Average Annual Variation (%)", line = 2 )

    # Prop years closed
    plotScenarioClevelands( scenarioName = scen, 
                            statName = "PropClosure",
                            timePeriods = Periods,
                            statsTable = stats,
                            MPorder = MPs,
                            midLine = NA,
                            mpLabs = F,
                            hzLine = hzLine,
                            highLightIdx = hlIdx )
    mtext( side = 1, text = "Prob TAC < 500t (%)", line = 2 )

  mtext( side = 3, outer = T, text = scen )

  panLegend( x = 0.1, y = 0.98,
              legTxt = c("3 Gen", "4 Gen"),
              lty = c(1,2),
              lwd = 2,
              pch = c(16,17),
              cex = 1.2 )

  dev.off()

  # objPerfFileName <- paste(scen, fileName, "objectivePerf.pdf", sep = "")

  # pdf( file = objPerfFileName, width = 12, height = 8 )
  
  # # First, plot short time period depletion plots
  # par( mfrow = c(1,2), mar = c(2,1,2,1), oma = c(3,12,4,3) )

  #   plotScenarioClevelands( scenarioName = scen, 
  #                           statName = "ProbGt.3B0",
  #                           timePeriods = Periods,
  #                           statsTable = stats,
  #                           midLine = c(.9,.95),
  #                           xLim = c(0,1),
  #                           MPorder = MPs,
  #                           mpLabs = T,
  #                           hzLine = hzLine,
  #                           highLightIdx = hlIdx )
  #   mtext( side = 1, text = expression(paste("P( ", B[t] > .3*B[0], " )") ), line = 3 )

  #   # plotScenarioClevelands( scenarioName = scen, 
  #   #                         statName = "deltaPdecline",
  #   #                         quantiles = FALSE,
  #   #                         timePeriods = Periods,
  #   #                         statsTable = stats,
  #   #                         midLine = c(0),
  #   #                         xLim = c(-1,1),
  #   #                         mpLabs = F,
  #   #                         hzLine = hzLine,
  #   #                         highLightIdx = hlIdx )
    
  #   # mtext( side = 1, text = "Objective 2 Performance", line = 3)  

  #   plotScenarioClevelands( scenarioName = scen, 
  #                           statName = "ProbGt.75B0",
  #                           timePeriods = Periods,
  #                           statsTable = stats,
  #                           midLine = c(.5,.75),
  #                           xLim = c(0,1),
  #                           MPorder = MPs,
  #                           mpLabs = F,
  #                           hzLine = hzLine,
  #                           highLightIdx = hlIdx )
  #   mtext( side = 1, text = expression(paste("P( ", B[t] > .75*B[0], " )") ), line = 3 )

  


  # mtext( side = 3, outer = T, text = scen, cex = 2, line = 2 )

  # dev.off()
}

plotComparativeScenarioObjs <- function(  scenList = scenarios[length(scenarios):1],
                                          Periods = timePeriods,
                                          periodLabels = perLabel,
                                          MPs = MPnames,
                                          hlIdx = NULL,
                                          fileName = "obj1",
                                          statName = "ProbGt.3B0",
                                          midLine = .9,
                                          xLim = c(0,1),
                                          hzLine = NA,
                                          stats = statTable,
                                          xlabel = expression(paste("P( ", B[t] > .3*B[0], " )") ) )
{
  outputFile <- paste(fileName, ".pdf", sep = "" )


  pdf( outputFile, height = 8, width = 6 * length(scenList) )

  par( mfrow = c(1, length(scenList)), mar = c(3,3,3,3), oma = c(3,12,4,3) )

  for( scenIdx in 1:length(scenList) )
  {
    scen <- scenList[scenIdx]
    if( scenIdx == 1 ) MPLabBool <- T
    else MPLabBool <- F

    plotScenarioClevelands( scenarioName = scen, 
                            statName = statName,
                            timePeriods = Periods,
                            statsTable = stats,
                            midLine = midLine,
                            xLim = xLim,
                            MPorder = MPs,
                            mpLabs = MPLabBool,
                            hzLine = hzLine,
                            highLightIdx = hlIdx )
    mtext( side = 3, text = scen, cex = 1.3)

    # if( scenIdx < length(scenList) ) plot(x = c(0,1), y = c(0,0.3), type = "n", axes = F, xlab = "", ylab = "")

    
  }

  mtext( side = 1, outer = T, text = xlabel, line =2 )

  dev.off()

}

# Cleveland plots from the perfTables
plotScenarioClevelands <- function( scenarioName = "WCVI_Mbar10", 
                                    statName = "FinalDep",
                                    timePeriods = c("Short","Med"),
                                    periodLabels = c("3 Gen", "4 Gen"),
                                    quantiles = TRUE,
                                    statsTable = stats,
                                    midLine = NULL,
                                    xLim = NULL,
                                    mpLabs = TRUE,
                                    MPorder = MPs,
                                    hzLine = NA,
                                    highLightIdx = hlIdx,
                                    highLightCol = "red" )
{
  if(quantiles)
  {
    colPrefix <- c("med", "Q1", "Q2")
    colNames <- paste( colPrefix, statName, sep = "" )
  }
  else colNames <- statName


  subStats <- statsTable %>%
                dplyr::filter( Scenario == scenarioName )

  plotRange <- range(subStats[,colNames])

  subStats <- subStats %>% filter( Period %in% timePeriods)

  subStats <- subStats[,c("Procedure","Period",colNames)]

  # browser()

  if(is.null (MPorder))
    MPs <- unique(statsTable$Procedure)
  else MPs <- MPorder

  if(length(timePeriods) > 1 )
    periodJitter <- -1 * seq(from = -.1, to = .1, length = length(timePeriods) )
  else periodJitter = 0

  if(is.null(xLim)) xLim <- plotRange

  plot( x = xLim, y = c(1,length(MPs)), type = "n", las = 1,
        xlab = "", ylab = "", axes = F )
    axis( side = 1 )
    abline( v = midLine, lty = 2, lwd = .8 )
    for( mpIdx in 1:length(MPs) )
    {
      for( perIdx in 1:length(timePeriods) )
      {
        timePeriod <- timePeriods[perIdx]
        mpStat <- subStats %>%
                  filter( Procedure == MPs[mpIdx],
                          Period == timePeriod )
        if(nrow(mpStat) != 1 ) browser()
        if(mpIdx %in% highLightIdx ) colour <- highLightCol
        else colour <- "grey30"
        if( quantiles )
          segments( x0 = mpStat[,colNames[2]], x1 = mpStat[, colNames[3]],
                    y0 = length(MPs) - mpIdx + 1 + periodJitter[perIdx], 
                    y1 = length(MPs) - mpIdx + 1 + periodJitter[perIdx], lwd = 3, col = colour, lty = perIdx )
        points( x = mpStat[, colNames[1]], y = length(MPs) - mpIdx + 1 + periodJitter[perIdx], pch = 15 + perIdx, cex = 1.6, col = colour  )
        abline( h = hzLine, lty = 5, col = "darkgreen" )
      }
    }
    if(mpLabs)
      axis( side = 2, labels = MPs, at = length(MPs):1, las = 1, cex = .8 )
    else axis( side = 2, labels = NA, at = length(MPs):1 )

}

plotDepCatchMultiPanels <- function(  MPnames = MPs, plotNameRoot = "DepCatch",
                                      scenarios = scenList, df = info.df, gfx )
{

  for( scenIdx in 1:length(scenarios) )
  {
    scen <- scenarios[scenIdx]

    depCatchPlot    <- paste(scen, plotNameRoot, ".pdf", sep = "" )
    depCatch_noFish <- paste(scen,plotNameRoot,"_noFish.pdf", sep = "" )

    noFishID <- df[  which(df$mpLabel == "NoFish" & df$scenarioLabel == scen)[1],
                          "simLabel"]
    noFishPath  <- file.path("..",noFishID,paste(noFishID, ".RData", sep = "") )

    if(!is.na(noFishID))
    {
      load(noFishPath)
      noFishBlob <- blob
    }

    if( "NoFish" %in% MPnames ) lenMPlist <- length(MPnames) - 1
    else lenMPlist <- length(MPnames)

    mpList <- vector( mode = "list", length = lenMPlist )
    mpListIdx <- 1

    pdf( file = depCatchPlot, width = length(MPnames)*2, height = 6 )
    par( mfcol = c(2,length(MPnames)), mar = c(1,1.5,1,1.5), oma = c(3,3,4,1))

    for( mpIdx in 1:length(MPnames) )
    {
      mp <- MPnames[mpIdx]
      df.sub <-   df %>%
                  filter( scenarioLabel == scen,
                          mpLabel == mp )

      simID     <- df.sub[1,]$simLabel
      if(is.na(simID)) next

      simPath  <- file.path("..",simID,paste(simID, ".RData", sep = "") )
      load(simPath)

      if(mpIdx == 1) gfx$doLegend <- TRUE
      else gfx$doLegend <- FALSE

      .plotTulipDepCat( blob, gfx = gfx, yLimD = c(0,2), yLimC = c(0,50),
                        refPts = FALSE )

      # Now rescale blob$Bt if
      if( mp != "NoFish" )
      {
        blob$om$SBt <- blob$om$SBt / noFishBlob$om$SBt
        blob$ctlList$opMod$B0 <- 1

        mpList[[mpListIdx]] <- blob
        names(mpList)[mpListIdx] <- mp
        mpListIdx <- mpListIdx + 1
      }
    }

    mtext( side = 3, outer = T, text = scen, cex = 1.3, line = 2.5)

    dev.off()

    if(is.na(noFishID)) next
    if("NoFish" %in% MPnames ) noFishScaleMPs <- MPnames[MPnames != "NoFish" ]
    pdf( file = depCatch_noFish, width = (lenMPlist)*2, height = 6 )
    par( mfcol = c(2,lenMPlist), mar = c(1,1.5,1,1.5), oma = c(3,3,4,1))

    for( idx in 1:length(mpList) )
    {
      if(idx == 1) gfx$doLegend <- TRUE
      else gfx$doLegend <- FALSE

      if( is.null(mpList[[idx]]) ) next

      .plotTulipDepCat( mpList[[idx]], gfx = gfx, yLimD = c(0,1), yLimC = c(0,50),
                        refPts = FALSE, DepLab = expression(SSB / SSB[NoFish]) )
    }
    dev.off()
  }
}


