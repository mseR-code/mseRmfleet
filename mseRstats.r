#------------------------------------------------------------------------------#
#-- Performance statistics functions                                         --#
#------------------------------------------------------------------------------#
library(dplyr)
# .calcPerfStats (Calculate performance statistics by calling .calcStatsXXX).
# Purpose:       Calculate various performance statistics with the function
#                name .calcStatsXXX, where XXX determined the statistics.
#                (1) Read the simulation tracking file to get the simulation
#                    ID, simulation label, scenario label, and procedure label.
#                (2) Make a data frame that holds the period definitions from
#                    guiPerf entry fields.
#                (3) Build a result data frame that has the simulation identifier
#                    fields, period definitions and statistics as columns. Each
#                    row respresents the peformance statistics for one simulation
#                    and time period.
#                (4) Loop through the time periods for each simulation and
#                    calculate the performance statistics for each combination.
# Parameters:    NONE.
# Returns:       A list with components simSummary1 (spreadsheet "view") and
#                simSummary2 (normalized "Access view").
# Source:        A.R. Kronlund
.calcPerfStats <- function()
{

  # Get the GUI parameters to calculate periods, local scope.
  win     <- .getWinName()
  guiInfo <- getWinVal( scope="L", winName=win )

  noSimVal <- -999


  # From mseRguiPerfFuns.r-------------------------------------------
  tmpFile <- .DEFSTATFILE
  trackData <- .getTrackingData( .PRJFLD )
  nSim      <- .getNumSims( trackData )

  # From mseRguiPerfFuns.r------------------------------------------- 

  # Get the simulation list, regardless of whether it has a valid Sim.
  #simList <- pfSimList[ c(1:pfNsim), ]

  # Get the period list.
  period <- list()
  period$Period <- c("Short","Med","Long")
  period$Year1  <- c( guiInfo$pfShort1, guiInfo$pfMed1,guiInfo$pfLong1 )
  period$Year2  <- c( guiInfo$pfShort2, guiInfo$pfMed2,guiInfo$pfLong2 )

  quantVals <- c( 0.05, guiInfo$pfProb1, 0.5, guiInfo$pfProb2, 0.95 )

  # Make an output object for performance statistics, there are a total of
  # nSim*nrow(pfPeriodList) rows because of Short, Medium, Long summaries, etc.
  # The number of columns is determined by the Simulation, Sceanario, Procedure,
  # Stamp, Period, t1, t2, plus the number of statistics calculated.

  #nSim        <- nrow( simList )
  nResults    <- nSim * length( period$Period )
  headerNames <- c( "SimFolder" ,"Scenario","Procedure","Period","t1","t2")
  statNames   <- c( "medAvgDep","Q1AvgDep","Q2AvgDep",
                    "medFinalDep","Q1FinalDep","Q2FinalDep",
                    "medLowDep","Q1LowDep","Q2LowDep",
                    "medAAV", "Q1AAV","Q2AAV",
                    "medAvgCatch","Q1AvgCatch","Q2AvgCatch",
                    "medLowCatch","Q1LowCatch","Q2LowCatch",
                    "medHighCatch","Q1HighCatch","Q2HighCatch",
                    "medAvgDiscard","Q1AvgDiscard","Q2AvgDiscard",
                    "criticalP","cautiousP","healthyP",
                    "yearAtTargetProb","probAtTargetYear","targetAtYearProb",
                    "probGteDepMSY","probGteLimit",
                    "pObj1","pObj3","pHealthy",
                    "t1Trend","trendPeriod","avgExpSlope",
                    "trendDec","trendInc","obsPdecline","pDecline",
                    "pGTlrp","pGTtarg","t1AvgCatch","t1AvgDep", "pObj4",
                    "medProbGt.75B0","Q1ProbGt.75B0","Q2ProbGt.75B0",
                    "medProbGt.3B0","Q1ProbGt.3B0","Q2ProbGt.3B0",
                    "medProbGt.6B0","Q1ProbGt.6B0","Q2ProbGt.6B0",
                    "medProbGtLTA","Q1ProbGtLTA","Q2ProbGtLTA",
                    "medProbGtrefB","Q1ProbGtrefB","Q2ProbGtrefB",
                    "medPropClosure","Q1PropClosure","Q2PropClosure",
                    "minProbBtGt.3B0" )

  colNames    <- c( headerNames, statNames )
  result      <- data.frame( matrix( NA, nrow=nResults,ncol=length(colNames) ),row.names=NULL )
  names( result ) <- colNames

  # Set the results to noSimVal
  result[ ,statNames ] <- noSimVal

  # Create a list to hold the NoFish MP for each scenario
  scenarios <- unique(trackData$scenario)
  scenarios <- scenarios[ scenarios != "" ]
  noFishBlobs <- vector(mode= "list", length = length(scenarios) )
  names(noFishBlobs) <- scenarios

  noFishTrack <-  trackData %>%
                  filter( mp == "NoFish" )

  for( scenIdx in 1:length(scenarios) )
  {
    scenarioName <- scenarios[scenIdx]
    scenNoFishTrack <-  noFishTrack %>%
                        filter( scenario == scenarioName )

    simFile     <- scenNoFishTrack[ 1, "simFile" ]
    simFolder   <- scenNoFishTrack[ 1, "simFolder" ]
    simFilePath <- file.path( .PRJFLD, simFolder, simFile )

    cat( "\nMSG (.subPerf) Loading",simFilePath,"...\n" )    
    load( file=simFilePath )
    assign( "blob", blob, pos=1 )
    noFishBlobs[[scenarioName]] <- blob
  }

  # Initialize row counter.
  iRow    <- 0

  # Load and calculate statistics for each simulation in tracking object.
  for ( i in 1:nSim )
  {
    #validSim <- FALSE
    #if ( simList$Stamp[i] !="NO_SIM" )
      validSim <- TRUE

    if ( validSim )
    {

      #browser()
      #folderName <- paste( getwd(),"/mseRproject/",simList$simFolder[i],sep="" )
      #fileName   <- paste(folderName,"/",simList$simFolder[i],".Rdata",sep="")

      # Load an Rdata working directory containing a list called blob.
      #cat( "\nMSG (.calcPerfStats) Loading",fileName,"...\n" )
      #load( file=fileName )

      simFile     <- trackData[ i, "simFile" ]
      simFolder   <- trackData[ i, "simFolder" ]
      simFilePath <- file.path( .PRJFLD, simFolder, simFile )

      cat( "\nMSG (.subPerf) Loading",simFilePath,"...\n" )    
      load( file=simFilePath )
      assign( "blob", blob, pos=1 )            
      
      # recalc ref pts
      opMod  <- blob$ctlList$opMod      
      tmp    <- calcRefPoints( as.ref(opMod) )
      refPts <- deref( tmp )
      blob$ctlList$refPts <- refPts
      
      ctlPars <- blob$ctlPars

      tMP    <- blob$ctlList$opMod$tMP
      nT     <- blob$ctlList$opMod$nT

      B0     <- blob$ctlList$opMod$B0
      Bmsy   <- blob$ctlList$refPts$ssbFmsy
      DepMSY <- Bmsy / B0

      limitB <- guiInfo$pfLimitMultBmsy * Bmsy
      upperB <- guiInfo$pfUpperMultBmsy * Bmsy

      # Zone boundaries on the depletion scale.
      if( !is.null(.DeptBLIM) )
        limitDep <- .DeptBLIM
      else
        limitDep <- limitB / B0
      if( !is.null(.DeptBUPPER) )
        upperDep <- .DeptBUPPER
      else
        upperDep <- upperB / B0
      
    }

    # Accumulate statistics for each period.
    for ( j in 1:length(period$Period) )
    {
      
      # Get the header information for this simulation and period.
      iRow <- iRow + 1
      result[ iRow, "SimFolder" ]  <- trackData[i,"simFolder"]
      result[ iRow, "Scenario" ]   <- trackData[i,"scenario"]
      result[ iRow, "Procedure" ]  <- trackData[i,"mp"]
      #result[ iRow, "Stamp" ]      <- "stamp"#simList$Stamp[i]
      result[ iRow, "Period" ]     <- period$Period[j]
      result[ iRow, "t1"     ]     <- period$Year1[j]
      result[ iRow, "t2"     ]     <- period$Year2[j]

      # Pull scenario name to scale Bt by NoFish
      scenarioName <- trackData[i,"scenario"]

      # Usefull for trend statistics.
      t1 <- result[ iRow, "t1" ]
      t2 <- result[ iRow, "t2" ]

      # Get the time index values that span this period.
      tdx <- c( period$Year1[j]:period$Year2[j] )

      if ( validSim )
      {
        # Get the variables to be summarized, these are nReps by nT matrices.
        Bt   <- blob$om$SBt[ ,c(2:ncol(blob$om$Bt)) ]
        Ct   <- blob$om$Ct[ ,c(2:ncol(blob$om$Ct)) ]
        Dt   <- apply( blob$om$Dt,c(1,2),sum )
        Dept <- Bt / blob$ctlList$opMod$B0

        if( !is.null(blob$ctlList$opMod$posteriorDraws) )
        {
          mcmcPar     <- blob$ctlList$opMod$mcmcPar
          postDraws   <- blob$ctlList$opMod$posteriorDraws  
          SB0         <- mcmcPar[postDraws,"sbo"]
          for( repIdx in 1:nrow(Dept) )
            Dept[repIdx,] <- Bt[repIdx,] / SB0[repIdx]
        }
        

        noFishBt <- noFishBlobs[[scenarioName]]$om$SBt
        noFishBt <- noFishBt[,2:ncol(noFishBt)]

        noFishDept <- Bt / noFishBt

      }
   
      #--- Depletion Statistics                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsDepletion( Dept[,tdx], quantVals )
        result[ iRow, "medAvgDep" ] <- tmp$medAvgDep
        result[ iRow, "Q1AvgDep" ]  <- tmp$qVals[1]
        result[ iRow, "Q2AvgDep" ]  <- tmp$qVals[5]
      }

      #--- Final depletion for period                                       ---#
      if ( validSim )
      {
        tmp <- .calcStatsFinalDep( Dept[,tdx], quantVals )
        result[ iRow, "medFinalDep" ] <- tmp$medFinalDep
        result[ iRow, "Q1FinalDep" ]  <- tmp$qVals[1]
        result[ iRow, "Q2FinalDep" ]  <- tmp$qVals[5]
      }

      #--- Low depletion for period                                         ---#
      if ( validSim )
      {
        tmp <- .calcStatsLowDep( Dept[,tdx], quantVals )
        result[ iRow, "medLowDep" ] <- tmp$medLowDep
        result[ iRow, "Q1LowDep" ]  <- tmp$qVals[2]
        result[ iRow, "Q2LowDep" ]  <- tmp$qVals[4]
      }

      #--- Catch Statistics                                                 ---#
      if ( validSim )
      {
        tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
        result[ iRow, "medAvgCatch" ] <- tmp$medAvgCatch
        result[ iRow, "Q1AvgCatch" ]  <- tmp$qVals[1]
        result[ iRow, "Q2AvgCatch" ]  <- tmp$qVals[5]
      }

      #--- Low catch for period                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsLowCat( Ct[,tdx], quantVals )
        result[ iRow, "medLowCatch" ] <- tmp$medLowCat
        result[ iRow, "Q1LowCatch" ]  <- tmp$qVals[2]
        result[ iRow, "Q2LowCatch" ]  <- tmp$qVals[4]
      }

      #--- Low catch for period                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsHighCat( Ct[,tdx], quantVals )
        result[ iRow, "medHighCatch" ] <- tmp$medHighCat
        result[ iRow, "Q1HighCatch" ]  <- tmp$qVals[2]
        result[ iRow, "Q2HighCatch" ]  <- tmp$qVals[4]
      }
    

      #--- AAV Catch Statistics                                             ---#
      if ( validSim )
      {
        tmp <- .calcStatsAAV( Ct, tdx, quantVals )
        result[ iRow,"medAAV" ] <- tmp$medAAV
        result[ iRow,"Q1AAV" ]  <- tmp$qVals[1]
        result[ iRow,"Q2AAV" ]  <- tmp$qVals[5]
      }

      #--- Zone Objectives Statistics                                       ---#

      # Note that the limitD and upperD are taken from the guiInfo$pfLimitMultBmsy
      # and multUsrBmsy, respectively, not the HCR reference points.
      # They are translated from the SSB scale to the depletion scale above.

      if ( validSim )
      {
        
        tmp <- .calcStatsZones( Dept[,tdx],limitDep,upperDep, quantVals )
        #result[ iRow,"medCriticalP" ] <- tmp$medCriticalP
        #result[ iRow,"medCautiousP" ] <- tmp$medCautiousP
        #result[ iRow,"medHealthyP"  ] <- tmp$medHealthyP
        result[ iRow,"criticalP"    ] <- tmp$pAllZone[1]
        result[ iRow,"cautiousP"    ] <- tmp$pAllZone[2]
        result[ iRow,"healthyP"     ] <- tmp$pAllZone[3]
      }

      #--- Objective Statistics by Period                                   ---#

      if ( validSim )
      {
        # Updated for Herring .3B0 LRP (DFO) and .75B0 TRP (NCN Goal 1)
        tmp <- .calcStatsRefPoints( Bt[,tdx], target=B0, targMult=.3, refProb=0.95 )
        result[ iRow,"pGTlrp" ] <- tmp
        tmp <- .calcStatsRefPoints( Bt[,tdx], target=B0, targMult=.75, refProb=0.5 )
        result[ iRow,"pGTtarg" ] <- tmp
      }

      # --- MSE objective statistics, hard coded by SDNJ May 10, 2018
      if( validSim )
      { 
        LTA <- mean(Bt[1,1:67])
        refB <- mean(Bt[1,38:46])

        #Hard code ProbGt.75B0 over tdx (NCN Goal 1)
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = B0, targMult = .75, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGt.75B0" ] <- tmp[3]
        result[ iRow, "Q1ProbGt.75B0" ] <- tmp[1]
        result[ iRow, "Q2ProbGt.75B0" ] <- tmp[5]

        #Hard code ProbGt.75NoFish over tdx (NCN Goal 1)
        tmp <- .calcQuantsRefPoints( noFishDept[,tdx], target = 1, targMult = .75, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGt.75NoFish" ] <- tmp[3]
        result[ iRow, "Q1ProbGt.75NoFish" ] <- tmp[1]
        result[ iRow, "Q2ProbGt.75NoFish" ] <- tmp[5]

  
        # MedProb NCN Goal 2 (.76B0 over 2 gens)
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = B0, targMult = .76, refProb = 1, probs = quantVals )
        result[ iRow, "medProbNCNGoal2" ] <- tmp[3]
        result[ iRow, "Q1ProbNCNGoal2" ] <- tmp[1]
        result[ iRow, "Q2ProbNCNGoal2" ] <- tmp[5]

        # MedProb NCN Goal 2 (.76NoFish over 2 gens)
        tmp <- .calcQuantsRefPoints( noFishDept[,tdx], target = 1, targMult = .76, refProb = 1, probs = quantVals )
        result[ iRow, "medProbNCNGoal2NoFish" ] <- tmp[3]
        result[ iRow, "Q1ProbNCNGoal2NoFish" ] <- tmp[1]
        result[ iRow, "Q2ProbNCNGoal2NoFish" ] <- tmp[5]


        # We should add the other USR candidates here
        # .6B0 over 2 gens
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = B0, targMult = .6, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGt.6B0" ] <- tmp[3]
        result[ iRow, "Q1ProbGt.6B0" ] <- tmp[1]
        result[ iRow, "Q2ProbGt.6B0" ] <- tmp[5]

    
        # LTA over 2 gens
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = LTA, targMult = 1, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGtLTA" ] <- tmp[3]
        result[ iRow, "Q1ProbGtLTA" ] <- tmp[1]
        result[ iRow, "Q2ProbGtLTA" ] <- tmp[5]

        # refB over 2 gens
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = refB, targMult = 1, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGtrefB" ] <- tmp[3]
        result[ iRow, "Q1ProbGtrefB" ] <- tmp[1]
        result[ iRow, "Q2ProbGtrefB" ] <- tmp[5]

        # average biomass over productive period
        
        # Limit reference point
        tmp <- .calcQuantsRefPoints( Bt[,tdx], target = B0, targMult = .3, refProb = 1, probs = quantVals )
        result[ iRow, "medProbGt.3B0" ] <- tmp[3]
        result[ iRow, "Q1ProbGt.3B0" ] <- tmp[1]
        result[ iRow, "Q2ProbGt.3B0" ] <- tmp[5]

        # Vertical integration of probability Dt > .3
        tmp <- .calcStatsMinProbGtX( Dept[,tdx], X = .3 )
        result[ iRow, "minProbBtGt.3B0" ] <- tmp
      }
      #--- Objective Statistics from GUI.

      if ( validSim )
      {
        # Note that the entire projection period is required here, not
        # just the current period of interest.
        baseVal <- 0
        #if ( pfBase=="Bmsy" )
          baseVal <- blob$ctlList$opMod$ssbFmsy
        #else if ( pfBase=="B0" )
         # baseVal <- blob$pars$B0
        tmp <- .calcStatsTarget( Dept=Dept, tMP=tMP, objDep=guiInfo$pfTargetMultBmsy, 
                                 objYear=45, objProb=0.5, outcome="prob" )
        result[ iRow,"yearAtTargetProb"  ] <- tmp$objYear
        result[ iRow,"probAtTargetYear"  ] <- tmp$objProb
        result[ iRow,"targetAtYearProb"  ] <- tmp$objDep
      }

      # Objectives 1-3 by period (use long period for paper, 30 years).
      .calcPropGtTarget <- function( x, target, FUN=mean )
      {
        # Calculate for each replicate (row of x), whether GE target or not.
        gtTarget <- function( x, target )
        {
          result <- rep( 0, length(x) )
          result <- ifelse( x > target, 1, result )
          result
        }

        # Number of time steps (years) in time slice.
        n <- ncol( x )
        
        # Matrix of (0,1) to show when x is GT target for each replicate.
        tmp <- t( apply( x,1,FUN=gtTarget, target ) )

        # Calculate sum of each row, which is the number GT target.
        nTarget <- apply( tmp,1,sum )

        # Proportion of time steps (years) GT target.
        pTarget <- nTarget / n

        # Mean proportion above target.
        avgPtarget <- mean( pTarget )

        result <- list( avgPtarget=avgPtarget, n=n, pTarget=pTarget, nTarget=nTarget )
        result
      }

      if ( validSim )
      {
        # Objective 1 - the mean proportion of projected years where SSB is in
        # the Cautious or Healthy zones.
        tmpObj1 <- .calcPropGtTarget( Bt[,tdx], target=limitB, FUN=mean )
        tmpObj3 <- .calcPropGtTarget( Bt[,tdx], target=Bmsy,   FUN=mean )
        tmpHealthy <- .calcPropGtTarget( Bt[,tdx], target=upperB, FUN=mean )

        result[ iRow,"pObj1" ] <- tmpObj1$avgPtarget
        result[ iRow,"pObj3" ] <- tmpObj3$avgPtarget
        result[ iRow,"pHealthy" ] <- tmpHealthy$avgPtarget
      }

      if( validSim )
      {
        # Hijack calcPropFloor() to calculate proportion
        # of years below a .5kt TAC, representing
        # lost fishing opportunities.
        tmp <- .calcPropFloor( Ct[,tdx], floor = .5 )
        tmp <- quantile( 1-tmp, probs = quantVals )
        result[iRow,"medPropClosure"] <- tmp[3]
        result[iRow,"Q1PropClosure"] <- tmp[1]
        result[iRow,"Q2PropClosure"] <- tmp[5]

      }

      #--- Policy Statistics                                                ---#

      if ( validSim )
      {
        tmp <- .calcStatsPolicy( Dept[,tdx], DepMSY, limitDep, tMP )
        #result[ iRow,"medProbGteDmsy"  ] <- tmp$medProbGteDmsy
        #result[ iRow,"medProbGteLimit" ] <- tmp$medProbGteLimit
        result[ iRow,"probGteDepMSY" ] <- tmp$probGteDepMSY
        result[ iRow,"probGteLimit"  ] <- tmp$probGteLimit
      }

      #--- Trend statistics - this is set to start at the first year of the period
      #--- for an interval of "delta" years to a maximum of nT-t1 years.

      if ( validSim )
      {
        tmp <- .calcStatsTrend( Bt, t1=t1, delta = t2 - t1 )

        SSB    <- as.numeric( Bt[,unique(tmp$t1) ] )
        target <- B0

        tmp$pDecline <- .calcStatsAccDecline( SSB, target, lowProb=0.05,
                          hiProb=0.5, multLrp=.3, multUsr=.75  )

        result[ iRow, "t1Trend" ] <- t1
        result[ iRow, "trendPeriod" ] <- 10
        result[ iRow, "avgExpSlope" ] <- mean( tmp$beta )
        nInc <- sum( tmp$beta >= 0.0 )
        nDec <- length( tmp$beta ) - nInc
        pObs <- nDec / length( tmp$beta )
        result[ iRow, "trendInc" ]    <- nInc
        result[ iRow, "trendDec" ]    <- nDec
        result[ iRow, "obsPdecline" ] <- pObs

        result[ iRow, "pDecline" ]    <- median( tmp$pDecline )

      }

      #--- Convenience - Depletion and Catch at t1 for each period.         ---#
      if ( validSim )
      {
        tmp <- .calcStatsCatch( Ct[,tdx], quantVals )
        result[ iRow, "t1AvgCatch" ] <- mean( Ct[,tdx[1] ] )
        result[ iRow, "t1AvgDep" ]   <- mean( Dept[,tdx[1] ] )
      }
    }     # Loop over j periods.

    # Do garbage collection to clean memory of loose ends, accumulated blobs.
    gc()

   if ( !validSim )
      cat( "\nMSG (.calcPerfStats) No feedback results for simulation ",i,"\n" )

  }   # Loop over i simulations.

  result2 <- data.frame( matrix( NA, nrow=(nrow(result)*length(statNames)),
                           ncol=(length(headerNames)+2) ) )

  names( result2 ) <- c( headerNames,"statistic","value" )

  # Now format simulation summary in normalized form.
  iRow <- 0
  for ( i in 1:nrow( result ) )
  {
    for ( j in 1:length(statNames) )
    {
      iRow <- iRow + 1
      result2[ iRow,c(1:length(headerNames)) ] <- result[ i,c(1:length(headerNames)) ]
      result2[ iRow,"statistic" ] <- statNames[j]
      result2[ iRow,"value" ]     <- result[ i,(length(headerNames)+j) ]
    }
  }

  perResult   <- 0     # Not used at this time.
  summary1 <- result
  summary2 <- result2

  return( list( summary1=summary1, summary2=summary2,
                perResult=perResult ) )
}


# .calcStatsMinProbGtX (Calculate the minimum probability of depletion > X)
# Purpsoe:      Calculate the minimum probability over reps of being above 
#               X year to year
# Parameters:   Dt    - catch biomass as an nRep by nT matrix.
#               X     - Value to be compared to
# Returns:      val, a list with the minimum (over years) probability (within
#               years/over reps) of Dt > X
# Notes:        Differs from .calcQunatsRefPoints type calcs as it
#               integrates over reps (vertically) first, then
#               takes the min of the yearly probability within tdx
# Source:       A.R. Kronlund
.calcStatsMinProbGtX <- function( Dt, X = .3 )
{
  # Updated depletion values with 1s (success) or 0s (failure)
  Dt[Dt > X] <- 1
  Dt[Dt <= X] <- 0

  # Calculate yearly prob (mean of successes)
  yearlyProbs <- apply(X = Dt, FUN = mean, MARGIN = 2 )

  # Return min value
  val <- min(yearlyProbs)
  val
}


# .calcStatsAAV (Calculate quantiles of Average Annual Varability statistics)
# Purpose:      Calculate quantiles of AAV statistics over simulation replicates.
# Parameters:   Dt    - catch biomass as an nRep by nT matrix.
#               tdx   - indices of time period for statistics.
#               probs - probabilities for quantiles.
# Returns:      val, a list with the median AAV over replicates and a vector
#               with the quantiles specified by probs.
# Notes:        In contrast to other .calcStatsXXX functions, this function
#               requires the entire 1:nT time period because of the lag in the
#               AAV calculations.
# Source:       A.R. Kronlund
.calcStatsAAV <- function( Dt, tdx, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # The period must be of length >= 2.  Can't form the difference otherwise.
  if ( length(Dt) >= 2 )
  {
    # Form the absolute catch differences over 1:nT by replicate.
    diffDt    <- t( apply( Dt,1,diff ) )
    absDiffDt <- abs( diffDt )

    # Shift differences by 1 to catch transition from previous period
    # to current period, i.e., sum(|C_t-C_t-1|) for t=t1,..,t2.  For example,
    # if the current year is 2008, you have to get the first catch difference
    # of 2008-2007 to respect C_t - C_t-1, i.e., when t=2008, t-1=2007.

    # Sum the absolute differences of the shifted absolute catch differences.
    sumAbsDiffDt <- apply( absDiffDt[,(tdx-1)],1,sum )

    # Sum the catch by replicate over the summary period specified by tdx.
    sumCatch <- apply( Dt[,tdx], 1, sum )

    # Compute the AAV by replicate.
    AAV <- ifelse( sumCatch > 0.0, sumAbsDiffDt / sumCatch * 100.0, 0.0 )
    # Compute the median of the AAV values for each replicate.
    medAAV <- median( AAV )
    # Compute the quantiles of the distribution.
    # Use the quantiles specified in the interface (qLower, qUpper).
    qVals <- quantile( AAV, probs=probs, na.rm = T )
  }
  else
  {
    # If the length of Dt is insufficent to form AAV, everything gets -1.
    medAAV <- -1
    qVals <- rep( -1, length(probs) )
  }
  val <- list( medAAV=medAAV, qVals=qVals )
  val
}


# .calcStatsCatch (Calculate quantiles of average catch statistics)
# Purpose:        Calculate quantiles of average catch statistics over
#                 simulation replicates.
# Parameters:     Dt    - catch biomass as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average catch over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsCatch <- function( Ct, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Compute average catch by replicate over the range specified in tdx.
  avgCatch <- apply( Ct,1,mean )
  # Compute the median of the average catch values.
  medAvgCatch <- median( avgCatch )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgCatch, probs=probs, na.rm = T )
  val <- list( medAvgCatch=medAvgCatch, qVals=qVals )
  val
}

# .calcStatsDiscard (Calculate quantiles of average discards statistics)
# Purpose:        Calculate quantiles of average catch statistics over
#                 simulation replicates.
# Parameters:     Dt    - catch biomass as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average catch over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsDiscard <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Compute average catch by replicate over the range specified in tdx.
  avgDiscard <- apply( Dt,1,mean )
  # Compute the median of the average catch values.
  medAvgDiscard <- median( avgDiscard )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgDiscard, probs=probs, na.rm = T )
  val <- list( medAvgDiscard=medAvgDiscard, qVals=qVals )
  val
}

# .calcStatsDepletion (Calculate quantiles of average depletion statistics)
# Purpose:        Calculate quantiles of average depletion statistics over
#                 simulation replicates.
# Parameters:     Dept  - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median average depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsDepletion <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Average depletion by replicate (avgDept is vector len=ncol(Dept)).
  avgDep <- apply( Dept,1,mean, na.rm = T )
  # Compute the median of the average depletion values.
  medAvgDep <- median( avgDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( avgDep, probs=probs, na.rm = T )
  val <- list( medAvgDep=medAvgDep, qVals=qVals )
  val
}


# .calcStatsFinalDep (Calculate quantiles of final depletion statistics)
# Purpose:        Calculate quantiles of final depletion statistics over
#                 simulation replicates.
# Parameters:     Dep   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 probs - probabilities for quantiles.
# Returns:        val, a list with the median final depletion over replicates
#                 and a vector with the quantiles specified by probs.
# Source:         A.R. Kronlund
.calcStatsFinalDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the final depletion in the period passed to function.
  # The passed Dept is an nRep by length(t1:t2) matrix.  Thus, ncol(Dept)
  # is the last column (t2) in the matrix - the final depletion for the period.
  finalDep <- Dept[ ,ncol(Dept) ]
  # Compute the median of the final depletion values, final year of period over reps.
  medFinalDep <- median( finalDep, na.rm  = T )
  # Compute the quantiles of the distribution.
  qVals <- quantile( finalDep, probs=probs, na.rm = T )
  val <- list( medFinalDep=medFinalDep, qVals=qVals )
  val
}

# .calcStatsHighCat (Calculate quantiles of low catch statistics)
# Purpose:         Calculate quantiles of low catch statistics over
#                  simulation replicates.
# Parameters:      Dt  - catch as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low catch over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsHighCat <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low catch values from the replicate.
  # This apply operation returns a vector of length=nRep.
  highCat <- apply( Dt,1,max )
  # Compute the median of the low depletion values for the period over reps.
  medHighCat <- median( highCat )
  # Compute the quantiles of the distribution.
  qVals <- quantile( highCat, probs=probs, na.rm = T )
  val <- list( medHighCat=medHighCat, qVals=qVals )
  val
}

# .calcStatsLowCat (Calculate quantiles of low catch statistics)
# Purpose:         Calculate quantiles of low catch statistics over
#                  simulation replicates.
# Parameters:      Dt  - catch as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low catch over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowCat <- function( Dt, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low catch values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowCat <- apply( Dt,1,min )
  # Compute the median of the low depletion values for the period over reps.
  medLowCat <- median( lowCat )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowCat, probs=probs, na.rm = T )
  val <- list( medLowCat=medLowCat, qVals=qVals )
  val
}


# .calcPropFloor (Calculate proportion of years in which catch is greater than or equal to floor)
# Purpose:         Calculate proportion over
#                  simulation replicates.
# Parameters:      Ct  - catch as an nRep by length(t1:t2) matrix.
#                  floor - catch floor
# Returns:         
# Source:          K.Holt
.calcPropFloor <- function( Ct, floor )
{
  
  .LTfloor<-function (x) {
    dum<-rep(0,length(x))
    dum[x>=floor]<-1
    p<-sum(dum)/length(dum)
    p
  }

  val<-apply(Ct,1,.LTfloor)
  val
}



# .calcStatsLowDep (Calculate quantiles of low depletion statistics)
# Purpose:         Calculate quantiles of low depletion statistics over
#                  simulation replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix.
#                  probs - probabilities for quantiles.
# Returns:         val, a list with the median low depletion over replicates
#                  and a vector with the quantiles specified by probs.
# Source:          A.R. Kronlund
.calcStatsLowDep <- function( Dept, probs=c(0.05,0.1,0.5,0.9,0.95) )
{
  # Extract the low depletion values from the replicate.
  # This apply operation returns a vector of length=nRep.
  lowDep <- apply( Dept,1,min, na.rm = T )
  # Compute the median of the low depletion values for the period over reps.
  medLowDep <- median( lowDep )
  # Compute the quantiles of the distribution.
  qVals <- quantile( lowDep, probs=probs, na.rm = T )
  val <- list( medLowDep=medLowDep, qVals=qVals )
  val
}


# .calcStatsPolicy (Calculate statistics related to Limit and Bmsy policy)
# Purpose:         (1) Calculate median probability of depletion > Dmsy for
#                      the time period over replicates,
#                  (2) Calculate the median probability of depletion >
#                      Dep at the LRP for the time period over replicates.
# Parameters:      Dept  - depletion as an nRep by length(t1:t2) matrix,
#                          where (t1:t2) defines the summary period.
#                  Dmsy  - depletion at Bmsy.
#                  zoneLimit - depletion at Critical-Cautious zone boundary.
#                  tMP   - start year of management procedure.
# Returns:         val, a list with the median statistics for the following:
#                  Pr(Dep>=Dmsy), Pr(Dep<Dmsy), Pr(Dep>=Dlimit), Pr(Dep<Dlimit).
# Source:          A.R. Kronlund
.calcStatsPolicy <- function( Dept, DepMSY, zoneLimit, tMP )
{
  isGTE <- function( x, bound )
  {
    result <- rep( "Below",length(x) )
    result <- ifelse( x >= bound, "Above", result )
    result
  }

  calcPcats <- function( x, bound )
  {
    # Calculate number and proportion GTE specified bound for each replicate.
    tmp <- t( apply( x,1,FUN=isGTE, bound ) )

    catLabels <- c( "Below","Above" )
    nCats <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(catLabels) ) )
    names( nCats ) <- catLabels
    pCats <- nCats

    for ( i in 1:nrow(tmp) )
      nCats[ i, ] <- table( factor(tmp[i,],levels=catLabels ) )

    # Calculate the proportions.
    pCats <- nCats / apply( nCats,1,sum )

    result <- list( nCats=nCats, pCats=pCats )
    result
  }

  # Calculate the median proportion GTE Bmsy.
  tmp <- calcPcats( Dept, DepMSY )
  medProbGteDepMSY <- median( tmp$pCats[,"Above"] )
  medProbLtDepMSY  <- median( tmp$pCats[,"Below"] )

  # Calculate the median proportion GTE zoneLimit.
  tmp <- calcPcats( Dept, zoneLimit )

  medProbGteLimit <- median( tmp$pCats[,"Above"] )
  medProbLtLimit  <- median( tmp$pCats[,"Below"] )

  # Now calculate irrespective of replicates.
  probGteLimit <- sum( Dept >= zoneLimit ) / length( Dept )
  probGteDepMSY  <- sum( Dept >= DepMSY      ) / length( Dept )

  val <- list( medProbGteDepMSY=medProbGteDepMSY,   medProbLtDepMSY=medProbLtDepMSY,
               medProbGteLimit=medProbGteLimit, medProbLtLimit=medProbLtLimit,
               probGteLimit=probGteLimit, probGteDepMSY=probGteDepMSY )
  val
}

.calcStatsRefPoints <- function( Bt, target, targMult=0.4, refProb=0.95 )
{
  refPt <- targMult * target

  # Find all years in period where Bt > refPt.
  tmp <- matrix( 0, nrow=nrow(Bt), ncol=ncol(Bt) )
  tmp[ Bt >= refPt ] <- 1

  # Count the number of years in each replicate where where Bt > refPt.
  val <- apply( tmp,1,sum )

  # Calculate the proportion of years in each replicate where Bt > refPt.
  pVal <- val / ncol( Bt )

  # Compute the mean proportion of years GT refPt.
  result <- median( pVal )
  result
}

.calcQuantsRefPoints <- function( Bt, target, targMult, refProb, probs )
{
  refPt <- targMult * target

  if(class(Bt) == "numeric" ) Bt <- matrix( Bt, ncol = 1)

  # Find all years in period where Bt > refPt.
  tmp <- matrix( 0, nrow=nrow(Bt), ncol=ncol(Bt) )
  tmp[ Bt >= refPt ] <- 1

  if( ncol(Bt) == 1 )
  {
    pVal <- sum(tmp) / nrow(tmp)
    return(pVal)
  } else {
    # Count the number of years in each replicate where where Bt > refPt.
    val <- apply( tmp,1,sum )

    # Calculate the proportion of years in each replicate where Bt > refPt.
    pVal <- val / ncol( Bt )

    # browser()

    # Compute the mean proportion of years GT refPt.
    result <- quantile( pVal, probs, na.rm = T )
    result
  }
  
}

# .calcStatsTarget (Calculate target statistics WRT dep, time, certainty)
# Purpose:         Calculate statistics for depletion, year and probability
#                  where the response variable is conditional on the remaining
#                  two variables.  Response variable determined by outcome.
#
# Parameters:  Dept       - entire nRep by length(1:nT) matrix of depletion.
#              tMP        - start year of management procedure.
#              objDep  - user specified depletion.
#              objYear - user specified year.
#              objProb - user specified probability (certainty).
#              outcome - one of "dep", "year", "prob" to indicate target outcome.
# Returns:     result, a list with all three variables outcome to indicate which
#                      is the response.
# Source:          A.R. Kronlund
.calcStatsTarget <- function( Dept, tMP, objDep, objYear, objProb, outcome )
{
  # Assume that the entire time series is passed as nRep by nT matrix.
#browser() 
  nT   <- ncol( Dept )           # Total number of time steps.
  tVec <- c( 1:nT )              # Vector of time index values.
  tdx  <- c( tMP:nT )            # Time index values for projection period.

  # (1) Calculate year when objDep is first attained with targetProb certainty.
  if ( outcome=="year" )
  {
    # Calculate the quantile coresponding to 1-objProb at each time step.
    # This means that objProb*100% of the distribution of depletion values
    # is greater than the quantile at each time step. This is a 1:nT vector.

    qObj <- apply( Dept, 2, quantile, probs=c(1.0-objProb) )

    # Find the index of the first PROJECTION time step where qObj >= objDep,
    # i.e., where the depletion quantile corresponding to objProb is greater
    # than the objDep.  This means you are objProb*100% sure that the objDep
    # has been attained.  No guarantee it will remain attained.

    isObj  <- qObj >= objDep           # TRUE/FALSE vector size 1:nT.
    tdxObj <- tdx[isObj[ tdx ]]        # TRUE in projection period.

    if ( !all(is.na(tdxObj)) )
      objYear <- min( tdxObj )         # 1st time step in projection period.
    else
      objYear <- 0                     # No values GTE the targetDep.
    
    cat( "\nMSG (.calcStatsTarget) Year = ",objYear," when depletion >=",objDep,
         "with",objProb,"probability.\n" )  
  }     # if outcome is "year".

  else if ( outcome=="prob" )
  {
    # (2) Calculate the quantile of objDep at the objYear index (tMP,...,nT).

    # Here we find the proportion of depletion values at time objYear that
    # exceed the objDep, i.e., certainty of exceeding objDep at objYear.
    # Setting objProb = 0.133 for HAL-2016 neutral F
    depVals <- Dept[ ,objYear ]
    objProb <- sum( depVals >= objDep ) / nrow(Dept)
    
    cat( "\nMSG (.calcStatsTarget) Probability is",objProb,"that depletion exceeds",
         objDep,"at year",objYear,"\n" )
  }     # if outcome="prob".

  else if ( outcome=="dep" )
  {
    # (3) Calculate the depletion at objYear and objProb.
    depVals <- Dept[ ,objYear ]    
    objDep <- quantile( depVals,probs=(1.0-objProb), na.rm = T )
    
    cat( "\nMSG (.calcStatsTarget) Depletion >=",objDep," at year",objYear,
         "with",objProb,"probability.\n" )
  }     # if outcome="dep".

  # Return a list result.
  result <- list( objDep=objDep, objYear=objYear, objProb=objProb, outcome=outcome )
  result
}     # END function .calcStatsTarget


# .calcStatsTarget (Calculate statistics for Xt WRT target, time, certainty)
# Purpose:         Calculate statistics determined by target objectives entry
#                  fields on guiPerf:
#      (1) yearAtTargetProb: year where Xt >= target with specified probablity,
#      (2) probAtTargetYear: probability of Xt >= target at the specified year,
#      (3) targetAtYearProb: target at probability at the specified time such
#                             that Pr(Xt >= target)=1-prob.
#
# Parameters:  Xt         - entire nRep by length(1:nT) matrix of depletion.
#              baseVal    - base value (Bmsy or B0).
#              tMP        - start year of management procedure.
#              targetMult - user specified target multiplier
#              targetYear - user specified target year.
#              targetProb - user specified target probability (certainty).
# Returns:     result, a list calculated yearAtTargetProb, probAtTargetYear, and
#              targetAtYearProb values, along with baseVal, targMult, targetYear, targetProb.
# Source:          A.R. Kronlund
.calcStatsTargetOLD <- function( Xt, baseVal, tMP, targetMult,targetYear,targetProb )
{
  # Assume that the entire time series Xt is passed as nRep by nT matrix.

  # Calculate the target.
  target <- targetMult * baseVal

  nT   <- ncol( Xt )             # Total number of time steps.
  tVec <- c( 1:nT )              # Vector of time index values.
  tdx  <- c( tMP:nT )            # Time index values for projection period.

  # (1) Calculate time when target is first attained with targetProb certainty.

  # Calculate the quantile corresponding to 1-targetProb at each time step.
  # This means that targetProb*100% of the distribution of target values
  # is greater than the quantile at each time step. This is a 1:nT vector.

  qTarget <- apply( Xt, 2, quantile, probs=c(1.0-targetProb) )

  # Find the index of the first PROJECTION time step where qTarget >= target,
  # i.e., where the target quantile corresponding to targetProb is greater
  # than the target.  This means you are targetProb*100% sure that the
  # target has been attained.  No guarantee it will remain attained.

  tSteps    <- c(1:nT)               # t=1,..., nT
  isTarget  <- qTarget >= target     # TRUE if quantile >= target.
  tdxTarget <- tSteps[ isTarget ]    # t values where quantile >= target.

  # Now determine if there are any in the projection period that exceed the target.
  if ( length(tdxTarget) > 0 )
  {
    tdxTarget <- tdxTarget[ tdxTarget >=tMP ]
    if ( length(tdxTarget) > 0 )
      yearAtTargetProb <- min(tdxTarget)
    else
      yearAtTargetProb <- -1                   # No values GTE the target.
  }
  else
    yearAtTargetProb <- -1                     # No values GTE the target.

  # (2) Calculate the quantile of target at the targetYear index (tMP,...,nT).

  # Here we find the proportion of target values at time targetYear that
  # exceed the targetDep, i.e., certainty of exceeding targetDep at targetYear.

  targetVals       <- Xt[ ,targetYear ]
  probAtTargetYear <- sum( targetVals >= target ) / nrow(Xt)

  # (3) Calculate the target at targetYear and targetProb.
  targetAtYearProb <- quantile( targetVals,probs=(1.0-targetProb), na.rm = T )

  # Return a list result.
  result <- list( yearAtTargetProb=yearAtTargetProb, probAtTargetYear=probAtTargetYear,
                  targetAtYearProb=targetAtYearProb,
            baseVal=baseVal, target=target,
            targetMult=targetMult, targetYear=targetYear, targetProb=targetProb )
  result
}

.calcStatsTrend <- function( y, t1, delta=10 )
{
  nCol_y <- ncol(y)
  delta  <- min(nCol_y-t1,delta)
  # y is a matrix of dimension nRep by nT.
  y1 <- as.numeric( y[ ,t1 ] )
  y2 <- as.numeric( y[ ,t1+delta ] )

  beta <- ( log(y2) - log(y1) ) / delta
  yPrime <- exp( (beta * delta) + log(y1) )
  result <- data.frame( t1=t1, t2=t1+delta, beta=beta, y1=y1, y2=y2, yPrime=yPrime )
  result
}

.calcStatsAccDecline <- function( SSB, target, lowProb=0.05, hiProb=0.5,
   multLrp=0.4, multUsr=0.8  )
{
  LRP     <- multLrp * target
  USR     <- multUsr * target
  pDecline <- lowProb + (hiProb-lowProb) * (SSB-LRP)/(target-LRP)
  pDecline <- ifelse( SSB < LRP,    lowProb, pDecline )
  pDecline <- ifelse( SSB > target, hiProb,  pDecline )
  pDecline
}

# .calcStatsZones (Calculate proportion of time SSB in each stock status zone)
# Purpose:        Calculate quantiles of the proportion of time (probability)
#                 that the SSB was in the Critical, Cautious, and Healthy stock
#                 status zones during time period over the simulation replicates.
# Parameters:     Dept   - depletion as an nRep by length(t1:t2) matrix,
#                         where (t1:t2) defines the summary period.
#                 limitD - boundary Crit-Cautious zones on the depletion scale.
#                 upperD - boundary Cautious-Healthy zones on depletion scale.
#                 probs  - probabilities for quantiles (not used yet).
# Returns:        val, a list with the median final depletion over replicates
# Source:         A.R. Kronlund
.calcStatsZones <- function( Dept, limitD, upperD, probs )
{
  # At this time the probs are not used. Only median statistics are computed.

  findZone <- function( x, limitD, upperD )
  {
    zone <- rep( "Critical",length(x) )
    zone <- ifelse( x > limitD, "Cautious",zone )
    zone <- ifelse( x > upperD, "Healthy", zone )
    zone
  }

  n <- ncol(Dept)

  # Calculate number in zone (Critical, Cautious Healthy) for each replicate.
  tmp <- t( apply( Dept,1,FUN=findZone, limitD, upperD ) )

  zoneLabels <- c( "Critical","Cautious","Healthy" )
  nZone <- data.frame( matrix( NA,nrow=nrow(tmp),ncol=length(zoneLabels) ) )
  names( nZone ) <- zoneLabels
  pZone <- nZone

  for ( i in 1:nrow(tmp) )
    nZone[ i, ] <- table( factor(tmp[i,],levels=zoneLabels ) )

  # Calculate the proportions.
  pZone <- nZone / apply( nZone,1,sum )

  # Now calculate the medians.
  medCriticalP <- median( pZone[,"Critical"] )
  medCautiousP <- median( pZone[,"Cautious"] )
  medHealthyP  <- median( pZone[,"Healthy" ] )

  # Now calculate the proportions ignoring replicates.
  nAllZone <- table( cut( Dept, breaks=c(0,limitD,upperD,max(Dept) ) ) )
  pAllZone <- nAllZone / length(Dept)

  val <- list( medCriticalP=medCriticalP, medCautiousP=medCautiousP,
               medHealthyP=medHealthyP, nZone=nZone, pZone=pZone,
               nAllZone=nAllZone, pAllZone=pAllZone )
  val
}

.saveSimStatsToFile <- function( obj )
{
  # obj is a simTracker list.

  nSim <- length(obj)

  varNames <- c( "Sim","Scenario","MP",
                 "h","M","B0",
                 "MSY","Umsy","Bmsy","Dmsy","Dlrp",
                 "avgSSBtMP","avgDeptMP","avgLegalBtMP","avgLegalHRtMP","avgCtMP" )

  result <- data.frame( matrix( NA, nrow=nSim, ncol=length(varNames) ) )
  names( result ) <- varNames

  for ( i in 1:nSim )
  {
    # Load an Rdata working directory containing a list called blob.
    simFile <- paste( "sim",obj[[i]]$om$stamp,".Rdata",sep="" )
    cat( "\nMSG (.saveSimStatsToFile) Loading",simFile,"...\n" )
    load( file=simFile )
    #assign( "blob", blob, pos=1 )

    scenarioName <- obj[[i]]$gui$scenarioName
    if (.Platform$OS.type=="windows" )
    {
      scenarioName <- iconv( scenarioName, to="ASCII//TRANSLIT" )
      scenarioName <- gsub( "\"","",scenarioName )
    }

    mpName <- obj[[i]]$gui$mpName

    result[ i,"Sim"      ] <- paste( "Sim",i,sep="" )
    result[ i,"Scenario" ] <- scenarioName
    result[ i,"MP"       ] <- mpName
    result[ i,"MSY"      ] <- blob$ctlList$opMod$landedFmsy

    # ARK (17-Dec-10) Changed Umsy to legalHRFmsy.
    #result[ i,"Umsy"     ] <- blob$pars$Umsy
    result[ i,"Umsy"     ] <- blob$ctlList$opMod$legalHRFmsy
    result[ i,"h"        ] <- blob$ctlList$opMod$rSteepness
    result[ i,"M"        ] <- blob$ctlList$opMod$M
    result[ i,"B0"       ] <- blob$ctlList$opMod$B0
    result[ i,"Bmsy"     ] <- blob$ctlList$opMod$ssbFmsy
    result[ i,"Dmsy"     ] <- result[ i,"Bmsy" ] / result[ i, "B0" ]
    result[ i,"Dlrp"     ] <- (0.4 * result[ i,"Bmsy" ] ) / result[ i, "B0" ]
    #result[ i,"Dusr"     ] <- (0.8 * result[ i,"Bmsy" ] ) / result[ i, "B0" ]

    tMP <- blob$pars$tMP
    result[ i,"avgSSBtMP"     ] <- mean( blob$om$Bt[ ,paste("B",tMP,sep="") ] )
    result[ i,"avgDeptMP"     ] <- mean( blob$om$Bt[ ,paste("B",tMP,sep="") ]/result[i,"B0"] )
    result[ i,"avgLegalBtMP"  ] <- mean( blob$om$legalB[ ,paste("lBiomass",tMP,sep="") ] )
    result[ i,"avgLegalHRtMP" ] <- mean( blob$om$legalHR[ ,paste("lHR",tMP,sep="" ) ] )

    # For some reason Ct is not named, but add one for the replicate ID.
    result[ i,"avgCtMP"       ] <- mean( blob$om$Ct[ ,tMP+1 ] )
  }
  cat( "\n" )

  # Save to file here.
  OStype <- .Platform$OS.type
  if ( OStype=="windows" )
  {
    fName <- "mseRsimStats.xls"
    if ( file.exists(fName) )
      fileGone <- file.remove( fName )

    conn <- RODBC::odbcConnectExcel( fName, readOnly=FALSE )
    .excelTable( conn, result, "SimStats", varNames, dimnames(result)[[1]] )
    close( conn )
  }
  else
  {
    fName <- "mseRsimStats.csv"
    write.csv( result, file = fName, append=FALSE, quote=TRUE, row.names=TRUE )
  }
  cat( "\nMSG (.saveSimStatsToFile) Parameters saved to ",fName,"\n" )

  tmp <- result
  tmp[ , c(4:ncol(tmp)) ] <- round( tmp[ ,c(4:ncol(tmp)) ], digits=3 )
  print( tmp )

  result
}

.plotWhatIfSurvey <- function( obj, tIndex=47, deltaSurvey=c(-1,seq(0.5,2,0.1)) )
{
  # 1. Get catch  data.
  # 2. Get survey data.
  # 3. Get tIndex survey point.  Apply deltaSurvey.

  varNames <- c( "Delta","tIndex","Index","estMSY","estBmsy","estFmsy",
                 "projBiomass","precF","lastCatch","legalHarv" )

  result <- matrix( NA, nrow=length(deltaSurvey), ncol=length(varNames) )
  result <- data.frame( result )
  names( result ) <- varNames

  parList <- .createList( obj$runMSEpar )

  for ( i in 1:nrow(result) )
  {
    result$Delta[i]  <- deltaSurvey[ i ]
    result$tIndex[i] <- tIndex

    #--------------------------------------------------------------------------#
    # Perform the production model stock assessment.                           #
    #--------------------------------------------------------------------------#

    tmp <- list()

    # Get the index values
    tmp$nIndices <- length( parList$mp$data$useIndex )

    #tmp$Itg       <- obj$mp$data$Itg[1:(t-1),parList$mp$data$useIndex]
    Itg <- parList$opModData$index$idxSeries[ ,1:(tIndex-1) ]

    # Run assessment
    if( parList$mp$assess$assessMethod=="pMod" )
    {
      tmp$Itg <- Itg

      if ( deltaSurvey[i]!=-1 )
        tmp$Itg[ 3,tIndex-1 ] <- Itg[ 3, tIndex-2 ] * deltaSurvey[ i ]

      result$Index[i] <- tmp$Itg[ 3,tIndex-1 ]

      tmp$Itg <- t( tmp$Itg )

      # SPC 19June2010: need to assemble and fill in the data required by pMod assessment
      # If t == tMP, use init values of 0 for Omega and user inputs for Bo and r
      #tmp$initMSY         <- mp$assess$initMSY
      #tmp$initFmsy        <- mp$assess$initFmsy
      tmp$lnOmega         <- rep(0,(tIndex-3-1))
      tmp$surveyFirstYear <- parList$mp$data$t1Index
      tmp$surveyLastYear  <- rep((tIndex-1),tmp$nIndices)
      tmp$IndexType       <- parList$mp$data$IndexType
      tmp$useIndex        <- parList$mp$data$useIndex
      tmp$rhoEIV          <- parList$mp$assess$rhoEIV
      tmp$muPriorMSY      <- parList$mp$assess$muPriorMSY
      tmp$sdPriorMSY      <- parList$mp$assess$sdPriorMSY
      tmp$muPriorFmsy     <- parList$mp$assess$muPriorFmsy
      tmp$sdPriorFmsy     <- parList$mp$assess$sdPriorFmsy

      #tmp$Dt              <- obj$om$Ct[1:(t-1)] #rowSums( om$Ctg[ (1:(t-1)), ] )
      retC <- parList$opModData$catch$landCatchMatrix
      tmp$Dt <- apply( retC[,3:ncol(retC)],1,sum ) / 1000.0
      #tmp$Dt <- obj$om$legalC[ 1,c(1:(tIndex-1))+1 ]

      stockAssessment       <- assessModPMod( tmp )

      result$estBmsy[i]     <- stockAssessment$estPars$Bmsy
      result$estMSY[i]      <- stockAssessment$estPars$MSY
      result$estFmsy[i]     <- stockAssessment$estPars$Fmsy
      result$projBiomass[i] <- stockAssessment$biomass
      result$lastCatch[i]   <- stockAssessment$estPars$dep

      # If the "always replace failures with last successful year" was selected in
      # guiSim, the following series of if statements implement this functionality
      fail <- FALSE
      ## Test for failure in production model to converge in current year:
      #if (stockAssessment$estPars$iExit !=1 | stockAssessment$estPars$maxGrad >= 0.0001)
      #  fail <- TRUE

      # Build harvest control rule object.
      # Most recent survey spawning biomass estimate (i.e.,from t-1)
      rule           <- obj$mp$hcr
      rule$assess    <- obj$mp$assess$assessMethod
      rule$biomass   <- stockAssessment$biomass
      rule$lastCatch <- stockAssessment$estPars$dep
      rule$remRate   <- stockAssessment$estPars$Fmsy

      rule$lowerB <- rule$specs$limitRefMult * stockAssessment$estPars$Bmsy
      rule$upperB <- rule$specs$upperRefMult * stockAssessment$estPars$Bmsy

      #rule$maxF      <- qlnorm(p=0.95,meanlog=log(tmp$muPriorFmsy),sdlog=tmp$sdPriorFmsy)

      .calcLegalCatch <- function( obj )
      {
        # Extract assessment quantities from harvest control rule list.
        biomass     <- obj$biomass
        lastCatch   <- obj$lastCatch

        # Extract fixed members of input harvest control rule list
        lastTAC     <- obj$lastCatch          # TAC[t-1]
        lam1        <- obj$specs$lam1         # Weight factor on TAC[t-1].
        remRate     <- obj$remRate            # Instantaneous fishing mortality rate reference

        lowerB <- obj$lowerB  # Limit reference point, where remRate=0
        upperB <- obj$upperB  # Upper stock reference, where remRate=Fmsy

        # Calculate effective removal rate based on biomass relative to ref pts
        adjF     <- remRate*(biomass-lowerB)/(upperB-lowerB)

        #adjF     <- min( adjF, maxF )
        if( adjF < 0 ) adjF <- 0
          adjF <- ifelse( biomass < upperB, adjF, remRate  )

        catchLim              <- lam1 * lastCatch + (1.0-lam1)*adjF*biomass

        result                <-list()
        result$precautionaryF <- adjF
        result$catchLimit     <- max( 0., catchLim )
        result
      }

      # Calculate catch limit
      targetHarv   <- .calcLegalCatch( rule )

      result$precF[i]   <- targetHarv$precautionaryF
      result$legalHarv[i] <- targetHarv$catchLimit

    }     # if method is pMod.
  }     # for i=1:nrow(result)

  xLim <- range( deltaSurvey[ deltaSurvey!=-1 ] )
  yLim <- range( 0,result$legalHarv )

  plot( xLim, yLim, type="n", axes=FALSE, xlab="", xlim=xLim, ylab="", ylim=yLim )

  idx <- c(1:nrow(result))[ result$Delta==-1 ]
  #points( 1,result$legalHarv[idx], cex=2, pch=3 )
  abline( h=result$legalHarv[idx], lty=2, lwd=2 )
  abline( h=2.35, lty=1 )

  abline( h=seq(0,2.5,0.1), col="lightgray" )

  abline( v=1, lty=3, lwd=2 )

  points( result$Delta, result$legalHarv, bg="white", cex=1.4 )

  axis( side=1, cex.axis=.CEXAXIS )
  axis( side=2, cex.axis=.CEXAXIS, las=.YAXISLAS )
  axis( side=3, labels=FALSE )
  axis( side=4, labels=FALSE )
  box()

  panLegend( 0.6,0.1, legTxt="No 2010 StRS Value", lty=2, lwd=2 )

  mtext( side=1, line=2, cex=.CEXLAB, "Multiplier of 2009 StRS Survey CPUE" )
  mtext( side=2, line=3, cex=.CEXLAB, "Legal Harvest 2011" )

  result
}



