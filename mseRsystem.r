
# Task and Issue List:

# (1) Cannot use legalC and Ct (sum Ctg) interchangeably - they are different.
#     SPC feeds legalC into SP, but then calculates lastCatch from Ctg. Very
#     similar but should pick one or the other - or explain it in the code.

# (2) Need to check deterministic (almost) behavior of procedures.

# (3) Control rules use of M needs to be confirmed/corrected.
  # SPC: unless assessCA is configured to estimate M, we should just
  # use the true average M. Note that we are simulating random M, so
  # that might be good enough to capture realism.

# (4) This issue is not resolved!!!!

  # Step 3: Determine linear interpolation of acceptable pDecline within zones
  # based on current stock status at nT, trend, lower and upper limits.

  # ARK (01-Dec-11) *** Should this be based on mean of terminal spawning
  # biomass, not projected as SPC early code (now lost) fragments suggest?
  
# (5) Implement %spr for calcRefPoints.

# (6) Need to add clarity over spawning and exploitable biomass.

# (7) Note that tpl's seem to return Ut, not Ft.  Problem?  Theory on why the
#     Baranov equations are needed please - I haved a hole in my education.

# (8) What does time2fail do?  Actually, did, as I've disabled it.

# (9) Most of the guts for time varying Mt are in the code, but I have not
#     modified ageLenOMod or the solveInit stuff.

#------------------------------------------------------------------------------#
#--- mseRsystem.r: Operating Model & Management Procedure Functions      ----- #
#------------------------------------------------------------------------------#
#                                                                              #
# (c) mseR: Management Strategy Evaluation in R - Sablefish Version 3.0        #
#                                                                              #
#     Copyright 2008-2013 by A.R. Kronlund and S.P. Cox.                       #
#                                                                              #
#     This software comes with ABSOLUTELY NO WARRANTY, expressed or implied.   #
#     You have been provided with a copy of mseR for educational purposes.     #
#     You are requested not to redistribute this software without seeking      #
#     permission from the authors.                                             #
#                                                                              #
#     Of course, comments and suggestions for improvement greedily accepted.   #
#                                                                              #
# "Pretty good management will do."  Bill de la Mare, Dec. 19, 2006.           #
#                                                                              #
#          "Success consists of going from failure to failure without          #
#                  loss of enthusiasm."  Winston Churchill.                    #
#                                                                              #
# CONVENTIONS:                                                                 #
#                                                                              #
# Standard C++ variable naming conventions are applied.  The first letter of a #
# variable name is lower case.  Each concatenated word or abbreviation is      #
# capitalized. For example, Lower Bound Multiplier becomes lowerBoundMult.     #
#                                                                              #
#------------------------------------------------------------------------------#
#--                                                                          --#
#-- mseRsystem.r: An mseR module that conatins functions used to run         --#
#--               the operating model and sub-models used in the management  --#
#--               procedure within a feedback loop                           --#
#--                                                                          --#
#--                                                                          --#
#-- Authors: A.R. Kronlund (Pacific Biological Station, Nanaimo, B.C.)       --#
#--          S.P. Cox (Simon Fraser University, Burnaby, B.C.)               --#
#--                                                                          --#
#-- First Implementation: 01-Dec-09 based on mseSable.r by Cox and Kronlund. --#
#--                                                                          --#
#--  Revisions:                                                              --#
#--                                                                          --#
#------------------------------------------------------------------------------#
# References:                                                                  #
#                                                                              #
# Cox, S.P. and Kronlund, A.R. 2008. Practical stakeholder-driven              #
#   harvest policies for groundfish fisheries in British Columbia, Canada.     #
#   Fisheries Research xx: ppp                                                 #
#                                                                              #
# DFO, 2006. A Harvest Strategy Compliant with the Precautionary Approach.     #
#   DFO Can. Sci. Advis. Sec. Sci. Advis. Rep. 2006/023.                       #
#                                                                              #
# NOTE: There is a NEW simulation control file structure (simCtlFile.txt).     #
#       This can be read using the folloiwng read.table function:              #
#                                                                              #
# ctlPars <- read.table( file=parFile,as.is=TRUE,header=TRUE,comment.char="#", #
#                        quote="",sep=" " )                                    #
#                                                                              #
# The resulting dataframe can be converted to a list structure using the call: #
# result <- .createList( ctlPars )                                             #
#                                                                              #
# HARVEST CONTROL RULE NOTES                                                   #
#                                                                              #
# (1) Stock status                                                             #
#                                                                              #
# Base: This is the metric for stock status, i.e., relative to Bmsy, B0, or    #
#       some Data control point like a survey CPUE (Data not implemented).     #
#       Multipliers for lower and upper control points are set to control rule #
#       shape.  These DO NOT have to correspond with the biological reference  #
#       points that demarcate the DFO Cautious, Critical, and Healthy Zones.   #
#                                                                              #
# Source: This is how stock status control points are calculated - from the    #
#         operating model values ("true") or estimated by the stock assessment #
#         method.  Note that this applies only to population dynamics methods. #
#         Eventually the Data option will be added.                            #
#                                                                              #
# (2) Reference removal rate                                                   #
#                                                                              #
# Base: This is the metric for the reference removal rate. The F multiplier    #
#       adjusts the selected F to say, fish at 0.9Fmsy if performance stats    #
#       indicate that a limit fishing mortality (Fmsy) is exceeded with higher #
#       than desired probability (not impleted yet...).                        #
#                                                                              #
# Source: This is how reference removal rate control points are calculated.    #
#         Can be from the operating model (OM) or management procedure (MP).   #
#         Note that this applies only to population dyanmics models, as the    #
#         moving average or Kalman filter always use the operating model.      #
#                                                                              #
# FUNCTIONS INCLUDED IN FILE                                                   #
# (listed in order of occurence)                                               #
#                                                                              #
# ------------------ Simulation Set-up and Execution ------------------------- #
#                                                                              #
# runMSE          : Main function controlling for entire feedback loop         #
# .createMP        : Create all operating model & mgmt. procedure objects       #
# .mgmtProc        : Runs "nReps" replications of simulation model              #
# .initPop         : Initialise all objects, for period 1 - tMP                 #
# solveInitPop    : Initialise spawning biomass Bt to target initial depletion #
#                                                                              #
# ------------------------ Operating Model Functions ------------------------- #
# ARK - Update for Sablefish                                                                              #
# ageLenOpMod            : Age-structured operating model                           #
# .calcLenAge       : Calculates von B length-at-age                           #
# .calcLogistic     : Calculates logistic ogive for maturity and selectivity   #
# .calcPat          : Calculates observed proportion-at-age data               #
# .calcRdevs        : Calculates lag-1 autocorrelated recruitment deviations   #
# .calcWtAge        : Calculates weight-at-age                                 #
# .fillRanWalk      : Creates a lag-1 autocorrelated error sequence            #
#                                                                              #
# ---------------------- Management Procedure Functions ---------------------- #
#                                                                              #
# NOTE: callProcedureXXX functions are wrappers for input of data to the       #
#       assessment methods. The only reliable way to isolate code changes due  #
# to assessment methods is to contain them in functions.  New methods can then #
# be added without impacting working code.  It means some redundancy in coding #
# which is not ideal, but sorting out complicated if branches is a very much a #
# Special Kind of Hell that should be avoided.                                 #
#                                                                              #
# assessModMA       : Stock assessment method for Moving Average               #
# assessModKF       : Stock assessment method for Kalman Filter                #
# assessModSP       : Stock assessment method for Surplus Production model     #
# assessModCA       : Stock assessment method for Catch At Age model           #
# callProcedureKalman : Wrapper for Kalman filter based procedures             #
# callProcedureMovingAvg : Wrapper for moving average based procedures         #
# callProcedureSP   : Wrapper for production model based procedures            #
# callProcedureCAA  : Wrapper for catch-at-age model bsaed procedures          #
# .updatePop        : Complete one iteration of feedback system                #
#                                                                              #
# calcHCRpopDyn     : HCR wrapper to call specific HCR for pop dynamics models #
# calcHCRsmoother   : HCR wrapper to call specific HCR for smoothers - Kalman  #
# .calcHCRconstantC : Constant catch HCR - NOT IMPLEMENTED                     #
# .calcHCRconstantF : Constant F HCR (status-based rule)                       #
# .calcHCRvariableF : Variable F HCR (status-based rule)                       #
# .calcHCRdeclineRisk : Acceptable prob. future decline rule                   #
#                                                                              #
# Control point calculations.                                                  #
#                                                                              #
# .caaModGetCtlPtsAdmb : Calc CAA HCR control points using refPts.exe          #
# .caaModCalcCtlPts    : Calc CAA HCR control points using .caaModGetCltPtsAdmb#
#                        or via an Rscript to refPts.DLL                       #
# .caaModCtlPtsUseR    : Calc CAA HCR control points using only R              #
#                                                                              #
# .ddModGetCtlPtsAdmb  : Calc DD HCR control points using refPtsDD.exe         #
# .ddModGetCtlPts      : Calc DD HCR control points using refPtsDD.DLL         #
#                                                                              #
# ------------------------------ Helper Functions ---------------------------- #
#                                                                              #
# .calcTimes        : Calculate years where ages on, survey on, assessment on, #
#                     HCR control points updated.  Maybe dependencies among.   #
# .makeFileName     : Builds a filename to avoid collisions with snow calls    #
# .saveBlob         : Save the blob                                            #
# .saveSimResults   : Saves the simulations results and updates list (this     #
#                     needs to be restored from mseRguiSimFuns.r               #
#                                                                              #
#------------------------------------------------------------------------------#
#                                                                              #
# INSTRUCTIONS FOR ADDING NEW MODEL OPTIONS                                    #
#                                                                              #
# (1) Add a new operating model:                                               #
#                                                                              #
#     (a) Create a new operating model function (xxxOMod) that follows format  #
#         of the current ageLenOpMod() function.                                    #
#           similar format as the current ageLenOpMod() function.                   #
#     (b) Modify solveInitPop() function so that new xxxOMod() is called in    #
#         place of ageLenOpMod() when needed.                                       #
#     (c) Modify .updatePop() function so new xxxOMod() is called in step 6    #
#         below instead of ageLenOpMod() when needed.                               #
#                                                                              #
# (2) Add a new assessment method:                                             #
#                                                                              #
#     (a) Create a new assessModxxx() function that follows the format of      #
#         asessModMA(), assessModKF(), and assessModProd().                    #
#     (b) Modify step 3 of .updatePop() function to call new assessModxxx()    #
#         function when it is selected from guiSim window.                     #
#                                                                              #
# (3) Add a new harvest control rule:                                          #
#                                                                              #
#     (a) Create a new calcHCRxxx() function that follows a similar format     #
#         as the current calcHCRxxx() functions and add to calcHCR.            #
#     (b) Modify steps 4 and 5 of .updatePop() function to call new            #
#         calcHCRxxx() function when selected by obj$mp$hcr$hcrType.           #
#                                                                              #
# Additional:                                                                  #
#                                                                              #
# (a) Modify mseRguiSimWin.txt to accommodate  new options in guiSim GUI.      #
# (b) Add new simulation control parameters to simCtlFile.txt.                 #
#                                                                              #
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#--                 Simulation Setup and execution functions                 --#
#------------------------------------------------------------------------------#

# runMSE
# Purpose:        Main function controlling for entire feedback
#                 loop simulation process. runMSE can be called
#                 from (1) guiSim(), or (2) from the console.
# Parameters:     none. input from guiSim via *.par file
# Returns:        none. saves blob to *.RData workspace
# Source:         S.P. Cox
runMSE <- function(  saveBlob=TRUE )
{

  require( tools )
  
  # No validity checking of .CTLFILE variables is done if runMSE is called
  # directly from the console.  In contrast, guiSim() calls via RUN button will
  # enforce checking of valid parmete3rs. prior to calling runMSE.

  # Read and load the MP control parameters into a list.
  if ( !exists( ".CTLFILE" ) )
  {
    .CTLFILE <- file.path( getwd(),"simCtlFile.txt" )
    assign( ".CTLFILE",.CTLFILE,pos=1 )
    cat( "\nMSG (runMSE) No simulation control file set, using ",.CTLFILE,"\n" )
  }
  
  ctlPars <- .readParFile( .CTLFILE )
  ctlList <- .createList( ctlPars )
  cat( "\nMSG (runMSE) Parameter list created.\n" )
 
  # Start timer.
  t1 <- proc.time()[3]
  
  opMod <- ctlList$opMod
  
  # Calculate life history schedules and OM equilibrium reference points.  
  cat( "\nMSG (.createMP) Calculating reference points...\n ")
  tmp <- calcRefPoints( as.ref(opMod) )
  refPts <- deref( tmp )
  cat( "\nMSG (.mgmtProc) Elapsed time in calcRefPoints =",proc.time()[3]-t1,"\n" )

  # Create the management procedure object.
  simObj <- .createMP( ctlList, refPts )

  # Run the management procedure.
  blob <- .mgmtProc( as.ref( simObj ) )

  # Save the blob particulars.
  blob$ctlPars   <- ctlPars
  blob$ctlList   <- ctlList
  blob$ctlList$refPts <- refPts
  blob$sim       <- TRUE
  stamp          <- .getStamp()
  blob$stamp     <- stamp
  blobFileName   <- paste( "sim",blob$stamp,".Rdata", sep="" )
  #blob$refPtList <- calcRefPoints( ctlList$opMod, list( FALL=TRUE, x=40 ) )
  blob$refPtList <- refPts

  # Save the blob to the working environment.
  assign( "blob",blob,pos=1 )

  # Make the simulation folder.
  simFolder <- paste( "sim", stamp, sep="" )
  dir.create( file.path( .PRJFLD, simFolder ) )
  cat( "\nMSG (runMSE) Created simulation folder ",simFolder,"in",.PRJFLD,"\n" )

  # Save the simXXX.Rdata version of the blob to the simulation folder.
  blobFilePath <- file.path( .PRJFLD,simFolder,blobFileName )

  if ( saveBlob )
    .saveBlob( blobFilePath, blob  )

  # Copy the simulation control file into the folder.
  file.copy( .CTLFILE, file.path( .PRJFLD, simFolder, basename(.CTLFILE) ) )

  # Copy the TPL file into the folder.
  if ( ctlList$mp$assess$methodId==.CAAMOD )
  {
    if( .ISCAMFLAG ) tplFile <- "iscam_ageM.tpl"
    else tplFile <- "assessca.tpl"

    file.copy( file.path( getwd(), tplFile ),
               file.path( .PRJFLD, simFolder, tplFile ) )
    if(!.ISCAMFLAG)
      file.copy( file.path( getwd(), "refptsca.tpl" ),
                 file.path( .PRJFLD, simFolder, "refptsca.tpl" ) )
  }

  if ( ctlList$mp$assess$methodId==.DDMOD )
  {
    file.copy( file.path( getwd(), "assessdd.tpl" ),
               file.path( .PRJFLD, simFolder, "assessdd.tpl" ) )

    file.copy( file.path( getwd(), "refptsdd.tpl" ),
               file.path( .PRJFLD, simFolder, "refptsdd.tpl" ) )
  }

  if ( ctlList$mp$assess$methodId==.PMOD )
    file.copy( file.path( getwd(), "assessSP.tpl" ),
               file.path( .PRJFLD, simFolder, "assessSP.tpl" ) )

  #if ( ctlList$opMod$historyType=="omFile" )
  #  file.copy( file.path( .PRJFLD, .DEFHSTFLD, .DEFHSTFILE ),
  #             file.path( .PRJFLD, simFolder,  .DEFHSTFILE ) )
#browser()
  # Write the *.info file to the simulation folder.
  infoFilePath <- file.path( .PRJFLD, simFolder, paste( "sim",stamp,".info",sep="" ) )
  .writeSimInfo( info=list( simTime=stamp,
    scenarioLabel=ctlList$gui$scenarioLabel,
    mpLabel=ctlList$gui$mpLabel,
    tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
    rank=1, group=1 ), simFolder, infoFilePath )

  # Get the tracking data for the project prior to this simulation.
  trackData <- .getTrackingData( projDir=.PRJFLD )

  # Get the number of simulations.
  nSims <- .getNumSims(trackData )
  cat( "\nMSG (runMSE) Total number of simulations in project = ",nSims,"\n" )

  # Write the revised INFO file to the simulation folder.
  .writeSimInfo( info=list( simTime=stamp,
    scenarioLabel=ctlList$gui$scenarioLabel,
    mpLabel=ctlList$gui$mpLabel,
    tMP=ctlList$opMod$tMP, nT=ctlList$opMod$nT, nReps=ctlList$gui$nReps,
    rank=nSims, group=1 ), simFolder, infoFilePath )

  return( invisible() )
}     # END function runMSE

#------------------------------------------------------------------------------#
#-- Age-structured operating model helper functions.                           #
#------------------------------------------------------------------------------#

# calcXat         generate stochastic age-proportions
# Purpose:        given a vector of true age-proportions, generate
#                 true sampled proportions, then observed following ageing error 
# Parameters:     uAge=vector of age props; tauIndexg=sampling error sd; eps=std norm errors;
#                 misQ=age misclassication matrix SPC: NOT SHOWN 08 Sept 09
# Returns:        result=observed age proportions
# Source:         S.P. Cox (ref: Schnute and Richards 1995)
.calcXat <- function( uAge,tauAge=0,eps )
{
  # Equation D.11, S.9, S.10: Proportions at age.

  # Number of age classes including plus group.
  A <- length( uAge )

  # Temporary vector for age-proportions
  xa <- vector( mode="numeric",length=A )

  # Observed proprortions at age.
  Xa <- vector( mode="numeric",length=A )

  # Deterministic proportions
  xa <- uAge
  xa[ xa==0 ] <- 1.e-8
  # Accounting for constraint 2.11 (proportions sum to 1)
  # Equation S.9, S.10.
  xa <- log(xa) + tauAge*eps - mean( log(xa)+tauAge*eps )
  Xa <- exp(xa) / sum( exp(xa) )
  
  #----------------------------------------------------------------------------#
  # ageing error to be added later; this version only includes sampling   #
  #      error in the age composition samples.                                 #
  #----------------------------------------------------------------------------#

  return( Xa )
}     # END function .calcXat


# .calcRdevs       
# Purpose:        calculates lag-1 autocorrelated recruitment deviations
# Parameters:     gammaR=lag-1 autocorrel.; sigmaR=recruitment std error,
#                 deltat=vector of std normals
# Returns:        vector of length nT with log-normal recruitment multipliers
# Source:         S.P. Cox
.calcRdevs <- function( gammaR,sigmaR,deltat )
{
  # Generate autocorrelated recruitment deviation vector.
  omegat <- vector( mode="numeric",length=length(deltat) )

  # Equation O2.12 for t=1, less the multiplcation by deltat.
  sigUnc  <- sigmaR / sqrt( 1.0 - gammaR*gammaR )
  
  # ARK (27-Nov-10) Used below for Equation 02.13.
  sig2Unc <- sigmaR*sigmaR / ( 1.0 - gammaR*gammaR )

  # Equation 02.12 for t=1.
  omegat[1] <- sigUnc * deltat[1]
  
  # Equation 02.12 for t=2,nT
  for ( t in 2:length(deltat) )
    omegat[t] <- omegat[t-1] * gammaR + sigmaR*deltat[t]

  # Equation 02.13.  Note these are now multpliers > 0, bias corrected.
  omegat <- exp( omegat - sig2Unc/2.0)
    
  tmp <- exp(sigmaR*deltat - sigmaR*sigmaR/2.)
  omegat
}     # END function .calcRdevs


# .fillRanWalk       
# Purpose:        creates a lag-1 autocorrelated error sequence
# Parameters:     gamma=lag-1 autocorrel.; sigma=std error,
#                 deltat=vector of std normals
# Returns:        nT-vector of log-normal error multipliers
# Usage:          tmp[t] = state[t]*error[t]
# Source:         S.P. Cox
.fillRanWalk <- function( gamma=0.5,sigma=0,deltat,init=NULL )
{
  # Generate autocorrelated recruitment deviation vector.
  omegat <- vector( mode="numeric",length=length(deltat) )

  # Uncorrelated error std dev and variance
  sigUnc  <- sigma / sqrt( 1.0 - gamma*gamma )
  sig2Unc <- sigma*sigma / ( 1.0 - gamma*gamma )
  
  # Initialize first value in sequence
  if( is.null(init) ) # if not supplied, scale 1st
    omegat[1] <- sigUnc * deltat[1]
  else            # use init
    omegat[1] <- init
  
  # The random walk...
  for ( t in 2:length(deltat) )
    omegat[t] <- omegat[t-1] * gamma + sigma*deltat[t]
  
  # note these are now multpliers > 0
  omegat <- exp( omegat - sig2Unc/2.0)
  return(omegat)
}     # END function .fillRanWalk

#------------------------------------------------------------------------------#
#-- Age-Structured Operating Model                                           --#
#------------------------------------------------------------------------------#

# ageLenOpMod     age-/length-based operating model for multiple fisheries      
# Purpose:        advance population age-/length-structure one time step,
#                 generate observations and Fs for one time step
# Parameters:     obj=complete simulation object up to t-1; t=current time
# Returns:        result=complete simulation object up to t
# Source:         S.P. Cox
ageLenOpMod <- function( objRef, t )
{
  # findOM -- string to use in finding this part of the code using search
  t1 <- proc.time()
  
  obj <- deref( objRef )
  
  ctlList <- obj$ctlList
  
  tmpTimes <- .calcTimes( ctlList )
  
  # Extract constants.
  nT     <- as.numeric( ctlList$opMod$nT )
  A      <- as.numeric( ctlList$opMod$nAges )
  minAge <- as.numeric( ctlList$mp$data$minAge )
  tMP    <- as.numeric( ctlList$opMod$tMP )
  nGear  <- as.numeric( ctlList$opMod$nGear )
  nGrps  <- as.numeric( ctlList$opMod$nGrps )

  # Set parameter values.
  B0         <- ctlList$opMod$B0
  rSteepness <- ctlList$opMod$rSteepness
  M          <- ctlList$opMod$M        # Average M without trend or pulse.
  Mt         <- obj$om$Mt              # M may be time-varying.  

  # Length-at-age-/group-
  nGrps <- ctlList$opMod$nGrps
  Lal   <- obj$refPtList$Lal
  Wal   <- obj$refPtList$Wal
  Wal[1] <- 0

  if( !is.null(ctlList$opMod$obsWtAge) & t <= tMP )
  {
    Wal[2:A] <- obj$om$Wta[t,2:A]
  }

  
  
  
  # Maturity ogive parameters
  A50   <- ctlList$opMod$aMat50
  A95   <- ctlList$opMod$aMat95
  Ma    <- obj$refPtList$Ma  
  Ma[1] <- 0
  
  # Retention probability for age-/length-/gear-  
  Palg <- obj$refPtList$Palg
  # Selectivity ogives for age-/length-/gear- 
  Salg <- obj$refPtList$Salg

  Salg[1,,] <- 0
  
  # avoidance future option: futureOption==1 means that
  # gear and fishing adjusted to avoid small (sub-legal < 55cm) fish
  if( ctlList$opMod$futureOption==1 & t > tMP )
  {
    for( g in 1:3 )
    {
      tmpS <- Salg[ ,, g]
      tmpS[ Lal < ctlList$opMod$sizeLim[g] ] <- 0.
      Salg[,,g] <- tmpS       
    }
  }
  
  # retention future option: futureOption==2 means that
  # retention probabilities are made knife-edge to simulate
  # a no high-grading (retention) fishery 
  if ( ctlList$opMod$futureOption==2 & t > tMP )
  {
    L95Dg <- vector("numeric", length=nGear)
    L50Dg <- vector("numeric", length=nGear)
    for( g in 1:nGear )
    {
        L95Dg[g] <- max(ctlList$opMod$L95Dg[g],
                    ctlList$opMod$sizeLimFuture[g])
        L50Dg[g] <- ctlList$opMod$L50Dg[g]
    }
    palgPars <- list( sizeLim  = ctlList$opMod$sizeLimFuture,
                      L50Dg    = L50Dg,
                      L95Dg    = L95Dg,
                      nGrps    = nGrps,
                      nGear    = nGear
                  )
    tmpP <- .calcPalg(palgPars,A=A,Lal=Lal)
    Palg <- tmpP
  }

  # futureOption==3 means full retention (simulated as retaining
  # all fish as in the Stratified Random Survey)
  if( ctlList$opMod$futureOption==3 & t > tMP )
  {
    for (a in 1:A)
    {
      for (l in 1:nGrps) Palg[a,l,1:3] <- Palg[a,l,5]
    } 
  }

  
  sigmaR    <- ctlList$opMod$sigmaR     # std error in log-recruitment
  gammaR    <- ctlList$opMod$gammaR     # autocorrelation in log-recruitment
  
  tauIndexg <- ctlList$opMod$tauIndexg  # std error in log-index
  tauAgeg   <- ctlList$opMod$tauAgeg
  
  useIndex  <- tmpTimes$useIndex
  k2Index   <- tmpTimes$k2Index
  useAges   <- tmpTimes$useAges
  
  # Extract derived parameters.
  rec.a     <- obj$refPtList$rec.a     # recruitment slope
  rec.b     <- obj$refPtList$rec.b     # recruitment dd parameter
  R0        <- obj$refPtList$R0        # unfished recruitment

  # if( t==1 )
  # {
      tmp <- read.rep(ctlList$opMod$repFileName)
  #   numAgeYr1_m <- tmp$Nta_m[ctlList$opMod$firstRepYear,]
  #   numAgeYr1_f <- tmp$Nta_f[ctlList$opMod$firstRepYear,]
      avgR        <- tmp$rbar
  # 

  recOption   <- ctlList$opMod$recruitmentOption

  if( (t > 1) & recOption=="avgR" )
    avgR <- obj$om$avgR
      
  # Vector of recruitments, same as Nat[1,t].
  Rt <- obj$om$Rt

  # Extract objects needed to hold model results.
  
  # State variables: Numbers at age, time and biomass at age, time.
  Nalt  <- obj$om$Nalt    # number-at-age-/length
  Balt  <- obj$om$Balt    # biomass-at-age-/length
  Bt    <- obj$om$Bt      # spawning biomass
  Nt    <- obj$om$Nt      # spawning number
  Btot  <- obj$om$Btot    # total biomass
  Ntot  <- obj$om$Ntot    # total number
  Mt    <- obj$om$Mt      # natural mortality at time t  

  # Controls: catch biomass, dead-discard biomass, 
  
  # Fishing mortality, and total mortality
  Catg  <- obj$om$Catg    # array of gear-specific catchBiomass-at-age
  uCatg <- obj$om$uCatg   # age-proportions in landed catch
  Ctg   <- obj$om$Ctg     # matrix of gear-specific total landed catch biomass
  Ct    <- obj$om$Ct      # vector of total landed catch biomass
  Dt    <- obj$om$Dt      # vector of total discard biomass.
  Datg  <- obj$om$Datg    # array of dead discard biomass-at-age
  Dtg   <- obj$om$Dtg     # matrix of gear-specific total discarded biomass
  Ftg   <- obj$om$Ftg     # matrix of gear-specific fishing mortality
  Zalt  <- obj$om$Zalt    # array of total mortality by age-/length-group

    
  fg    <- ctlList$opMod$fg       # Relative Fs by gear 
  dg    <- ctlList$opMod$dg       # discard mortality rates by gear
  
  # Gear-specific catchability, cpue power, and deterministic biomass index.
  qg     <- ctlList$opMod$qg      # vector of catchability by gear
  powerg <- ctlList$opMod$powerg  # vector of CPUE-power parameters by gear

  # Extract standard normal deviates for errors.
  deltat     <- obj$om$errors$deltat     # std normal error in log-recruitment
  omegat     <- obj$om$errors$omegat     # autocorrelated recruitment errors
  epsilontg  <- obj$om$errors$epsilontg  # std normal error in log-indices
  epsilongat <- obj$om$errors$epsilongat # std normal error in age-proportions

  # Initialise population if t=1, else update variables from last time step.
  if ( t==1 )
  {
    # Recruitment, number-at-age, biomass-at-age
    Rt[t]     <- obj$ref$R0
    #Nalt[,,t] <- outer( numAgeYr1, rep( obj$refPtList$piOne, nGrps ) )
    numAgeYr1 <- tmp$N[1,]
    numAgeYr1 <- c(exp(M[nGrps])*tmp$N[2,1],numAgeYr1)
    Nalt[,1:nGrps,t] <- numAgeYr1

    Balt[,,t] <- Nalt[,,t]*Wal
  
    # Total biomass and number
    Btot[t] <- sum( Balt[,,t] )
    Ntot[t] <- sum( Nalt[,,t] )
  } # end "if(t==1)"
  else  # t>1
  {
    # Spawning biomass and number.
    Bt[t-1] <- sum(Balt[,nGrps,t-1]*Ma*exp(-Zalt[,1:nGrps,t-1]))
    Nt[t-1] <- sum(Nalt[,nGrps,t-1]*Ma*exp(-Zalt[,1:nGrps,t-1]))
    # Calculate age-1 recruitment, Beverton-Holt stock-recruitment.
    #if( t>44 )
    if( recOption=="Beverton-Holt" )
    {
      tmpR      <- rec.a*Bt[t-1]/( 1.0 + rec.b*Bt[t-1] )
      if( t < tMP ) omegat[t] <- exp(Mt[t]) * tmp$N[t+1,1] / tmpR
    }
    else
        tmpR <- avgR

    Rt[t] <- omegat[t]*tmpR #(S-R gives females only)

    # if ( t == tMP) browser()
    
    # Update numbers-at-age.
    # Nalt[1,,t] <- outer( Rt[t], rep( obj$pars$piOne, nGrps ) )
    Nalt[1,,t] <- Rt[t]*rep( obj$refPtList$piOne, nGrps )

    # age-2 to age-(A-1)
    for( a in 2:(A-1) ){
      # loop over growth-groups
      for( l in 1:nGrps ){
        Nalt[a,l,t] <- Nalt[a-1,l,t-1]*exp( -Zalt[a-1,l,t-1] )
      }
    }
    for( l in 1:nGrps )
      Nalt[A,l,t] <- Nalt[A-1,l,t-1]*exp(-Zalt[A-1,l,t-1]) + Nalt[A,l,t-1]*exp(-Zalt[A,l,t-1])

    Balt[,,t] <- Nalt[,,t]*Wal


    # Total biomass and number
    Btot[t] <- sum( Balt[,,t] )
    Ntot[t] <- sum( Nalt[,,t] )
  }  # end t>1 ifelse
  
  # Calc fishing mortality by age-/growth-group
  Falg <- array( data=NA, dim=c(A,nGrps,nGear) )
        
  # Solve catch equation for this year's Ftg based on tac allocated among gears
  solveOK <- FALSE
  nTimes  <- 0
  Ftg[t,] <- rep( 0,length(Ftg[t,]) ) 
  .PAUSE  <<- FALSE
  # if (t >= tMP) browser()
  if( t < tMP )
  {
    Ftg[t,] <- t(tmp$ft)[t,]
  }

  if ( sum(Ctg[t,1:3]) > 0. & t >= tMP ) # don't bother if fishery catch=0
  {
    gT <<- t
    
    initF <- c(rep(0.223,3),0,0)   # SJ this is hardcoded in for a given # of fisheries/surveys. Too path specific!!!!
    idx0  <- which(Ctg[t,] < 0)
    Ctg[t,idx0] <- 0.
    baranovTime <<- t
    #if(t>17) browser()

    solveF <- .solveBaranovNoDisc( B=Balt[,,t], S=Salg, F=initF, M=M,
                                   C=Ctg[t,], lam=ctlList$opMod$baranovSteps )
    
    solveF[ solveF > 10 ] <- 10
    if( any( solveF < 0 ) )
    {
        cat("Warning: negative Ftg... t = ", t, "\n")
        cat( solveF, "\n" )
        solveF[ solveF < 0. ] <- 0.
    }
    Ftg[t,] <- solveF
  }

  # Compute total mortality by summing M and Fg over gear types
  Zalt[,,t] <- matrix(Mt[t], ncol = nGrps, nrow = A, byrow = TRUE)
  for( g in 1:(nGear-2) )
    Zalt[,,t] <- Zalt[,,t] + Salg[,,g]*Ftg[t,g]


  # Spawning biomass and number - this recalculates from earlier, apply mortality as
  # spawning as at the end of the year
  Bt[t]   <- sum(Balt[,1:nGrps,t]*Ma*exp(-Zalt[,1:nGrps,t]))
  Nt[t]   <- sum(Nalt[,1:nGrps,t]*Ma*exp(-Zalt[,1:nGrps,t]))
    

  # Catch- and dead-discards-at-age 
  legalC <- 0.; legalD <- 0.; sublegalD <- 0.
  for( g in 1:(nGear-2) )
  {
    Cal <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*Ftg[t,g]/Zalt[,,t]
    Dal <- 0
    # Dal <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*Ftg[t,g]/Zalt[,,t]
    #Dal <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*Ftg[t,g]*Palg[,,g]/Zalt[,,t]            
    
    # catch-age/year/gear
    Catg[,t,g]  <- Cal
    Datg[,t,g]  <- Dal
    
    # catch-year/gear
    Ctg[t,g] <- sum( Catg[,t,g] )
    Dtg[t,g] <- sum( Datg[,t,g] )
    
    # accumulate legal catch, discards, and sublegal total discards
    legalC    <- legalC + sum( Cal*obj$refPtList$Legal )
    legalD    <- legalD + sum( Dal*obj$refPtList$Legal )
    sublegalD <- sublegalD + sum( Dal*(1.-obj$refPtList$Legal) )
  }

  # Total landings and harvest rates.
  legalB     <- sum( Balt[,,t]*obj$refPtList$Legal )
  sublegalB  <- sum( Balt[,,t]*(1.-obj$refPtList$Legal) )
  Ct[t]      <- sum( Ctg[t,1:3] )
  Dt[t]      <- sum( Dtg[t,1:3] )
  legalHR    <- (legalC + legalD)/Bt[t]
  if( sublegalB == 0 ) sublegalHR <- sublegalD*sublegalB
  else sublegalHR <- sublegalD/sublegalB


  # Gear-specific cpue, catch, discards, and sample age-proportions
  for( i in 1:length(useIndex) )
  {
    g <- useIndex[i]
    # if( t > (tMP-2) )
    # {
    #     obj$om$Itg[t,g]      <- qg[g]*sum( Salg[,,g]*Balt[,,t] )
    #     # obj$om$Itg[t,g]      <- 1.0*sum( Salg[,,g]*Balt[,,t] )
    #     obj$mp$data$Itg[t,g] <- obj$om$Itg[t,g]*exp( tauIndexg[i]*epsilontg[t,g] - tauIndexg[i]*tauIndexg[i]/2. )        
    # }
    # Changed tMP to tMP-1 to impute survey values for tMP.
    if( !.INDEXSERIESINPUT | (t >= (tMP)) ) 
    {
      
      # Only impute a tMP point for indices active in the future.
      if ( k2Index[i] > 0 )
      {
         obj$om$Itg[t,g] <- qg[g]*Bt[t]^powerg[g]
         # browser()
      }
      if( attr(obj$mp$data$Itg,"indexOn")[t,g]==TRUE )
        obj$mp$data$Itg[t,g] <- obj$om$Itg[t,g]*exp( tauIndexg[i]*epsilontg[t,g] - tauIndexg[i]*tauIndexg[i]/2. )        
    }
  }

  for( i in 1:length(useAges) )
  {
    g <- useAges[i]
    if( !.AGESERIESINPUT | (t >= tMP) )
    {
      # if(t >= tMP - 1) browser()
      if( Ctg[t,g] > 0. ) 
      {
        uCatg[minAge:A,t,g] <- Catg[minAge:A,t,g]/Ctg[t,g] # true proportions in catch, A
        # sampled age-proportions in landed catch
        obj$mp$data$Xatg[minAge:A,t,g]   <- .calcXat( uAge=uCatg[minAge:A,t,g],tauAge=tauAgeg[i],eps=epsilongat[g,minAge:A,t] )
        # add "misQ" to argument list to include age misclassification matrix
      }      
    }
  }

  # browser()
  
  # Update all objects in obj for this time step.
  obj$om$Nalt  <- Nalt    # numbers-at-age
  obj$om$Balt  <- Balt    # biomass-at-age
  obj$om$Zalt  <- Zalt    # total mortality
  obj$om$Bt    <- Bt      # spawning biomass
  obj$om$Nt    <- Nt      # spawning population
  obj$om$Btot  <- Btot    # total biomass
  obj$om$Ntot  <- Ntot    # total abundance
  obj$om$Rt    <- Rt      # recruitment
  
  if( t==1 )
    obj$om$avgR  <- avgR      # recruitment

  obj$om$Ct    <- Ct      # landed catch in biomass
  obj$om$Dt    <- Dt      # discards in biomass
  obj$om$Ctg   <- Ctg     # landed catch in biomass by gear
  obj$om$Catg  <- Catg    # landed catch by age/gear
  obj$om$uCatg <- uCatg   # age-proportions in landed catch
  obj$om$Dtg   <- Dtg     # dead discard in biomass by gear
  obj$om$Datg  <- Datg    # dead discards by age/gear
  obj$om$Ftg   <- Ftg     # realized fishing mortality rate by gear
  
  obj$om$legalHR[t]        <- legalHR      # legal harvest rate
  obj$om$sublegalHR[t]     <- sublegalHR   # sublegal harvest rate
  obj$om$legalB[t]         <- legalB       # legal biomass
  obj$om$sublegalB[t]      <- sublegalB    # sublegal biomass
  obj$om$legalC[t]         <- legalC       # legal catch
  obj$om$legalD[t]         <- legalD       # legal discards
  obj$om$sublegalD[t]      <- sublegalD    # sublegal discards
  
  # obs/process errors
  obj$om$errors$omegat     <- omegat       # recruitment process error
  obj$om$errors$epsilontg  <- epsilontg    # index obs error
  obj$om$errors$epsilongat <- epsilongat   # age prop sampling error

  return( as.ref(obj) )
}     # END function ageLenOpMod

#------------------------------------------------------------------------------#
#-- MP Assessment Method Helper functions                                    --#
#------------------------------------------------------------------------------#

# .assessMod       
# Purpose:        generates the "stock assessment", here an n-point
#                 moving average of a reference survey
# Parameters:     obj=list containing It=survey biomasses;
#                 avgPoints=number of points to average
# Returns:        average biomass over the last n surveys
# Source:         S.P. Cox
.assessModMA <- function( obj )
{
  # Compute the moving average of the survey
  # using the most recent avgPoints surveys
  It        <- obj$It
  q         <- obj$q # this needs to be in mp!!
  avgPoints <- obj$avgPoints

  # Remove missing values.
  tmp     <- It[ !is.na(It) ]
  nPoints <- min( length(tmp),avgPoints )
  tVec    <- c( (length(tmp) - nPoints + 1 ):length(tmp) )

  # Moving average of survey values.
  movinAvg <- exp( mean( log(tmp[tVec]/q) ) )
  assessment         <- list()
  assessment$biomass <- movinAvg
  return( assessment )
}    # END function .assessModMA

# .assessModMA_Herring      
# Purpose:        generates the "stock assessment", here a simple
#                 data based rule assuming q = 1
# Parameters:     obj=list containing It=survey biomasses;
#                 avgPoints=number of points to average
# Returns:        average biomass over the last n surveys
# Source:         S.P. Cox
.assessModMA_Herring <- function( obj )
{
  # Compute the moving average of the survey
  # using the most recent avgPoints surveys
  It        <- obj$Itg

  assessment         <- list()
  assessment$biomass <- It[nrow(It),2]

  return( assessment )
}    # END function .assessModMA


callProcedureSP <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.
  # ARK (08-Sep-13) This code not currently used.  Necessary steps are in
  # updatePop.

  spObj   <- list()                        # List to pass to assessModProd
  spObj$t <- t                             # Current time step.
  nT      <- obj$opMod$nT                  # Number of years in simulation.
  tMP     <- obj$opMod$tMP                 # Year MP first applied, tMP.
  
  # (1) Data.
  
  # Catch summed over gear types in Ct from Ctg.  Discards are Dt and Dtg.
  Ct       <- obj$om$Ct[1:(t-1)]           # Extract catch history.
  spObj$Ct <- Ct

  # Extract survey index attributes before they are lost when It extracted.
  surveyOn       <- attributes( obj$mp$data$It )$surveyOn

  #It             <- obj$mp$data$It[ c(1:(t-1)) ]  # Extract stock index values.
  #spObj$It       <- It                            # Stock index
  
  Itg               <- matrix( obj$mp$data$Itg )
  Itg[ is.na(Itg) ] <- -1
  dim(Itg)          <- dim( obj$mp$data$Itg )
  spObj$Itg         <- Itg

  spObj$t1Survey <- obj$mp$data$t1Survey          # First survey year.
  spObj$tSurvey  <- t - 1                         # Last survey year.

  # (2) Assessment Method

  spObj$pmMsy   <- obj$mp$assess$spPmMsy
  spObj$psdMsy  <- obj$mp$assess$spPsdMsy
  spObj$pmFmsy  <- obj$mp$assess$spPmFmsy
  spObj$psdFmsy <- obj$mp$assess$spPsdFmsy
  spObj$Msy     <- obj$mp$assess$spMsy
  spObj$Fmsy    <- obj$mp$assess$spFmsy
  spObj$lnOmega <- rep( 0,(t-3) )
  spObj$rho     <- obj$mp$assess$spRhoEiv

  spObj$indexType <- as.numeric( as.logical(obj$mp$assess$spSurveyRel) )
  
  # (3) Harvest control rule parameters.

  hcrType       <- obj$mp$hcr$hcrType
  spObj$hcrType <- hcrType

  if ( hcrType == "constantF" )
    spObj$ruleType <-1 
  if ( hcrType == "variableF" )
    spObj$ruleType <- 2
  if ( hcrType == "declineRisk" )
    spObj$ruleType <- 3
   if ( hcrType == "hcrUser" )
    spObj$ruleType <- 4

  spObj$nProjYears <- 1
  if ( hcrType == "declineRisk" )
    spObj$nProjYears <- obj$mp$hcr$nProjYears

  spObj$trendYears   <- obj$mp$hcr$trendYears
  spObj$rSeed        <- obj$opMod$rSeed 

  # Add in parameters to control MLE or MCMC in ADMB.
  spObj$nMCMC         <- obj$mp$hcr$nMCMC
  spObj$nThin         <- obj$mp$hcr$nThin
  spObj$useMLE        <- obj$mp$hcr$useMLE

  # Quota levels for projection.
  spObj$Qlevel <- seq( obj$mp$hcr$lowerQuota, obj$mp$hcr$upperQuota,
                       obj$mp$hcr$binQuota )

  spObj$idxCtlPts <- obj$mp$hcr$idxCtlPts      # From .calcTimes function.
  
  # -- Call the Surplus production model.
  
  result <- assessModSP( spObj )

  result
}     # END function callAssessSP

# .assessModSP
# Purpose:        Runs a production model to estimate biomass
# Parameters:     obj=list of assessment info from updatePop
# Returns:        current biomass estimate as well as estimated parameters
#                 from surplus production model fit
# Notes:          Production model is run from an ADMB executable(assessSP.exe)
# Source:
.assessModSP <- function( spObj )
{
  exeName <- "./assesssp"

  datFile  <- file.path( getwd(), paste( exeName,".dat", sep="" ) )
  exeFile  <- file.path( getwd(), exeName )
  mcmcFile <- file.path( getwd(), "mcout.dat" )
  pinFile  <- file.path( getwd(), paste( exeName,".pin", sep="" ) )  
  repFile  <- file.path( getwd(), paste( exeName,".rep", sep="" ) )  
  stdFile  <- file.path( getwd(), paste( exeName,".std", sep="" ) )  
  
  t <- spObj$t

  # (1) Extract and write the model parameters to pin file.

  cat( file=pinFile, "## ", exeName,": Surplus production model parameters.\n" )
  cat( file=pinFile, "## Written:",     date(),                 "\n", append=T )
  cat( file=pinFile, "# lnMsy\n",       log(spObj$spMsy),       "\n", append=T )
  cat( file=pinFile, "# lnFmsy\n",      log(spObj$spFmsy),      "\n", append=T )
  cat( file=pinFile, "# pmMsy\n",       spObj$spPmMsy,          "\n", append=T )
  cat( file=pinFile, "# psdMsy\n",      spObj$spPsdMsy,         "\n", append=T )
  cat( file=pinFile, "# pmFmsy\n",      spObj$spPmFmsy,         "\n", append=T )
  cat( file=pinFile, "# psdFmsy\n",     spObj$spPsdFmsy,        "\n", append=T )
  cat( file=pinFile, "# rho\n",         spObj$spRhoEiv,         "\n", append=T )  
  cat( file=pinFile, "# lnOmega\n",     spObj$lnOmega,          "\n", append=T )
  cat( file=pinFile, "# log_init_B_mult\n", log(spObj$sp_initB), "\n", append=T )

  # (2) Write the data to the dat file.
  
  cat( file=datFile, "## ",exeName,": Surplus production model data.\n" )
  cat( file=datFile, "## Written:", date(), "\n", append=T )
  cat( file=datFile, "# nT nIndices \n",                              append=T )
  cat( file=datFile, spObj$nT, spObj$nIndices, "\n", append=T )
  cat( file=datFile, "# nProjYears \n", append=T )
  cat( file=datFile, spObj$nProjYears, "\n", append=T )
  cat( file=datFile, "# trendYears \n", append=T )
  cat( file=datFile, spObj$trendYears, "\n", append=T )
  cat( file=datFile, "# rSeed \n", append=T )
  cat( file=datFile, spObj$rSeed, "\n", append=T )
  cat( file=datFile, "# ruleType \n", append=T )
  cat( file=datFile, spObj$ruleType, "\n", append=T )
  cat( file=datFile, "# nQlevels \n", append=T )
  cat( file=datFile, length( spObj$Qlevel ), "\n", append=T )
  cat( file=datFile, "# Qlevel \n", append=T )
  cat( file=datFile, spObj$Qlevel, "\n", append=T )

  cat( file=datFile, 1, "\n", append=T )
  cat( file=datFile, 2, "\n", append=T )
  cat( file=datFile, 3, "\n", append=T )
  cat( file=datFile, -1, "\n", append=T )

  cat( file=datFile, "# Catch biomass (katch, tonnes)\n",             append=T )
  cat( file=datFile, spObj$Ct,                                  "\n", append=T )
  cat( file=datFile, "# Index series indexType: 0=abs, 1=rel)\n",     append=T )
  cat( file=datFile, spObj$indexType,                           "\n", append=T )
  cat( file=datFile, "# First year for each index (fYear)\n",         append=T )
  cat( file=datFile, spObj$indexFirstYear,                     "\n", append=T )
  cat( file=datFile, "# Last year for each index (lYear)\n",          append=T )
  cat( file=datFile, spObj$indexLastYear,                      "\n", append=T )
  cat( file=datFile, "# Index series (possibly a matrix)\n",          append=T )
  #cat( file=outFile, It,                                        "\n", append=T )
  for( i in 1:ncol(spObj$Itg) ) 
    cat( file=datFile, t(spObj$Itg[,i]),"\n",                   "\n", append=T )  
  
  # (3) Call the surplus production model (assessSP.exe) ADMB program

  starttime <- Sys.time()                  # Set the start time for minimization.

  # Concept here is to always run MCMC but for constantF or variableF where the
  # risk adjusted Fmsy option is FALSE we run nMCMC=1 to get the MPDs as the
  # first row.  That way the same code can be used whether MPD or MCMC.
  
  # ARK (12-Dec-11) Cannot have NULL assignment to mcOut.
  mcOut      <- NA

  # Changed by ARK (01-May-13) to match assessCA.
  mcmcString <- paste( " -mcmc ",spObj$nMCMC," -mcsave ",spObj$nThin, sep="" )
  
  # Set start time for optimization
  starttime <- Sys.time()

  xFactor <- 0.2
  for ( i in 1:4 )
  {
    hessPosDef <- TRUE

    if( .Platform$OS.type=="unix" )
    { 
      system( command=paste( exeName," ",mcmcString," -iprint 100 -maxfn 5000",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )

      system( command=paste( exeName," ","-mceval",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T  )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe ", mcmcString," "," -iprint 100 -maxfn 5000", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
              
      system( paste( exeFile,".exe -mceval", sep="" ), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
    }

    # Check if there is a valid mcmc output file, i.e., was the Hessian positive
    # definite, if not must use old mcmc output file and set hessPosDef to FALSE.
    # This condition is then flagged in mp$assess$runStatus$hessPosDef (T,F).
    # The fix put in by
    #    SPC will use the previous year's Umsy, but the failure will show up in
    #    diagnostics plots.    
  
    tmpMCMC  <- try( read.table( mcmcFile, header=T, sep=" " ), silent=TRUE )
    
    if ( class( tmpMCMC )=="try-error" )
    {
      hessPosDef <- FALSE

      cat( file=pinFile, "## ", exeName,": Surplus production model parameters.\n" )
      cat( file=pinFile, "## Written:",     date(),                 "\n", append=T )
      cat( file=pinFile, "# lnMsy\n",       log(spObj$spMsy)*(1.0+i*xFactor), "\n", append=T )
      cat( file=pinFile, "# lnFmsy\n",      log(spObj$spFmsy)*(1.0+i*xFactor),"\n", append=T )
      cat( file=pinFile, "# pmMsy\n",       spObj$spPmMsy,          "\n", append=T )
      cat( file=pinFile, "# psdMsy\n",      spObj$spPsdMsy,         "\n", append=T )
      cat( file=pinFile, "# pmFmsy\n",      spObj$spPmFmsy,         "\n", append=T )
      cat( file=pinFile, "# psdFmsy\n",     spObj$spPsdFmsy,        "\n", append=T )
      cat( file=pinFile, "# rho\n",         spObj$spRhoEiv,         "\n", append=T )  
      cat( file=pinFile, "# lnOmega\n",     spObj$lnOmega,          "\n", append=T )
      
      cat( "\nMSG (.assessModSP) Hessian not positive definite on attempt ",i," re-trying...\n" )
      cat( "\nMSG (.assessModSP) log(spMsy)  = ", log(spObj$spMsy)*(1.0+i*xFactor),"\n" )
      cat( "\nMSG (.assessModSP) log(spFmsy) = ", log(spObj$spFmsy)*(1.0+i*xFactor),"\n" )
    }
      
    if ( hessPosDef )
      break
  }

  if ( class( tmpMCMC )=="try-error" )
  {
    hessPosDef <- FALSE  
    cat( "\nHessian not positive definite, using last good mcout.dat","\n" )
    mcmcFile <- file.path( getwd(), "mcout.bck" )          
    if ( file.exists( mcmcFile ) )
      tmpMCMC <- read.table( mcmcFile, header=T, sep=" " )
      
    # Save the *.pin and *.dat files for forensics.
    badPinFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".pin", sep="" ) )
    file.copy( pinFile, badPinFile, overwrite=TRUE )
    badDatFile <- file.path( getwd(), .FTEMP, paste( exeName,t,".dat", sep="" ) )
    file.copy( datFile, badDatFile, overwrite=TRUE )      
  }
  else
  {
    # Save the mcout.dat file in case the next Hessian is not positive definite.
    file.copy( "mcout.dat","mcout.bck", overwrite=TRUE )  
  }

  # Read the ADMB report file.
  assessSP <- lisread( repFile, quiet=TRUE )

  # Read MLE standard deviations for Bmsy and Fmsy estimates.
  stdBmsy       <- NA
  stdMSY        <- NA
  stdFmsy       <- NA
  stdProjExpBio <- NA
 
  if ( hessPosDef )
  {
    if ( file.exists( stdFile ) )
    {
      stderrs       <- read.table( stdFile, as.is=TRUE, header=FALSE, skip=1, sep="" )
      stdBmsy       <- as.numeric( stderrs[ stderrs$V2=="Fmsy", ncol(stderrs) ] )
      stdFmsy       <- as.numeric( stderrs[ stderrs$V2=="Bmsy", ncol(stderrs) ] )
      stdProjExpBio <- as.numeric( stderrs[ stderrs$V2=="projExpBio", ncol(stderrs) ] )
    }
  }
  
  # ARK *** (21-Jun-13) Cox changed this elswhere, to put thinning at ADMB call.
  nThin <- 1
  if( spObj$useMLE==FALSE )
  {
    if ( spObj$hcrType=="declineRisk" )
    {
      # The MCMC output is in blocks of nQuotaLevels, thin from each quota level.
      idxQ <- split( c(1:nrow(tmpMCMC)), tmpMCMC$Q )
      iRow <- as.vector( sapply( idxQ,
           function(x) { return( x[ seq(1,length(x),nThin)] ) } ) )
    }
    else if ( spObj$hcrType=="variableF" | spObj$hcrType=="constantF" )
    {
      # constantF or variableF: thin MCMC chain based on nThin from GUI.
      tmpN  <- nrow( tmpMCMC )
      iRow  <- seq( 1, tmpN, by=nThin )
    }

    # Extract thinned chain.
    mcOut <- tmpMCMC[ iRow, ]
  }
  else
    mcOut <- tmpMCMC[1,]     # First row of MCMC output are the MLEs.
  
  endtime <- Sys.time()                      # End time for minization.
  elapsed <- difftime( endtime,starttime )   # Elapsed minimization time.  

  assessment <- assessSP

  assessment$mpdPars <- list( objFun  = assessSP$objFun,
                              totLike = assessSP$total_likelihood,
                              totPriors = assessSP$total_priors,
                              fpen    = assessSP$fpen,
                              bpen    = assessSP$bpen,  
                              ssbFmsy = assessSP$Bmsy,
                              Fmsy    = assessSP$Fmsy,
                              B0      = assessSP$Bmsy*2.,
                              Msy     = assessSP$Msy,
                              q       = assessSP$q,
                              stdBmsy = stdBmsy,
                              stdFmsy = stdFmsy,

                              projExpBio    = assessSP$projExpBio,
                              stdProjExpBio = stdProjExpBio,
                              dep     = assessSP$D,
                              lastDt  = assessSP$lastDt )
                              
  assessment$mcOut <- mcOut                          
                              
  assessment$runStatus <- list( exeName       = exeName,
                                objFun        = assessSP$objFun,
                                maxGrad       = assessSP$maxGrad,
                                nEval         = assessSP$nEval,
                                convT         = elapsed[[1]],
                                iExit         = assessSP$iExit,
                                hessPosDef    = hessPosDef,
                                fisheryClosed = NA,
                                deadFlag      = NA )
  
  # Exploitable and spawning biomass are the same for the production model.
  assessment$exploitBt <- assessment$Bt
  assessment$spawnBt   <- assessment$Bt
											 	  
  # ARK (17-Nov-10) Just get this back to updatePop and store by gear type there.
  #assessment$ItScaled <- assessment$ItScaled

  return( assessment )
}     # END function .assessModPmod

# assessModISCAM
# Purpose:       Runs a statistical catch-age model to estimate biomass       #
#                and fishery reference points if required.
# Parameters:    obj=list containing everything, because this thing needs it  #
# Returns:       current biomass estimate, stock dynamics and parameters,     #
#                minimization details, yield and ref pt calcs                 #
# Notes:         Execution is platform-dependent - see system() call          #
# Source:        S.P. Cox (1-May-10)                                          #
assessModISCAM <- function( caObj )
{
  # Statistical catch-age model.
  # This implmentation is a wrapper that simply calls the ISCAM estimation
  # procedure, reads the output and returns the assessmemt
  #
  # (1) Extract and write the model parameters and data using iscamWrite()
  # (3) Make a system call to the ADMB program (mac=./iscam_ageM).
  # (4) Read the ADMB report file (iscamCtl.rep).
  # (5) Update the pars list component and tack on some ADMB minimizer details.
  #

  t <- caObj$t
  exeName <- "iscam_ageM"
  
  mcOut <- NA
  
  # mcmcString <- ""
  mcmcString <- paste( " -mcmc ", caObj$nMCMC," -mcsave ",caObj$nThin, sep="" )

  datFileFull  <- file.path( getwd(), "iscam_top.dat" )
  datFile      <- "iscam_top.dat"
  exeFile  <- file.path( getwd(), exeName )
  mcmcFile <- file.path( getwd(), paste(exeName,".mcmc",sep = "") )
  # pinFile  <- file.path( getwd(), paste( exeName,".pin", sep="" ) )  
  # repFile  <- file.path( getwd(), paste( exeName,".rep", sep="" ) )  
  # stdFile  <- file.path( getwd(), paste( exeName,".std", sep="" ) )

  # Set start time for optimization
  starttime <- Sys.time()
  
  xFactor <- 0.2
  for ( i in 1:4 )
  {
    iscamWrite(caObj)
    hessPosDef <- TRUE
    if( .Platform$OS.type=="unix" )
    { 
      runModel <- paste( "./", exeName, " -ind ",datFile, 
                         " -iprint 100 ", mcmcString, sep="" )
      system(  command=runModel,
                wait=TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE )

      mcEval <- paste( "./", exeName, " -ind ", datFile, " -mceval", sep = "" )
      system( command=mcEval,
              wait=TRUE, ignore.stdout = TRUE, ignore.stderr = TRUE )
    }
    else     # Windows branch call to "system".
    {
      system( paste( exeFile,".exe -ind ",datFile," -iprint 100 -maxfn 5000"), wait=TRUE,
              show.output.on.console=.SHOWOUTPUTONCONSOLE )
            
      system( command=paste( exeFile," ","-mceval",sep="" ),
              show.output.on.console=.SHOWOUTPUTONCONSOLE, wait=TRUE, ignore.stdout=T  )
    }
    # Check if there is a valid mcmc output file, i.e., was the Hessian positive
    # definite, if not must use old mcmc output file and set hessPosDef to FALSE.
    # This condition is then flagged in mp$assess$runStatus$hessPosDef (T,F).
    tmpMCMC  <- try( read.table( mcmcFile, header=T, sep="",
                      strip.white = TRUE ), silent=TRUE ) 
    # Increment starting R0 and R value to hopefully get convergence
    if ( class( tmpMCMC )=="try-error" )
    {
      caObj$R0 <- exp(log(caObj$R0)*(1.0+i*xFactor))
    }
    
    if ( hessPosDef )
      break
  }
  

  if ( class( tmpMCMC )=="try-error" )
  {
    hessPosDef <- FALSE
    cat( "\nHessian not positive definite, using last good mcout.dat","\n" )
    mcmcFile <- file.path( getwd(), "mcout.bck" )          
    if ( file.exists( mcmcFile ) )
      tmpMCMC <- read.table( mcmcFile, header=T )
  } else {
    # Save the mcout.dat file in case the next Hessian is not positive definite.
    file.copy( mcmcFile,"mcout.bck", overwrite=TRUE )  
    system( paste("rm ", mcmcFile, sep = "" ) )
    system( paste("rm ", exeFile, ".psv", sep = "" ) )
  }          
  # The thinning of the MCMC chain depends on (a) MLE vs. MCMC, (b) HCR.
  # SPC (18-Apr-13): Not anymore!! mcout.dat not generated by -mceval call
  # so it will already be thinned. Therefore, fixing nThin here == 1.
  # nThin <- 1
  # if( caObj$useMLE==FALSE )
  # {
  #   if ( caObj$hcrType == "declineRisk" )
  #   {
  #     # The MCMC output is in blocks of nQuotaLevels, thin from each quota level.
  #     idxQ <- split( c(1:nrow(tmpMCMC)), tmpMCMC$Q )
  #     iRow <- as.vector( sapply( idxQ,
  #          function(x) { return( x[ seq(1,length(x),nThin)] ) } ) )
  #   }
  #   else if ( caObj$hcrType=="variableF" | caObj$hcrType=="constantF" )
  #   {
  #     # constantF or variableF: thin MCMC chain based on nThin from GUI.
  #     tmpN  <- nrow( tmpMCMC )
  #     iRow  <- seq( 1, tmpN, by=nThin )
  #   }

  #   # Extract thinned chain.
  #   mcOut <- tmpMCMC[ iRow, ]
  # }
  # else
  
  mcOut <- tmpMCMC[1,]     # First row of MCMC output are the MLEs.

  # Set end time for optimization
  endtime <- Sys.time()       
  
  # Calculate elapsed time
  elapsed <- difftime( endtime,starttime )     

  # Read the report file - this should really be the MCout stuff, change
  # the procedure to read that in
  fit <- try(read.rep(paste(exeFile,".rep",sep = "")))
  if(class(fit) == "try-error") browser()
  else  system( paste("rm ", exeFile, ".rep", sep = "" ) )
 
  # d) Save assessment results to output list

  # SPC (26-Jan-13) This list (i.e., rep file output) always contains 
  # projSpawnBio and projExpBio which are 1-year ahead projections of each.
  assessment        <- fit
      #cat("\n Printing assessment list from rep file \n")
      #print(assessment)

  assessment$mpdPars <- list( objFun        = fit$fit$nlogl,
                              R0            = fit$ro,
                              SSB0          = fit$sbo,
                              B0            = fit$sbo,
                              Msy           = fit$msy,
                              q             = fit$q,
                              projExpBio    = fit$future_bt3andolder,
                              lastDt        = fit$sbt[t]/fit$sbo )
                              
  assessment$mcOut                    <- mcOut                                          
  assessment$runStatus                <- fit$fit[c("nopar","nlogl","maxgrad","npar","logDetHess")]
  assessment$runStatus$hessPosDef     <- hessPosDef
  assessment$runStatus$fisheryClosed  <- NA
  assessment$runStatus$deadFlag       <- NA

  assessment$exploitBt <- fit$bt[t]
  assessment$spawnBt   <- fit$sbt[t]


  return( assessment )
}     # END function assessModCA

callProcedureISCAM <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

  caObj    <- list()                        # List to pass to assessModProd

  opMod     <- obj$ctlList$opMod
  om        <- obj$om

  years <- .INITYEAR:(.INITYEAR + t - 2)
  
  caObj$t  <- t                             # Current time step.
  caObj$nT <- opMod$nT
  tMP      <- opMod$tMP                 # Year MP first applied, tMP.

  # (1) Data.

  caObj$A         <- opMod$nAges
  minAge          <- obj$ctlList$mp$data$minAge
  patg            <- obj$mp$data$Xatg[,(1:(t-1)),]

  # Age observation indices
  useAges         <- eval(parse(text = obj$ctlList$mp$data$useAges) )
  t1Ages          <- eval(parse(text = obj$ctlList$mp$data$t1Ages) )
  t2Ages          <- eval(parse(text = obj$ctlList$mp$data$t2Ages) )
  nAgeObs         <- t - t1Ages

  pAtg            <- matrix( NA, nrow = sum(nAgeObs), ncol = caObj$A + 2 - minAge + 1)
  initRow         <- 1
  for(i in 1:length(useAges))
  {
    ageGear <- useAges[i]
    rowIdx <- initRow:(initRow + nAgeObs[i] - 1)
    pAtg[rowIdx,1] <- years[t1Ages[i]:(t-1)]
    pAtg[rowIdx,2] <- ageGear
    pAtg[rowIdx,3:(caObj$A+2-minAge + 1)] <- t(patg[minAge:caObj$A,,ageGear])
    initRow <- initRow + nAgeObs[i]
  }

  # Now get rid of rows with no age observations.
  loseRows <- c()
  for( rIdx in 1:nrow(pAtg))
  {
    if(any(is.na(pAtg[rIdx,])))
      loseRows <- c(loseRows,rIdx)
  }
  pAtg <- pAtg[-loseRows,]
  nAgeObs <- c()
  for(i in 1:length(useAges))
  {
    ageGear <- useAges[i]
    nAgeObs <- c(nAgeObs, sum(pAtg[,2] == ageGear) )
  }


  caObj$pAtg    <- pAtg
  caObj$ageIdx  <- useAges
  caObj$nAgeObs <- nAgeObs

  # Extract survey index attributes before they are lost when It extracted.
  surveyOn   <- attributes( obj$mp$data$It )$indexOn

  # survey observations
  Itg                 <- obj$mp$data$It[ c(1:(t-1)), ]  # Extract stock index values.
  surveys             <- eval(parse(text = obj$ctlList$mp$data$useIndex) )
  caObj$nSurv         <- length(surveys)
  caObj$nObs          <- apply(X = !is.na(Itg), FUN = sum, MARGIN = 2 )[surveys]
  caObj$surveyType    <- 3

  It <- matrix(NA, nrow = sum(caObj$nObs), ncol = 5 )

  initRow <- 1
  for(i in 1:length(surveys))
  {
    survIdx           <- surveys[i]
    surveyOnIdx       <- which(!is.na(Itg[,survIdx])) 
    idxLikeWeight     <- obj$ctlList$mp$data$inputIndex$it_wt[i,1]
    It[initRow:(initRow + caObj$nObs[i] - 1), 1] <- years[surveyOnIdx]
    It[initRow:(initRow + caObj$nObs[i] - 1), 2] <- Itg[surveyOnIdx,survIdx]
    It[initRow:(initRow + caObj$nObs[i] - 1), 3] <- survIdx
    It[initRow:(initRow + caObj$nObs[i] - 1), 4] <- idxLikeWeight
    It[initRow:(initRow + caObj$nObs[i] - 1), 5] <- 1
    initRow <- initRow + caObj$nObs[i]
  }
  
  caObj$It       <- It                            # Stock index

  caObj$initYear      <- .INITYEAR
  caObj$lastYear      <- .INITYEAR + t - 2
  caObj$fisheryFlag   <- opMod$allocProp

  Ctg         <- obj$om$Ctg[1:(t-1),]           # Extract catch history.
  Ctg[is.na(Ctg)] <- 0
  Ctg <- cbind( seq(from = .INITYEAR, length = t-1), Ctg )
  caObj$Ctg   <- Ctg
  lastQuota   <- sum(Ctg[t-1,],na.rm=T)
  # Will need to update surveys to 0 prop, renormalise

  caObj$catchType     <- 1 # hard coded to biomass
  caObj$M             <- mean(opMod$endM)

  # growth pars
  VBt0 <- log ( (mean(opMod$Linf)-mean(opMod$L1))/mean(opMod$Linf))/mean(opMod$vonK) + 1

  caObj$VBgrowth      <- c(mean(opMod$Linf),mean(opMod$vonK),VBt0)
  caObj$lengthWt      <- c(mean(opMod$c1),mean(opMod$c2))
  caObj$matAgeLogPars <- c(opMod$aMat50,opMod$aMat95)
  caObj$matAge        <- .calcMa( A=caObj$A, A95=opMod$aMat95, A50=opMod$aMat50 )


  # Wt at age
  caObj$nWtObs        <- t
  wtAge               <- obj$om$Wta[1:t,2:caObj$A]
  wtAge               <- cbind( seq(from = .INITYEAR, length = t), wtAge )
  caObj$wtAge         <- wtAge


  # other stuff to make the model run
  caObj$nMclass       <- 1
  caObj$fixMdevs      <- matrix(rep(0,t-2),ncol = t-2, nrow = caObj$nMclass)
  

  caObj$iscamCtrl <- obj$ctlList$mp$assess$iscam

  caObj$R0          <- obj$ref$R0
  caObj$rSteepness  <- obj$ref$rSteepness
  caObj$M           <- mean(obj$ctlList$opMod$M)

  caObj$nMCMC     <- obj$ctlList$mp$hcr$nMCMC
  caObj$nThin     <- obj$ctlList$mp$hcr$nThin

  # -- Call assessment model, which will do its own file read/write
  result <- assessModISCAM( caObj )

  result  
}     # END function callProcedureCAA


# iscamWrite()
# Function that writes dat and ctl files for iscam from the contents
# of the operating model list.
# inputs:   obj=ctlList from runSimEst()
# ouputs:   NULL
# usage:    to prepare data for the estimator
iscamWrite <- function ( obj )
{ 
  # Calculate nGear
  nGear <- length(obj$fisheryFlag)

  # file names
  datFile <- "iscam_ageM.dat"
  ctlFile <- "iscam_ageM.ctl"
  pfcFile <- "iscam_ageM.pfc"

  # Write dat file (taken from RL's code)
  cat( file=datFile, "## simulation for Herring SOG " )
  cat( file=datFile, "## written:",date(),"\n", append=T )
  cat( file=datFile, "## ____Model Dimensions____","\n", append=T )
  cat( file=datFile, "## first year of data","\n", append=T )
  cat( file=datFile, obj$initYear,"\n", append=T )
  cat( file=datFile, "## last year of data","\n", append=T )
  cat( file=datFile, obj$lastYear,"\n", append=T )
  cat( file=datFile, "## age of youngest age class","\n", append=T )
  cat( file=datFile, obj$iscamCtrl$minAge,"\n", append=T )
  cat( file=datFile, "## age of plus group","\n", append=T )
  cat( file=datFile, obj$A,"\n", append=T )
  cat( file=datFile, "## number of gears (ngear)","\n", append=T )
  cat( file=datFile, nGear,"\n", append=T )                       ### see 
  cat( file=datFile, "## flags for fishery (1) or survey (0) in ngears","\n", append=T )
  cat( file=datFile, obj$fisheryFlag,"\n", append=T )  ## include in runSimSca
  cat( file=datFile, "## NEW: Catch type","\n", append=T )
  cat( file=datFile, "## 1 = biomass","\n", append=T )
  cat( file=datFile, "## 2 = catch in numbers","\n", append=T )
  cat( file=datFile, "## 3 = roe removed (expressed as tons of spawning biomass equivalents)","\n", append=T )
  cat( file=datFile, rep(obj$catchType,nGear),"\n", append=T )                     ## include in runSimSca
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## natural mortality rate (m)","\n", append=T )
  cat( file=datFile, obj$M,"\n", append=T )                             ##### check this value 
  cat( file=datFile, "## growth parameters (linf,k,to)","\n", append=T )
  # cat( file=datFile, c(27.0,",", 0.48,",", 0),"\n", append=T )
  cat( file=datFile, obj$VBgrowth,"\n", append=T )   ## include in runSimSca
  cat( file=datFile, "## length-weight allometry (a,b)","\n", append=T )  
  # cat( file=datFile, c(4.5e-6,",", 3.1270),"\n", append=T )                  ## include in runSimSca
  cat( file=datFile, obj$lengthWt,"\n", append=T )
  cat( file=datFile, "## maturity at age (am=log(3)/k) & gm=std for logistic","\n", append=T )
  # cat( file=datFile, c(2.055,",",0.05),"\n", append=T )                  ## include in runSimSca
  cat( file=datFile, obj$matAgeLogPars,"\n", append=T )                  ## include in runSimSca
  cat( file=datFile, "## init_vector mat(sage,nage)     input maturity vector (**not used)","\n", append=T )
  # cat( file=datFile, c(0.001, 0.24974, 0.90, 1, 1, 1, 1, 1, 1, 1),"\n", append=T )   ## include in runSimSca
  cat( file=datFile, obj$matAge[obj$iscamCtrl$minAge:obj$A],"\n", append=T )   ## include in runSimSca
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## Time series data observed catch","\n", append=T )
  cat( file=datFile, "## Year P1 P2 P3 S1 S2","\n", append=T )
  write.table(  file=datFile, x = round(obj$Ctg,10), quote = FALSE, append = TRUE, col.names=FALSE, 
                row.names = FALSE )

  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## Relative Abundance index from fisheries independent survey (it)","\n", append=T )
  cat( file=datFile, "## nit","\n", append=T )
  cat( file=datFile, obj$nSurv,"\n", append=T )
  cat( file=datFile, "## nit_nobs","\n", append=T )
  cat( file=datFile, obj$nObs,"\n", append=T ) 
  cat( file=datFile, "## 1 = survey is proportional to vulnerable numbers","\n", append=T )
  cat( file=datFile, "## 2 = survey is proportional to vulnerable biomass","\n", append=T )
  cat( file=datFile, "## 3 = survey is proportional to spawning biomass (e.g., herring spawn survey)","\n", append=T )
  cat( file=datFile, rep(obj$surveyType,obj$nSurv),"\n", append=T ) 
  cat( file=datFile, "## iyr  it  gear  wt  survey_timing","\n", append=T )
  write.table( file=datFile, round( obj$It,4), quote=FALSE, row.names=FALSE, col.names=FALSE, append=T )
  
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## ________________________","\n", append=T )
  cat( file=datFile, "## Age composition data by year, gear (ages 2-15+)","\n", append=T )
  cat( file=datFile, "## na_gears","\n", append=T )
  cat( file=datFile, length(obj$ageIdx),"\n", append=T )
  cat( file=datFile, "## na_nobs","\n", append=T )
  cat( file=datFile, obj$nAgeObs,"\n", append=T ) 
  cat( file=datFile, "##  a_sage","\n", append=T )
  cat( file=datFile, rep(obj$iscamCtrl$minAge,length(obj$ageIdx)),"\n", append=T )
  cat( file=datFile, "## a_page","\n", append=T )
  cat( file=datFile, rep(obj$A,length(obj$ageIdx)),"\n", append=T )
  cat(file = datFile, "#yr gear      V2   V3   V4   V5  V6  V7  V8 V9 V10  #Number aged\n",append=T)
  write.table( file=datFile, round( obj$pAtg,4), quote=FALSE, row.names=FALSE, col.names=FALSE, append=T )  

  cat( file=datFile, "## ________________________","\n", append=T ) 
  cat( file=datFile, "## n_wt_obs","\n", append=T )
  cat( file=datFile, obj$nWtObs,"\n", append=T )                       #### ## include in runSimSca
  cat( file=datFile, "## Mean weight-at-age in kilograms ","\n", append=T )
  for(j in 1:obj$nWtObs)
  {
    cat( file=datFile, signif(obj$wtAge[j,],12), "\n", append=T )  
  }
  cat( file=datFile, "## ________________________","\n", append=T ) 
  
  cat( file=datFile, "## n_m_class\n", append="T")
  cat( file=datFile, obj$nMclass, "\n", append=TRUE)  

  cat( file=datFile, "## fix_m_devs\n", append="T")
  write.table( file=datFile, obj$fixMdevs, row.names = FALSE, col.names=FALSE, quote=FALSE, append=TRUE)  

  cat( file=datFile, "## eof","\n", append=T )
  cat( file=datFile, 999,"\n", append=T ) 

  priorVars <- obj$iscamCtrl$priors
  recInitPhz <- ifelse( obj$iscamCtrl$unfished == 1, -1, 1)

  # Now write control file (code taken from RL's original)
  #              ival      lb   ubp  phz    prior       p1     p2            # parameter name
  par_r0     <-c(log(obj$R0),  -5, 15   ,4     ,0           ,-5.0   ,15)        # log_r0
  par_ste    <-c(  obj$rSteepness,   0.2, 1.0  ,4     ,3    ,priorVars$steepnessBeta1, priorVars$steepnessBeta2 )      # steepness
  par_m      <-c(log(obj$M),-5.0, 5.0,3,1,priorVars$meanInitM, priorVars$sdInitM)       # log.m
  par_avgrec <-c(log(obj$R0)  ,  -5.0, 15   ,1     ,0           ,-5.0   ,15 )       # log_avgrec
  par_recini <-c(log(obj$R0),  -5.0, 15   ,recInitPhz     ,0           ,-5.0   ,15)        # log_recinit
  par_rho    <-c(0.3043478 , 0.001, 0.999 ,3,3  ,17.08696  ,39.0559)     # rho
  par_kappa  <-c(0.8695652 ,  0.01, 5.0  ,3,4 ,25.0   ,28.75)     # kappa

  #OTHER MISCELLANEOUS CONTROLS
  ADMB_output<-0              ## 1 verbose ADMB output (0=off, 1=on)
  Rec_mod<-1                  ## 2 recruitment model (1=beverton-holt, 2=ricker)
  sd_catch_1phase<- 0.100     ## 3 std in observed catches in first phase.
  sd_catch_last_phase<- 0.0707 ## 4 std in observed catches in last phase.
  # unfish_1year<-0             ## 5 Assume unfished in first year (0=FALSE, 1=TRUE)
  min_age_prop<-0.02          ## 6 Minimum proportion to consider in age-proportions for dmvlogistic
  mean_F<-0.20                ## 7 Mean fishing mortality for regularizing the estimates of Ft
  mean_f_1phase<- 0.01        ## 8 std in mean fishing mortality in first phase
  mean_f_last_phase<-2.00     ## 9 std in mean fishing mortality in last phase
  # phase_m_devs<- 3 ## 10 phase for estimating m_deviations (use -1 to turn off mdevs)
  sd_m<-0.1                   ## 11 std in deviations for natural mortality
  # numb_nodes_m<-12        ## 12 number of estimated nodes for deviations in natural mortality
  fraction_F<-1.00            ## 13 fraction of total mortality that takes place prior to spawning
  age_comp_like<-1            ## 14 switch for age-composition likelihood (1=dmvlogistic,2=dmultinom)
  selx_renorm <- 0

  cat( file=ctlFile, "## CTL file written by herringM sim-est procedure", append=F)
  cat( file=ctlFile, "## _______________________________________________________________________________________\n", append=T )
  cat( file=ctlFile, "##                         GENERIC HERRING CONTROLS\n", append=T )
  cat( file=ctlFile, "##                            written:",date(),"\n", append=T )
  cat( file=ctlFile, "## ___________________________CONTROLS FOR ESTIMATED PARAMETERS__________________________\n" , append=T)
  cat( file=ctlFile, "##   Prior descriptions:\n" , append=T)
  cat( file=ctlFile, "##                    -0 uniform (0,0)\n", append=T )
  cat( file=ctlFile, "##                    -1 normal (p1=mu,p2=sig)\n", append=T )
  cat( file=ctlFile, "##                    -2 lognormal (p1=log(mu),p2=sig)\n", append=T )
  cat( file=ctlFile, "##                    -3 beta (p1=alpha,p2=beta)\n", append=T )
  cat( file=ctlFile, "##                    -4 gamma(p1=alpha,p2=beta)\n" , append=T)
  cat( file=ctlFile, "## ______________________________________________________________________________________\n", append=T )
  cat( file=ctlFile, 7 - 1 + obj$nMclass, "","# npar","\n", append=T )
  cat( file=ctlFile, "##    ival  lb   ub  phz  prior  p1  p2  parameter name \n", append=T )
  cat( file=ctlFile, "## ______________________________________________________________________________________\n", append=T )
  cat( file=ctlFile, par_r0, "","# log_r0","\n", append=T )
  cat( file=ctlFile, par_ste, "","# steepness","\n", append=T )
  cat( file=ctlFile, par_m, "","# log.m","\n", append=T )
  cat( file=ctlFile, par_avgrec, "","# log_avgrec","\n", append=T )
  cat( file=ctlFile, par_recini, "","# log_recinit","\n", append=T )
  cat( file=ctlFile, par_rho, "","# rho","\n", append=T )
  cat( file=ctlFile, par_kappa, "","# kappa (precision)","\n", append=T )
  if( obj$nMclass > 1 )
  {
    for( k in 2:obj$nMclass)
    {
      par_m <- c(assess$ctl$m_priorMean[k],-5.0, 5.0,assess$ctl$m_phz[k],assess$ctl$m_prior[k],assess$ctl$m_priorMean[k],assess$ctl$m_priorSD[k])
      cat( par_m, " # log.m", k ,"\n", append=T, file=ctlFile  )
    } 
  }
  cat( file=ctlFile, "## ______________________________________________________________________________________\n", append=T )
  cat( file=ctlFile, "## ____________________________________________________________\n", append=T )
  cat( file=ctlFile, "## _________________________SELECTIVITY PARAMETERS_______________________________________\n", append=T )
  cat( file=ctlFile, "##  OPTIONS FOR SELECTIVITY:\n", append=T )
  cat( file=ctlFile, "##              1) logistic selectivity parameters\n" , append=T)
  cat( file=ctlFile, "##              2) selectivity coefficients\n", append=T )
  cat( file=ctlFile, "##              3) a constant cubic spline with age-nodes\n", append=T )
  cat( file=ctlFile, "##              4) a time varying cubic spline with age-nodes\n" , append=T)
  cat( file=ctlFile, "##              5) a time varying bicubic spline with age & year nodes\n" , append=T)
  cat( file=ctlFile, "##              6) fixed logistic (set isel_type=6, and estimation phase to -1)\n", append=T )
  cat( file=ctlFile, "##              7) logistic function of body weight.\n", append=T )
  cat( file=ctlFile, "##              sig=0.05 0.10 0.15 0.20 0.30 0.40 0.50\n" , append=T)
  cat( file=ctlFile, "##              wt =200. 50.0 22.2 12.5 5.56 3.12 2.00\n" , append=T)

  cat( file=ctlFile, "## Gear 1:3 fishery:       Gear 4-5 survey\n" , append=T)
  cat( file=ctlFile, "## isel_typ \n", append=T )
  cat( file=ctlFile, c(1,1,1,6,6), "\n", append=T )
  cat( file=ctlFile, "## Age at 50% selectivity (logistic) \n", append=T )
  cat( file=ctlFile, c(3,3,4,2.055,2.055 ), "\n", append=T )
  # cat( file=ctlFile, age_50, "\n", append=T )
  cat( file=ctlFile, "## STD at 50% selectivity (logistic) \n", append=T )
  # cat( file=ctlFile, std_50_sel, "\n", append=T )
  cat( file=ctlFile, c(0.25,0.25,0.25,0.05,0.05 ), "\n", append=T )
  cat( file=ctlFile, "## No. of age nodes for each gear (0 to ignore). \n", append=T )
  cat( file=ctlFile, c(5,5,5,0,0 ), "\n", append=T )
  cat( file=ctlFile, "## No. of year nodes for each gear (0 to ignore).\n", append=T )
  cat( file=ctlFile, c(12,3,10,0,0 ), "\n", append=T )
  cat( file=ctlFile, "## Estimation phase\n", append=T )
  cat( file=ctlFile, c(2,2,2,-1,-1 ), "\n", append=T )
  # cat( file=ctlFile, c(-1,-1,-1,-1,-1), "\n", append=T )
  cat( file=ctlFile, "## Penalty weight for 2nd differences w=1/(2*sig^2)\n", append=T )
  cat( file=ctlFile, c(125,125,12.5,12.5,12.5 ), "\n", append=T )
  cat( file=ctlFile, "## Penalty weight for dome-shaped selectivity 1=1/(2*sig^2)\n", append=T )
  cat( file=ctlFile, c(50,50,200,200,200 ), "\n", append=T )

  cat( file=ctlFile, "##_________________________________________##\n", append=T )
  cat( file=ctlFile, "##_________________________________________##\n" , append=T)
  cat( file=ctlFile, "##           Priors for Survey q           ##\n" , append=T)
  cat( file=ctlFile, "##_________________________________________##\n" , append=T)
  cat( file=ctlFile, "## nits  #number of surveys\n", append=T )
  cat( file=ctlFile, obj$nSurv, "\n", append=T )
  cat( file=ctlFile, "## priors 0=uniform density   1=normal density\n", append=T )
  cat( file=ctlFile, c(0,1), "\n", append=T )
  cat( file=ctlFile, "## prior log(mean)\n", append=T )
  cat( file=ctlFile, priorVars$logqPrior1, "\n", append=T )
  cat( file=ctlFile, "## prior sd\n", append=T )
  cat( file=ctlFile, priorVars$logqPrior2, "\n", append=T )

  cat( file=ctlFile, "##______________________________________________________________________________##\n", append=T )
  cat( file=ctlFile, "##_______OTHER MISCELLANEOUS CONTROL_______##\n", append=T )
  cat( file=ctlFile, ADMB_output, ""," ## 1 verbose ADMB output (0=off, 1=on)","\n", append=T )
  cat( file=ctlFile, Rec_mod, ""," ## 2 recruitment model (1=beverton-holt, 2=ricker)","\n", append=T )
  cat( file=ctlFile, sd_catch_1phase, ""," ## 3 std in observed catches in first phase","\n", append=T )
  cat( file=ctlFile, sd_catch_last_phase, ""," ## 4 std in observed catches in last phase","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$unfished, "","         ## 5 Assume unfished in first year (0=FALSE, 1=TRUE)","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$minAgeProp, "","         ## 6 Minimum proportion to consider in age-proportions for dmvlogistic","\n", append=T )
  cat( file=ctlFile, mean_F, "","  ## 7 Mean fishing mortality for regularizing the estimates of Ft","\n", append=T )
  cat( file=ctlFile, mean_f_1phase, ""," ## 8 std in mean fishing mortality in first phase","\n", append=T )
  cat( file=ctlFile, mean_f_last_phase, ""," ## 9 std in mean fishing mortality in last phase","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$mDevPhz, ""," ## 10 phase for estimating m_deviations (use -1 to turn off mdevs)","\n", append=T )
  cat( file=ctlFile, sd_m, ""," ## 11 std in deviations for natural mortality","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$mDevNodes, ""," ## 12 number of estimated nodes for deviations in natural mortality","\n", append=T )
  cat( file=ctlFile, fraction_F, ""," ## 13 fraction of total mortality that takes place prior to spawning","\n", append=T )
  cat( file=ctlFile, age_comp_like, ""," ## 14 switch for age-composition likelihood (1=dmvlogistic,2=dmultinom)","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$selxRenorm, ""," ## 15 switch for selectivity renormalisation factor (0=mean,1=max,2=none)","\n", append=T )

  cat( file=ctlFile, "##_________________________________________##\n", append=T )

  
  cat( file = ctlFile, "# mDevPhz \n", append = T)
  cat( file = ctlFile, obj$iscamCtrl$mDevPhz, "\n", append = T)
  cat( file = ctlFile, "# mAges \n", append = T)
  cat( file = ctlFile, obj$A, "\n", append = T)

  cat( file=ctlFile, "##operational control points for harvest control rule\n", append=T)
  cat( file=ctlFile, obj$iscamCtrl$cutoff, "","   ##cutoff_fraction prop of prefishery biomass to unfished biomass used to calculate cutoff ","\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$targHR,"","   ##target_hr","\n", append=T )
  cat( file=ctlFile, "#fixed_cutoff (tonnes)\n", append=T )
  cat( file=ctlFile, obj$iscamCtrl$fixCutoff, "\n", append=T )
  cat( file=ctlFile, "## eofc\n", append=T )
  cat( file=ctlFile, 999, "\n", append=T )

  ## Now copy in the new projection control file

  #########################################
  ## modify Projection file control (pfc) ##
  #########################################
  
  lastNyear = 4 
  
  cat( file=pfcFile, "## simulation for Herring SOG " )
  cat( file=pfcFile, "## written:",date(),"\n", append=T )
  cat( file=pfcFile, "## Projection file control (pfc) ##","\n", append=T )
  cat( file=pfcFile, "## n_tac  length of catch vector","\n", append=T )
  cat( file=pfcFile, 15,"\n", append=T )
  cat( file=pfcFile, "## tac vector (1000 mt)","\n", append=T )
  cat( file=pfcFile, "0 10.0 10.6 12.6 17.0 18.25 21.8 22.5 25.0 25.9 30.0 38.0 40.0 46.5 50.0 \n", append=T )
  cat( file=pfcFile, "## Control options ##","\n", append=T )
  cat( file=pfcFile, "## Length of control vector","\n", append=T )
  cat( file=pfcFile, 8,"\n", append=T )
  cat( file=pfcFile, "## - 1) Start year for mean natural mortality rate","\n", append=T )
  cat( file=pfcFile, 2011,"\n", append=T )
  cat( file=pfcFile, "## - 2)  Last year for mean natural mortality rate","\n", append=T )
  cat( file=pfcFile, obj$lastYear,"\n", append=T )
  cat( file=pfcFile, "## - 3) Start year for average fecundity/weight-at-age in projections","\n", append=T )
  cat( file=pfcFile, 2011,"\n", append=T )
  cat( file=pfcFile, "## - 4)  Last year for average fecundity/weight-at-age in projections","\n", append=T )
  cat( file=pfcFile, obj$lastYear,"\n", append=T )
  cat( file=pfcFile, "##not currently used","\n", append=T )
  cat( file=pfcFile, "## - 5) Start year for average recruitment period in projections.","\n", append=T )
  cat( file=pfcFile, obj$initYear,"\n", append=T )  
  cat( file=pfcFile, "## - 6)   End year for average recruitment period in projections.","\n", append=T )
  cat( file=pfcFile, 2012,"\n", append=T ) ### I modify this line
  ### no idea about these options
  cat( file=pfcFile, "## - 7) start of reference period","\n", append=T )
  cat( file=pfcFile, 2000,"\n", append=T ) 
  cat( file=pfcFile, "## - 8) end of reference period","\n", append=T )
  cat( file=pfcFile, 2006,"\n", append=T ) 
  cat( file=pfcFile, "## eof","\n", append=T )
  cat( file=pfcFile, -999,"\n", append=T )


  # file.copy (assess$pfc,pfcFile,overwrite=TRUE)


  # Now just create the top-level dat file
  topDat <- paste("iscam_top.dat", sep = "")
  cat ( file=topDat, "# Main dat file ISCAM \n", sep = "", append=F)
  cat ( file=topDat, "# Updated ", date(), "\n", sep = "", append=T)
  cat ( file=topDat, "# order as: .dat, .ctl, .pfc", append = T)
  cat ( file=topDat, "\n", sep = "", append = T)
  cat ( file=topDat, datFile, "\n", append = T)
  cat ( file=topDat, ctlFile, "\n", append = T)
  cat ( file=topDat, pfcFile, "\n", append = T)

  cat ("\nMSG (writeISCAM) EM dat and ctl files created.\n", sep = "")
  return()
}


#------------------------------------------------------------------------------#
#-- Harvest Control Rule Functions                                           --#
#------------------------------------------------------------------------------#

# .calcLegalHarvRule       
# Purpose:        harvest control rule to generate total quota during mp period
# Parameters:     obj=list containing all variables and parameters
#                 necessary to compute the quota
# Returns:        the quota = total landed catch
# Source:         S.P. Cox
.calcLegalHarvRule <- function( obj )
{
  # Extract assessment quantities from harvest control rule list.
  t            <- obj$t
  legalBiomass <- obj$biomass
  remRate      <- obj$remRate[t]       # Instantaneous fishing mortality rate reference
  lowerBound   <- obj$lowerBound[t]    # Lower control point, where remRate=0
  upperBound   <- obj$upperBound[t]    # Upper control point, where remRate
  
  # Safeguards to limit F in case assessment failed in a particular year
  fail         <- obj$assessFailed     # TRUE is assessment model failed
  maxF         <- obj$maxF             # Maximum allowable F in case estimation fails.
  
  # ARK (13-Oct-13) Added catchFloor.
  catchFloor <- obj$catchFloor

  # HCR as defined in Eqs H4.2 in Table 5 of main doc.

  if( legalBiomass < lowerBound ) 
    adjF <- 0.
  if( legalBiomass >= lowerBound & legalBiomass < upperBound )
    adjF <- remRate*(legalBiomass-lowerBound)/(upperBound-lowerBound)
  if( legalBiomass >= upperBound ) 
    adjF <- remRate 
  
  # If the assessment failed, limit F to maxF
  if( fail==TRUE & adjF > maxF )
    adjF <- maxF

  # Final catch limit
  if( obj$assessMethod == .PERFECT )
    catchLim <- adjF*legalBiomass*(1.-exp(-obj$M-adjF))/(adjF + obj$M )
  else
    catchLim <- adjF*legalBiomass

  # Now check for catchFloor.
  if ( catchFloor >= 0 )
  {
    catchLim <- max( c(catchFloor, catchLim) )
    #cat( "\nMSG (.calcLegaLHarvRule) Catch floor of ", catchFloor," and catchLim = ",
    #     catchLim, "\n" )
  }

  result                <-list()
  result$precautionaryF <- adjF
  result$catchLimit     <- catchLim
  result
}     # END function .calcLegalHarvRule


# .calcHarvRuleHerringDFOMP
# Purpose:        harvest control rule to generate total quota during mp period
# Parameters:     obj=list containing all variables and parameters
#                 necessary to compute the quota
# Returns:        the quota = total landed catch
# Source:         S.P. Cox
.calcHarvRuleHerringDFOMP <- function( obj )
{
  # Extract assessment quantities from harvest control rule list.
  t            <- obj$t
  legalBiomass <- obj$biomass
  remRate      <- obj$remRate[t]       # Instantaneous fishing mortality rate reference
  lowerBound   <- obj$lowerBound[t]    # Lower control point, where remRate=0
  upperBound   <- obj$upperBound[t]    # Upper control point, where remRate
  
  # convert remRate to U
  remRate <- 1 - exp( - remRate )

  # Safeguards to limit F in case assessment failed in a particular year
  fail         <- obj$assessFailed     # TRUE is assessment model failed
  maxF         <- 1 - exp( - obj$maxF) # Maximum allowable F in case estimation fails.


  if( legalBiomass < lowerBound ) 
    adjF <- 0.
  if( legalBiomass >= lowerBound & legalBiomass < upperBound )
    adjF <- (legalBiomass-lowerBound)/legalBiomass
  if( legalBiomass >= upperBound ) 
    adjF <- remRate 
  
  # If the assessment failed, limit F to maxF
  if( fail==TRUE & adjF > maxF )
    adjF <- maxF

  # Final catch limit treating adjF as a harvest rate
  catchLim <- adjF*legalBiomass


  result                <-list()
  result$precautionaryF <- adjF
  result$catchLimit     <- catchLim
  result
}     # END function .calcHarvRuleHerringDFOMP


# .calcHCRsmoother       
# Purpose:        empirical harvest control rule to generate the quota
# Parameters:     obj=list containing current biomass and F
# Returns:        F and the quota=catch
# Source:         S.P. Cox
.calcHCRsmoother <- function( hcrList )
{
  # This rule is used for the Moving Average and Kalman filter procedures, and
  # uses the operating model biological reference points as the operational
  # control points.
  # Extract assessment quantities from harvest control rule list.
  biomass    <- hcrList$biomass
  lastCatch  <- hcrList$lastCatch 
  M          <- hcrList$M

  # Extract fixed members of input harvest control rule list
  t          <- hcrList$t
  tMP        <- hcrList$tMP
  lastCatch  <- hcrList$lastCatch      # Catch in last year of projection.
  lambda1    <- hcrList$lambda1        # Weight factor on TAC[t-1].
  remRate    <- hcrList$F              # Instantaneous fishing mortality rate.
  lowerBound <- hcrList$lowerBound[t]  # Lower HCR bound, where remRate=0
  upperBound <- hcrList$upperBound[t]  # Upper HCR bound
  #browser()
  # Constant catch
  if ( hcrList$hcrType == "constantC" )
  {
    catchLim  <- lambda1*lastCatch + (1.0-lambda1)*hcrList$consTAC
    remRate <- ifelse(catchLim < biomass, 
                      -log(1.-catchLim/biomass),
                        1.0
                      )
  }
  # Constant-F
  else if ( hcrList$hcrType == "constantF" )
  {
    tmp <- remRate
    if ( tmp < 0 )
      tmp <- 0
    catchLim  <- (1.-exp(-remRate))*biomass
    catchLim  <- lambda1 * lastCatch + (1.0-lambda1)*catchLim
  }
  else
    cat( "\nERROR (calcHCRsmoother) Invalid hcrType for assessment method\n" )

  result <- list( precautionaryF = remRate, catchLimit=max( 0, catchLim ), 
                    projExpBio=biomass )
  result
}     # END function .calcHCRsmoother

# DEPRECATED, SPC 6-Dec-2014
.calcHCRsmootherOLD <- function( obj )
{
  # This rule is used for the Moving Average and Kalman filter procedures, and
  # uses the operating model biological reference points as the operational
  # control points.
  # Extract assessment quantities from harvest control rule list.
  biomass <- obj$biomass
  remRate <- obj$F

  # Constant-exploitation rate rule
  catchLim <- remRate*biomass
  
  result <- list( precautionaryF = remRate, catchLimit=max( 0, catchLim ), 
                    projExpBio=biomass )
  result
}     # END function calcHCRsmoother


#------------------------------------------------------------------------------#
#-- Management Procedure Feedback Loop Functions                             --#
#------------------------------------------------------------------------------#

# .createMP
# Purpose:        create all operating model (om), management procedure (mp),
#                 and reference points and life history (refPtList) objects.
# Parameters:     ctlObj=list containing all variables and parameters input from
#                 the simCtlFile.
# Returns:        a new list with objects for all state variables, ref pts, etc.
# Source:         S.P. Cox
.createMP <- function( ctlObj, refPtsObj )
{
  # Add common parameters to all objects
  tMP               <- ctlObj$opMod$tMP
  nT                <- ctlObj$opMod$nT
  nAges             <- ctlObj$opMod$nAges
  nGear             <- ctlObj$opMod$nGear
  nGrps             <- ctlObj$opMod$nGrps

  # Calculate all catch, index, ages, method, and control point index values.
  tmpTimes <- .calcTimes( ctlObj )

  #----------------------------------------------------------------------------#
  # (1) BEGIN OPERATING MODEL SETUP - stored under om list                     #
  #----------------------------------------------------------------------------#

  # Set om list elements to NULL as placeholders.
  om <- list( Nalt=NULL,      Balt=NULL,      Zalt=NULL,    Bt=NULL,
              Nt=NULL,        Btot=NULL,      Ntot=NULL,    Rt=NULL,
              uCatg=NULL,     Ct=NULL,        Ctg=NULL,     Catg=NULL,
              Dt=NULL,        Dtg=NULL,       Mt=NULL,
              Ft=NULL,        Ftg=NULL,       legalHR=NULL, sublegalHR=NULL,
              legalB=NULL,    sublegalB=NULL, legalC=NULL,  legalD=NULL,
              sublegalD=NULL, Dt=NULL,        Dtg=NULL,     Itg=NULL )

  # State variables: numbers at a,t and biomass at a,t.
  om$Nalt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$Balt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$Zalt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$Bt         <- rep( NA,nT )
  om$Nt         <- rep( NA,nT )
  om$Btot       <- rep( NA,nT )
  om$Ntot       <- rep( NA,nT )
  om$Mt         <- rep( NA,nT )
  om$Rt         <- rep( NA,nT )

  # Controls: fishing mortality, catch numbers, and catch biomass.
  om$Ft         <- rep( NA,nT )
  om$Ct         <- rep( NA,nT )
  om$Dt         <- rep( NA,nT )

  om$Ftg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Ctg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Catg       <- array( data=NA, dim=c(nAges,nT,nGear) )
  om$Dtg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Datg       <- array( data=NA, dim=c(nAges,nT,nGear) )

  om$legalHR    <- rep(NA,nT)
  om$sublegalHR <- rep(NA,nT)
  om$legalB     <- rep(NA,nT)
  om$sublegalB  <- rep(NA,nT)
  om$legalC     <- rep(NA,nT)
  om$legalD     <- rep(NA,nT)
  om$sublegalD  <- rep(NA,nT)

  # Deterministic CPUE and age-proportions, fishery-independent indices come last.
  om$Itg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$uCatg      <- array( data=NA, dim=c(nAges,nT,nGear) )

  # Create variable to hold standard normal errors to be filledin initPop:
  #   deltat      recruitment deviation std normal errors
  #   omegat      recruitment process error
  #   epsilontg   biomass index obs errors by gear
  #   epsilongat  age-prop sampling errors by gear
  om$errors <- list( deltat=NULL, epsilontg=NULL, omegat=NULL, epsilongat=NULL )

  #----------------------------------------------------------------------------#
  #-- END POPULATION MODEL SETUP - obj$om complete                           --#
  #----------------------------------------------------------------------------#

  #----------------------------------------------------------------------------#
  # (2) BEGIN MANAGEMENT PROCEDURE SETUP - stored under mp list.               #
  #----------------------------------------------------------------------------#

  mp <- list( data=NULL, assess=NULL, hcr=NULL )

  mp$data$tMP   <- tMP
  mp$assess$tMP <- tMP
  mp$hcr$tMP    <- tMP

  mp$data$nT    <- nT
  mp$assess$nT  <- nT
  mp$hcr$nT     <- nT

  #----------------------------------------------------------------------------#
  # (2a) BEGIN CATCH DATA - stored under obj$mp$data                         --#
  #----------------------------------------------------------------------------#

  if ( !is.null( ctlObj$mp$data$inputCatch ) )
  {
    # Extract catch and assign to blob elements, convering to 000s mt.
    catch <- ctlObj$mp$data$inputCatch
    nRow  <- nrow( t(catch$ct) )  
    om$Ctg[1:nRow, ] <- t(catch$ct)

    cat( "\nMSG (.createMP) Extracted catch and converted units.\n" )   
    .CATCHSERIESINPUT <<- TRUE
  }
  else
  {
    cat( "\nMSG (.createMP) Calculating catch from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # (2b) BEGIN INDEX DATA - stored under obj$mp$data                         --#
  #----------------------------------------------------------------------------#

  if ( !is.null(ctlObj$mp$data$inputIndex) )
  {
    # Extract abundance indices from input file and assign 
    # to blob elements...
    indices  <- ctlObj$mp$data$inputIndex$it
    useIndex <- tmpTimes$useIndex
    om$Itg[, useIndex] <- NA

    initObs <- 1
    for( rIdx in 1:nrow(indices) )
    {
      nObs <- sum(!is.na(indices[rIdx,]))
      gIdx <- useIndex[rIdx]
      om$Itg[initObs:(initObs+nObs-1),gIdx] <- indices[rIdx,1:nObs]
      initObs <- nObs+1
    }

    om$Itg[ om$Itg < 0 ] <- NA
  
    # SPC: note the hack in using the transpose of the indices. Should
    #      probably determine this on the fly, or else make sure it is
    #      always done this way. I suppose that ncol>nrow could be used
    #      to test whether transpose is needed, except in very data-limited case.

    cat( "\nMSG (.createMP) Read stock abundance indices from file.\n" )
    .INDEXSERIESINPUT <<- TRUE
  }
  else
  {
    cat( "\nMSG (.createMP) Calculating indices from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # (2c) BEGIN AGE DATA                                                      --#
  #----------------------------------------------------------------------------#

  if ( !is.null(ctlObj$mp$data$inputAges) )
  {
    ages <- ctlObj$mp$data$inputAges$Ahat

    # Extract age comps one-by-one and put into uCatg, this code assumes that
    # the age compositions are always at EOF.
    j <- length(ages) - length( ages$ageIndex ) + 1
    for( i in ages$ageIndex )
    {
      paa          <- t( ages[[j]] )
      paa[ paa<0 ] <- NA
      minAge       <- ctlObj$mp$data$minAge
      plusGroupAge <- ctlObj$opMod$nAges
      # om$uCatg[minAge:plusGroupAge,1:ncol(paa),i] <- paa[minAge:plusGroupAge,]
      # ARK (04-Nov-10) Revised because know no fill with -1 for ages < minAge.
      om$uCatg[minAge:plusGroupAge,1:ncol(paa),i] <- paa
      j <- j + 1
    }
    
    cat( "\nMSG (.createMP) Read age data from file.\n" )    
    .AGESERIESINPUT <<- TRUE
  }
  else
  {
    cat( "\nMSG (.createMP) Calculating ages from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # (2d) BEGIN OTHER DATA                                                    --#
  #----------------------------------------------------------------------------#

  if ( !is.null(ctlObj$opModData$other) )
  {
    other <- ctlObj$opModData$other
    print( other )
    print( names(other) )
    
    cat( "\nMSG (.createMP) Read other data from file.\n" )    
    scan()
  }
  else
  {
    cat( "\nMSG (.createMP) No other calculations from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # END DATA INITIALIZATION                                                  --#
  #----------------------------------------------------------------------------#

  # Set indexOn as "attributes" of the indices, which will be carried along
  # with mp$data$Itg without having to pass them separately.

  # Stochastic CPUE and biomass surveys - identical structure to om$Itg.
  if( !.INDEXSERIESINPUT )
    Itg <- matrix( NA, nrow=nT, ncol=nGear )
  else
    Itg <- om$Itg

  # Fill index attributes
  attr( Itg, "indexOn" ) <- tmpTimes$indexOn
  attr( Itg, "t1Index" ) <- tmpTimes$t1Index
  attr( Itg, "t2Index" ) <- tmpTimes$t2Index
  mp$data$Itg  <- Itg

  # ARK (17-Nov-10) Added ItgScaled.  This is in the mp object passed for filling
  # by the call to the assessment model.  It will be of dimensions nGear by nT
  # but the return from assessMod will be length(useIndex) by t.

  ItgScaled <- matrix( NA, nrow=nGear, ncol=nT )
  mp$assess$ItgScaled <- ItgScaled
  mp$time2fail <- nT+100

  # Objects to store sample age-props and stock assessments
  if( !.AGESERIESINPUT )
    mp$data$Xatg  <- array( data=NA, dim=c(nAges,nT,nGear) )
  else
    mp$data$Xatg  <- om$uCatg

  # Stock assessment dependent outputs for a particular MP.  Will be transfered
  # to blob, so one column less than the mpdPars object in blob since no iRep.
  # Problem here is have to hard-wire ncol to match ADMB outputs for each model.
    
  nColRunStatus <- 25
  
  if ( ctlObj$mp$assess$methodId == .MOVAVG | ctlObj$mp$assess$methodId == .KALMAN )
  {
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nT-tMP+1, ncol=3 ) )
  }
  
  if ( ctlObj$mp$assess$methodId == .CAAMOD )
  {
    mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  
	  # runStatus will hold ADMB convergence stats, Hessian status, DEADFLAG,
	  # reliable parameter status etc.
	  mp$assess$runStatus <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus ) )
  }
  
  if ( ctlObj$mp$assess$methodId == .DDMOD )
  {
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  
	  # runStatus will hold ADMB convergence stats, Hessian status, DEADFLAG,
	  # reliable parameter status etc.
	  mp$assess$runStatus <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus) )
  }  

  if ( ctlObj$mp$assess$methodId == .PMOD )
  {
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )
	  mp$assess$pdfPars <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=25 ) )

	  # runStatus will hold ADMB convergence stats, Hessian status, DEADFLAG,
	  # reliable parameter status etc.
	  mp$assess$runStatus <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus) )
  }
  
	# Stock assessment outputs.
	if( ctlObj$mp$assess$methodId == .PERFECT )
	{
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1),ncol=4 ) )
	  
	  # runStatus will hold ADMB convergence stats, Hessian status, DEADFLAG,
	  # reliable parameter status etc.
	  mp$assess$runStatus <- data.frame( matrix( NA,nrow=(nT-tMP+1), ncol=nColRunStatus) )	  
	}  

  mp$assess$Ftg <- matrix( NA,nrow=nT,ncol=nGear )

  #----------------------------------------------------------------------------#
	#-- Biomass control points                                                 --#
	#----------------------------------------------------------------------------#

	# Case 1: Bmsy is basis for HCR control points; equilibrium value is used.
  if ( ctlObj$mp$hcr$statusBase == "statusBaseBmsy" & ctlObj$mp$hcr$statusSource=="statusSrceEquil" )
  {
    # Set statusBase to Bmsy for all nT years.
    mp$hcr$Bref <- rep( refPtsObj$ssbFmsy, nT )
  }
  
	# Case 2: B0 is basis for HCR control points; equilibrium value is used.
  if ( ctlObj$mp$hcr$statusBase == "statusBaseB0" & ctlObj$mp$hcr$statusSource=="statusSrceEquil" )
  {
    # Set statusBase to B0 for all nT years.  
    mp$hcr$Bref <- rep( refPtsObj$ssbF0, nT )
  }
  
  # Case 3: Bmsy is basis for HCR control points; estimated from assessment model.
  if ( ctlObj$mp$hcr$statusBase == "statusBaseBmsy" & ctlObj$mp$hcr$statusSource=="statusSrceEst" )
  {
    # Create a vector to be filled each annual estimate.
    mp$hcr$Bref <- rep( NA, nT )
  }
  
	# Case 4: B0 is basis for HCR control points; estimated from assessment model.
  if ( ctlObj$mp$hcr$statusBase == "statusBaseB0" & ctlObj$mp$hcr$statusSource=="statusSrceEst" )
  {
    # Create a vector to be filled each annual estimate.  
    mp$hcr$Bref <- rep( NA, nT )
  }
  
  #----------------------------------------------------------------------------#
	#-- Fishery rate control points                                            --#
	#----------------------------------------------------------------------------#
	
  # Case 1: Reference removal rate input directly from simCtlFile.
  if ( ctlObj$mp$hcr$remRefBase == "rrBaseFinput" )
  {
    mp$hcr$remRate <- rep( ctlObj$mp$hcr$inputF, nT )
  }
  
	# Case 2: Reference removal rate based on Fmsy; equilibrium value is used.
  if ( ctlObj$mp$hcr$remRefBase == "rrBaseFmsy" & ctlObj$mp$hcr$remRefSource == "rrSrceEquil" )
  {
    mp$hcr$remRate <- rep( refPtsObj$refPts$Fmsy, nT )
  }
  
	# Case 3: Reference removal rate based on Fmsy; estimated from assessment model.
  if ( ctlObj$mp$hcr$remRefBase == "rrBaseFmsy" & ctlObj$mp$hcr$remRefSource == "rrSrceEst" )
  {
    # Create Fmsy vector to be filled with annual estimates.
    mp$hcr$remRate <- rep( NA, nT )
  }

	# Case 4: Reference removal rate is set to F0.1.
	if ( ctlObj$mp$hcr$remRefBase == "rrBaseF01" ) 
	{
    mp$hcr$remRate <- rep( refPtsObj$F01, nT )
  }

  # Case 5: Reference removal rate is set to Fspr; %spr specified by user.
  if ( ctlObj$mp$hcr$remRefBase == "rrBaseFspr" ) 
  {
    fspr<-.getFx( ctlObj$opMod, x=ctlObj$mp$hcr$sprX)$Fx
    mp$hcr$remRate <- rep( fspr, nT )
  }
  
  # Compile list of years in which reference points will be estimated
  mp$hcr$idxCtlPts <- rep( NA, nT-tMP+1 )
  if ( ctlObj$mp$hcr$remRefSource == "rrSrceEst" | ctlObj$mp$hcr$statusSource == "statusSrceEst" )
  {
    mp$hcr$idxCtlPts <- tmpTimes$idxCtlPts
  }	

  # Create vectors for lower and upper control point bounds on Bref scale.
  mp$hcr$lowerBound <- rep( NA, nT )
  mp$hcr$upperBound <- rep( NA, nT )

  # Vector to store target Ft based on biomass assessment estimate and HCR
  mp$hcr$targetFt <- rep( NA, nT )
  
  # Vector to store paAdj value for eBioAdj, adjusted exploitable biomass.
  mp$hcr$eBioAdj <- rep( NA,nT )
  
  # Vector to store pStar = observed probablity of future decline.
  mp$hcr$pStar <- rep( NA,nT )

  # Adjust MCMC chain and thin if MLE, does not affect GUI.
  if ( ctlObj$mp$hcr$hcrType=="constantF" || ctlObj$mp$hcr$hcrType=="variableF" )
  {
    if ( ctlObj$mp$hcr$useMLE )
    {
      mp$hcr$nMCMC <- 1
      mp$hcr$nThin <- 1
    }
  }
	
	if( ctlObj$mp$assess$methodId == .PMOD )
	{
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1),ncol=25 ) )	  
	}
	
	if( ctlObj$mp$assess$methodId == .DDMOD )
	{
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1),ncol=25 ) )	  
	}	
	
	if( ctlObj$mp$assess$methodId == .CAAMOD )
	{
	  mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1),ncol=25 ) )	  
	}
	
  # Retrospective biomass estimates and recruitment.
	mp$assess$retroExpBt <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
	mp$assess$retroRt    <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  mp$assess$retroMt    <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )

  # Build the return object.
  obj <- list( ctlList=ctlObj, om=om, mp=mp, refPtList=refPtsObj ) 

  # Return the main list object
  return( obj )
}     # END .createMP function


# .mgmtProc        
# Purpose:        runs "nReps" replications of simulation model
# Parameters:     obj=complete simulation object from createMP
# Returns:        the "blob" of all simulated states and data
#                 for "nReps" of "nT" years each
# Source:         S.P. Cox
.mgmtProc <- function( objRef )
{
  # writeProgress   Private function     
  # Purpose:        writes current status of simulation
  # Parameters:     i=current rep; n=total reps
  # Returns:        nothing
  # Source:         S.P. Cox, modified A.R. Kronlund (07-Feb-10)
  writeProgress <- function( i , n )
  {
     if ( i %% 1 == 0 )
      cat( "\n  Completed ", i, " of ", n, " replicates\n" )
     if ( i==n )
      cat( "\n(mgmtProc) Feedback loop completed\n" )
  }     # END function writeProgress  
  
  obj <- deref( objRef )
  
  ctlList <- obj$ctlList
  
  # Extract simulation dimensions
  nAges  <- ctlList$opMod$nAges
  nReps  <- ctlList$gui$nReps
  tMP    <- ctlList$opMod$tMP
  nT     <- ctlList$opMod$nT
  nGear  <- ctlList$opMod$nGear

  #------------------------------------------------------
  # Create blob objects pars,om,and mp to store all simulation results
  #------------------------------------------------------

  # Operating model objects to attached to blob for performance analysis. 
  om <- list( iSeed      = rep( NA, nReps ),
              Bt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Nt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Btot       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ntot       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Mt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),               
              Rt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ct         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ctg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              Dt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Dtg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              Ftg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              uCatg      = array( data=NA,dim=c(nReps,nAges,nT,nGear) ),
              legalHR    = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalHR = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalB     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalB  = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalC     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalD     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalD  = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              Itg        = array( data=NA,dim=c(nReps,nT,nGear) )
            )
  
  # Management procedure object to be attached to blob list. 
  mp                  <- list(   data=NULL, assess=NULL, hcr=NULL )
  
  mp$data$Itg         <- array( data=NA, dim=c(nReps,nT,nGear) )
  mp$data$pat         <- array( data=NA, dim=c(nReps,nAges,nT,nGear) )  
  mp$assess$ItgScaled <- array( data=NA, dim=c(nReps,nT,nGear) )
  
  mp$data$Xatg        <- array( data=NA, dim=c(nReps,nAges,nT,nGear) )
  mp$assess$pat       <- array( data=NA, dim=c(nReps,nAges,nT,nGear) )

  mp$assess$Rt        <- matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=(nT+2) )

  # Base fishing rate and biomass for control points points used in harvest control rule
  mp$hcr$Fref         <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  mp$hcr$Bref         <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  
  # Control rule bounds vary among years if Fref or Bref estimated.
  mp$hcr$lowerBound   <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  mp$hcr$upperBound   <- matrix( NA,nrow=nReps,ncol=(nT+1) )

  # paAdj value for eBioAdj.
  mp$hcr$eBioAdj      <- matrix( NA,nrow=nReps,ncol=(nT+1) )
  
  # Target removal rate based on harvest control rule and estimated biomass
  mp$hcr$targetFt     <- matrix( NA,nrow=nReps,ncol=(nT+1) )

  # Target removal rate based on harvest control rule and estimated biomass
  mp$hcr$precautionaryFt <- matrix( NA,nrow=nReps,ncol=(nT+1) )

  # Assessment method options within mp
  if ( ctlList$mp$assess$methodId == .PMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.
      mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
      mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  

      mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )
  }
	
  if ( ctlList$mp$assess$methodId == .CAAMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.
	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
	  mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  
    
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )
  }
	
  if( ctlList$mp$assess$methodId == .DDMOD )
  {
    # This is for blob - needs to be 1 more column than createMP version for iRep.
	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$mpdPars)+1 ) )
                                       
	  mp$assess$pdfPars   <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$pdfPars)+1 ) )	  
    
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=nReps*(nT-tMP+1),
                                       ncol=ncol(obj$mp$assess$runStatus)+1 ) )      
  }	
	
  if( ctlList$mp$assess$methodId == .MOVAVG | ctlList$mp$assess$methodId == .KALMAN )
  {
     mp$assess$mpdPars <- data.frame( matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=2))
     mp$assess$pdfPars <- mp$assess$mpdPars
 	  
 	  # These methods can have DEADFLAG, perhaps other things so just make runStatus the same.
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=1 ) )  	  
  }
	
	# Perfect information.
  if( ctlList$mp$assess$methodId == .PERFECT )
  { 
	  mp$assess$mpdPars   <- data.frame( matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=5 ) ) 

 	  # These methods can have DEADFLAG, perhaps other things so just make runStatus the same.
    mp$assess$runStatus <- data.frame( matrix( NA, nrow=(nReps*(nT-tMP+1)), ncol=1 ) )  	  
  }

  # Retrospective statistics.
  mp$assess$retroExpBt   <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroSpawnBt <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroRt      <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroMt      <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  
  # Not strictly assessment values, but if Decline Risk HCR have to store this...
  mp$assess$trendBio     <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$assess$trendVal     <- matrix( NA, nrow=nReps, ncol=(nT+1) )

  #----------------------------------------------------------------------------#
  #-- Initialize the blob object to hold all simulation results.             --#
  #----------------------------------------------------------------------------#

  blob <- list( ctlList=ctlList, om=om, mp=mp ) 
  
  #----------------------------------------------------------------------------#
  
  # Loop over simulation replicates.
  .LASTSOLUTION <<- NULL
  
  cat( "\nMSG (.mgmtProc) Running feedback loop...\n" ) 
  for ( i in 1:nReps )
  {
    # Initialize .DEADFLAG and .FISHERYCLOSED
    .DEADFLAG      <<- FALSE  
    .FISHERYCLOSED <<- FALSE  
  
    # Pass the replicate index.
    ctlList$opMod$rep <- i
  
    # Set random number seed in a way that can be reconstructed later.
    set.seed( ctlList$opMod$rSeed + i )

    blob$om$iSeed[i] <- ctlList$opMod$rSeed + i

    startTime <- proc.time()[3]    
    t1 <- proc.time()[3]

    # Initialise simulation object for t=1,2,...tMP-1 pre-MP period.
    obj <- deref( .initPop( as.ref(obj) ) )

    cat( "\nMSG (.mgmtProc) Elapsed time in .initPop = ", proc.time()[3]-t1, "\n" )
    
    # Initialize annual biomass assessments and target F
    #obj$mp$hcr$targetFt <- rep( NA,nT )
    obj$mp$hcr$precautionaryFt <- rep( NA,nT )

    # Initialize stock assessment model outputs (K.Holt; 07-Aug-09)
	if ( ctlList$mp$assess$methodId == .PERFECT )
	{
	    obj$mp$assess$mpdPars <- data.frame( matrix( NA, nrow=(nT-tMP+1), ncol=4 ) )
  	}
  	
    if ( ctlList$mp$assess$methodId == .MOVAVG )
    {
        obj$mp$assess$mpdPars <- data.frame( matrix( NA, nrow=(nT-tMP+1), ncol=4 ) )
    }

	if ( ctlList$mp$assess$methodId == .PMOD )
	{
	    obj$mp$assess$mpdPars <- data.frame( matrix( NA, nrow=(nT-tMP+1), ncol=25 ) )
  	}
  	
	  if ( ctlList$mp$assess$methodId == .CAAMOD )
    {
      obj$mp$assess$mpdPars <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=25 ) )
      obj$mp$assess$Rt      <- matrix( NA, nrow=(nT-tMP+1), ncol=(nT+1) )
      obj$mp$assess$pat     <- matrix( NA, nrow=nAges, ncol=nT )
    }
 
    # Loop over years tMP:nT projecting feedback management strategy
    if( nT > tMP ) 
    {
      for ( t in tMP:nT )
      {
        # Fill om and mp objects.
        obj <- .updatePop( obj, t )
      }# end year loop. 
    }
    
    # Fill current row of the blob for operating model components.
    blob$om$Bt[i,]         <- c( i, obj$om$Bt )
    blob$om$Nt[i,]         <- c( i, obj$om$Nt )
    blob$om$Btot[i,]       <- c( i, obj$om$Btot )
    blob$om$Ntot[i,]       <- c( i, obj$om$Ntot )
    blob$om$Mt[i,]         <- c( i, obj$om$Mt )    
    blob$om$Rt[i,]         <- c( i, obj$om$Rt )
    blob$om$Ct[i,]         <- c( i, obj$om$Ct )
    blob$om$Dt[i,]         <- c( i, obj$om$Dt )
    blob$om$Ctg[i,,]       <- obj$om$Ctg
    blob$om$Dtg[i,,]       <- obj$om$Dtg
    blob$om$Ftg[i,,]       <- obj$om$Ftg
    blob$om$Itg[i,,]       <- obj$om$Itg
    blob$om$uCatg[i,,,]    <- obj$om$uCatg
    blob$om$legalHR[i,]    <- c(i,obj$om$legalHR)
    blob$om$sublegalHR[i,] <- c(i,obj$om$sublegalHR)
    blob$om$legalB[i,]     <- c(i,obj$om$legalB)
    blob$om$sublegalB[i,]  <- c(i,obj$om$sublegalB)
    blob$om$legalC[i,]     <- c(i,obj$om$legalC)
    blob$om$legalD[i,]     <- c(i,obj$om$legalD)
    blob$om$sublegalD[i,]  <- c(i,obj$om$sublegalD)
    
    # Fill current row of the blob for mp components. 
    blob$mp$data$Itg[i,,]  <- obj$mp$data$Itg
    
    # Added ItgScaled to blob.
    # LHS is dim(nRep,nT,nGear), but RHS is dim(nGear,nT)...
    blob$mp$assess$ItgScaled[i,,] <- t(obj$mp$assess$ItgScaled)
    
    blob$mp$data$Xatg[i,,,] <- obj$mp$data$Xatg
    blob$mp$hcr$precautionaryFt[i,] <- c( i, obj$mp$hcr$precautionaryFt )

    # Fill in the blob with estimated or true HCR pars
    blob$mp$hcr$Bref[i,] <- c( i, obj$mp$hcr$Bref )
    blob$mp$hcr$Fref[i,] <- c( i, obj$mp$hcr$remRate )
    
    blob$mp$hcr$lowerBound[i,] <- c( i, obj$mp$hcr$lowerBound )
    blob$mp$hcr$upperBound[i,] <- c( i, obj$mp$hcr$upperBound )
    
    # This is just the last one done!  use blob$mp$hcr elements for replicates.
    blob$mp$hcr$specs <- obj$mp$hcr[1:10]  # include specification used for hcr

    # Assessment outputs: fill rows of blob corresponding to multiple years
    # to examine retrospective behaviour.
    startRow  <- (i-1)*(nT-tMP) + i
    endRow    <- startRow + (nT-tMP)
    reps      <- rep( i, (nT-tMP+1) )

    if( ctlList$mp$assess$methodId != .MOVAVG )
    {
        blob$mp$assess$mpdPars[ startRow:endRow, ]      <- cbind( reps, obj$mp$assess$mpdPars )
        blob$mp$assess$runStatus[ startRow:endRow, ]    <- cbind( reps, obj$mp$assess$runStatus )
        blob$mp$assess$retroSpawnBt[ startRow:endRow, ] <- cbind( reps, obj$mp$assess$retroSpawnBt )
        blob$mp$assess$retroRt[ startRow:endRow, ]      <- cbind( reps, obj$mp$assess$retroRt )        
        blob$mp$assess$retroMt[ startRow:endRow, ]      <- cbind( reps, obj$mp$assess$retroMt )        
    }

    blob$mp$assess$retroExpBt[ startRow:endRow, ]   <- cbind( reps, obj$mp$assess$retroExpBt )
    
    diffTime <- proc.time()[3] - startTime
    writeProgress( i, nReps )
    cat( "Elapsed time = ", diffTime, "\n" )
  
  }     # End simulation loop.
  
  # Label columns the OM components of the blob.
  tmpNames <- paste("B",c(1:nT),sep="")
  colnames( blob$om$Bt ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("N",c(1:nT),sep="")
  colnames( blob$om$Nt ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("Btot",c(1:nT),sep="")
  colnames( blob$om$Btot ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("Ntot",c(1:nT),sep="")
  colnames( blob$om$Ntot ) <- c( "iRep", tmpNames )

  tmpNames <- paste("M",c(1:nT), sep="")
  colnames( blob$om$Mt ) <- c( "iRep", tmpNames )  
  
  tmpNames <- paste("R",c(1:nT),sep="")
  colnames( blob$om$Rt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("lHR",c(1:nT),sep="")
  colnames( blob$om$legalHR ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("slHR",c(1:nT),sep="")
  colnames( blob$om$sublegalHR ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("lBiomass",c(1:nT),sep="")
  colnames( blob$om$legalB ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("slBiomass",c(1:nT),sep="")
  colnames( blob$om$sublegalB ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("lCatch",c(1:nT),sep="")
  colnames( blob$om$legalC ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("lDisc",c(1:nT),sep="")
  colnames( blob$om$legalD ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("slDisc",c(1:nT),sep="")
  colnames( blob$om$sublegalD ) <- c( "iRep", tmpNames )
  
  # ARK These are arrays, dimensioned (nRep,nT-tMP+1,nGear).
  #     There is some question of whether iRep should be added.
  #     For now, assume nobody will diddle the sort order and that the
  #     replicates can be indexed as written...
  
  tmpNames <- paste("Itg",c(1:nT),sep="")
  dimnames( blob$om$Itg ) <- list( NULL, tmpNames, NULL )
  
  tmpNames <- paste("F",c(1:nT),sep="")
  dimnames( blob$om$Ft ) <- list( NULL, tmpNames, NULL )
  
  # Label columns the MP components of the blob.
  tmpNames <- paste("Itg",c(1:nT),sep="")
  dimnames( blob$mp$data$Itg ) <- list( NULL, tmpNames, NULL )

  tmpNames <- paste( "ItgScaled",c(1:nT),sep="" )
  dimnames( blob$mp$assess$ItgScaled ) <- list( NULL, tmpNames, NULL )

  if( ctlList$mp$assess$methodId != .MOVAVG )
  {
      tmpNames <- names(obj$mp$assess$mpdPars)
      colnames( blob$mp$assess$mpdPars ) <- c( "iRep", tmpNames )

      tmpNames <- names(obj$mp$assess$pdfPars)
      colnames( blob$mp$assess$pdfPars ) <- c( "iRep", tmpNames )

      tmpNames <- names( obj$mp$assess$runStatus )
      colnames( blob$mp$assess$runStatus ) <- c( "iRep", tmpNames )

      tmpNames <- paste( "retroRt", c(1:nT), sep="" )
      colnames( blob$mp$assess$retroRt )<-c("iRep", "tStep", tmpNames ) 

      tmpNames <- paste( "retroMt", c(1:nT), sep="" )
      colnames( blob$mp$assess$retroMt )<-c("iRep", "tStep", tmpNames ) 
      
      tmpNames <- paste("retroSpawnBt",c(1:nT),sep="")
      colnames( blob$mp$assess$retroSpawnBt )<-c("iRep", "tStep", tmpNames )    
  }
  tmpNames <- paste("retroExpBt",c(1:nT),sep="")
  colnames( blob$mp$assess$retroExpBt )<-c("iRep", "tStep", tmpNames )
  
  # Label columns the HCR components of the blob.
  tmpNames <- paste("precF",c(1:nT),sep="")
  colnames( blob$mp$hcr$precautionaryFt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("Bref",c(1:nT),sep="")
  colnames( blob$mp$hcr$Bref ) <- c( "iRep", tmpNames )

  tmpNames <- paste("Fref",c(1:nT),sep="")
  colnames( blob$mp$hcr$Fref ) <- c( "iRep", tmpNames )
  
  # ARK (01-Sep-13) Should change to lowerBound and upperBound.                              
  tmpNames <- paste("LB",c(1:nT),sep="")
  colnames( blob$mp$hcr$lowerBound ) <- c( "iRep", tmpNames )

  tmpNames <- paste("UB",c(1:nT),sep="")
  colnames( blob$mp$hcr$upperBound ) <- c( "iRep", tmpNames )
  
  return( blob )    
}

# .initPop       
# Purpose:        initialise all operating model (om), management procedure (mp),
#                 and parameter (pars) objects, for period 1 - tMP.
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui and createMP
# Returns:        a new list with objects for all state variables, ref pts, etc.
#                 Operating model states will be initialised for pre-MP period such
#                 that spawning biomass Bt is approx. initDepTarg*B0, where initDepTarg
#                 is input from the gui
# Source:         S.P. Cox
.initPop <- function( objRef )
{

  # Dereference objRef passed by reference using package "ref".
  obj <- deref( objRef )

  ctlList <- obj$ctlList
  tmpTimes <- .calcTimes( ctlList )
  
  # Get simulation dimensions.
  nAges <- ctlList$opMod$nAges
  nT    <- ctlList$opMod$nT
  tMP   <- ctlList$opMod$tMP
  rep   <- ctlList$opMod$rep
  nGear <- ctlList$opMod$nGear
  nGrps <- ctlList$opMod$nGrps
  
  #----------------------------------------------------------------------------#
  #-- CATCH DATA                                                             --#
  #----------------------------------------------------------------------------#
   
  if ( !is.null(ctlList$mp$data$inputCatch) )
  {
    # Extract catch and assign to blob elements...
    catch <- ctlList$mp$data$inputCatch
    nRow  <- nrow(t(catch$ct))
     
    # Scale catch to 000s metric tons.
    obj$om$Ctg[1:nRow, ] <- t(catch$ct)
    cat( "\nMSG (.initPop) Extracted catch and converted units.\n" )     
    .CATCHSERIESINPUT <<- TRUE         
  }
  
  if ( is.null( ctlList$mp$data$inputCatch ) )
  {
    cat( "\nMSG (.initPop) Calculating catch from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  #-- Generate natural mortality process errors                              --#
  #
  # ARK (13-Oct-13) If this is here, a different set of random values will   --#
  # fill the recruitment devs, and therefore results differ from 2010.       --#
  #----------------------------------------------------------------------------#

  RandomWalkM <- TRUE
  if ( RandomWalkM )
  {
      # Natural mortality: lag-1 autocorrelated, log-normal process errors,
      #                    No linear trend, or 50% higher during pulse yrs
      Mt        <- vector( mode="numeric", length=nT )
      
      # Random walk M scaled to mean==1
      deltaM    <- rnorm( n=nT,mean=0,sd=1 )      # Random normal deviates.
      sigmaM    <- ctlList$opMod$sigmaM           # Natural mortality rate CV.
      gammaM    <- ctlList$opMod$gammaM           # Lag-1 autocorr in M
      ranM      <- .fillRanWalk( gamma=gammaM, sigma=sigmaM, deltat=deltaM )
      ranM      <- ranM/mean( ranM ) 
      Mt[1]     <- ctlList$opMod$M                # Scale Mt[1] to equal input mean M.
      Mt[2:nT]  <- Mt[1] * ranM[ c(2:nT) ]
      endM      <- ctlList$opMod$endM

      # Trend M
      trendM    <- (log(endM) - log( Mt[1]) ) / (nT-1)
      Mt[2:nT]  <- Mt[1]*exp( trendM*c(2:nT) ) * ranM[2:nT]

      # PulseM
      if ( ctlList$opMod$pulseMFrq != 0 ) 
        pPulse   <- 1/ctlList$opMod$pulseMFrq                          
      else pPulse <- 0
      nPulse   <- rbinom( n=1, size=(nT-tMP + 1), prob=pPulse ) # number of events to sample
      pulseYrs <- sample( x=tMP:nT, size=nPulse, replace=FALSE )# sampled years 
      Mt[pulseYrs] <- Mt[pulseYrs] * ctlList$opMod$pulseMSize 
      
      # Output
      obj$om$Mt <- Mt
  }
  else
  {
      obj$om$Mt <- rep( ctlList$opMod$M[1:nGrps], nT )
  }

  if( !is.null( ctlList$opMod$estMdevs) )
  {
    Mdevs <- ctlList$opMod$estMdevs$log_m_devs
    for(t in 2:(tMP-1) ) 
      obj$om$Mt[t] <- obj$om$Mt[t-1]*exp(Mdevs[t-1])

    if( !is.null(ctlList$opMod$kYearsMbar) )
    {
      k <-  ctlList$opMod$kYearsMbar
      Mbar <- mean( obj$om$Mt[ (tMP - k):(tMP - 1 ) ] )
      endM <- Mbar
    }

    if(is.null(ctlList$opMod$endMphase))
    {
      trendM    <- (log(endM) - log( obj$om$Mt[t]) ) / (nT - t)
      obj$om$Mt[tMP:nT] <- obj$om$Mt[tMP-1] * ranM[tMP:nT] * exp( trendM * (1:(nT-tMP+1)) )
    } else {
      phaseTime <- ctlList$opMod$endMphase
      trendM    <- (log(endM) - log( obj$om$Mt[t]) ) / phaseTime
      obj$om$Mt[tMP:(tMP+phaseTime-1)] <- obj$om$Mt[tMP-1] * ranM[tMP:(tMP+phaseTime-1)] * exp( trendM * (1:phaseTime) )
      obj$om$Mt[(tMP + phaseTime):nT] <- obj$om$Mt[tMP + phaseTime-1] * ranM[(tMP+phaseTime):nT]
    }


    # Now modify the projected Mt time series to begin at the end of 
    # the history    
    obj$om$Mt[pulseYrs] <- obj$om$Mt[pulseYrs] * ctlList$opMod$pulseMSize
  }

  if( !is.null( ctlList$opMod$obsWtAge) )
  {
    obj$om$Wta <- matrix(0, ncol = nAges, nrow = nT )
    obj$om$Wta[1:(tMP),2:nAges] <- ctlList$opMod$obsWtAge$wt_obs

    obj$om$Wta[(tMP+1):nT,]   <- matrix(  obj$refPtList$Wal, nrow = (nT - tMP), 
                                      ncol = nAges, byrow = T )
  }


  # Draw tsBoot indices anyways, then use in projected Mt
  # matrices (this way the random draws for each run are the same)
  if(!is.null(ctlList$opMod$kBoot))
    initBoot <- tMP - ctlList$opMod$kBoot
  else initBoot <- 1
  bootMt <- tsBoot( x = obj$om$Mt[initBoot:(tMP-1)], 
                    length = length((tMP):nT), 
                    tolSD = ctlList$opMod$bootTolSD,
                    tolMult = ctlList$opMod$bootTolMult )

  if( ctlList$opMod$useBoot )
  {
    obj$om$Mt[tMP:nT]   <- bootMt
  }

  #----------------------------------------------------------------------------#
  #-- Generate recruitment process errors                                    --#
  #----------------------------------------------------------------------------#

  # Recruitment standard normals for t = 1,..nT.
  deltat  <- rnorm( n=nT,mean=0,sd=1 )

  # Generate lag-1 autocorrelated recruitment deviations.
  gammaR <- ctlList$opMod$gammaR # lag-1 autocorrel.
  sigmaR <- ctlList$opMod$sigmaR # std error
  
  # This should create (autocorrelated) sequence of rec. devs.
  omegat <- .calcRdevs( gammaR=gammaR,sigmaR=sigmaR,deltat=deltat )
  # rep <- lisread("scal_base.rep")
  
  if ( !is.null(ctlList$opMod$estRecDevs) )
  {
    scalrecDevs <- ctlList$opMod$estRecDevs
    recDevs <- scalrecDevs$delta
    # the offset allows for generating recDevs in years < tMP - 1
    recDevsOffset <- ctlList$opMod$recDevsOffset
    
    # Finally replace recruitment deviations from file in the omegat.
    omegat[(1+recDevsOffset):(length(recDevs)+recDevsOffset)] <- exp( recDevs )

    cat( "\nMSG (.initPop) Read recruitment deviations from file...\n" )
    .RECSERIESINPUT <<- TRUE
  }
  # Input numbers-at-age and length for the historical period
  if( !is.null( ctlList$opMod$repFile ) )
  {
    tmpN <- ctlList$opMod$repFile$N[1,]
    tmpN <- c(exp(ctlList$opMod$M)*tmpN[1],tmpN)
  }
  
  if ( is.null( ctlList$opMod$estRecDevs ) )
  {
    cat( "\nMSG (.initPop) Calculating recruitment deviations from operating model.\n" )
  }  
  
  #----------------------------------------------------------------------------#
  #-- Generate observation errors                                            --#
  #----------------------------------------------------------------------------#
  
  # Generate the observation errors for the two periods (t1,t2). tauIndexg is for
  # abundance indices and tauAgeg is for age proportions, both are gear-specific.
  
  # ARK and SPC (10-Nov-10):
  # 1. tauAgeg    has length 3, i.e., age series c(1,4,5),
  # 2. tauIndex g has length 3, i.e., index series c(1,4,5). 

  # ARK (02-Sep-13) Note that mseR-Finfish has ability to specify CVs and random
  # CVs according to inverse gamma.  We'll leave this for now in mseR-Sable.
  
  tauIndexg <- ctlList$opMod$tauIndexg
  tauAgeg   <- ctlList$opMod$tauAgeg     
  
  t1Index   <- attributes( obj$mp$data$Itg )$t1Index
  t2Index   <- attributes( obj$mp$data$Itg )$t2Index

  # Assign std normal observation errors by gear
  epsilontg  <- matrix( NA, nrow=nT, ncol=nGear )
  epsilongat <- array( data=NA, dim=c(nGear,nAges,nT) )

  useAges  <- tmpTimes$useAges
  useIndex <- tmpTimes$useIndex
  
  for( i in 1:length(useIndex) )
  {
    # Normally-distributed index errors
    epsilontg[(1:nT),useIndex[i]] <- rnorm(nT,0,1)
  }
  
  for ( i in 1:length(useAges) )
  {
    # Normally-distributed logistic age-proportion errors
    # ARK (14-Oct-13) This appears wrong to me - changed to sd=1.
    
    #epsilongat[useAges[i],,] <- matrix( rnorm( nAges*nT, mean=0, sd=tauAgeg[i] ),
    #                                    nrow=nAges, ncol=nT )
    epsilongat[useAges[i],,] <- matrix( rnorm( nAges*nT, mean=0, sd=1 ),
                                        nrow=nAges, ncol=nT )
    
  }

  # Assign to "errors" object within operating model list.
  obj$om$errors <- list( omegat=omegat,
                         deltat=deltat,
                         epsilontg=epsilontg,
                         epsilongat=epsilongat )


  #----------------------------------------------------------------------------#
  #-- Initialize population and fishery history                              --#
  #----------------------------------------------------------------------------#

  # Given target depletion level and stochastic recruitment find the catches
  # Ct[1:tMP-1] that give stock status as near as possible to target depletion at tMP-1.
  # We need to do this because the operating model needs to be run for 1:tMP to
  # generate all the data and states for that time. We CAN skip the optimization.

  tmp <- deref( .solveInitPop( as.ref(obj), tMP ) )

  # Now populate operating model object with states, data, etc.
  obj$om            <- tmp$om
  obj$mp$data$Itg   <- tmp$mp$data$Itg     # Stock abundance indices.
  obj$mp$data$Xatg  <- tmp$mp$data$Xatg    # Ageing data.
  
  # ARK (02-Sep-13) I don't know what time2fail does...
  obj$mp$time2fail  <- nT + 100
  
  return( as.ref(obj) )
}     # END function .initPop.


.fnBaranov <- function( fPars, B, S, P, D, M, C, nIter=50, lam=0.35, quiet=TRUE )
{
	F <- exp( fPars ) # input values are log-scale to contrain > 0
	nGear  <- length(F)
	f      <- 0.
	J      <- rep( 0., nGear^2 )
	dim(J) <- c( nGear,nGear )
  nTimes <- 0
	falg <- array( data=NA, dim=dim(S) )
  for( g in 1:nGear )
    falg[,,g] <- S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
	Z <- M + matrixsum( falg )
	for( g in 1:nGear )
	{
    # compute function fg(F) <- C - Cg(F)
		Bprime <- B*S[,,g]*(1.-P[,,g])
    tmp  <- Bprime*F[g]*(1.-exp(-Z))/Z
    f[g] <- (C[g] - sum(tmp))^2. 
  }
  return( sum(f) )
}     # END function .fnBaranov

#dFD(f,"F")
# (s * p * B * (1 - exp(-(s * F * (p + (1 - p) * D) + M))) + s * 
#     F * p * B * (exp(-(s * F * (p + (1 - p) * D) + M)) * (s * 
#     (p + (1 - p) * D))))/(s * F * (p + (1 - p) * D) + M) - s * 
#     F * p * B * (1 - exp(-(s * F * (p + (1 - p) * D) + M))) * 
#     (s * (p + (1 - p) * D))/(s * F * (p + (1 - p) * D) + M)^2
# >
.solveBaranov <- function( F, B, S, P, D, M, C, nIter=500, lam=0.35, quiet=TRUE )
{
  tmp <- 0.
  for( g in 1:length(F) )
  {
    
    Bprime <- B*S[,,g]*(1.-P[,,g])
    tmp[g]  <- C[g]/sum(Bprime)
    #tmp[g]  <- -log( 1.0-tmp[g] )
  }
  
  #tryF <- try( log((-1.)*log(1-tmp)) )
  #tryF[ tryF=="NaN" ] <- log(0.5)
  #if( length(tryF) > 0 )
  #  initLogF <- tryF
  #else
  #  initLogF <- log(F)
  #initLogF <- tmp
  #if( .PAUSE ) browser()
  #solveF <- optim( par=initLogF, fn=fnBaranov, method="BFGS", control=list(maxit=100),
  #                 B=B,S=S,P=P,D=D,M=M, C=C )
  #return( exp(solveF$par) )
  
  return(tmp)
}     # END function .solveBaranov

# Numerical solution to catch equations
.solveBaranovNum <- function( B, S, P, D, F, M, C, nIter=10, lam=0.5, quiet=TRUE )
{
    nGear  <- length(F)
    if(class(B) == "numeric") B <- as.matrix(B, nrow = length(B), ncol = 1)
    # Znew <- matrix ( M, nrow = nrow ( B), ncol = ncol(B), byrow=TRUE )
    # for( g in 1:(nGear-2) ) # SJ: Again, too path specific.
    # {    
    #     Bprime <- B*S[,,g]*(1.-P[,,g])
    #     F[g]   <- -1. * log (1 -  C[g]/sum(Bprime) )
    #     Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
    # }
    diff <- function( F )
    {
        Z <- matrix(M, byrow = TRUE, ncol = ncol(B), nrow = nrow(B))
        Fg <- exp(F)
        for( g in 1:(nGear-2) )
        {
            Z <- Z + S[,,g]*Fg[g]
        }
        f <- 0
        for( g in 1:(nGear-2) )
        {
            Bprime <- B*(1.-P[,,g])
            tmp    <- Bprime*S[,,g]*Fg[g]*(1.-exp(-Z))/Z
            f   <- f + (C[g] - sum(tmp))^2
        }  
        return(f)    
    }
    
    numF <- try(optim( par=rep(0,nGear-2), fn=diff ))
    if(class(numF) == "try-error") browser()

    Fs <- exp(numF$par)
    #if( tt >= 17 ) browser()
    return( c(Fs,0,0) )
   
}     # END function .solveBaranovNum


.solveBaranovNoDisc <- function( B, S, F, M, C, nIter=5, lam=0.5, quiet=TRUE )
{
  nGear  <- length(F)
  Znew <- M

    for( g in 1:(nGear-2) )
    {    

        Bprime <- B*S[,,g]
        F[g]   <- C[g]/sum(Bprime)
        Znew   <- Znew + S[,,g]*F[g]
        
    }
    for( iter in 1:nIter )
    {
        Z <- Znew; Znew <- M
        for( g in 1:(nGear-2) )
        {
            Bprime <- B*S[,,g]
            tmp    <- Bprime*F[g]*(1.-exp(-Z))/Z
            f      <- C[g] - sum(tmp); tmp <- 0
            tmp    <- S[,,g]*( exp(-Z)-F[g]/Z^2 )*Bprime -
                      ( 1.-exp(-Z) )*Bprime/Z       
            J      <- sum(tmp); tmp <- 0.
            F[g]   <- F[g] - f/J
            Znew   <- Znew + S[,,g]*F[g]
            #if( baranovTime > 20 ) browser()
        }
    } 
    #F[ F > 1.0 ] <- 1.0 
    # could also return Z so it only needs to be computed once.
    #browser()
    return(F) 
}     # END function .solveBaranov1


.solveBaranov1 <- function( B, S, P, D, F, M, C, nIter=5, lam=0.5, quiet=TRUE )
{
  nGear  <- length(F)
  Znew <- M

    for( g in 1:(nGear-2) )
    {    

        Bprime <- B*S[,,g]*(1.-P[,,g])
        F[g]   <- C[g]/sum(Bprime)
        Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
        
    }
    for( iter in 1:nIter )
    {
        Z <- Znew; Znew <- M
        for( g in 1:(nGear-2) )
        {
            Bprime <- B*S[,,g]*(1.-P[,,g])
            tmp    <- Bprime*F[g]*(1.-exp(-Z))/Z
            f      <- C[g] - sum(tmp); tmp <- 0
            tmp    <- S[,,g]*( D[g]*P[,,g]-P[,,g]+1. )*( exp(-Z)-F[g]/Z^2 )*Bprime -
                      ( 1.-exp(-Z) )*Bprime/Z				
            J      <- sum(tmp); tmp <- 0.
            F[g]   <- F[g] - f/J
            Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
            #if( baranovTime > 20 ) browser()
        }
    }	
    #F[ F > 1.0 ] <- 1.0 
    # could also return Z so it only needs to be computed once.
    #browser()
    return(F)	
}     # END function .solveBaranov1


# .solveInitPop       
# Purpose:        optimisation algorithm to initialise the operating 
#                 model spawning biomass Bt to target initial depletion at tMP-1. 
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui, createMP, and initPop; tMP=time when management procedure begins
# Returns:        a list with initialised operating model states and data for
#                 period 1,2,tMP. Biomass Bt should be near initTargDep as set in gui
# Source:         S.P. Cox
.solveInitPop <- function( objRef, tMP )
{
  # might want to have an F and a C version of this function for initializing

  # runModel       
  # Purpose:        Private function to run operating model and calculate objective
  #                 function based on target depletion and cumulative catch
  # Parameters:     pars=two multipliers that scale Ft to M for t=1,2,...tMP-1
  # Returns:        List containing operating model states and objective function 
  #                 value f=squared deviation from initDepTarg plus log(cumulative catch). 
  #                 Multiplier 1000 can be adjusted to get better precision
  # Source:         S.P. Cox
  runModel <- function( pars, objRef )
  {
    if( !.CATCHSERIESINPUT )
    {
      initCatch <- exp(pars)    
      obj <- deref( objRef )
      # target depletion level - input from GUI
      initDepTarg <- obj$opMod$initDepTarg
      tMP         <- obj$opMod$tMP
      tt          <-seq( from=1,to=(tMP-1),by=1 )
      TT          <- tMP - 1
      obj$om$Ctg[1:(tMP-1),] <- outer(initCatch,obj$opMod$allocProp)          
    }

    #cat( "\nPre opMod call in initPop...\n" )
    #print( obj$om$Ctg )
    
    # Run model for the pre-MP period
    for ( t in 1:(tMP-1) )
    {
      obj <- ageLenOpMod( as.ref(obj),t )
    }

    obj <- deref( obj )
    
    f <- NA
    if( !is.null(pars) )
    {
      # realised depletion
      initBio <- obj$opMod$B0
      termBio <- obj$om$Bt[(tMP-1)]
      realDep <- termBio/initBio

      # objective function components
      f        <- (realDep - initDepTarg)^2.
      AvgCatch <- mean( rowSums( obj$om$Ctg[1:(tMP-2),] ) )
    }
    result   <- obj
    result$f <- f
    return( as.ref(result) )

  }     # END function runModel
  
  # getObjFunctionVal       
  # Purpose:        Private function to run operating model and extract objective function
  # Parameters:     pars=two multipliers that scale Ft to M for t=1,2,...tMP-1
  # Returns:        Objective function f=squared deviation from initDepTarg plus 
  #                 log(cumulative catch). 
  # Source:         S.P. Cox
  getObjFunctionVal <- function( pars, objRef )
  {
    val <- deref( runModel( pars, objRef ) )$f
    val
  }      # END function getObjFunctionVal function
  
  #----------------------------------------------------------------------------#
  
  # Begin solveInitPop
  obj <- deref( objRef )  
  
  # Determine if catch series has already been input
  if ( !.CATCHSERIESINPUT )
  {
    MSY <- obj$opMod$yieldFmsy      # M only used for scaling Fs 

    # Parameter definitions
    targDep <- obj$opMod$initDepTarg
    t1  <- proc.time()[3]
    nT  <- obj$opMod$nT
    tMP <- obj$opMod$tMP

    solveObj <- list( pars=obj$opMod, om=obj$om, mp=obj$mp )
    #spl <- splinefun(x=.INITTIMES*tMP,y=.INITMSYMULT*MSY)
    #xvals <- seq(1,obj$opMod$tMP-1,1)
    #initPars <- log( spl(xvals) )
    
    # Bypass optim call
    optPar <- list()
    #optPar$par   <- initPars

    # Make optim call
    #optPar <- optimize(f=getObjFunctionVal, maximum=FALSE, interval=c(1,3), 
                       #lower=1, upper=3, tol=0.0001, objRef=as.ref(solveObj) )
    #optPar  <- optim( par=initPars, fn=getObjFunctionVal, method="BFGS",
     #                 objRef=as.ref(solveObj), control=list(reltol=0.1 )  )  
    #cat( "Elapsed time in optimize = ", proc.time()[3]-t1, "\n" )
  }
  else
  {
    optPar <- list()
    optPar$par <- NULL
  }
  
  # Re-generate the operating model and data objects with the optimised Fs
  solveObj    <- deref( runModel( optPar$par, objRef=as.ref(solveObj) ) )
  obj$om      <- solveObj$om
  obj$mp$data <- solveObj$mp$data
  obj$pars    <- solveObj$pars
  
  # Fish population should now be close to the initial target
  # depletion regardless of the stochastic recruitment history.
  # Note that there might be a consistent, small difference 
  # from the target. This two-parameter spline method can
  # sometimes give weird history; if unsatisfied, use multiF
  # method that implements a spline with an arbitrary number
  # of knots ~ 5 is usually the most practical, e.g., for a 30-year
  # history.
  return( as.ref(obj) )
}     # END function .solveInitPop 


# .updatePop       
# Purpose:        Complete one iteration of feedback system
# Parameters:     obj=complete simulation object up to t-1; t=current time
# Returns:        obj=complete simulation object up to t
# Source:         S.P. Cox
.updatePop <- function( obj, t )
{
  # Flow of updatePop 
  # 1. extract last catch
  # 2. extract survey info
  # 3. perform stock assessment
  # 4. set catch limit
  # 5. update operating model state variables and data
  # 6. return updated simulation object

  # 0. Extract operating model and management procedure objects

  ctlList   <- obj$ctlList
  tmpTimes  <- .calcTimes( ctlList )
  
  refPtList <- obj$refPtList
  om        <- obj$om
  mp        <- obj$mp

  nT        <- ctlList$opMod$nT
  tMP       <- ctlList$opMod$tMP
  tRow      <- t - tMP + 1

  # 1. Extract total landed catch (quota) from last year 
  lastCatch <- sum( om$Ctg[(t-1),] )
  indexYr   <- mp$hcr$refPtYr-(tMP-1)

  # 3. Perform stock assessment step

  tmp <- list()

  # Index values
  useIndex     <- tmpTimes$useIndex
  tmp$nIndices <- length( useIndex )
  tmp$nT       <- t - 1 
  
  # Extract stock indices and set NA to -1 for assessSP.
  Itg               <- matrix( obj$mp$data$Itg )
  Itg[ is.na(Itg) ] <- -1  
  dim(Itg)          <- dim( obj$mp$data$Itg )
  tmp$Itg           <- Itg[ 1:(t-1),useIndex ]

  # Run assessment
  if ( ctlList$mp$assess$methodId == .PERFECT )
  {
    stockAssessment <- list()
    stockAssessment$biomass      <- om$legalB[t-1]
    stockAssessment$mpdPars$Fmsy <- refPtList$Fmsy
    stockAssessment$mpdPars$Bmsy <- refPtList$ssbFmsy
    stockAssessment$mpdPars$B0   <- obj$refPtList$ssbF0
    
    nCols <- ncol( mp$assess$mpdPars ) - 4
    mp$assess$mpdPars[(t-obj$pars$tMP+1),] <- c( t,stockAssessment$mpdPars,rep(NA,nCols) )
    names( mp$assess$mpdPars )  <-  c( "tStep",names(stockAssessment$mpdPars) )
  }
  if ( ctlList$mp$assess$methodId == .KALMAN )
  {
    tmp$It <- mp$data$Itg[1:(t-1),1]
    tmp$kfGain <- ctlList$mp$assess$kfGain
    stockAssessment <- assessModKF( tmp )
  }

  if ( ctlList$mp$assess$methodId == .MOVAVG )
  {

    # browser()
    tmp$avgPoints   <- ctlList$mp$assess$avgPoints
    tmp$q           <- ctlList$mp$hcr$qIndex
    tmp$t           <- t - 1

    stockAssessment <- .assessModMA_Herring( tmp )
    tRow <- t - tMP + 1
    
    # Update ItgScaled.
    val <- t(tmp$Itg)
    mp$assess$ItgScaled[tmpTimes$useIndex,c(1:(t-1)) ] <- val
    mp$assess$ItgScaled[mp$assess$ItgScaled < 0] <- NA
    
    val <- c( t,stockAssessment$biomass,stockAssessment$biomass )
    mp$assess$retroExpBt[ tRow, c(1:length(val)) ] <- val

  }

  if ( ctlList$mp$assess$methodId == .PMOD )
  {
    # need to assemble and fill in the data required by PMOD assessment
	  # If t == tMP, use init values of 0 for ln omega.
    tmp$spMsy           <- ctlList$mp$assess$spMsy
    tmp$spFmsy          <- ctlList$mp$assess$spFmsy
    tmp$sp_initB        <- ctlList$mp$assess$sp_initB
    tmp$lnOmega         <- rep(0,(t-2))
    tmp$indexFirstYear  <- tmpTimes$t1Index
    tmp$indexLastYear   <- rep((t-1),tmp$nIndices)
    tmp$indexType       <- eval( parse( text=ctlList$mp$data$indexType ) )
    tmp$useIndex        <- tmpTimes$useIndex
    tmp$spRhoEiv        <- ctlList$mp$assess$spRhoEiv
    tmp$spPmMsy         <- ctlList$mp$assess$spPmMsy
    tmp$spPsdMsy        <- ctlList$mp$assess$spPsdMsy
    tmp$spPmFmsy        <- ctlList$mp$assess$spPmFmsy
    tmp$spPsdFmsy       <- ctlList$mp$assess$spPsdFmsy
   
    # rowSums( om$Ctg[ 1:(t-1)),] )
    # ARK (08-Sep-13) This is slightly less than Ct because of correction for
    # size limit.  Should this not be total landings?
    tmp$Ct              <- om$legalC[1:(t-1)]
    
    hcrType             <- ctlList$mp$hcr$hcrType
    tmp$hcrType         <- hcrType

    if ( hcrType == "constantF" )
      tmp$ruleType <-1 
    if ( hcrType == "variableF" )
       tmp$ruleType <- 2
    if ( hcrType == "declineRisk" )
       tmp$ruleType <- 3
    if ( hcrType == "hcrUser" )
       tmp$ruleType <- 4

    tmp$nProjYears <- 1
    if ( hcrType == "declineRisk" )
      tmp$nProjYears <- ctlList$mp$hcr$nProjYears

    tmp$trendYears   <- ctlList$mp$hcr$trendYears
    tmp$rSeed        <- ctlList$opMod$rSeed 

    # Add in parameters to control MLE or MCMC in ADMB.
    tmp$nMCMC         <- ctlList$mp$hcr$nMCMC
    tmp$nThin         <- ctlList$mp$hcr$nThin
    tmp$useMLE        <- ctlList$mp$hcr$useMLE

    # Quota levels for projection.
    tmp$Qlevel <- seq( ctlList$mp$hcr$lowerQuota, ctlList$mp$hcr$upperQuota,
                       ctlList$mp$hcr$binQuota )

    tmp$idxCtlPts <- ctlList$mp$hcr$idxCtlPts      # From .calcTimes function.    
    
    stockAssessment <- .assessModSP( tmp )

    tRow <- t - tMP + 1

    val <- c( t, unlist( stockAssessment$runStatus ) )
    mp$assess$runStatus[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$runStatus )[c(1:length(val))] <- c( "tStep",names(stockAssessment$runStatus) )

    val <- c( t,unlist(stockAssessment$mpdPars) )
    mp$assess$mpdPars[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$mpdPars )[c(1:length(val))] <- c( "tStep",names(unlist(stockAssessment$mpdPars)) )
    
    # ARK (17-Nov-10) Update ItgScaled from pMod.
    for ( i in 1:length(tmpTimes$useIndex) )
    {
      val <- stockAssessment$ItScaled[i,]
      mp$assess$ItgScaled[tmpTimes$useIndex[i],c(1:(t-1)) ] <- ifelse( val==-1, NA, val )

    }
    
    # Fill next row of "stepwise" (i.e., retrospective) biomass estimates - use exploitable
    # even tho it is not clear that production model is exploitable or whatever.
    
    val <- c( t,stockAssessment$exploitBt,stockAssessment$projExpBio )
    mp$assess$retroExpBt[ tRow, c(1:length(val)) ] <- val
    mp$assess$retroSpawnBt          <- mp$assess$retroExpBt
    
    val <- c( t, stockAssessment$lnOmega )
    mp$assess$retroRt[ tRow, c(1:length(val)) ] <- val
    
  }     # ENDIF methodId=.PMOD

  if ( ctlList$mp$assess$methodId == .CAAMOD )
  {
    # Take the shortcut provided by the newer version of mseR,
    # will need grooming to work with multiple fleets
    stockAssessment <- callProcedureISCAM( obj, t)

    tRow <- t - tMP + 1

    stockAssessment$runStatus[c("est","std","cor","cov","names")] <- NULL
    val <- c( t, unlist( stockAssessment$runStatus ) )
    mp$assess$runStatus[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$runStatus )[c(1:length(val))] <- c( "tStep",names(stockAssessment$runStatus) )

    val <- c( t,unlist(stockAssessment$mpdPars) )
    mp$assess$mpdPars[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$mpdPars )[c(1:length(val))] <- c( "tStep",names(unlist(stockAssessment$mpdPars)) )
    
    # ARK (17-Nov-10) Update ItgScaled from ISCAM.
    for ( i in 1:length(tmpTimes$useIndex) )
    {
      val <- (stockAssessment$pit[i,]/stockAssessment$q[i])[!is.na(stockAssessment$pit[i,])]
      idx <- tmpTimes$useIndex[i]
      indexOn <- !is.na(obj$om$Itg[,idx])[1:(t-1)]
      mp$assess$ItgScaled[tmpTimes$useIndex[i],which(indexOn)] <- val
    }
    
    # Fill next row of "stepwise" (i.e., retrospective) biomass estimates
    val <- c( t,stockAssessment$bt )
    mp$assess$retroExpBt[ tRow, c(1:length(val)) ] <- val
    val <- c( t,stockAssessment$sbt )
    if(is.null(mp$assess$retroSpawnBt)) 
      mp$assess$retroSpawnBt <- mp$assess$retroExpBt
    mp$assess$retroSpawnBt[ tRow, c(1:length(val))] <- val

    val <- c( t, stockAssessment$delta )
    mp$assess$retroRt[ tRow, c(1:length(val)) ] <- val

    # Get logMdevs
    val <- c(t,0, stockAssessment$log_m_devs )
    mp$assess$retroMt[tRow, c(1:length(val))] <- val      


  }     # ENDIF methodId=.CAAMOD.

  # 4. Extract estimated control points for HCR (if required)

  # Control point update frequency - vector containing time step for the
  # estimates of control points that should be used for this time step. This
  # vector from .calcTimes is set such that tMP is set to 1.
  # Only applies to assessment models that can estimate control points. 
  
  idxCtlPts <- mp$hcr$idxCtlPts

  # This is within function .updatePop, just in case you are lost...
  
  # Moving average and Kalman filter methods do not have reference points from
  # the assessment method in the procedure, the ref pts come from the opMod.
  
  methodId     <- ctlList$mp$assess$methodId      # Assessment method.
  # above sb: constantF
  statusBase   <- ctlList$mp$hcr$statusBase       # Base is Bmsy, Bo.
  statusSource <- ctlList$mp$hcr$statusSource     # From OM or MP?
    
  remRefBase   <- ctlList$mp$hcr$remRefBase       # Base is Fmsy, F01.
  remRefSource <- ctlList$mp$hcr$remRefSource     # From OM or MP? 
  # above sb: inputF 
     
  # STOCK STATUS Control points from the operating model.
  if ( statusSource == "statusSrceEquil" )
  {
    if ( statusBase == "statusBaseBmsy" )
      mp$hcr$Bref[t] <- refPtList$ssbFmsy
      
    if ( statusBase == "statusBaseB0" )
      mp$hcr$Bref[t] <- refPtList$ssbF0

  }     # ENDIF statusSource=statusSrceEquil
    
  # STOCK STATUS Control points estimated from the assessment method.
  else
  {
    # ARK (01-Sep-13)
    # Explanation of how idxCtlPts works: idxCtlPts is computed by .calcTimes.
    # It holds the time step indices for the control points to use in the HCR.
    # e.g., idxCtlPts could be c( 1, 1, 1, 4, 4, 4, 7, 7 ,7, 10, 10, 10, 13, 13, 13 ).
    # This is for a situation where there are 15 years of projection, with control
    # point updating every 3 years.  tRow is the current time step.  Thus, the
    # expression idxCtlPts[tRow] gives the row index of the control points to
    # use in the HCR.  So, as t goes from tMP, tMP+1, tMP+3, the value of
    # idxCtlPts is always 1.  Therefore, take the value in row 1 of the estimated
    # control points 3 times.  For tMP+4, tMP+5, tMP+5, take the value in the
    # 4th row of the estimated control points 3 times.

    if ( statusBase == "statusBaseBmsy" )
      mp$hcr$Bref[t] <- mp$assess$mpdPars$ssbFmsy[ idxCtlPts[tRow] ]

    if ( statusBase == "statusBaseB0" )
      mp$hcr$Bref[t] <- mp$assess$mpdPars$B0[ idxCtlPts[tRow] ]
  }     # ENDIF statusSource != statusSrceEquil
  
  # REFERENCE REMOVAL RATE from the operating model.
  if ( remRefSource == "rrSrceEquil" )
  {
    if ( remRefBase == "rrBaseFmsy" )
      mp$hcr$remRate[t] <- refPtList$Fmsy
      
    if ( remRefBase == "rrBaseF01" )
      mp$hcr$remRate[t] <- refPtList$F01
      
    # This has to be calculated from the OM for now, so it
    # is not subject to idxCtlPts as it does not change during the projection.
    # It could, but don't worry about it now.  But it could be different than
    # the value of sprX specified in the default OM refPtList, so calculate.
    if ( remRefBase == "rrBaseFspr" )
      mp$hcr$remRate[t] <- .getFx( ctlList$opMod, x=ctlList$mp$hcr$sprX)$Fx

    if ( remRefBase == "rrBaseFinput" )
      mp$hcr$remRate[t] <- ctlList$mp$hcr$inputF 
       

  }     # ENDIF remRefSource == "rrSrceEquil"    
  else # REFERENCE REMOVAL RATE estimated from the assessment method.
  {   
    if ( remRefBase == "rrBaseFmsy" )
    {
      # browser(cat("I'm here!"))
      mp$hcr$remRate[t] <- mp$assess$mpdPars$Fmsy[ idxCtlPts[tRow] ]
    }
      
    # SPC says: I would just use OM to save time by avoiding .calcRefPoints.
    if ( remRefBase == "rrBaseF01" )
      mp$hcr$remRate[t] <- refPtList$F01
      
    if ( remRefBase == "rrBaseFspr" )
    {
      # This has to be calculated from the OM for now, so it
      # is not subject to idxCtlPts as it does not change during the projection.
      # It could, but don't worry about it now.  But it could be different than
      # the value of sprX specified in the default OM refPtList, so calculate.
      proxy <- .getFx( ctlList$opMod, x=ctlList$mp$hcr$sprX)$Fx
      obj$mp$hcr$remRate[t] <- proxy
    }
    if ( remRefBase == "rrBaseFinput" )
    {
      if( !is.null(ctlList$mp$hcr$phaseFtime) )
      {
        if( t < ctlList$mp$hcr$phaseFtime )
        {
            fmult <- 0.08/ctlList$mp$hcr$inputF - 1.
            f1    <- fmult/(ctlList$mp$hcr$phaseFtime - tMP)        
            mp$hcr$remRate[t] <- max(ctlList$mp$hcr$inputF*(1. + f1*(ctlList$mp$hcr$phaseFtime-t)),
                                     ctlList$mp$hcr$inputF)                 
        }
        else
          mp$hcr$remRate[t] <- ctlList$mp$hcr$inputF
                
      }        
    }
    else
      mp$hcr$remRate[t] <- ctlList$mp$hcr$inputF
      
  }  # ENDIF remRefSource != "rrSrceEquil"
 
  # Update lower and upper HCR control points.
  mp$hcr$lowerBound[t] <- mp$hcr$Bref[t] * ctlList$mp$hcr$lowerBoundMult
  mp$hcr$upperBound[t] <- mp$hcr$Bref[t] * ctlList$mp$hcr$upperBoundMult  
                  
  # HCRs based on Empirical methods
  if ( (ctlList$mp$assess$methodId == .MOVAVG) ) 
  {
      # Build harvest control rule list object.
      rule              <- mp$hcr
      rule$assessMethod <- ctlList$mp$assess$methodId
      rule$t            <- t
      rule$biomass      <- stockAssessment$biomass
      rule$maxF         <- log(1/(1-ctlList$mp$assess$iscam$targHR))
      rule$catchFloor   <- ctlList$mp$hcr$catchFloor
      rule$assessFailed <- FALSE

      # Calculate catch limit
      targetHarv   <- .calcHarvRuleHerringDFOMP( rule )  
  }
  else
  {
      # Build harvest control rule object, need to check that legalBiomass input.
      # Most recent survey spawning biomass estimate (i.e.,from t-1)
      rule              <- mp$hcr
      rule$assessMethod <- ctlList$mp$assess$methodId
      rule$t            <- t
      rule$biomass      <- stockAssessment$mpdPars$projExpBio
      rule$maxF         <- log(1/(1-ctlList$mp$assess$iscam$targHR))
      rule$catchFloor   <- ctlList$mp$hcr$catchFloor
      rule$assessFailed <- FALSE

      # Calculate catch limit
      targetHarv   <- .calcHarvRuleHerringDFOMP( rule )    
  }

  # Apply % limit on TAC change as independent floor and ceiling, as long
  # as maxDeltaTAC != NULL
  tmpTAC0     <- targetHarv$catchLimit
  # initialize floor and ceiling to current TAC 
  tacFloor    <- tmpTAC0
  tacCeiling  <- tmpTAC0

  # do the floor?
  if( is.null(ctlList$mp$hcr$maxDeltaTACf) )
  {
    # no limit on TAC change
    targetHarv$catchLimit <- tmpTAC0
  }
  else
  {
    # if maxDeltaTACf==0, then use constant TAC
    if( ctlList$mp$hcr$maxDeltaTACf==0 )
    {
      targetHarv$catchLimit <- ctlList$mp$hcr$consTAC      
    }
    if(ctlList$mp$hcr$maxDeltaTACf > 0 )
    {
      deltaTACf   <- ctlList$mp$hcr$maxDeltaTACf      
      tacFloor   <- lastCatch*(1.-deltaTACf)
    }
  }
  # do the ceiling
  if( is.null(ctlList$mp$hcr$maxDeltaTACc) )
  {
    # no limit on TAC change
    targetHarv$catchLimit <- tmpTAC0
  }
  else
  {
    # if maxDeltaTACc==0, then use constant TAC
    if( ctlList$mp$hcr$maxDeltaTACc==0 )
    {
      targetHarv$catchLimit <- ctlList$mp$hcr$consTAC      
    }
    if(ctlList$mp$hcr$maxDeltaTACc > 0 )
    {
      if( ctlList$mp$hcr$deltaTACcType == "absolute" )
        tacCeiling   <- ctlList$mp$hcr$catchCeiling
      else
      {
        deltaTACc   <- ctlList$mp$hcr$maxDeltaTACc      
        tacCeiling <- lastCatch*(1.+deltaTACc)
      }
    }
  }
  tmpTAC     <- max( tmpTAC0, tacFloor )
  tmpTAC     <- min( tmpTAC, tacCeiling )
  targetHarv$catchLimit <- tmpTAC      
      
  # Changing philosophy of what was .DEADFLAG.  The current
  # implementation causes fishery to stay closed for entire projection period.
  # This was originally implemented with reference to the OM biomass to try to
  # keep the stock from going negative, and was changed at some point to projExpBio.
  
  # However, this step actually evaluates whether the perception is that the
  # catch arises from the HCR is going to take 90% of the projected exploitable
  # biomass.  Call it .FISHERYCLOSED which will apply for one year.
  
  # Actually, can this ever be triggered?  projExpBio is fed into the control rule...
  # Yes! Because F is applied, large F~3 can generate U greater than 0.95.
  
  tmpCatch <- c( targetHarv$catchLimit, 0.90*stockAssessment$projExpBio )
  # browser()
  # tmpCatch <- targetHarv$catchLimit

  choice   <- which.min(tmpCatch)
  if ( choice==2 )
    .FISHERYCLOSED <<- TRUE  
  
  # Set catch limit for the year
  om$Ctg[ t, ] <- rep( 0, ncol(om$Ctg) )
  om$Ctg[t,]   <- ctlList$opMod$allocProp * min(tmpCatch)

  if(targetHarv$catchLimit == 0) om$Ctg[t,c(1,4:5)] <- om$Ctg[t-1,c(1,4:5)]

 
  mp$assess$runStatus[ tRow, "deadFlag" ]      <- .DEADFLAG
  mp$assess$runStatus[ tRow, "fisheryClosed" ] <- .FISHERYCLOSED
  .FISHERYCLOSED <<- FALSE   
      
  # 5. Update the simulated population.
  obj$om              <- om
  obj$mp              <- mp
  obj$mp$data         <- mp$data
  obj$mp$assess$Bt[t] <- stockAssessment$mpdPars$projExpBio
  obj$mp$hcr          <- mp$hcr
  obj$mp$hcr$precautionaryFt[t] <- targetHarv$precautionaryF # target F prescribed by control rule
  obj <- deref( ageLenOpMod( as.ref(obj),t ) )

  #if ( obj$om$Bt[t] < (0.05*ctlList$opMod$B0) )
  #{
    .DEADFLAG <<- FALSE
  #}
 
  return( obj )
}     # END function .updatePop





#------------------------------------------------------------------------------#
#-- mseRsystem.r Helper Functions                                            --#
#------------------------------------------------------------------------------#

.calcTimes <- function( obj )
{
  # This function calculates when stock indexing survey, ages, assessment or
  # control points are turned on/off. This has to be done in the same function
  # as there are dependencies among the various inputs.  For example, the rule
  # control points cannot be updated when there is no assessment.  Also, there
  # is no option for assessment frequency prior to tMP.

  nT     <- obj$opMod$nT                 # Maximum year of simulation.
  tMP    <- obj$opMod$tMP                # First year of management procedure.
  nGear  <- obj$opMod$nGear              # Number of gears.

  result       <- list()                 # The result.
  result$nT    <- nT
  result$tMP   <- tMP
  result$nGear <- nGear

  #----------------------------------------------------------------------------#
  #-- Stock index timing                                                       #
  #                                                                            #
  # Constraints:                                                               #
  #                                                                            #
  # 1 <= t1Index[i] < tMP <= t2Index[i] <= nT                                #
  #----------------------------------------------------------------------------#

  useIndex <- eval( parse( text=obj$mp$data$useIndex ) )  # Which indices are being used?
  # Index design settings.
  t1Index  <- eval( parse( text=obj$mp$data$t1Index ) )   # First year of indices Period 1.
  t2Index  <- eval( parse( text=obj$mp$data$t2Index ) )   # First year of indices Period 2.
  k1Index  <- eval( parse( text=obj$mp$data$k1Index ) )   # Index frequency in Period 1.
  k2Index  <- eval( parse( text=obj$mp$data$k2Index ) )   # Index frequency in Period 2.

  # Boolean control matrx of nGear by Time (t=1,..,nT) that determines times
  # at which an index is available. Initialize all values to FALSE.
  indexOn <- matrix( FALSE, nrow=nT, ncol=nGear )
  
  idPer1  <- as.list( rep(-1,nGear) )
  idPer2  <- idPer1  

  for ( i in 1:length(useIndex) )
  {
    iGear <- useIndex[i]

    # Set indexOn==TRUE when index is done in Period 1.
    if ( obj$mp$data$chkIndex1 )
      idPeriod1 <- eval( parse( text=obj$mp$data$idxIndex1 ) )
    else
    {
      if ( k1Index[i] > 0 )
        idPeriod1 <- seq( from=t1Index[i], to=(t2Index[i]-1), by=k1Index[i] )
      else
        #idPeriod1 <- rep( 0, t2Index[i]-t1Index[i]+1 )        
        idPeriod1 <- -1
    }

    if ( all(idPeriod1 > 0) )
      indexOn[ idPeriod1, iGear ] <- TRUE
    idPer1[[iGear]] <- idPeriod1

    # Set indexOn==TRUE when a index is done in Period 2.
    if ( obj$mp$data$chkIndex2 )
      idPeriod2 <- eval( parse( text=obj$mp$data$idxIndex2 ) )
    else
    {
      if ( k2Index[i] > 0 )
        idPeriod2 <- seq( from=t2Index[i], to=nT, by=k2Index[i] )
      else
        idPeriod2 <- -1     
    }

    if ( all(idPeriod2 > 0) )
      indexOn[ idPeriod2, iGear ] <- TRUE
    idPer2[[iGear]] <- idPeriod2
  }     # for i over useIndex

  result$useIndex    <- useIndex
  result$t1Index     <- t1Index
  result$t2Index     <- t2Index
  result$k1Index     <- k1Index
  result$k2Index     <- k2Index
  result$per1Index   <- idPer1
  result$per2Index   <- idPer2
  result$indexOn     <- indexOn

  #----------------------------------------------------------------------------#
  #-- Ages timing                                                              #
  #                                                                            #
  # Constraints: None, we can have observed ages any time in each Period to    #
  # accommodate simulating ages from survey or commercial catches.             #
  # For example, we may wish to do surveys every two years, but collect ages   #
  # every year from the commercial fishery.                                    #
  #                                                                            #
  #----------------------------------------------------------------------------#

  useAges    <- eval( parse( text=obj$mp$data$useAges ) ) # Which indices have ages?

  # Ageing data design settings.
  t1Ages     <- eval( parse( text=obj$mp$data$t1Ages ) )  # First year of ages for Period 1.
  t2Ages     <- eval( parse( text=obj$mp$data$t2Ages ) )  # First year of ages for Period 2.
  k1Ages     <- eval( parse( text=obj$mp$data$k1Ages ) )  # Age data frequency in Period 1.
  k2Ages     <- eval( parse( text=obj$mp$data$k2Ages ) )  # Age data frequency in Period 2.

  # Boolean control matrx of nGear by Time (t=1,..,nT) that determines times
  # at which an ages is available. Initialize all values to FALSE.
  agesOn <- matrix( FALSE, nrow=nT, ncol=nGear )

  idPer1  <- as.list( rep(-1,nGear) )
  idPer2  <- idPer1  
  
  for ( i in 1:length(useAges) )
  {
    iGear <- useAges[i]
    # Set indexOn==TRUE when index is done in Period 1.
    if ( obj$mp$data$chkAges1 )
      idPeriod1 <- eval( parse( text=obj$mp$data$idxAges1 ) )
    else
    {
      if ( k1Ages[i] > 0 )
        idPeriod1 <- seq( from=t1Ages[i], to=(t2Ages[i]-1), by=k1Ages[i] )
      else
        #idPeriod1 <- rep( 0, t2Ages[i]-t1Ages[i]+1 )
        idPeriod1 <- -1
    }

    if ( all(idPeriod1 > 0) )
      agesOn[ idPeriod1, iGear ] <- TRUE
    idPer1[[iGear]] <- idPeriod1

    # Set agesOn==TRUE when ages in Period 2.
    if ( obj$mp$data$chkAges2 )
      idPeriod2 <- eval( parse( text=obj$mp$data$idxAges2 ) )
    else
    {
      if ( k2Ages[i] > 0 )
        idPeriod2 <- seq( from=t2Ages[i], to=nT, by=k2Ages[i] )
      else
        #idPeriod2 <- rep( 0, nT-t2Ages[i]+1 )        
        idPeriod2 <- -1
    }

    if ( all(idPeriod2 > 0) )
      agesOn[ idPeriod2, iGear ] <- TRUE
    idPer2[[iGear]] <- idPeriod2
  }     # for i over useAges

  result$useAges  <- useAges
  result$t1Ages   <- t1Ages
  result$t2Ages   <- t2Ages
  result$k1Ages   <- k1Ages
  result$k2Ages   <- k2Ages
  result$per1Ages <- idPer1
  result$per2Ages <- idPer2
  result$agesOn   <- agesOn

  #----------------------------------------------------------------------------#
  #-- Assessment method timing                                                 #
  #                                                                            #
  # Constraints: There is no timing of assessments prior to tMP.               #
  # First assessment is at tMP-1, which is Period 1. Perido 2 starts at t>=tMP #
  #----------------------------------------------------------------------------#

  t1Method <- obj$mp$assess$t1Method       # First year of assessment Period 1.
  t2Method <- obj$mp$assess$t2Method       # First year of assessment Period 2.
  k1Method <- obj$mp$assess$k1Method       # Frequency of assessment Period 1.
  k2Method <- obj$mp$assess$k2Method       # Frequency of assessment Period 2.

  # Create a Boolean control vector t=1,..,nT that determines years in which a
  # asessment should be done. Initialize all values to FALSE.
  methodOn <- rep( FALSE, nT )

  # Set methodOn==TRUE when an assessment is done either in Period or Period 2.
  # The value of t1Method must be less than tMP, however, the value of
  # t2Method can be any value t1Method < t2Method <=nT.

  # Vector with elements of surveyOn=TRUE for survey years in "old" period.
  idPer1 <- seq( from=t1Method, to=t2Method, by=k1Method )

  # Set methodOn==TRUE for these elements
  methodOn[ idPer1 ] <- TRUE

  # Set methodOn==TRUE when an assessment is done in Period 2.
  if ( obj$mp$assess$chkMethod2 )
    idPer2 <- eval( parse( text=obj$mp$assess$idxMethod2 ) )
  else
    idPer2 <- seq( from=t2Method, to=nT, by=k2Method )

  # Set methodOn==TRUE for these elements.
  methodOn[ idPer2 ] <- TRUE

  result$t1Method   <- t1Method
  result$t2Method   <- t2Method
  result$k1Method   <- k1Method
  result$k2Method   <- k2Method
  result$per1Method <- idPer1
  result$per2Method <- idPer2
  result$methodOn   <- methodOn

  # Create a vector tMP:nT that contains the time index where catch is to be taken
  # in interim times between assessments if forecasts are NOT used.  This holds
  # the catch constant at the last assessment and HCR result if applied.
  # For example, if ctlPtFreq=5, tMP=50, nT=60:
  # idx = c( 50, 55, 60 )
  # tCtlPts = c( 50, 50, 50, 50, 50, 55, 55, 55, 55, 55, 60, 60, 60, 60, 60 )

  # Get time steps when assessment done.
  tMethod <- c(tMP:nT)[ methodOn[tMP:nT] ]

  # Get the difference in the times to calc number times each assessment used.
  tDiff <- diff( tMethod )

  # Get the maximum tMethod, and concatenate difference between it and nT.
  tDiff <- c( tDiff, nT-max(tMethod)+1 )
  tMethod <- rep( tMethod, tDiff )

  result$tMethod   <- tMethod
  result$idxMethod <- tMethod - (tMP-1)

  #----------------------------------------------------------------------------#
  #-- Control Point update timing                                              #
  #                                                                            #
  # Constraints:                                                               #
  #                                                                            #
  # 1. There must be an update on tMP.                                         #
  # 2. No updating of control points in the HCR without a stock assessment.    #
  #    but there can be stock assessments without updating of control points.  #                                             #
  #----------------------------------------------------------------------------#

  ctlPtFreq <- obj$mp$hcr$ctlPtFreq

  # Create a Boolean control vector t=1,..,nT that determines years in which
  # control points are updated.  Initialize all values to FALSE.
  ctlPtsOn <- rep( FALSE, nT )

  if ( ctlPtFreq > 0 )
  {
    idx <- seq( from=tMP, to=nT, by=ctlPtFreq )
    ctlPtsOn[ idx ] <- TRUE
  }
  else
  {
    ctlPtsOn[ tMP ] <- TRUE
  }

  # Now ensure that the updates are not more frequent than the assessments.
  ctlPtsOn[ tMP:nT ] <- as.logical( ctlPtsOn[ tMP:nT] * methodOn[ tMP:nT ] )

  # Create a vector tMP:nT that contains the time index where catch is to be taken
  # in interim times between assessments if model is NOT used.  This holds
  # the catch constant at the last assessment and HCR result if applied.
  # For example, if ctlPtFreq=5, tMP=50, nT=60:
  # idx = c( 50, 55, 60 )
  # tCtlPts = c( 50, 50, 50, 50, 50, 55, 55, 55, 55, 55, 60, 60, 60, 60, 60 )

  # Get time steps when assessment done.
  tCtlPts <- c(tMP:nT)[ ctlPtsOn[tMP:nT] ]

  # Get the difference in the times to calc number times each assessment used.
  tDiff <- diff( tCtlPts )

  # Get the maximum tMethod, and concatenate difference between it and nT.
  tDiff <- c( tDiff, nT-max(tCtlPts)+1 )
  tCtlPts <- rep( tCtlPts, tDiff )

  # Calculate indices relative to tMP, so that times run from 1 to (nT-tMP+1).
  idxCtlPts <- tCtlPts - (tMP-1)

  result$ctlPtFreq <- ctlPtFreq
  result$ctlPtsOn  <- as.logical( ctlPtsOn )
  result$tCtlPts   <- tCtlPts
  result$idxCtlPts <- idxCtlPts

  #----------------------------------------------------------------------------#

  result
}     # END function .calcTimes

# .saveBlob (Saves the simulations results and updates list)
# Purpose:        Saves the simulations results stored in blob to a .RData file.
# Parameters:     obj is a "blob" object holding simulation results.
# Returns:        File name of the saved results.
# Source:         A.R. Kronlund
.saveBlob <- function( blobFileName, blob )
{
  cat( "\nMSG (.saveBlob) Writing simulation results to file: ",blobFileName,"\n" )
  save( blob, file=blobFileName )
}     # END function .saveBlob


.writePinDat <- function( blob, iRep=1 )
{
  #----------------------------------------------------------------------------#
  # SEAN - This is where you put code to write the operating model data to     #
  # ADMB pin and dat files for conditioning - use for testing with known data. #
  #                                                                            #
  # This function writes specified replicate to a pin file - .FOMPIN           #
  # This function writes specified replicate to a dat file - .FOMDAT           #
  #                                                                            #
  # A simulation must exist - there is a check in the GUI that prevents this   #
  # function from being called if a simulation has not been run.               #
  #                                                                            #
  # NOTE: This function assumes you want data for t=1,...,tMP-1 to be written. #
  #       It could be extended to have t=tStart,..., tEnd with appropriate     #
  #       care with indexing.                                                  #
  #                                                                            #
  # ARK (14-Sep-13) Re-written to accommodate mseR2013.                        #
  #----------------------------------------------------------------------------#

  # ARK (05-Sep-10) THESE WILL EVENTUALLY APPEAR IN mseRglobals.r.
  #----------------------------------------------------------------------------#
  # sableOpMod I/O files.                                                      #
  #----------------------------------------------------------------------------#

  .FOPMODAGES <- "opModAges.dat"
  .FOPMODCAT  <- "opModCatch.dat"
  .FOPMODCTL  <- "opModControl.ctl"
  .FOPMODIDX  <- "opModIndex.dat"
  .FOPMODLIFE <- "opModLifeScheds.dat"
  .FOPMODPIN  <- "sableOpMod.pin"

  #----------------------------------------------------------------------------#

  # Get the complete simulation control file.
  ctlList <- blob$ctlList
  tmpTimes <- .calcTimes( ctlList )

  # Extract opMod elements.
  opMod <- ctlList$opMod
  
  # Extract mp elements.
  mp  <- ctlList$mp
  
  # Calculate life history schedules and OM equilibrium reference points.
  t1 <- proc.time()[3]
  cat( "\nMSG (.writePinDat) Calculating reference points, please wait...\n" )
  tmp <- calcRefPoints( as.ref(opMod) )
  refPts <- deref( tmp )
  cat( "\nMSG (.mgmtProc) Elapsed time in calcRefPoints =",proc.time()[3]-t1,"\n" )  

  nFisheries <- opMod$nGear
  nGrps      <- opMod$nGrps
  nT         <- opMod$tMP - 1  # Output to the last year before MP starts, not nT.
  minAge     <- 3
  nAges      <- opMod$nAges

  dateStamp <- date()          # Get a common date stamp for the pin and dat files.

  # Create an indexing vector for time t=1,...,tMP-1 for extractions.
  tdx <- c( 1:nT )

  #----------------------------------------------------------------------------#
  # Write operating model control file for ADMB, usually "opModControl.ctl".   #
  #----------------------------------------------------------------------------#

  # ARK (05-Sep-10) A global called .FOPMODCTL will be set to "opModControl.ctl".
  # So you can use .FOMPMODCTL instead of ctlFile.

  cat( file=.FOPMODCTL, "## mseR2013: Operating model control settings written",dateStamp,"\n" )
  cat( file=.FOPMODCTL, "# baranovIter\n",  opMod$baranovIter,  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# baranovSteps\n", opMod$baranovSteps, "\n", append=TRUE )

  nAges <- nAges - minAge + 1

  # Estimation phase numbers for model parameters
  cat( file=.FOPMODCTL, "## Estimation phases",                "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phB0\n",        1,                  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phSteep\n",     2,                  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phM\n",         3,                  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phCPUEPower\n", rep(1,nFisheries),  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phTagRR\n",     rep(-1,nFisheries), "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phGammaR\n",   -1,                  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phSigmaR\n",   -1,                  "\n", append=TRUE )
  cat( file=.FOPMODCTL, "# phRecDevs\n", c(-1,rep(5,nT-2),-1), "\n", append=TRUE )

  cat( "\nMSG (.writePinDat) Operating model control parameters written to ",.FOPMODCTL,"\n" )

  #----------------------------------------------------------------------------#
  # Write life history schedules for ADMB, usually "opModLifeScheds.dat".      #
  #----------------------------------------------------------------------------#

  lifeHistFile <- "opModLifeScheds.dat"

  # ARK (05-Sep-10)  Global will be set where .FOPMODLIFE is "opModLifeScheds.dat".
  # So you can use .FOPMODLIFE instead of lifeHistFile.

  cat( file=.FOPMODLIFE, "## mseR2013: Operating model life history written",dateStamp,"\n" )
  cat( file=.FOPMODLIFE, "# sizeLim\n",  opMod$sizeLim,   "\n", append=TRUE )
  cat( file=.FOPMODLIFE, "# dg\n",       opMod$dg,        "\n", append=TRUE )
  cat( file=.FOPMODLIFE, "# nGrps\n",    opMod$nGrps,     "\n", append=TRUE )
  cat( file=.FOPMODLIFE, "# Lal\n",   append=TRUE )
  write(x=t(refPts$Lal),  file=lifeHistFile, ncolumns = nGrps, append = TRUE, sep = " ")
  cat( file=.FOPMODLIFE, "# Wal\n",   append=TRUE )
  write(x=t(refPts$Wal),  file=lifeHistFile, ncolumns = nGrps, append = TRUE, sep = " ")
  cat( file=.FOPMODLIFE, "# Legal\n", append=TRUE )
  write(x=t(refPts$Legal),file=lifeHistFile, ncolumns = nGrps, append = TRUE, sep = " ")
  cat( file=.FOPMODLIFE, "# Ma\n", append=TRUE )
  matMa <- matrix( refPts$Ma,nrow=length(refPts$Ma), ncol = nGrps, byrow=F )
  write(x=t(matMa),file=.FOPMODLIFE, ncolumns = nGrps, append = TRUE, sep = " ")
  for( i in 1:nFisheries )
  {
    cat( file=.FOPMODLIFE, paste("# Fishery ",i," selectivity\n", sep=""), append=TRUE )
    write(x=t(refPts$Salg[,,i]), file=lifeHistFile, ncolumns = nGrps, append = TRUE, sep = " ")
  }
  for( i in 1:nFisheries )
  {
    cat( file=.FOPMODLIFE, paste("# Fishery ",i," discard prob\n", sep=""), append=TRUE )
    write(x=t(refPts$Palg[,,i]), file=.FOPMODLIFE, ncolumns = nGrps, append = TRUE, sep = " ")
  }

  cat( "\nMSG (.writePinDat) Life history data written to ",.FOPMODLIFE,"\n" )

  #----------------------------------------------------------------------------#
  # Write *.PIN file for ADMB, usually "sableOpMod.pin".                       #
  #----------------------------------------------------------------------------#

  # ARK (05-Sep-10) Doesn't pinFile have to be sableOpMod.pin?  If so, please use
  # .FOPMODPIN instead of pinFile.  It will be a global set to sableOpMod.pin.

  cat( file=.FOPMODPIN, "## mseR2013: Operating model parameters written",dateStamp,"\n" )
  cat( file=.FOPMODPIN, "# logB0\n",      log(opMod$B0),     "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# rSteepness\n", opMod$rSteepness,  "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# logM\n",       log(opMod$M),      "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# cpuePower\n",  opMod$powerg,      "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# tagRepRate\n", opMod$qg,          "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# gammaR\n",     opMod$gammaR,      "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# logSigmaR\n",  log(opMod$sigmaR), "\n", append=TRUE )
  cat( file=.FOPMODPIN, "# recDevs\n",    rep(0,nT),         "\n", append=TRUE )
  
  cat( "\nMSG (.writePinDat) Operating model parameters written to ",.FOPMODPIN,"\n" )
  
  #----------------------------------------------------------------------------#
  # Write stock abundance index data file for ADMB, usually "opModIndex.dat".  #
  #----------------------------------------------------------------------------#

  idxRelative   <- eval( parse( text=mp$data$indexType ) )  # 0 is absolute, 1 is relative.
  idxIndex      <- tmpTimes$useIndex                        # Is the index used?
  nIndexSeries  <- length( idxIndex )
  idxFirstYear  <- tmpTimes$t1Index
  idxLastYear   <- tmpTimes$t2Index
  idxLikeWeight <- mp$assess$idxLikeWeight

  # Indices are stored in blob$om$Itg and blob$mp$data$Itg. Assume "data" version
  # has random noise added, and that the "om" version does not.
  # Itg is dimensioned [nRep,nT,nGear].

  idxSeries <- blob$mp$data$Itg[iRep,tdx,idxIndex]

  # Transpose the series.
  idxSeries <- t( idxSeries )

  # Change all NA to -1.
  idxSeries[ is.na(idxSeries) ] <- -1

  cat( file=.FOPMODIDX, "## mseR2013: Abundance index data written to file ",.FOPMODIDX," on ",
                         dateStamp,"\n" )
  cat( file=.FOPMODIDX, "# nIndexSeries\n",  append=TRUE )
  cat( file=.FOPMODIDX, nIndexSeries, "\n",  append=TRUE )
  cat( file=.FOPMODIDX, "# idxIndex\n",      append=TRUE )
  cat( file=.FOPMODIDX, idxIndex,     "\n",  append=TRUE )
  cat( file=.FOPMODIDX, "# idxRelative\n",   append=TRUE )
  #cat( file=.FOPMODIDX, idxRelative[idxIndex], "\n",   append=TRUE )
  cat( file=.FOPMODIDX, idxRelative,   "\n",   append=TRUE )  
  cat( file=.FOPMODIDX, "# idxLikeWeight\n", append=TRUE )
  #cat( file=.FOPMODIDX, idxLikeWeight[idxIndex], "\n", append=TRUE )
  cat( file=.FOPMODIDX, idxLikeWeight, "\n", append=TRUE )  
  cat( file=.FOPMODIDX, "# idxFirstYear\n",  append=TRUE )
  #cat( file=.FOPMODIDX, idxFirstYear[idxIndex], "\n",  append=TRUE )
  cat( file=.FOPMODIDX, idxFirstYear, "\n",  append=TRUE )  
  cat( file=.FOPMODIDX, "# idxLastYear\n",   append=TRUE )
  #cat( file=.FOPMODIDX, idxLastYear[idxIndex], "\n",   append=TRUE )
  cat( file=.FOPMODIDX, idxLastYear, "\n",   append=TRUE )  

  cat( file=.FOPMODIDX, "## Index order: ",
       paste( opMod$gNames[idxIndex], collapse=" " ),"\n", append=TRUE )
  cat( file=.FOPMODIDX, "# idxSeries\n",               append=TRUE )
  write.table( file=.FOPMODIDX, round(idxSeries,digits=4), col.names=FALSE,
                                row.names=FALSE, sep=" ", append=TRUE )

  cat( "\nMSG (.writePinDat) Abundance index data written to ",.FOPMODIDX,"\n" )

  #----------------------------------------------------------------------------#
  # Write catch data file for ADMB, usually "opModCatch.dat".                  #
  #----------------------------------------------------------------------------#

  # landCatchMatrix is an nT by nFish + 2 matrix of retained catches where
  # column 1 is the year and column 2 is the time step.  Other columns are
  # retained catch by fishery.  Ctg is dimensioned [nRep,nT,nGear].

  Ctg    <- blob$om$Ctg
  dimCtg <- dim(Ctg)

  landCatchMatrix     <- matrix( 0, nrow=nT,ncol=dimCtg[3]+2 )
  landCatchMatrix[,1] <- c( 1:nrow(landCatchMatrix) )
  landCatchMatrix[,2] <- c( 1:nrow(landCatchMatrix) )

  for ( g in 1:dimCtg[3] )
    landCatchMatrix[ ,g+2 ] <- round( Ctg[ iRep,tdx,g ], digits=4 )

  cat( file=.FOPMODCAT, "## mseR2013: Catch data written to file ",.FOPMODCAT," on ",
                        dateStamp,"\n" )
  cat( file=.FOPMODCAT, "# nT\n",    append=TRUE )
  cat( file=.FOPMODCAT, nT, "\n",    append=TRUE )
  cat( file=.FOPMODCAT, "# nFisheries\n", append=TRUE )
  cat( file=.FOPMODCAT, nFisheries, "\n", append=TRUE )

  cat( file=.FOPMODCAT, "## Fishery order: ",paste(opMod$gNames,collapse=" " ),
       "\n", append=TRUE )
  cat( file=.FOPMODCAT, "# landCatchMatrix\n",                  append=TRUE )
  write.table( file=.FOPMODCAT, landCatchMatrix, col.names=FALSE,
                                row.names=FALSE, sep=" ",  append=TRUE )

  cat( "\nMSG (.writePinDat) Sablefish catch data written to data file ",
       .FOPMODCAT,"\n" )

  #----------------------------------------------------------------------------#
  # Write age proportions for ADMB, usually "opModAges.dat".                   #
  #----------------------------------------------------------------------------#

  # ARK (05-Sep-10) I assumed that blob$mp$dat$Xatg actually contained
  # the observed age proportions - NOTE THERE ARE SOME VALUES THAT ARE NaN !!!!
  # But it is only dimensioned [nAges,nT,nGear] so I'm not sure what it is as
  # there is no replicate index.

  # There is an assumption as well that any index will have age proportions output.
  # But, we could have an index without ages, or vice-versa (less likely).

  rDigits <- 6     # Number of digits in rounding age proportions.
  plusGroupAge <- opMod$nAges

  useAges      <- tmpTimes$useAges
  nAgeSeries   <- length( useAges )
  ageFirstYear <- tmpTimes$t1Index[ useAges ]      # Assumes same first years as index.
  ageLastYear  <- tmpTimes$t2Index[ useAges ]      # Assumes same last years as index.

  cat( file=.FOPMODAGES, "## mseR2013: Proportions at age written to file ",.FOPMODAGES," ",dateStamp,"\n" )
  cat( file=.FOPMODAGES, "# minAge\n",       append=TRUE )
  cat( file=.FOPMODAGES, minAge,"\n",        append=TRUE )
  cat( file=.FOPMODAGES, "# plusGroupAge\n", append=TRUE )
  cat( file=.FOPMODAGES, plusGroupAge, "\n", append=TRUE )
  cat( file=.FOPMODAGES, "# nAgeSeries\n",   append=TRUE )
  cat( file=.FOPMODAGES, nAgeSeries, "\n",   append=TRUE )
  cat( file=.FOPMODAGES, "# ageIndex\n",     append=TRUE )
  cat( file=.FOPMODAGES, useAges, "\n",     append=TRUE )

  cat( file=.FOPMODAGES, "# ageFirstYear\n", append=TRUE )
  cat( file=.FOPMODAGES, ageFirstYear, "\n", append=TRUE )
  cat( file=.FOPMODAGES, "# ageLastYear\n",  append=TRUE )
  cat( file=.FOPMODAGES, ageLastYear, "\n",  append=TRUE )

  # Write out generic names ageSeries plus the ageIndex index.
  for ( g in useAges )
  {
    cat( file=.FOPMODAGES, paste( "## ageSeries",g, " ", opMod$gNames[g],"\n",
         sep="" ), append=TRUE )
    cat( file=.FOPMODAGES, paste( "# ageSeries",g,sep="" ), "\n", append=TRUE )

    # extract transpose of age proportions 
    paa <- t( blob$mp$data$Xatg[ iRep,minAge:plusGroupAge,tdx,g ] )
    paa[ is.na(paa) ] <- -1

    write.table( file=.FOPMODAGES, round( paa, digits=rDigits ),
                 col.names=FALSE, row.names=FALSE, sep=" ", append=TRUE )

  }
  cat( "\nMSG (.writePinDat) Proportions at age written to ",.FOPMODAGES,"\n" )
  cat( "\nMSG (.writePinDat) End of of writing *.dat and *.pin files.\n" )
  return( invisible() )
}     # .writePinDat function.


.caaModCtlPtsUseR <- function( i, pars, mcmcData, hcrList )
{
  # want obj$refPts$Fmsy for refpts####.pin
  # pars: blob$opMod
  # mcmcData: data.frame created from "mcout.dat" file.
  # hcrList: blob$mp$hcr

  Fmsy    <- NA
  ssbFmsy <- NA

  obj$rec.a  <- 4.*rSteepness*obj$R0 / ( B0*(1.-rSteepness) )
  obj$rec.b  <- (5.*rSteepness-1.) / ( B0*(1.-rSteepness) )

  # Update pars copy with mcmc parameter values.  
  pars$rSteepness <- mcmcData[ i, "rSteepness" ]
  pars$B0         <- mcmcData[ i, "B0" ]
  pars$R0         <- mcmcData[ i, "R0" ]
  pars$M          <- mcmcData[ i, "M" ]
  pars$rec.a      <- 4.*pars$rSteepness*pars$R0 / ( pars$B0*(1.-pars$rSteepness) )
  pars$rec.b      <- (5.*pars$rSteepness-1.) / ( pars$B0*(1.-pars$rSteepness) )
  pars$aMat50     <- mcmcData[ i, "aMat50" ]
  pars$aMat95     <- mcmcData[ i, "aMat95" ]
  pars$Linf       <- mcmcData[ i, "Linf" ]
  pars$L1         <- mcmcData[ i, "L1" ]
  pars$vonK       <- mcmcData[ i, "vonK" ]
  pars$c1         <- mcmcData[ i, "c1" ]
  pars$c2         <- mcmcData[ i, "c2" ]

  # ARK (23-Dec-11) Should we add maturity parameters, growth for generality?
  refPoints         <- list()
  refPoints$ssbFmsy <- NA
  refPoints$Fmsy    <- NA

  # ARK (23-Dec-11) Should we grab F0.1 too?
  
  # If decline risk rule the reference points are the same for each replicate
  # of the block of quota levels, Q1, Q2, ..., Qn.  Therefore, look for "Q" and
  # determine if the i-th value of Q is the same as Q[1].  If so, calculate the
  # reference points. Else, fill in the reference points from the last calculated values.
  
  doRefPts <- TRUE
  if ( (hcrList$hcrType=="declineRisk") && (mcmcData$Q[i]!=mcmcData$Q[1]) )
  {
    # Don't calculate reference points.
    doRefPts     <- FALSE
    tmpRefPoints <- list( Fmsy=NA, ssbFmsy=NA )
  }
  
  tmpRefPoints <- NULL
  if ( doRefPts )
  {
    tmpRefPoints <- try( calcRefPoints( pars, 
      rpList=list( FALL=FALSE, Fmsy=TRUE ), application="MP" ),
      silent=TRUE )
    
    if ( i %% 10 == 0 )
      cat( "\nMSG (.caaModCtlPtsUseR) Calculating reference points for draw",i,"\n" )      
  }

  statusSource <- hcrList$statusSource
  remRefSource <- hcrList$remRefSource
  ctlPtSrce    <- "MP"

  # .calcRefPoints succeeded, or was not calculated, so use MSY-based reference points.
  if ( class( tmpRefPoints ) == "list" )
  {
    Fmsy    <- tmpRefPoints$Fmsy
    ssbFmsy <- tmpRefPoints$ssbFmsy
  }
      
  result <- list( ctlPtSrce=ctlPtSrce, ssbFmsy=ssbFmsy, Fmsy=Fmsy )  
  result
}     # END function .caaModCtlPtsUseR


# .tsBootIdx
# Purpose:        creates a time-series bootstrap sample from
#                 historical input values (e.g., Mt, alphat,etc)
# Parameters:     x=the values to sample, length=length of projection,
#                 lims=numeric of historical limits to sample between
#                 margin=dimension (row,col,etc) to sample across
# Returns:        nT-vector of matrix/vector index values
# Usage:          tmp[t] = state[t]*error[t]
# Source:         Sam Johnson and S.P. Cox
.tsBootIdx <- function (  x = histData, length = nT,
                          lims = NULL, margin = NULL )
{
  # recover the length of the historical data
  if ( is.null (dim(x)) ) nDat <- length ( x )
  else nDat <- dim (x)[margin]
  # Initialise vector to hold bootstrapped index values
  Yt <- numeric ( length = 0 )
  # Initialise bootstrapped TS length
  lSim <- 0
  while ( lSim < length )
  { 
    # Now start bootstrapping, no tolerances are used here...
    if ( is.null(lims)) histIdx <- 1:nDat else histIdx <- lims[1]:lims[2]
    segStart <- sample ( histIdx, size = 1 )
    
    # Set max length of first block based on segStart
    maxLenNew <- min ( histIdx[length(histIdx)] - segStart + 1, length - lSim )
    
    # Sample a random segment length uniformly from available lengths
    segLen <- sample ( x = 1:maxLenNew, size = 1 )
    # Compute segment ending position
    segEnd <- segStart + segLen - 1
    Yt <- c ( Yt, segStart:segEnd )
    # Add new length to lSim
    lSim <- lSim + segLen
  }
  return ( Yt )  
} # END function .tsBootIdx


# .tsBoot
# Purpose:        creates a time-series bootstrap sample from
#                 historical input values (e.g., Mt, alphat,etc)
# Parameters:     x=the values to sample, length=length of projection,
#                 tolSD=std dev for starting value, tolMult=how many
#                 tolSDs to allow.
#                 deltat=vector of std normals
# Returns:        nT-vector of log-normal error multipliers
# Usage:          tmp[t] = state[t]*error[t]
# Source:         S.D.N. Johnson and S.P. Cox
tsBoot <- function ( seed = NULL, x = histData, length = nT,
                     tolSD = 0.1, tolMult = 1 )
{
  # recover length of historical data
  nDat <- length ( x )
  # Initialise bootstrapped vector
  Yt <- numeric ( length = 0 )
  # Initialise bootstrapped TS length
  lSim <- 0
  # Set seed if provided
  if ( !is.null(seed) ) set.seed ( seed )
  while ( lSim < length )
  { 
    # If first block, then constrain the initial value to 
    # lie within predetermined interval
    # defined by tolSD and tolMult (defined in simCtlFile)
    if ( lSim == 0 ) 
    { 
      # Create max and min values for initial point
      minInit <- x [ nDat ] - tolSD * tolMult
      maxInit <- x [ nDat ] + tolSD * tolMult
      # Restrict to those possible starting points within range
      startIdx <- which ( (x <= maxInit) & (x >= minInit) )
      # Sample initial point based on bounds (maxInit, minInit)
      if ( length ( startIdx ) == 1 ) segStart <- startIdx
      else segStart <- sample ( startIdx, size = 1 )

      # if not the first block, sample from x
    } else segStart <- sample ( 1:nDat, size = 1 )
    
    # Set max length of first block based on segStart
    maxLenNew <- min ( nDat - segStart + 1, length - lSim )
    
    # Sample a random segment length uniformly from available lengths
    segLen <- sample ( x = 1:maxLenNew, size = 1 )
    # Compute segment ending position
    segEnd <- segStart + segLen - 1
    Yt <- c ( Yt, x [ segStart:segEnd ] )
    # Add new length to lSim
    lSim <- lSim + segLen
  }
  return ( Yt )  
}
