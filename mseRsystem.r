
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
#         of the current ageLenOpMod() function.                               #
#           similar format as the current ageLenOpMod() function.              #
#     (b) Modify solveInitPop() function so that new xxxOMod() is called in    #
#         place of ageLenOpMod() when needed.                                  #
#     (c) Modify .updatePop() function so new xxxOMod() is called in step 6    #
#         below instead of ageLenOpMod() when needed.                          #
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
runMSE <- function( ctlFile = "./simCtlFile.txt",  saveBlob=TRUE )
{

  require( tools )
  
  # No validity checking of .CTLFILE variables is done if runMSE is called
  # directly from the console.  In contrast, guiSim() calls via RUN button will
  # enforce checking of valid parmete3rs. prior to calling runMSE.
  
  ctlPars <- .readParFile( ctlFile )
  ctlList <- .createList( ctlPars )

  if( !ctlList$gui$quiet )
    cat( "\nMSG (runMSE) Parameter list created.\n" )
 
  # Start timer.
  t1 <- proc.time()[3]

  # Read rep file if included
  if( !is.null(ctlList$opMod$repFileName) )
  {
    ctlList$opMod$repFile <- lisread(ctlList$opMod$repFileName)

    if(ctlList$opMod$fillFromRep) 
      ctlList <- fillOpmodFromRep( obj = ctlList )
  }
  
  opMod <- ctlList$opMod
  
  # Calculate life history schedules and OM equilibrium reference points.  
  if( !ctlList$gui$quiet )
    cat( "\nMSG (.createMP) Calculating reference points...\n ")
  
  refPts <- calcRefPoints( opMod)

  if( !ctlList$gui$quiet )
    cat( "\nMSG (.mgmtProc) Elapsed time in calcRefPoints =",proc.time()[3]-t1,"\n" )

  if( !is.null(opMod$posteriorSamples) )
  {
    # Set seed and draw random sample of replicates here.
    mcmcParPath   <- file.path(ctlList$opMod$posteriorSamples,"mcmcPar.csv")
    mcmcPar       <- read.csv( mcmcParPath, header =T ) 
    samples       <- .quantileStratSample(  seed  = ctlList$opMod$postSampleSeed,
                                            post = mcmcPar,
                                            par1 = "m", par2 = "sbo",
                                            nBreaks = 10 )
    ctlList$opMod$posteriorDraws <- samples
    ctlList$opMod$mcmcPar <- mcmcPar
  }

  # Create the management procedure object.
  simObj <- .createMP( ctlList, refPts )

  # Run the management procedure.
  blob <- .mgmtProc(  simObj )

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
  if( !ctlList$gui$quiet )
    cat( "\nMSG (runMSE) Created simulation folder ",simFolder,"in",.PRJFLD,"\n" )

  # Save the simXXX.Rdata version of the blob to the simulation folder.
  blobFilePath <- file.path( .PRJFLD,simFolder,blobFileName )

  if ( saveBlob )
    .saveBlob( blobFilePath, blob  )

  # Copy the simulation control file into the folder.
  file.copy( ctlFile, file.path( .PRJFLD, simFolder, basename(ctlFile) ) )

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

  # Copy the TPL file into the folder.
  if ( ctlList$mp$assess$methodId==.ISCAMRW )
  {
    if( .ISCAMFLAG ) tplFile <- "iscam_ageM_rw.tpl"

    file.copy( file.path( getwd(), tplFile ),
               file.path( .PRJFLD, simFolder, tplFile ) )
  }


  if ( ctlList$mp$assess$methodId==.DDMOD )
  {
    file.copy( file.path( getwd(), "assessDD.cpp" ),
               file.path( .PRJFLD, simFolder, "assessDD.cpp" ) )

    file.copy( file.path( getwd(), "refPtsDD.cpp" ),
               file.path( .PRJFLD, simFolder, "refPtsDD.cpp" ) )
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

# fillOpmodFromRep()
# Automates filling the operating model sublist
# of the control list, avoiding changing ctlList
# parameters when switching rep files.
fillOpmodFromRep <- function( obj )
{
  repFile <- obj$opMod$repFile

  # Get leading pars
  obj$opMod$B0          <- repFile$SSB0
  obj$opMod$M           <- c(repFile$M_m, repFile$M_f)
  obj$opMod$rSteepness  <- repFile$h

  tMP <- obj$opMod$tMP

  # Calculate allocProp from last 20 years of fishing
  Ctg <- cbind( repFile$landedCtg1,
                repFile$landedCtg2,
                repFile$landedCtg3,
                repFile$landedCtg4,
                repFile$landedCtg5 )
  # 
  allocProp <- colSums(Ctg[(tMP-15):(tMP-1),])
  allocProp <- allocProp/sum(allocProp)
  obj$opMod$allocProp <- allocProp

  # Selectivity
  obj$opMod$L50Cg1 <- repFile$alpha_g1
  obj$opMod$L95Cg1 <- repFile$beta_g1

  # Now add the step parameter for asymptotic
  obj$opMod$selType <- repFile$selType
  selType           <- repFile$selType
  obj$opMod$L95Cg1[which(selType == 1)] <- repFile$alpha_g1[which(selType == 1)]
  obj$opMod$L50Cg1[which(selType == 1)] <- repFile$alpha_g1[which(selType == 1)] - repFile$beta_g1[which(selType == 1)]

  # Obs model variance
  obj$opMod$tauAgeg     <- (repFile$tauAge_m + repFile$tauAge_f)/2
  obj$opMod$tauIndexg   <- repFile$tauIndex

  obj$opMod$qg[c(1,4,5)] <- repFile$qg

  return(obj)
}

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
ageLenOpMod <- function( obj, t )
{
  # findOM -- string to use in finding this part of the code using search
  t1 <- proc.time()
  
  ctlList <- obj$ctlList
  opMod   <- ctlList$opMod
  
  tmpTimes <- .calcTimes( ctlList )
  
  # Extract constants.
  nT     <- as.numeric( ctlList$opMod$nT )
  A      <- as.numeric( ctlList$opMod$nAges )
  minAge <- as.numeric( ctlList$mp$data$minAge )
  tMP    <- as.numeric( ctlList$opMod$tMP )
  nGear  <- as.numeric( ctlList$opMod$nGear )

  # Set parameter values.
  B0         <- ctlList$opMod$B0
  rSteepness <- ctlList$opMod$rSteepness
  M          <- ctlList$opMod$M
  Mt         <- obj$om$Mt        # Average M without trend or pulse.
 

  # Length-at-age-/group-
  nGrps <- ctlList$opMod$nGrps
  Lal   <- obj$refPtList$Lal
  Wal   <- obj$refPtList$Wal

  # Get landed value matrix
  Val <- obj$refPtList$Val
  
  # Maturity ogive parameters
  A50   <- ctlList$opMod$aMat50
  A95   <- ctlList$opMod$aMat95
  Ma    <- obj$refPtList$Ma  

  futureAvoid <- ctlList$opMod$futureAvoid

  repFile <- ctlList$opMod$repFile
  
  # Retention probability for age-/length-/gear-  
  Palg <- obj$refPtList$Palg
  # Selectivity ogives for age-/length-/gear- 
  Salg <- obj$refPtList$Salg
  if( t >= tMP & futureAvoid ) # Recalculate selectivity using futureSel option
  {
    if( !is.null(ctlList$opMod$selAge) )
      selAge <- ctlList$opMod$selAge
    else selAge <- FALSE
    salgPars <- list( L50Cg1 = ctlList$opMod$L50Cg1,
                      L95Cg1 = ctlList$opMod$L95Cg1,
                      L95Cg2 = ctlList$opMod$L95Cg2,
                      L50Cg2 = ctlList$opMod$L50Cg2,
                      selType = ctlList$opMod$selType,
                      nGrps  = ctlList$opMod$nGrps,
                      nGear  = ctlList$opMod$nGear,
                      avoidProb = ctlList$opMod$avoidProb ,
                      selAge    = selAge )
    Salg <- .calcSalg( salgPars, A = A, Lal = Lal )
  }
  
  # retention future option: futureOption==2 means that
  # retention probabilities are made knife-edge to simulate
  # a no high-grading (retention) fishery 
  if ( 2 %in% ctlList$opMod$futureOption & t > tMP )
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
                      nGear    = nGear,
                      repFile  = repFile )
    tmpP <- .calcPalg(palgPars,A=A,Lal=Lal)
    Palg <- tmpP
  }

  # Overwrite prob discarding if juveCapProp == 0
  if( ctlList$mp$hcr$juveCapProp == 0 & t >= tMP )
  {
    Palg[,,1:(nGear - 2)] <- 0 
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

  if( t == 1 )
    avgR  <- repFile$avgR
  

  # We should get the Fs from the rep file
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
  Catg    <- obj$om$Catg    # array of gear-specific catchBiomass-at-age
  uCatg   <- obj$om$uCatg   # age-proportions in landed catch
  Ctg     <- obj$om$Ctg     # matrix of gear-specific total landed catch biomass
  adjCtg  <- obj$om$adjCtg  # matrix of gear-specific total landed catch biomass
  Ct      <- obj$om$Ct      # vector of total landed catch biomass
  Dt      <- obj$om$Dt      # vector of total discard biomass.
  Datg    <- obj$om$Datg    # array of dead discard biomass-at-age
  Dtg     <- obj$om$Dtg     # matrix of gear-specific total discarded biomass
  Ftg     <- obj$om$Ftg     # matrix of gear-specific fishing mortality
  Zalt    <- obj$om$Zalt    # array of total mortality by age-/length-group

  # Value of catch and discards
  valCatg   <- obj$om$valCatg
  valCtg    <- obj$om$valCtg
  valDatg   <- obj$om$valDatg
  valDtg    <- obj$om$valDtg

  if( t == 1 )
  {
    Discatg <- Datg
    Disctg  <- Dtg
    capCtg  <- Ctg
  } else {
    Discatg <- obj$om$Discatg
    Disctg  <- obj$om$Disctg
    capCtg  <- obj$om$capCtg
  }

  if( t > 1 )
    histAveDiscards <- obj$om$histAveDiscards

  if( t == 1 )
    overage_tg <- array( 0, dim = c(nT,nGear) )


  if( t > 1 )
    overage_tg <- obj$om$overage_tg

    
  fg    <- ctlList$opMod$fg       # Relative Fs by gear 
  dg    <- ctlList$opMod$dg       # discard mortality rates by gear\
  if( t >= tMP & !is.null(ctlList$opMod$dgFuture) )
    dg  <- ctlList$opMod$dgFuture 
  
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
    Rt[t]     <- obj$refPtList$R0
    #Nalt[,,t] <- outer( numAgeYr1, rep( obj$refPtList$piOne, nGrps ) )
    Nalt[,1:2,t] <- obj$refPtList$numAgeYr1
    # Nalt[,2,t] <- numAgeYr1_f
    Balt[,,t] <- Nalt[,,t]*Wal
    # Spawning biomass and number.
    Bt[t] <- sum(Balt[,2,t]*Ma)
    Nt[t] <- sum(Nalt[,2,t]*Ma)

    # Total biomass and number
    Btot[t] <- sum( Balt[,,t] )
    Ntot[t] <- sum( Nalt[,,t] )

    # Populate Fs with rep file
    Ftg[1:(tMP-1),1] <- repFile$Ftg1
    Ftg[1:(tMP-1),2] <- repFile$Ftg2
    Ftg[1:(tMP-1),3] <- repFile$Ftg3

    # Take average total discards
    histDiscards      <- cbind( repFile$obs_relCtg1, repFile$obs_relCtg2, repFile$obs_relCtg3 )
    histDiscards[histDiscards < 0] <- NA
    histTotalDiscards <- rowSums(histDiscards, na.rm = T )

    # Use 2006 onwards
    histAveDiscards   <- mean(histTotalDiscards[.DISCYEARS - .INITYEAR + 1])

  } # end "if(t==1)"
  else  # t>1
  {
    # Calculate age-1 recruitment, Beverton-Holt stock-recruitment.
    #if( t>44 )
    if( recOption=="Beverton-Holt" )
    {
        tmpR <- rec.a*Bt[t-1]/( 1.0 + rec.b*Bt[t-1] )
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

    # Spawning biomass and number...only use female group
    # Beginning of year.
    Bt[t]   <- sum(Balt[,2,t]*Ma)
    Nt[t]   <- sum(Nalt[,2,t]*Ma)


    # Total biomass and number
    Btot[t] <- sum( Balt[,,t] )
    Ntot[t] <- sum( Nalt[,,t] )
  }  # end t>1 ifelse

  if( t < tMP )
    Ctg[t,] <- Ctg[t,]/1000.


  # Adjust catch by amortized overage
  if( t > tMP )
    Ctg[t,] <- Ctg[t,] - overage_tg[t,]
  

  # Calc fishing mortality by age-/growth-group
  Falg <- array( data=NA, dim=c(A,nGrps,nGear) )
        
  # Solve catch equation for this year's Ftg based on tac allocated among gears
  # Not sure what these are for...
  solveOK <- FALSE
  nTimes  <- 0
  .PAUSE  <<- FALSE


  if( sum(Ctg[t,1:3]) > 0. & t >= tMP ) # don't bother if fishery catch=0
  {
    gT <<- t
    relLal <- matrix(1,nrow=nrow(Lal),ncol=ncol(Lal))
    # No size limit prior to 1988
    #if( t < 20  )
    #{
    #    relLal[ Lal < 81 ] <- 0.
    #    for( g in 1:4 )
    #        Palg[,,g] <- Palg[,,g]*relLal
    #}
    initF <- c(rep(0.05,3),0,0)   # SJ this is hardcoded in for a given # of fisheries/surveys. Too path specific!!!!
    idx0  <- which(Ctg[t,] < 0)
    Ctg[t,idx0] <- 0.
    baranovTime <<- t
    #if(t>17) browser()
    # SPC 7-Mar-2016: The output catch in the final two years jumps for g=1
    # to almost 3x the input catch.
    # Also, still getting SSB way higher here than SCAL output for 1998
    # Also, the post-tMP simulated biomass seems too consistent 
    # among 3 trial reps
        
    solveF <- .solveBaranov1(  B=Balt[,,t], S=Salg, P=Palg, D=dg, F=initF, M=Mt[,t],
                                    C=Ctg[t,], lam=ctlList$opMod$baranovSteps )

    solveF[ solveF > 1 ] <- 1.
    if( any( solveF < 0 ) )
    {
        cat("Warning: negative Ftg... t = ", t, "\n")
        cat( solveF, "\n" )
        solveF[ solveF < 0. ] <- 0.
    }
    Ftg[t,] <- solveF
  } 
  if( sum(Ctg[t,1:3]) == 0.) 
  {
    Ftg[t,] <- 0
  }

  # Compute total mortality by summing M and Fg over gear types
  Zalt[,,t] <- matrix(Mt[,t], ncol = nGrps, nrow = A, byrow = TRUE)
  for( g in 1:(nGear-2) )
    Zalt[,,t] <- Zalt[,,t] + Salg[,,g]*Ftg[t,g]*(dg[g]*Palg[,,g] - Palg[,,g] + 1.)

  # Catch- (Cal), dead-discards (Dal), and total-discards-at-age (Discal)
  legalC <- 0.; legalD <- 0.; sublegalC <- 0.; sublegalD <- 0.
  sublegalDisc <- c(0.,0.,0.)
  for( g in 1:(nGear-2) )
  {
    Cal     <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*Ftg[t,g]*(1.-Palg[,,g])/Zalt[,,t]
    Dal     <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*(1.-exp(-dg[g]))*Ftg[t,g]*Palg[,,g]/Zalt[,,t]
    Discal  <- Balt[,,t]*(1.-exp(-Zalt[,,t]))*Salg[,,g]*Ftg[t,g]*Palg[,,g]/Zalt[,,t]
    
    # catch-age/year/gear
    Catg[,t,g]    <- rowSums( Cal )
    valCatg[,t,g] <- rowSums( (Cal / Wal) * Val + opMod$collarPrice * Cal/Wal )
    Datg[,t,g]    <- rowSums( Dal )
    valDatg[,t,g] <- rowSums( (Dal / Wal) * Val + opMod$collarPrice * Dal/Wal )

    Discatg[,t,g] <- rowSums( Discal )


    # catch-year/gear
    capCtg[t,g] <- sum( Catg[,t,g] )
    valCtg[t,g] <- sum( valCatg[,t,g] )
    Dtg[t,g]    <- sum( Datg[,t,g] )
    valDtg[t,g] <- sum( valDatg[,t,g] )
    Disctg[t,g] <- sum( Discatg[,t,g] )
    
    # accumulate legal catch, legal dead discards, and sublegal total dead discards
    legalC    <- legalC + sum( Cal )
    legalD    <- legalD + sum( Dal )
    sublegalD <- sublegalD + sum( Dal )

    Ctg[t,g]  <- capCtg[t,g]

    # Now sum sublegal discards
    sublegalDisc[g] <- sum( Discal )
  }



  # Now apply the sublegal cap if catch is positive
  if( !is.null(ctlList$mp$hcr$juveCapProp) & t >= tMP )
  { 
    juveCapProp <- ctlList$mp$hcr$juveCapProp
    sublegalC   <- 0.
    sublegalCap <- juveCapProp * histAveDiscards * ctlList$mp$hcr$juveCapAlloc

    amortPeriod <- ctlList$mp$hcr$juveDiscAmortization
    for( g in 1:(nGear - 2) )
    {
      # Calculate
      tmpOver <- Disctg[t,g] - sublegalCap[g]

      # Amortize
      tmpOver <- tmpOver / amortPeriod
      
      # Add to overage calculation
      nAmortYrs <- min(5, nT - t - 1 )
      
      if( nAmortYrs > 0)
        overage_tg[ t + (1:nAmortYrs), g ] <- overage_tg[ t + (1:nAmortYrs), g ] + tmpOver
    }

    # Now stop overage from going negative (avoid inflating legal TAC)
    overage_tg[overage_tg < 0] <- 0
  }


  # Total landings and harvest rates.
  legalB     <- sum( Balt[,,t]*(1-Palg[,,1]) )
  sublegalB  <- sum( Balt[,,t]*(Palg[,,1]) )
  Ct[t]      <- sum( Ctg[t,1:3] )
  Dt[t]      <- sum( Dtg[t,1:3] )
  legalHR    <- (legalC+legalD)/legalB
  sublegalHR <- (sublegalD+sublegalC)/sublegalB

  if(any(!is.na(obj$om$Itg[1:(tMP-20),5])))
    browser()

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
    if( !.INDEXSERIESINPUT | (t > (tMP-1)) ) 
    {
      # Only impute a tMP point for indices active in the future.
      if ( k2Index[i] > 0 )
      {
         obj$om$Itg[t,g] <- qg[g]*sum( Salg[,,g]*Balt[,,t] )^powerg[g]
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
      if( any(!is.finite(Ctg[t,]))) browser()
      if( Ctg[t,g] > 0. ) 
      {
        uCatg[minAge:A,t,g] <- Catg[minAge:A,t,g]/Ctg[t,g] # true proportions in catch, A
        # sampled age-proportions in landed catch
        obj$mp$data$Xatg[minAge:A,t,g]   <- .calcXat( uAge=uCatg[minAge:A,t,g],tauAge=tauAgeg[i],eps=epsilongat[g,minAge:A,t] )
        # add "misQ" to argument list to include age misclassification matrix
      }      
    }
  }

  # Update all objects in obj for this time step.
  obj$om$Nalt  <- Nalt    # numbers-at-age
  obj$om$Balt  <- Balt    # biomass-at-age
  obj$om$Zalt  <- Zalt    # total mortality
  obj$om$Bt    <- Bt      # spawning biomass
  obj$om$Nt    <- Nt      # spawning population
  obj$om$Btot  <- Btot    # total biomass
  obj$om$Ntot  <- Ntot    # total abundance
  obj$om$Rt    <- Rt      # recruitment
  obj$om$Mt    <- Mt      # natural mortality - may have been pulsed
  
  if( t==1 )
    obj$om$avgR  <- avgR      # recruitment

  obj$om$Ct       <- Ct      # landed catch in biomass
  obj$om$Dt       <- Dt      # discards in biomass
  obj$om$Ctg      <- Ctg     # landed catch in biomass by gear
  obj$om$adjCtg   <- adjCtg  # Adjusted (for discarded juves) catch in biomass by gear
  obj$om$Catg     <- Catg    # landed catch by age/gear
  obj$om$uCatg    <- uCatg   # age-proportions in landed catch
  obj$om$Dtg      <- Dtg     # dead discard in biomass by gear
  obj$om$Datg     <- Datg    # dead discards by age/gear
  obj$om$Discatg  <- Discatg  # all discards by age/gear  
  obj$om$Disctg   <- Disctg   # all discards
  obj$om$capCtg   <- Ctg
  obj$om$Ftg      <- Ftg     # realized fishing mortality rate by gear

  # Update value OM matrices
  obj$om$valCtg     <- valCtg
  obj$om$valCatg    <- valCatg
  obj$om$valDtg     <- valDtg
  obj$om$valDatg    <- valDatg
  obj$om$overage_tg <- overage_tg
  
  obj$om$legalHR[t]        <- legalHR      # legal harvest rate
  obj$om$sublegalHR[t]     <- sublegalHR   # sublegal harvest rate
  obj$om$legalB[t]         <- legalB       # legal biomass
  obj$om$sublegalB[t]      <- sublegalB    # sublegal biomass
  obj$om$legalC[t]         <- legalC       # legal catch
  obj$om$legalD[t]         <- legalD       # legal discards
  obj$om$sublegalD[t]      <- sublegalD    # sublegal discards
  obj$om$histAveDiscards   <- histAveDiscards
  
  # obs/process errors
  obj$om$errors$omegat     <- omegat       # recruitment process error
  obj$om$errors$epsilontg  <- epsilontg    # index obs error
  obj$om$errors$epsilongat <- epsilongat   # age prop sampling error

  return( obj )
}     # END function ageLenOpMod

#------------------------------------------------------------------------------#
#-- MP Assessment Method Helper functions                                    --#
#------------------------------------------------------------------------------#

# .assessModMA       
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
  avgPoints <- obj$avgPoints

  # Remove missing values.
  tmp     <- It[ !is.na(It) ]
  nPoints <- min( length(tmp),avgPoints )
  tVec    <- c( (length(tmp) - nPoints + 1 ):length(tmp) )

  # Moving average of survey values.
  movinAvg <- exp( mean( log(tmp[tVec]) ) )
  assessment         <- list()
  assessment$biomass <- movinAvg
  return( assessment )
}    # END function .assessModMA

# .callProcedureMovingAvg       
# Purpose:        wrapper function for the MovAvg MP
# Parameters:     obj=list containing om and mp lists
#                 t=current time step
# Returns:        stock assessment output from .assessModMA()
# Source:         S.P. Cox
.callProcedureMovingAvg <- function( obj, t )
{
  # Extract operating model and management procedure objects
  om <- obj$om
  mp <- obj$mp

  # Extract index values - take last survey
  It <- mp$data$Itg[ c(1:(t-1)), 5 ]
 
  # Assessment frequency information - not implemented yet
  tmp             <- list()               # Temporary list
  tmp$It          <- It                   # Stock index      
  tmp$avgPoints   <- mp$assess$avgPoints  # Number of index point to average
  stockAssessment <- .assessModMA( tmp )   # Call moving average method
  stockAssessment
}     # END function callAssessMovingAvg


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

# .callProcedureSP       
# Purpose:        wrapper function for the Surplus Production MP
# Parameters:     obj=list containing om and mp lists
#                 t=current time step
# Returns:        stock assessment output from .assessModSP()
# Source:         S.P. Cox
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

  # Silence SP model output?
  spObj$quiet     <- obj$gui$quiet
  
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
  
  t     <- spObj$t

  quiet <- spObj$quiet

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
              intern=TRUE, wait=TRUE, ignore.stdout=quiet, ignore.stderr = quiet )

      system( command=paste( exeName," ","-mceval",sep="" ),
              intern=TRUE, wait=TRUE, ignore.stdout=T, ignore.stderr = quiet )
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
  assessment$SBt <- c(assessSP$Bt, assessSP$projExpBio)

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

# .callProcedureCAA       
# Purpose:        wrapper function for the assessCA MP
# Parameters:     obj=list containing om and mp lists
#                 t=current time step
# Returns:        stock assessment output from .assessModCAA()
# Source:         S.P. Cox, updated for mseRmfleet by SDN Johnson
.callProcedureCAA <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

  caObj     <- list()                        # List to pass to assessModCAA

  assessCtl <- obj$ctlList$mp$assess

  # Fit control pars
  caObj$maxPhase    <- assessCtl$caaMaxPhase
  caObj$silent      <- assessCtl$caaSilent
  caObj$calcSD      <- TRUE
  caObj$maxEval     <- 1e4
  caObj$maxIter     <- 1e4

  # Pull opMod and om
  opMod           <- obj$ctlList$opMod
  om              <- obj$om

  # Get ages
  caObj$A         <- opMod$nAges
  minAge          <- obj$ctlList$mp$data$minAge
  patg            <- obj$mp$data$Xatg[,(1:(t-1)),]

  # Coerce missing ages to -1
  patg[is.na(patg)] <- -1
  patg[1,,] <- 0

  # Get times
  caObj$t         <- t                # Current time step.
  caObj$nT        <- opMod$nT         # Total number of time steps
  tMP             <- opMod$tMP        # Year MP first applied, tMP.
  
  # Get indices
  I_tg              <- om$Itg[1:(caObj$t-1),]
  I_tg[is.na(I_tg)] <- -1

  # Observed weight at age
  wtAge_ta          <- obj$om$Wta[1:(caObj$t-1),]
  wtAge_ta[,1]      <- 0.02

  
  
  # (1) Data.
  caObj$data          <- list()

  # Populate data
  caObj$data$I_tg                 <- I_tg
  caObj$data$C_tg                 <- om$Ctg[1:(caObj$t-1),]
  caObj$data$A_atg                <- patg
  caObj$data$survType_g           <- c(-1,-1,-1,2,2)
  caObj$data$indexType_g          <- c(-1,-1,-1,0,1)
  caObj$data$calcIndex_g          <- c(0,0,0,1,1)
  caObj$data$selType_g            <- c(0,0,0,0,0)
  caObj$data$selX_g               <- c(0,0,0,0,0)
  caObj$data$fleetTiming          <- c(.48,.49,.5,.98,0.99)
  caObj$data$initCode             <- 1
  caObj$data$posPenFactor         <- 1e4
  caObj$data$firstRecDev          <- 2
  caObj$data$minAgeProp           <- .02
  caObj$data$wtAge_ta             <- wtAge_ta
  caObj$data$spawnTiming          <- 1
  caObj$data$nYearsAveProj        <- assessCtl$caaMoveAveYrs

  checkNA <- function( x )
  {
    cond <- any(is.na(x))
    
    cond 
  }

  naData <- lapply(X = caObj$data, FUN = checkNA )
  if(any(naData))
  {
    whichData <- which(naData)
    cat("\n NAs found in data object", whichData, "\n")
    browser()
  }


  IGa <- .1
  IGmode <- c(.9306,1.0856) * .461853
  IGb <- IGmode * (1 + IGa)

  IGa_R     <- .1
  IGmode_R  <- .706
  IGb_R     <- IGmode_R * (1 + IGa_R)


  # (2) pars
  caObj$pars <- list()
  # Populate init pars
  caObj$pars$lnB0                 <- 5
  caObj$pars$logit_ySteepness     <- 0
  caObj$pars$lnM                  <- -.79
  caObj$pars$log_initN_mult       <- rep(0,caObj$A)
  caObj$pars$lnSelAlpha_g         <- log(c(2.66622,2.91102,4.67541,0,0))
  caObj$pars$lnSelBeta_g          <- log(c(3.549228,3.902951,5.862269,0.01,0.01))
  caObj$pars$effSampleSize_g      <- rep(100,5)
  caObj$pars$recDevs_t            <- rep(0,caObj$t - 3 )
  caObj$pars$lnsigmaR             <- 0
  caObj$pars$omegaM_t             <- rep(0,caObj$t-2)
  caObj$pars$lnsigmaM             <- log(.1)
  caObj$pars$obstau2IGa           <- rep(IGa,5)
  caObj$pars$obstau2IGb           <- rep(IGb,5)
  caObj$pars$sig2RPrior           <- c(IGa_R,IGb_R)
  caObj$pars$sig2MPrior           <- c(1,0.02)
  caObj$pars$rSteepBetaPrior      <- c(10,4.92)
  caObj$pars$initMPrior           <- c(-.79,.4)
  caObj$pars$mq                   <- c(1,1,1,.5,0.1)
  caObj$pars$sdq                  <- c(2,2,2,2,0.01)
  caObj$pars$aMat                 <- c(2.55,3.12)
  caObj$pars$Linf                 <- c(20)
  caObj$pars$L1                   <- c(4)
  caObj$pars$vonK                 <- c(.275)
  caObj$pars$lenWt                <- c(1.04e-5,3.07)
  caObj$pars$mSelMode_g           <- c(3.549228,3.902951,5.862269,2.202222,2.202222)
  caObj$pars$selModeCV            <- 1

  # Random effects
  caObj$random                    <- assessCtl$caaRandomEff

  # (3) Map 
  # list for identifying/switching off parameters
  # Only need to define "special" maps here 
  # e.g. selectivity
  caObj$map <- list()

  # selMap
  selMap <- c(101,102,103,NA,NA)

  # Populate
  caObj$map$lnSelAlpha_g          <- factor(selMap)
  caObj$map$lnSelBeta_g           <- factor(selMap + 10)

  # Phases
  caObj$phases$lnB0               <- 1
  caObj$phases$logit_ySteepness   <- 2
  caObj$phases$lnM                <- 2
  caObj$phases$log_initN_mult     <- 1
  caObj$phases$lnSelAlpha_g       <- 4
  caObj$phases$lnSelBeta_g        <- 4
  caObj$phases$recDevs_t          <- 3
  caObj$phases$lnsigmaR           <- 6
  caObj$phases$omegaM_t           <- 5
  caObj$phases$lnsigmaM           <- -1

  # Check that map and par lengths match
  parLen <- lapply( X = caObj$pars, FUN = length)
  mapLen <- lapply( X = caObj$map, FUN = length)
  parLen <- parLen[names(parLen) %in% names(mapLen)]

  lenDiff <- unlist(parLen[names(mapLen)]) - unlist(mapLen)

  if(any(lenDiff != 0) )
  {
    whichDiff <- which(lenDiff != 0)
    cat("\n Map and par lengths don't match for ", names(whichDiff) ," \n") 

    browser()
  }

  if(obj$ctlList$mp$assess$writeOut)
  {
    cat("\n Saving asessCA lists to working directory. \n")
    save(caObj,file = "assessCALists.RData")
  }
  

  # -- Call assessment model, which will do its own estimation procedure
  result <- .assessModCAA( caObj )

  result  
}     # END function callProcedureCAA

# .assessModCAA
# Purpose:       Runs a statistical catch-age model to estimate biomass       #
#                and fishery reference points if required.
# Parameters:    obj=list containing everything, because this thing needs it  #
# Returns:       current biomass estimate, stock dynamics and parameters,     #
#                minimization details, yield and ref pt calcs                 #
# Notes:         Execution is platform-dependent - see system() call          #
# Source:        S.P. Cox (1-May-10) updated by SDN Johnson, 12-Jun-19        #
.assessModCAA <- function( caObj )
{
  # Statistical catch-age model.
  # This implmentation is a wrapper that applies the assessCA
  # model using a phased minimisation procedure TMBPhase()
  #
  #
  nT  <- caObj$nT
  t   <- caObj$t

  # Load the TMB model
  dyn.load(dynlib("assessCA")) 

  # Run TMBphase
  phzList <- TMBphase(  data = caObj$data, 
                        parameters = caObj$pars, 
                        random = caObj$random,
                        phases = caObj$phases, 
                        base_map = caObj$map,
                        maxPhase = caObj$maxPhase,
                        model_name = "assessCA",
                        optimizer = "nlminb",
                        silent = caObj$silent,
                        calcSD = caObj$calcSD,
                        maxEval = caObj$maxEval,
                        maxIter = caObj$maxIter ) 


  assessment  <- phzList$repOpt
  sdrep       <- phzList$sdrep

  assessment$sbt <- assessment$SB_t

  assessment$mpdPars <- list( objFun        = phzList$objFun,
                              R0            = assessment$R0,
                              SSB0          = assessment$B0,
                              B0            = assessment$B0,
                              q             = assessment$qhat_g,
                              projExpBio    = assessment$SB_t[t],
                              lastDt        = assessment$D_t[t-1] )
                              
  assessment$mcOut                    <- NULL                                          
  # assessment$runStatus                <- tmpFit$fit[c("nopar","nlogl","maxgrad","npar","logDetHess")]
  assessment$runStatus$maxGrad        <- phzList$maxGrad
  assessment$runStatus$hessPosDef     <- sdrep$pdHess
  assessment$runStatus$objFun         <- phzList$objFun
  assessment$runStatus$fisheryClosed  <- NA
  assessment$runStatus$deadFlag       <- NA
  assessment$runStatus$assessFailed   <- !phzList$success 
  assessment$phzList                  <- phzList

  assessment$exploitBt <- assessment$SB_t[t]
  assessment$spawnBt   <- assessment$SB_t[t]

  dyn.unload(dynlib("assessCA"))          # Dynamically link the C++ code


  return( assessment )
}     # END function assessModCAA



# .callProcedureDD       
# Purpose:        wrapper function for the Delay Difference MP
# Parameters:     obj=list containing om and mp lists
#                 t=current time step
# Returns:        stock assessment output from .assessModMA()
# Source:         S.D.N. Johnson 12-Jun-19
callProcedureDD <- function( obj, t )
{
  # Input object is from .createMP, it is NOT a blob.  So obj$opMod has all
  # reference points and life history schedules.

  ddObj     <- list()                        # List to pass to assessModDD

  assessCtl   <- obj$ctlList$mp$assess

  ddObj$quiet <- obj$ctlList$gui$quiet

  # Fit control pars
  ddObj$calcSD      <- FALSE
  ddObj$maxEval     <- 1e4
  ddObj$maxIter     <- 1e4

  # Pull opMod and om
  opMod           <- obj$ctlList$opMod
  om              <- obj$om
  mp              <- obj$mp

  
  # Get times
  ddObj$t         <- t                # Current time step.
  ddObj$nT        <- opMod$nT         # Total number of time steps
  tMP             <- opMod$tMP        # Year MP first applied, tMP.
  
  # Get indices
  useIndex          <- eval( parse( text=obj$ctlList$mp$data$useIndex ) )
  I_tg              <- mp$data$Itg[1:(ddObj$t-1),useIndex]
  I_tg[is.na(I_tg)] <- -1
  nIndices          <- length(useIndex)


  # Make FW pars
  FWpars  <- .makeFWpars( vonK  = mean(opMod$vonK), 
                          Linf  = mean(opMod$Linf), 
                          L1    = mean(opMod$L1), 
                          c1.lw = mean(opMod$c1), 
                          c2.lw = mean(opMod$c2), 
                          nAges = opMod$nAges )

  kage    <- FWpars$DDpars$DDpar.hi["kage"]

  # Set first estimated recruitment deviation
  firstRecDev <- 10

  
  # (1) Data.
  ddObj$data          <- list()

  # Populate data
  ddObj$data$I_tg            <- I_tg
  ddObj$data$C_t             <- apply(X = om$Ctg[1:(ddObj$t-1),], FUN = sum, MARGIN = 1)
  ddObj$data$wbar_t          <- rep(-1, ddObj$t-1)
  ddObj$data$kage            <- FWpars$DDpars$DDpar.hi["kage"]
  ddObj$data$alpha           <- FWpars$DDpars$DDpar.hi[2]
  ddObj$data$rho             <- FWpars$DDpars$DDpar.hi[3]
  ddObj$data$wk              <- FWpars$wtAge[kage]
  ddObj$data$initBioCode     <- 1 - obj$ctlList$mp$assess$ddUnfished
  ddObj$data$useWbar         <- 0
  ddObj$data$survType_g      <- as.integer(rep(0,nIndices))
  ddObj$data$indexType_g     <- as.integer(rep(0,nIndices))
  ddObj$data$firstRecDev     <- firstRecDev
  ddObj$data$pospenScale     <- 1e3 


  checkNA <- function( x )
  {
    cond <- any(is.na(x))
    
    cond 
  }

  naData <- lapply(X = ddObj$data, FUN = checkNA )
  if(any(unlist(naData)))
  {
    whichData <- which(unlist(naData))
    cat("\n NAs found in data object", whichData, "\n")
    browser()
  }



  IGa_R     <- .1
  IGmode_R  <- opMod$sigmaR
  IGb_R     <- IGmode_R * (1 + IGa_R)

  # Get mode of obs error IG prior
  tauObs  <- opMod$tauIndexg
  tau2Obs <- tauObs^2

  # Prior on q
  mq  <- rep(.5,3)
  sdq <- mq / 2

  # (2) pars
  ddObj$pars <- list()
  # Populate init pars
  # Make parameter list
  ddObj$pars$logith         <- 0
  ddObj$pars$lnB0           <- log(sum(ddObj$data$C_t))
  ddObj$pars$lnM            <- log(obj$ctlList$mp$assess$ddPmM)
  ddObj$pars$lntauObs_g     <- log(tauObs)
  ddObj$pars$lnq_g          <- log(mq)
  ddObj$pars$recDevs_t      <- rep(0, t - 1 - firstRecDev - kage)
  ddObj$pars$lnsigmaR       <- 0
  ddObj$pars$gammaR         <- 0.
  ddObj$pars$lnFinit        <- -5
  ddObj$pars$sig2RPrior     <- c(1,2)
  ddObj$pars$mM             <- obj$ctlList$mp$assess$ddPmM
  ddObj$pars$sdM            <- obj$ctlList$mp$assess$ddPsdM
  ddObj$pars$hPrior         <- c(11,9)
  ddObj$pars$mq             <- mq
  ddObj$pars$sdq            <- sdq
  

  # Random effects
  ddObj$random              <- "recDevs_t"

  # (3) Map 
  # list for identifying/switching off parameters
  # Only need to define "special" maps here 
  # e.g. selectivity
  ddObj$map <- list()

  # make prior variances inactive
  ddObj$map$lnsigmaR      <- factor(NA)
  ddObj$map$lnM           <- factor(NA)
  ddObj$map$gammaR        <- factor(NA)
  ddObj$map$lnFinit       <- factor(NA)
  ddObj$map$sig2RPrior    <- factor(c(NA,NA))
  ddObj$map$mM            <- factor(NA)
  ddObj$map$sdM           <- factor(NA)
  ddObj$map$hPrior        <- factor(c(NA,NA))
  ddObj$map$mq            <- factor(rep(NA,nIndices))
  ddObj$map$sdq           <- factor(rep(NA,nIndices))
  ddObj$map$lntauObs_g    <- factor(rep(NA,nIndices))


  

  # Check that map and par lengths match
  parLen <- lapply( X = ddObj$pars, FUN = length)
  mapLen <- lapply( X = ddObj$map, FUN = length)
  parLen <- parLen[names(parLen) %in% names(mapLen)]

  lenDiff <- unlist(parLen[names(mapLen)]) - unlist(mapLen)

  if(any(lenDiff != 0) )
  {
    whichDiff <- which(lenDiff != 0)
    cat("\n Map and par lengths don't match for ", names(whichDiff) ," \n") 

    browser()
  }

  if(obj$ctlList$mp$assess$writeOut)
  {
    cat("\n Saving assessDD lists to working directory. \n")
    save(ddObj,file = "assessDDLists.RData")
  }
  

  # -- Call assessment model, which will do its own file read/write
  result <- assessModDD( ddObj )

  result  
}     # END function callProcedureDD

# assessModDD
# Purpose:       Runs a delay difference TMB model to estimate stock status   #
#                and calculate fishery reference points
# Parameters:    obj=list containing everything the model needs               #
# Returns:       current biomass estimate, stock dynamics and parameters,     #
#                minimization details, yield and ref pt calcs                 #
# Source:        S. D. N. Johnson (modified from assessModCAA, June 12 2019)  #
assessModDD <- function( ddObj )
{
  # Schnute-Deriso Delay Difference model
  # This implmentation is a wrapper that calls the nlminb estimation
  # procedure, with the TMB model as the gradient function, and 
  # returns the assessmemt
  #
  nT  <- ddObj$nT
  t   <- ddObj$t

  # Implement a refit routine here, then 
  # we can say something about assessfailed.

  ctrl <- list( eval.max = ddObj$maxEval, 
                iter.max = ddObj$maxIter )

  objFE <- MakeADFun( data=ddObj$data,
                      parameters=ddObj$pars,
                      map=ddObj$map, 
                      random = NULL, silent = ddObj$quiet,
                      DLL = "assessDD" )

  initPars <- objFE$par

  fitFE <- try( nlminb (  start     = initPars,
                          objective = objFE$fn,
                          gradient  = objFE$gr,
                          control   = ctrl ) )

  nTries <- 1

  while( class(fitFE) == "try-error" )
  {
    nPars <- length(initPars)
    initPars <- initPars + exp(rnorm(nPars,0,1))
    fitFE <- try( nlminb (  start     = initPars,
                            objective = objRE$fn,
                            gradient  = objRE$gr,
                            control   = ctrl ) )
  } 


  rpt     <- objFE$report()
  sdrpt   <- sdreport(objFE)
  maxGrad <- max(abs(objFE$gr()))



  # else {
  #   # Save the fixed effects object as 
  #   # the assessment object in case
  #   # RE method fails
    

  #   # attempt fitting with random effects
  #   objRE <- MakeADFun( data=ddObj$data,
  #                     parameters=ddObj$pars,
  #                     map=ddObj$map, 
  #                     random=c("recDevs_t","lnsigmaR","lnFinit"), 
  #                     silent = ddObj$quiet,
  #                     DLL = "assessDD" )

  #   bestPars <- fitFE$par[names(fitFE$par) %in% names(objRE$par)]

  #   fitRE <- try( nlminb (  start     = bestPars,
  #                           objective = objRE$fn,
  #                           gradient  = objRE$gr,
  #                           control   = ctrl ) )

  #   while(class(fitRE) == "try-error")
  #   {
  #     nPars <- length(bestPars)
  #     bestPars <- bestPars + exp(rnorm(nPars,0,1))
  #     fitRE <- try( nlminb (  start     = bestPars,
  #                             objective = objRE$fn,
  #                             gradient  = objRE$gr,
  #                             control   = ctrl ) )
  #   }
  
  #   # Apply refitting procedure again

  #   # If REs fit succesfully, then
  #   # save RE object as assessment object
  #   if( fitRE$convergence == 0 )
  #   {
  #     rpt   <- objRE$report()
  #     sdrpt <- sdreport(objRE)
  #     maxGrad <- max(abs(objRE$gr()))
  #   }
      
  # }
  

  refPtsData <- list( rec_a = rpt$reca,
                      rec_b = rpt$recb,
                      M = rpt$M,
                      alpha = rpt$alpha,
                      rho = rpt$rho,
                      w_k = rpt$wk )

  refPtsPar <- list(Fmsy = 0.05 )

  objRP <- MakeADFun( data = refPtsData,
                      parameters = refPtsPar,
                      map = NULL,
                      DLL = "refPtsDD",
                      silent = ddObj$quiet,
                      random = NULL )

  fitRP <- try(nlminb ( start     = objRP$par,
                        objective = objRP$fn,
                        gradient  = objRP$gr,
                        control   = ctrl ) )

  if( class( fitRP ) == "try-error" )
    browser()


  # Start arranging things for output
  assessment        <- rpt
  assessment$sd     <- sdrpt
  assessment$refPts <- objRP$report()

  assessment$SBt <- assessment$B_t

  assessment$mpdPars <- list( objFun        = assessment$objFun,
                              R0            = assessment$R0,
                              SSB0          = assessment$B0,
                              q             = assessment$q_g,
                              projExpBio    = assessment$B_t[t],
                              lastDt        = assessment$D_t[t-1],
                              ssbFmsy       = assessment$refPts$Bmsy,
                              yieldFmsy     = assessment$refPts$MSY,
                              Fmsy          = assessment$refPts$Fmsy )
                              
  assessment$mcOut                    <- NULL                                          
  assessment$runStatus$maxGrad        <- maxGrad
  assessment$runStatus$hessPosDef     <- assessment$sd$pdHess
  assessment$runStatus$objFun         <- assessment$objFun
  assessment$runStatus$fisheryClosed  <- NA
  assessment$runStatus$deadFlag       <- NA
  assessment$runStatus$assessFailed   <- FALSE

  assessment$exploitBt <- assessment$B_t[t]
  assessment$spawnBt   <- assessment$B_t[t]

  


  return( assessment )
}     # END function assessModDD



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

  # Calculate the slow-up rule
  slowUpYears  <- obj$slowUpYears
  if( is.null(slowUpYears) )
    slowUpYears <- 1

  # Get indices for recent years
  recentYears <- (t - slowUpYears + 1):(t)

  # Change for slow up rule: overwrite legal biomass (3+ fish in year t)
  # with the spawning biomass estimate in years 1:t from the AM
  legalBiomass <- c(obj$SBt)[recentYears]

  # Calculate hockey-stock legal harvest rule, with slow up
  # rule enforced (normal rule is if slowUpYears = 1)
  if( any(legalBiomass < lowerBound ) ) 
    adjF <- 0.
  if( all(legalBiomass >= lowerBound ) )
  {
    if( legalBiomass[slowUpYears] < upperBound )
    adjF <- remRate*(legalBiomass[slowUpYears]-lowerBound)/(upperBound-lowerBound)
  if( legalBiomass[slowUpYears] >= upperBound ) 
    adjF <- remRate 
  }
   


  # If the assessment failed, limit F to maxF
  if( fail==TRUE & adjF > maxF )
    adjF <- maxF

  # Final catch limit
  catchLim <- adjF*legalBiomass[slowUpYears]

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
    
  # Calculate the slow-up rule
  slowUpYears  <- obj$slowUpYears
  if( is.null(slowUpYears) )
    slowUpYears <- 1

  # Get indices for recent years
  recentYears <- (t - slowUpYears + 1):(t)

  # Change for slow up rule: overwrite legal biomass (3+ fish in year t)
  # with the spawning biomass estimate in years 1:t from the AM
  legalBiomass <- c(obj$sbt)[recentYears]
  # Overwrite the sbt estimate with the 3+ fish for the last year
  legalBiomass[slowUpYears] <- obj$biomass

  # convert remRate to U

  remRate <- obj$targHR

  # Safeguards to limit F in case assessment failed in a particular year
  fail         <- obj$assessFailed     # TRUE is assessment model failed
  maxF         <- 1 - exp( - obj$maxF) # Maximum allowable F in case estimation fails.


  if( any(legalBiomass < lowerBound ) ) 
  {
    adjF <- 0.
  }
  if( all(legalBiomass >= lowerBound ) )
    adjF <- remRate
  
  # If the assessment failed, limit F to maxF
  if( fail==TRUE & adjF > maxF )
    adjF <- maxF

  # Final catch limit treating adjF as a harvest rate
  catchLim <- min( adjF*legalBiomass[slowUpYears], max(legalBiomass[slowUpYears] - lowerBound,0) )


  result                <-list()
  result$precautionaryF <- adjF
  result$catchLimit     <- catchLim
  result
}     # END function .calcHarvRuleHerringDFOMP


# .calcHCR_ccRule       
# Purpose:        harvest control rule to generate the quota
# Parameters:     obj=list containing all variables and parameters
#                 necessary to compute the quota
# Requires:       global variable Bmin = initial value for lowest biomass
# Returns:        the quota=catch
# Source:         S.P. Cox
.calcHCR_ccRule <- function( hcrList )
{
  
  # Function to find the minimum biomass over past 10 years
  # from which the stock has recovered.  
  find_min <- function(x)
  {
    # Goal: find the minumum value of x that has been "recovered" from.
    # In other words, find the smallest datapoint x[t] such that x[t] < x[t+1].
    # If NA is returned, then no such point has been found.
  
    if( length(x) == 1 ) return(NA)
  
    # propose first datapoint as current minimum
    min_curr <- x[1]
  
    # find another datapoint that is less than the current minimum
    # and also has a subsequent datapoint that is smaller than itself
    if( length(x)>2 ) {
      for( i in 2:(length(x)-1) ) {
        if( x[i]<min_curr && x[i]<x[i+1] ) min_curr <- x[i]
      }
    }
    # if the current minimum is still the first datapoint,
    # check the subsequent point
    if( min_curr==x[1] && x[1]>x[2] ) min_curr <- NA
    if(!is.na(min_curr)){
      if(min_curr==0) {
        min_curr <- 0.05
      }
    }
    min_curr
  }

  t   <- hcrList$t - 1
  I   <- hcrList$ccBiomass # this is survey only
  C   <- hcrList$Dt
  u   <- hcrList$remRate[t]

  # Biomass over previous ten years
  if( t > 10 ) B10 <- I[(t-10):t] + C[(t-10):t]
  else         B10 <- I[1:t] + C[1:t]

  # Proposed Bmin
  Bmin_prop <- find_min(B10)



  # Accept Bmin_prop if it exists, otherwise use last year's
  if(!is.na(Bmin_prop)) Bmin <<- Bmin_prop

  if( I[t] > Bmin ) {
    catchLimit <- u*(I[t]+C[t])
  } else {
    catchLimit <- 0
  }

  result <- list( catchLimit=max( 0, catchLimit ), 
                  precautionaryF=min(u,catchLimit/(I[t] + C[t])),
                  eBioAdj=NA )
  result
}     # END function calcHCR_ccRule

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
              SBt = NULL,     FBt=NULL,
              Nt=NULL,        Btot=NULL,      Ntot=NULL,    Rt=NULL,
              uCatg=NULL,     Ct=NULL,        Ctg=NULL,     Catg=NULL,
              valCtg=NULL,    valCatg=NULL,   valDtg=NULL,  valDatg=NULL,
              Dt=NULL,        Dtg=NULL,       Mt=NULL,      Ft=NULL,        
              Ftg=NULL,       legalHR=NULL, sublegalHR=NULL, spawnHR = NULL,
              legalB=NULL,    sublegalB=NULL, legalC=NULL,  legalD=NULL,
              sublegalD=NULL, Dt=NULL,        Dtg=NULL,     Itg=NULL )

  # State variables: numbers at a,t and biomass at a,t.
  om$Nalt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$Balt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$Zalt       <- array( data=NA, dim=c(nAges,nGrps,nT) )
  om$SBt        <- rep( NA, nT )
  om$FBt        <- rep( NA, nT )
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

  # Gear- and age-specific fishing mortality, catch, and discards
  om$Ftg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Ctg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Catg       <- array( data=NA, dim=c(nAges,nT,nGear) )
  om$Dtg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$Datg       <- array( data=NA, dim=c(nAges,nT,nGear) )

  # Value of catch-at-age and discards-at-age
  om$valCtg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$valCatg       <- array( data=NA, dim=c(nAges,nT,nGear) )
  om$valDtg        <- matrix( NA, nrow=nT, ncol=nGear )
  om$valDatg       <- array( data=NA, dim=c(nAges,nT,nGear) )

  om$legalHR    <- rep(NA,nT)
  om$spawnHR    <- rep(NA,nT)
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
    catch <- catch$landCatchMatrix
    years <- catch[,1]
    times <- catch[,2]

    om$Ctg[1:(tMP-1),1:ncol(om$Ctg)] <- catch[,3:7]

    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Extracted catch and converted units.\n" )   
    .CATCHSERIESINPUT <<- TRUE
  }
  else
  {
    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Calculating catch from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # (2b) BEGIN INDEX DATA - stored under obj$mp$data                         --#
  #----------------------------------------------------------------------------#

  if ( !is.null(ctlObj$mp$data$inputIndex) )
  {
    # Extract abundance indices from input file and assign 
    # to blob elements...
    indices  <- ctlObj$mp$data$inputIndex
    indices  <- indices$idxSeries
    useIndex <- tmpTimes$useIndex
    om$Itg[1:(tMP-1), useIndex] <- t(indices)

    om$Itg[ om$Itg < 0 ] <- NA
  
    # SPC: note the hack in using the transpose of the indices. Should
    #      probably determine this on the fly, or else make sure it is
    #      always done this way. I suppose that ncol>nrow could be used
    #      to test whether transpose is needed, except in very data-limited case.
    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Read stock abundance indices from file.\n" )
    
    .INDEXSERIESINPUT <<- TRUE
  }
  else
  {
    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Calculating indices from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  # (2c) BEGIN AGE DATA                                                      --#
  #----------------------------------------------------------------------------#

  if ( !is.null(ctlObj$mp$data$inputAges) )
  {
    browser()
    ages <- ctlObj$mp$data$inputAges
    ageObs <- ctlObj$mp$data$inputAges$d3_A
    
    # Use observations from ISCAM in historical period
    years <- ageObs[,1]
    times <- years - .INITYEAR + 1
    gears <- ageObs[,2]

    minAge       <- ctlObj$mp$data$minAge
    plusGroupAge <- ctlObj$opMod$nAges

    gearNums <- unique(gears)
    for( g in gearNums)
    {
      gearRows <- which(gears == g)
      gearTimes <- times[gears == g]
      for( gIdx in 1:length(gearTimes) )
      {
        gRow <- gearRows[gIdx]
        gTime <- gearTimes[gIdx]
        ageSamples <- ageObs[gRow, 6:14]
        om$uCatg[minAge:plusGroupAge,gTime,g] <- ageSamples/sum(ageSamples)
      }
    }

    
    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Read age data from file.\n" )    
    .AGESERIESINPUT <<- TRUE
  }
  else
  {
    if( !ctlObj$gui$quiet )
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

    if( !ctlObj$gui$quiet )
      cat( "\nMSG (.createMP) Read other data from file.\n" )    
    scan()
  }
  else
  {
    if( !ctlObj$gui$quiet )
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
  
  if ( ctlObj$mp$assess$methodId == .CAAMOD | ctlObj$mp$assess$methodId == .ISCAMRW )
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
  
  if( ctlObj$mp$assess$methodId == .CAAMOD | ctlObj$mp$assess$methodId == .ISCAMRW )
  {
    mp$assess$mpdPars <- data.frame( matrix( NA,nrow=(nT-tMP+1),ncol=25 ) )   
  }
  
  # Retrospective biomass estimates and recruitment.
  mp$assess$retroExpBt    <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  mp$assess$retroRt       <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  mp$assess$retroMt       <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
  mp$assess$retroBio3plus <- matrix( NA,nrow=(nT-tMP+1),ncol=(nT+1) )
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
.mgmtProc <- function( obj )
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
  
  cleanObj <- obj
  
  ctlList <- obj$ctlList
  
  # Extract simulation dimensions
  nAges  <- ctlList$opMod$nAges
  nReps  <- ctlList$gui$nReps
  tMP    <- ctlList$opMod$tMP
  nT     <- ctlList$opMod$nT
  nGear  <- ctlList$opMod$nGear
  nGrps  <- ctlList$opMod$nGrps

  #------------------------------------------------------
  # Create blob objects pars,om,and mp to store all simulation results
  #------------------------------------------------------

  # Operating model objects to attached to blob for performance analysis. 
  om <- list( iSeed      = rep( NA, nReps ),
              Bt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              SBt        = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              FBt        = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Nt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Btot       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ntot       = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Mt         = array( NA, dim = c(nReps,nGrps,nT) ),               
              Rt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ct         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Ctg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              overage_tg = array( data=NA,dim=c(nReps,nT,nGear) ),
              valCtg     = array( data=NA,dim=c(nReps,nT,nGear) ),
              Catg       = array( data = NA, dim = c(nReps, nAges, nT, nGear)),
              valCatg    = array( data = NA, dim = c(nReps, nAges, nT, nGear)),
              Dt         = matrix( NA,nrow=nReps,ncol=(nT+1) ),
              Dtg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              valDtg     = array( data=NA,dim=c(nReps,nT,nGear) ),
              Datg       = array( data = NA, dim = c(nReps, nAges, nT, nGear)),
              valDatg    = array( data = NA, dim = c(nReps, nAges, nT, nGear)),
              Ftg        = array( data=NA,dim=c(nReps,nT,nGear) ),
              uCatg      = array( data=NA,dim=c(nReps,nAges,nT,nGear) ),
              legalHR    = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              spawnHR    = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalHR = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalB     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalB  = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalC     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              legalD     = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              sublegalD  = matrix( NA, nrow=nReps, ncol=(nT+1) ),
              Itg        = array( data=NA,dim=c(nReps,nT,nGear) )
            )
  
  om$errors <-  list( omegat     = matrix( NA,nrow=nReps,ncol=(nT+1) ),
                      deltat     = matrix( NA,nrow=nReps,ncol=(nT+1) ) )

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
  
  if ( ctlList$mp$assess$methodId == .CAAMOD | ctlList$mp$assess$methodId == .ISCAMRW  )
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
  mp$assess$retroExpBt     <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroSpawnBt   <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroRt        <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  mp$assess$retroMt        <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )
  
  # Not strictly assessment values, but if Decline Risk HCR have to store this...
  mp$assess$trendBio       <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  mp$assess$trendVal       <- matrix( NA, nrow=nReps, ncol=(nT+1) )
  # Not strictly assessment values, but if slowUpRule is being used we need
  # to draw on this
  mp$assess$retroBio3plus  <- matrix( NA, nrow=(nReps*(nT-tMP+1)),  ncol=(nT+2) )

  #----------------------------------------------------------------------------#
  #-- Initialize the blob object to hold all simulation results.             --#
  #----------------------------------------------------------------------------#

  blob <- list( ctlList=ctlList, om=om, mp=mp ) 
  
  #----------------------------------------------------------------------------#
  
  # Loop over simulation replicates.
  .LASTSOLUTION <<- NULL
  if(!obj$ctlList$gui$quiet)
    cat( "\nMSG (.mgmtProc) Running feedback loop...\n" ) 

  for ( i in 1:nReps )
  {
    # Initialize .DEADFLAG and .FISHERYCLOSED
    .DEADFLAG      <<- FALSE  
    .FISHERYCLOSED <<- FALSE  

    obj <- cleanObj
  
    # Pass the replicate index.
    obj$ctlList$opMod$rep <- i
  
    # Set random number seed in a way that can be reconstructed later.
    set.seed( ctlList$opMod$rSeed + i )

    blob$om$iSeed[i] <- ctlList$opMod$rSeed + i

    startTime <- proc.time()[3]    
    t1 <- proc.time()[3]

    # Initialise simulation object for t=1,2,...tMP-1 pre-MP period.
    obj <- .initPop( obj )

    if(!obj$ctlList$gui$quiet)
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
    
    if ( ctlList$mp$assess$methodId == .CAAMOD |  ctlList$mp$assess$methodId == .ISCAMRW )
    {
      obj$mp$assess$mpdPars     <- data.frame( matrix(NA,nrow=(nT-tMP+1), ncol=25 ) )
      obj$mp$assess$Rt          <- matrix( NA, nrow=(nT-tMP+1), ncol=(nT+1) )
      obj$mp$assess$pat         <- matrix( NA, nrow=nAges, ncol=nT )
    }
 
    # Loop over years tMP:nT projecting feedback management strategy
    if( nT > tMP ) 
    {
      # If "NoFish" or "PerfectInfo", skip the assessment feedback
      # loop and use a projection function
      if( ctlList$gui$mpLabel %in% c("NoFish", "PerfectInfo") |
          substr(obj$ctlList$gui$mpLabel,1,11) == "PerfectInfo" )
      {
        objBackup <- obj
        objNext         <- try(.projPopNoAssess( obj ))

        blob$maxTimeStep  <- nT
        blob$maxRep       <- i
        
        if(class(obj) == "try-error")
        {
          blob$success <- FALSE
          message("Error in .projPopNoAssess() for replicate ", i,
                  " at time step ", blob$maxTimeStep, " saving progress up to here." )
          break
        }
        obj <- objNext
      }
      else for ( t in tMP:nT )
      {
        # Fill om and mp objects.
        objNext <- try(.updatePop( obj, t ))

        blob$maxRep       <- i
        blob$maxTimeStep  <- t

        if( class(objNext) == "try-error" )
        {
          blob$success <- FALSE
          message("Error in population dynamics function .updatePop() for replicate ", i,
                  " at time step ", t, "; saving progress up to here." )
          break
        } 
        # Overwrite obj with objNext if .updatePop() 
        # didn't throw an error
        obj <- objNext
      }# end year loop. 
    }
    
    # Fill current row of the blob for operating model components.
    blob$om$Bt[i,]          <- c( i, obj$om$Bt )
    blob$om$SBt[i,]         <- c( i, obj$om$SBt )
    blob$om$FBt[i,]         <- c( i, obj$om$FBt )
    blob$om$Nt[i,]          <- c( i, obj$om$Nt )
    blob$om$Btot[i,]        <- c( i, obj$om$Btot )
    blob$om$Ntot[i,]        <- c( i, obj$om$Ntot )
    blob$om$Mt[i,,]         <- c( obj$om$Mt )    
    blob$om$Rt[i,]          <- c( i, obj$om$Rt )
    blob$om$Ct[i,]          <- c( i, obj$om$Ct )
    blob$om$Dt[i,]          <- c( i, obj$om$Dt )
    blob$om$Ctg[i,,]        <- obj$om$Ctg
    blob$om$overage_tg[i,,] <- obj$om$overage_tg
    blob$om$valCtg[i,,]     <- obj$om$valCtg
    blob$om$Catg[i,,,]      <- obj$om$Catg
    blob$om$valCatg[i,,,]   <- obj$om$valCatg
    blob$om$Dtg[i,,]        <- obj$om$Dtg
    blob$om$valDtg[i,,]     <- obj$om$valDtg
    blob$om$valDatg[i,,,]   <- obj$om$valDatg
    blob$om$Ftg[i,,]        <- obj$om$Ftg
    blob$om$Itg[i,,]        <- obj$om$Itg
    blob$om$uCatg[i,,,]     <- obj$om$uCatg
    blob$om$legalHR[i,]     <- c(i,obj$om$legalHR)
    blob$om$spawnHR[i,]     <- c(i,obj$om$spawnHR)
    blob$om$sublegalHR[i,]  <- c(i,obj$om$sublegalHR)
    blob$om$legalB[i,]      <- c(i,obj$om$legalB)
    blob$om$sublegalB[i,]   <- c(i,obj$om$sublegalB)
    blob$om$legalC[i,]      <- c(i,obj$om$legalC)
    blob$om$legalD[i,]      <- c(i,obj$om$legalD)
    blob$om$sublegalD[i,]   <- c(i,obj$om$sublegalD)

    blob$om$errors$deltat[i,] <- c(i,obj$om$errors$deltat)
    blob$om$errors$omegat[i,] <- c(i,obj$om$errors$omegat)
    
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

        blob$mp$assess$retroBio3plus[startRow:endRow, ] <- cbind( reps, obj$mp$assess$retroBio3plus )        
    }

    blob$mp$assess$retroExpBt[ startRow:endRow, ]   <- cbind( reps, obj$mp$assess$retroExpBt )
    
    diffTime <- proc.time()[3] - startTime
    if(!obj$ctlList$gui$quiet)
    {
      writeProgress( i, nReps )
      cat( "Elapsed time = ", diffTime, "\n" )  
    }
    
    if( class(objNext) == "try-error" & obj$ctlList$gui$stopOnBadRep )
    {
      message("Stopping on bad replicate, saving progress to sim folder." )
      break
    }
  
  }     # End simulation loop.
  
  # Label columns the OM components of the blob.
  tmpNames <- paste("B",c(1:nT),sep="")
  colnames( blob$om$Bt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("SB",c(1:nT),sep="")
  colnames( blob$om$SBt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("FB",c(1:nT),sep="")
  colnames( blob$om$FBt ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("N",c(1:nT),sep="")
  colnames( blob$om$Nt ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("Btot",c(1:nT),sep="")
  colnames( blob$om$Btot ) <- c( "iRep", tmpNames )
  
  tmpNames <- paste("Ntot",c(1:nT),sep="")
  colnames( blob$om$Ntot ) <- c( "iRep", tmpNames )

  tmpNames <- paste("M",c(1:nT), sep="")
  dimnames( blob$om$Mt ) <- list( NULL, NULL, NULL )  
  
  tmpNames <- paste("R",c(1:nT),sep="")
  colnames( blob$om$Rt ) <- c( "iRep", tmpNames )

  tmpNames <- paste("lHR",c(1:nT),sep="")
  colnames( blob$om$legalHR ) <- c( "iRep", tmpNames )

  tmpNames <- paste("spHR",c(1:nT),sep="")
  colnames( blob$om$spawnHR ) <- c( "iRep", tmpNames )
  
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

      tmpNames <- paste( "retroBio3plus", c(1:nT), sep="" )
      colnames( blob$mp$assess$retroBio3plus )<-c("iRep", "tStep", tmpNames ) 
      
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

# .projPopNoAssess
# Purpose:        algorithm to project the population in tMP:nT using
#                 known values (either no fishing, F == 0, or
#                 perfect information, applying HCR to OM bio)
# Parameters:     obj=list containing all objects and parameters generated by
#                 gui, createMP, and initPop; tMP=time when management procedure begins,
#                 nT=time when simulation ends
# Returns:        a list with initialised operating model states, data, and Fs for
#                 period 1:nT. 
# Source:         modified from .solveProjProp (S.P. Cox) in mseR-Finfish by 
#                 SDN Johnson for  multifleet mseR
# NOTE: need some objectives input via simCtlFile
.projPopNoAssess <- function( obj )
{

  # This projection function doesn't need to run the assessment
  # model, but will still need to update the population
  # in ageLenOpMod(). Given the multi-fleet nature, this
  # requires that we generate the catch and feed it to the
  # ageLenOpMod() function, which will require the HCR.
  tMP <- obj$ctlList$opMod$tMP
  nT  <- obj$ctlList$opMod$nT
  B0  <- obj$ctlList$opMod$B0

  # Loop over time periods - essentially follow .updatePop(),
  # but ignore all the messy MPs
  for( t in (tMP-1):(nT-1) )
  {
    # 0. Project population forward 1 time step
    # with no catch, skip to next time step if
    # no fishing is occuring
    obj$om$Ctg[t+1,] <- rep(0.0,5)
    obj <- ageLenOpMod( obj, t+1 )

    if( obj$ctlList$gui$mpLabel == "NoFish" ) next

    # 1. Pull out biomass post M next year (this is fihsing with
    # perfect info)
    FBt   <- obj$om$FBt
    Bt    <- obj$om$FBt[t+1]

    # browser()

    # 2. Calculate catch for next time step based on 
    # mpLabel
    # Base assumption of 0
    targetHarv <- 0
    nYears <- obj$ctlList$mp$hcr$nYearsAboveCutoff

    if( is.null(nYears) ) nYears <- 1

    recentYears <- ( t + 1 ):( t - nYears + 2 )

    # If PerfectInfo, calculate based on spawning biomass
    if(  substr(obj$ctlList$gui$mpLabel,1,11) == "PerfectInfo" )
    {
      targetHR    <- obj$ctlList$mp$hcr$targHRHerring
      if( obj$ctlList$mp$hcr$rule == "herring" )
      {
        cutoff      <- obj$ctlList$mp$hcr$herringCutoffVal
        cutoffType  <- obj$ctlList$mp$hcr$herringCutoffType
        if( cutoffType == "relative" ) cutoff <- cutoff*B0
        if( all(FBt[recentYears] > cutoff ) )
        { 
            targetHarv <- min( targetHR * Bt, Bt - cutoff )
        }
      }
      if( obj$ctlList$mp$hcr$rule == "linear" )
      {
        lowerBound <- obj$ctlList$mp$hcr$lowerBoundMult * B0
        upperBound <- obj$ctlList$mp$hcr$upperBoundMult * B0
        if( all(FBt[recentYears] > lowerBound) )
        {
          if( Bt <= upperBound )
            targetHarv <- targetHR * (Bt - lowerBound)/(upperBound - lowerBound) * Bt
          if( Bt > upperBound )
            targetHarv <- targetHR * Bt
        }
      }
    }
    if( !is.null(obj$ctlList$mp$hcr$catchCeiling) ) 
      targetHarv <- min(obj$ctlList$mp$hcr$catchCeiling, targetHarv)

    newCatch <- targetHarv * obj$ctlList$opMod$allocProp


    # Save catch in om, adding in test fishery
    obj$om$Ctg[t+1,] <- newCatch

    # 3. Rerun operating model for next time step
    # with new catch
    obj <- ageLenOpMod( obj, t+1 )
  }

  return( obj )

}     # END function .projPopNoAssess()

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
.initPop <- function( obj )
{

  ctlList <- obj$ctlList
  tmpTimes <- .calcTimes( ctlList )
  
  # Get simulation dimensions.
  nAges <- ctlList$opMod$nAges
  nT    <- ctlList$opMod$nT
  tMP   <- ctlList$opMod$tMP
  rep   <- ctlList$opMod$rep
  nGear <- ctlList$opMod$nGear
  nGrps <- ctlList$opMod$nGrps



  # initialise from MCMC draws if provided
  if( !is.null(ctlList$opMod$posteriorSamples) ) 
  {
    if(!obj$ctlList$gui$quiet)
      cat(  "\nMSG (.initPop) Sampling posterior for historical period, \n",
            "\nMSG (.initPop) Initialisation may take longer than usual.\n", sep = "" )

    # Get posterior sample number
    repNo     <- ctlList$opMod$rep
    postDraw  <- ctlList$opMod$posteriorDraws[repNo]

    # Read in mcmc samples
    mcmcMpath     <- file.path(ctlList$opMod$posteriorSamples,"mcmcMt.csv")
    mcmcFpath     <- file.path(ctlList$opMod$posteriorSamples,"mcmcFt.csv")
    mcmcRtpath    <- file.path(ctlList$opMod$posteriorSamples,"mcmcRt.csv")
    mcmcSBpath    <- file.path(ctlList$opMod$posteriorSamples,"mcmcSBt.csv")
    mcmcParpath   <- file.path(ctlList$opMod$posteriorSamples,"mcmcPar.csv")

    mcmcM         <- read.csv( mcmcMpath, header = T, stringsAsFactors = FALSE)
    mcmcF         <- read.csv( mcmcFpath, header = T, stringsAsFactors = FALSE)
    mcmcRt        <- read.csv( mcmcRtpath, header = T, stringsAsFactors = FALSE)
    mcmcPar       <- read.csv( mcmcParpath, header = T, stringsAsFactors = FALSE)
    mcmcSBt       <- read.csv( mcmcSBpath, header = T, stringsAsFactors = FALSE)

    # Create a new inputF matrix
    inputFtg  <- matrix( nrow = nT, ncol = 5 )
    inputRt   <- numeric(length = nT)
    for( g in 1:5 )
    {
      gearName <- paste("gear",g, sep = "")
      cols <- which(grepl( pattern = gearName, x = colnames(mcmcF) ))
      inputFtg[1:(tMP-1),g] <- as.numeric(mcmcF[postDraw,cols])
    }

    # Creat input Rt vector, adjust for age-1 recruitment
    inputRt[1:67] <- as.numeric(mcmcRt[postDraw,2:68] * exp(mcmcM[postDraw,1:67] ) )
    # inputRt[1]    <- mcmcPar[postDraw,"rbar_ag1"] * exp(mcmcM[postDraw,1])

    # Update Mdevs for inserting into history
    obj$ctlList$opMod$estMdevs <- list( M = t(as.matrix(mcmcM[postDraw,], nrow = 1) ) )

    # Now save the sample draws to new parts of opMod list
    # for use in solveInitPop
    obj$ctlList$opMod$inputFtg    <- inputFtg
    obj$ctlList$opMod$inputRt     <- inputRt
    obj$ctlList$opMod$mcmcPar     <- mcmcPar[postDraw,]

    # Update biological parameters for reconditioning
    obj$ctlList$opMod$B0          <- as.numeric( mcmcPar[postDraw, "sbo"] )
    obj$ctlList$opMod$M           <- as.numeric( mcmcPar[postDraw, "m"] )
    obj$ctlList$opMod$recM        <- mean( as.numeric( mcmcM[postDraw, ] ) )
    if(!is.null(obj$ctlList$opMod$endMrule))
    {
      if( obj$ctlList$opMod$endMrule == "mean" )
        obj$ctlList$opMod$endM        <- mean( as.numeric( mcmcM[postDraw, ] ) )
      if( obj$ctlList$opMod$endMrule == "1.5jump" )
        obj$ctlList$opMod$endM        <- 1.5 * mean( as.numeric( mcmcM[postDraw, ] ) )
      if( obj$ctlList$opMod$endMrule == "0.5jump" )
        obj$ctlList$opMod$endM        <- 0.5 * mean( as.numeric( mcmcM[postDraw, ] ) )
      if( obj$ctlList$opMod$endMrule == "0.75jump" )
        obj$ctlList$opMod$endM        <- 0.75 * mean( as.numeric( mcmcM[postDraw, ] ) )
      if( obj$ctlList$opMod$endMrule == "0.25pctile" )
        obj$ctlList$opMod$endM        <- quantile(as.numeric( mcmcM[postDraw, ] ), prob = 0.25 )
    }

    # Will need to recalculate Salg from here, rather than re-calling refPts
    obj$ctlList$opMod$L50Cg1      <- as.numeric(mcmcPar[postDraw, c("sel_gear1","sel_gear2","sel_gear3","sel_gear4","sel_gear5")])
    obj$ctlList$opMod$L95Cg1      <- as.numeric(mcmcPar[postDraw, c("sel_gear1","sel_gear2","sel_gear3","sel_gear4","sel_gear5")] +
                                      log(19) * mcmcPar[postDraw, c("sel_sd1","sel_sd2","sel_sd3","sel_sd4","sel_sd5")] )
    obj$ctlList$opMod$qg          <- c(1,1,1, as.numeric(mcmcPar[postDraw,c("q1","q2")]) )
    obj$ctlList$opMod$rSteepness  <- as.numeric(mcmcPar[postDraw,"h"])

    # Recalculate variance parameters from EIV pars
    varTheta  <- as.numeric(mcmcPar[postDraw, "vartheta"])
    varRho    <- as.numeric(mcmcPar[postDraw, "rho"])
    tau2      <- varRho * 1/varTheta
    sigma2R   <- (1 - varRho) * 1/varTheta

    obj$ctlList$opMod$tauIndexg <- c(sqrt(tau2), sqrt(tau2)/1.167)
    obj$ctlList$opMod$sigmaR    <- sqrt(sigma2R)

    # Other variance parameters (ageing error) are not reported
    # so we can leave those alone for now.

    # recalculate selectivity and overwrite recruitment parameters
    mcmcOpMod <- obj$ctlList$opMod
    mcmcSched <- .calcSchedules( mcmcOpMod )
    obj$refPtList$Salg  <- mcmcSched$Salg

    # Get B0 and steepness
    B0 <- mcmcOpMod$B0
    rSteepness <- mcmcOpMod$rSteepness

    # overwrite recruitment pars
    obj$refPtList$R0    <- as.numeric(mcmcPar[postDraw, "ro"])*exp(mean( as.numeric( mcmcM[postDraw, ] ) ))
    obj$refPtList$rec.a <- 4.*rSteepness*obj$refPtList$R0 / ( B0*(1.-rSteepness) )
    obj$refPtList$rec.b  <- (5.*rSteepness-1.) / ( B0*(1.-rSteepness) )

  }

  ctlList <- obj$ctlList
  
  #----------------------------------------------------------------------------#
  #-- CATCH DATA                                                             --#
  #----------------------------------------------------------------------------#
   
  if ( !is.null(ctlList$mp$data$inputCatch) )
  {
    # Extract catch and assign to blob elements, convering to 000s mt.
    catch <- ctlList$mp$data$inputCatch
    catch <- catch$landCatchMatrix
    years <- catch[,1]
    times <- catch[,2]

    obj$om$Ctg[times,] <- catch[,3:7]

    obj$om$Ctg[is.na(obj$om$Ctg)] <- 0
    if(!obj$ctlList$gui$quiet)
      cat( "\nMSG (.initPop) Extracted catch and converted units.\n" )     
    .CATCHSERIESINPUT <<- TRUE         
  }
  
  if ( is.null( ctlList$mp$data$inputCatch ) )
  {
    if(!obj$ctlList$gui$quiet)
      cat( "\nMSG (.initPop) Calculating catch from operating model.\n" )
  }

  #----------------------------------------------------------------------------#
  #-- Generate natural mortality process errors                              --#
  #
  # ARK (13-Oct-13) If this is here, a different set of random values will   --#
  # fill the recruitment devs, and therefore results differ from 2010.       --#
  #----------------------------------------------------------------------------#

  RandomWalkM <- FALSE
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
      Mt[1]     <- ctlList$opMod$M[1]             # Scale Mt[1] to equal input mean M.
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
      pulseMt <- Mt
      pulseMt[pulseYrs] <- pulseMt[pulseYrs] * ctlList$opMod$pulseMSize 

      # Output a base version and a pulse version
      obj$om$Mt       <- Mt
      obj$om$pulseMt  <- pulseMt
  }
  else
  {
      obj$om$Mt <- matrix(  ctlList$opMod$M[1:nGrps], 
                            nrow = nGrps,
                            ncol = nT, byrow = FALSE )
  }

  if( !is.null(ctlList$opMod$estMdevs ) )
  {
    # Get M time series from repFile or from MCMC samples
    estMdevs <- ctlList$opMod$estMdevs
    if( estMdevs == "repFile" )
      estMdevs <- ctlList$opMod$repFile

    obj$om$Mt[,1:(tMP-1)] <- estMdevs$M[,1]

    if( !is.null(ctlList$opMod$kYearsMbar) )
    {
      k <-  ctlList$opMod$kYearsMbar
      Mbar <- mean( obj$om$Mt[ ,(tMP - k):(tMP - 1 ) ] )
      endM <- Mbar
    }

    # Now modify the projected Mt time series to begin at the end of 
    # the history  
    if(is.null(ctlList$opMod$endMphase))
    {
      trendM    <- (log(endM) - log( obj$om$Mt[tMP-1]) ) / (nT - tMP+1)
      obj$om$Mt[tMP:nT] <- obj$om$Mt[tMP-1] * ranM[tMP:nT] * exp( trendM * (1:(nT-tMP+1)) )
    } else {
      phaseTime <- ctlList$opMod$endMphase
      trendM    <- (log(endM) - log( obj$om$Mt[tMP-1]) ) / phaseTime
      obj$om$Mt[tMP:(tMP+phaseTime-1)] <- obj$om$Mt[tMP-1] * ranM[tMP:(tMP+phaseTime-1)] * exp( trendM * (1:phaseTime) )
      obj$om$Mt[(tMP + phaseTime):nT] <- obj$om$Mt[tMP + phaseTime-1] * ranM[(tMP+phaseTime):nT]
    }

    # Save a copy of Mt to use as the pulsed
    # version, this way the pulse is still drawn
    # and the random values line up
    obj$om$pulseMt <- obj$om$Mt
    
    # And add the pulse
    obj$om$pulseMt[pulseYrs] <- obj$om$Mt[pulseYrs] * ctlList$opMod$pulseMSize
  }


  # Draw tsBoot indices anyways, then use in projected Mt
  # matrices (this way the random draws for each run are the same)
  if(!is.null(ctlList$opMod$kBoot))
    initBoot <- tMP - ctlList$opMod$kBoot
  else initBoot <- 1
  bootMt <- tsBoot( x = obj$om$Mt[1,initBoot:(tMP-1)], 
                    length = length((tMP):nT), 
                    tolSD = ctlList$opMod$bootTolSD,
                    tolMult = ctlList$opMod$bootTolMult )

  if( ctlList$opMod$useBoot )
  {
    obj$om$Mt[,tMP:nT]   <- bootMt
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
    recDevs <- scalrecDevs$recDevs
    # the offset allows for generating recDevs in years < tMP - 1
    recDevsOffset <- ctlList$opMod$recDevsOffset
    
    # Finally replace recruitment deviations from file in the omegat.
    omegat[1:(tMP - recDevsOffset - 1)] <- exp( recDevs[1:(tMP - recDevsOffset - 1)] )

    if(!obj$ctlList$gui$quiet)
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
    if(!obj$ctlList$gui$quiet)
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

  # Need to update reference points for MCMC draws


  # Given target depletion level and stochastic recruitment find the catches
  # Ct[1:tMP-1] that give stock status as near as possible to target depletion at tMP-1.
  # We need to do this because the operating model needs to be run for 1:tMP to
  # generate all the data and states for that time. We CAN skip the optimization.

  tmp <- .solveInitPop( obj, tMP )

  # Now populate operating model object with states, data, etc.
  obj$om            <- tmp$om
  obj$mp$data$Itg   <- tmp$mp$data$Itg     # Stock abundance indices.
  obj$mp$data$Xatg  <- tmp$mp$data$Xatg    # Ageing data.
  
  # ARK (02-Sep-13) I don't know what time2fail does...
  obj$mp$time2fail  <- nT + 100
  
  return( obj )
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
    if(class(B) == "numeric") 
      B <- as.matrix(B, nrow = length(B), ncol = 1)
    # Znew <- matrix ( M, nrow = nrow ( B), ncol = ncol(B), byrow=TRUE )
    # for( g in 1:(nGear-2) ) # SJ: Again, too path specific.
    # {    
    #     Bprime <- B*S[,,g]*(1.-P[,,g])
    #     F[g]   <- -1. * log (1 -  C[g]/sum(Bprime) )
    #     Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
    # }
    diff <- function( F )
    {
        Znew <- matrix(M, byrow = TRUE, ncol = ncol(B), nrow = nrow(B))
        Fg <- exp(F)
        for( g in 1:(nGear-2) )
        {
            Znew   <- Znew + S[,,g]*Fg[g]*(D[g]*P[,,g] - P[,,g] + 1.)
        }
        f <- 0
        for( g in 1:(nGear-2) )
        {
            Bprime <- B*(1.-P[,,g])
            tmp    <- Bprime*S[,,g]*Fg[g]*(1.-exp(-Znew))/Znew
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


.solveBaranovMfleet <- function(  B, S, F, M, C, P, D,
                                  nIter = 10, lam = rep(.5,3), 
                                  quiet = TRUE )
{
  # Follow other functions, but use new derivation of J
  nGear <- length(F)
  nAge  <- nrow(B)
  nGrps <- ncol(B) 
  Znew  <- matrix(M, nrow = nAge, ncol = nGrps, byrow = TRUE)

  for( g in 1:(nGear-2) ) # SJ: Again, too path specific.
  {    
      Bprime <- B*S[,,g]*(1.-P[,,g])
      F[g]   <- C[g]/sum(Bprime)
      Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
  }

  for( iter in 1:nIter )
  {
    Z <- Znew; 
    Znew <- matrix(M, nrow = nAge, ncol = nGrps, byrow = TRUE)
    for( g in 1:(nGear-2) )
    {
        # Compute estimated removals
        tmp    <- Bprime*F[g]*(1.-exp(-Z))/Z
        f      <- C[g] - sum(tmp); 
        # Compute dZdF for helping in the following calcs
        dZdF   <- S[,,g] * (D[g] * P[,,g] - P[,,g] + 1)
        # Split Jacobian up
        tmpJ1  <- Bprime
        tmpJ2  <- Z * (1 - exp(-Z)) + F[g] * Z * dZdF * exp(-Z)
        tmpJ3  <- F[g] * (1 - exp(-Z)) * dZdF
        
        J       <- -1*sum(tmpJ1 * (tmpJ2 - tmpJ3)/Z^2);
        F[g]    <- F[g] - lam[g]*f/J
        Z       <- Z + S[,,g]*F[g]
        tmp    <- 0
        tmpJ   <- 0
    }
  } 
    #F[ F > 1.0 ] <- 1.0 
    # could also return Z so it only needs to be computed once.
    return(F) 
}

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
  Znew <- matrix(M, byrow = TRUE, nrow = nrow(B), ncol = ncol(B))

    for( g in 1:(nGear-2) )
    {    

        Bprime <- B*S[,,g]*(1.-P[,,g])
        F[g]   <- C[g]/sum(Bprime)
        Znew   <- Znew + S[,,g]*F[g]*(D[g]*P[,,g] - P[,,g] + 1.)
        
    }
    for( iter in 1:nIter )
    {
        Z <- Znew
        Znew <- matrix(M, byrow = TRUE, nrow = nrow(B), ncol = ncol(B))
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
.solveInitPop <- function( obj, tMP )
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
  runModel <- function( pars, obj )
  {
    if( !.CATCHSERIESINPUT )
    {
      initCatch <- exp(pars)    
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
      obj <- ageLenOpMod( obj,t )
    }

    
    
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
    return( result )

  }     # END function runModel
  
  # getObjFunctionVal       
  # Purpose:        Private function to run operating model and extract objective function
  # Parameters:     pars=two multipliers that scale Ft to M for t=1,2,...tMP-1
  # Returns:        Objective function f=squared deviation from initDepTarg plus 
  #                 log(cumulative catch). 
  # Source:         S.P. Cox
  getObjFunctionVal <- function( pars, obj )
  {
    val <- runModel( pars, obj )$f
    val
  }      # END function getObjFunctionVal function
  
  #----------------------------------------------------------------------------#

  # Begin solveInitPop
  
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
    solveObj <- obj
    optPar <- list()
    optPar$par <- NULL
  }
  

  # Re-generate the operating model and data objects with the optimised Fs
  solveObj    <- runModel( optPar$par, solveObj )
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
  return( obj )
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
    tmp$It <- mp$data$Itg[1:(t-1),5]
    tmp$kfGain <- ctlList$mp$assess$kfGain
    stockAssessment <- assessModKF( tmp )
  }

  if ( ctlList$mp$assess$methodId == .MOVAVG )
  {

    tmp$om          <- om
    tmp$mp          <- mp

    stockAssessment <- .callProcedureMovingAvg( tmp, t )
    tRow <- t - tMP + 1
    
    # Update ItgScaled.
    val <- t(mp$data$Itg[1:(t-1),tmpTimes$useIndex])
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
    tmp$quiet           <- ctlList$gui$quiet
    
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

    tmp$t             <- t
    tmp$trendYears    <- ctlList$mp$hcr$trendYears
    tmp$rSeed         <- ctlList$opMod$rSeed 

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

  if( ctlList$mp$assess$methodId == .DDMOD )
  {
    # Call DD AM using wrapper function, which
    # will arrange all the simulated data into
    # the right form
    stockAssessment <- callProcedureDD( obj, t )

    tRow <- t - tMP + 1

    stockAssessment$runStatus[c("est","std","cor","cov","names")] <- NULL
    val <- c( t, unlist( stockAssessment$runStatus ) )
    mp$assess$runStatus[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$runStatus )[c(1:length(val))] <- c( "tStep",names(stockAssessment$runStatus) )

    val <- c( t,unlist(stockAssessment$mpdPars) )
    mp$assess$mpdPars[ tRow,c(1:length(val)) ] <- val
    names( mp$assess$mpdPars )[c(1:length(val))] <- c( "tStep",names(unlist(stockAssessment$mpdPars)) )
    
    # Update ItgScaled from DD output
    for ( i in 1:length(tmpTimes$useIndex) )
    {
      idx       <- tmpTimes$useIndex[i]
      survType  <- stockAssessment$survType_g[i]
      if( survType == 0 )
        val <- stockAssessment$B_t[1:(t-1)]
      if( survType == 1 )
        val <- stockAssessment$N_t[1:(t-1)]

      indexOn <- !is.na(obj$om$Itg[,idx])[1:(t-1)]
      mp$assess$ItgScaled[idx,which(indexOn)] <- val[indexOn]
    }
    
    # Fill next row of "stepwise" (i.e., retrospective) biomass estimates
    val <- c( t,stockAssessment$B_t )
    mp$assess$retroExpBt[ tRow, c(1:length(val)) ] <- val
    val <- c( t,stockAssessment$B_t )
    if(is.null(mp$assess$retroSpawnBt)) 
      mp$assess$retroSpawnBt <- mp$assess$retroExpBt
    mp$assess$retroSpawnBt[ tRow, c(1:length(val))] <- val

    val <- c( t, stockAssessment$omegaR_t[1:(t-1)] )
    mp$assess$retroRt[ tRow, c(1:length(val)) ] <- val
    
    # Make a new list saving the assessment info so 
    # we can compare it to other things later

    mp$saveAssessment[[tRow]] <- stockAssessment

  }

  if ( ctlList$mp$assess$methodId == .CAAMOD )
  {
    # Take the shortcut provided by the newer version of mseR,
    # will need grooming to work with multiple fleets
    stockAssessment <- .callProcedureCAA( obj, t)

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
      idx       <- tmpTimes$useIndex[i]
      survType  <- stockAssessment$survType_g[idx]
      if( survType == 0 )
        val <- stockAssessment$vulnB_tg[1:(t-1),idx]
      if( survType == 1 )
        val <- stockAssessment$vulnN_tg[1:(t-1),idx]
      if( survType == 2 )
        val <- stockAssessment$SB_t[1:(t-1)]

      indexOn <- !is.na(obj$om$Itg[,idx])[1:(t-1)]
      mp$assess$ItgScaled[idx,which(indexOn)] <- val[indexOn]
    }
    
    # Fill next row of "stepwise" (i.e., retrospective) biomass estimates
    val <- c( t,stockAssessment$SB_t )
    mp$assess$retroExpBt[ tRow, c(1:length(val)) ] <- val
    val <- c( t,stockAssessment$SB_t )
    if(is.null(mp$assess$retroSpawnBt)) 
      mp$assess$retroSpawnBt <- mp$assess$retroExpBt
    mp$assess$retroSpawnBt[ tRow, c(1:length(val))] <- val

    val <- c( t, stockAssessment$delta )
    mp$assess$retroRt[ tRow, c(1:length(val)) ] <- val

    # Get logMdevs
    val <- c(t,0, stockAssessment$log_m_devs )
    mp$assess$retroMt[tRow, c(1:length(val))] <- val  

    # Get retrospective 3+ biomass
    mp$assess$retroBio3plus[tRow,1:(t+1)] <- c(t,stockAssessment$sbt)
    mp$assess$retroBio3plus[tRow,t+1] <- stockAssessment$mpdPars$projExpBio

    # Make a new list saving the assessment info so 
    # we can compare it to other things later

    mp$saveAssessment[[tRow]] <- stockAssessment


  }     # ENDIF methodId=.CAAMOD

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
  if ( statusSource == "statusSrceEquil" |
       methodId %in% c(.MOVAVG,.KALMAN ) )
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
      mp$hcr$Bref[t] <- mp$assess$mpdPars$SSB0[ idxCtlPts[tRow] ]
  }     # ENDIF statusSource != statusSrceEquil
  
  # REFERENCE REMOVAL RATE from the operating model.
  if ( remRefSource == "rrSrceEquil" |
       methodId %in% c(.MOVAVG,.KALMAN )  )
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
            fmult <- ctlList$mp$hcr$phaseFinitF/ctlList$mp$hcr$inputF - 1.
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
 
  # Update control points
  mp$hcr$lowerBound[t] <- ctlList$mp$hcr$lowerBoundMult * mp$hcr$Bref[t]
  mp$hcr$upperBound[t] <- ctlList$mp$hcr$upperBoundMult * mp$hcr$Bref[t]
  
                  
  # HCRs based on Empirical methods
  if ( (ctlList$mp$assess$methodId == .MOVAVG) ) 
  {

      # Build harvest control rule list object.
      rule              <- mp$hcr
      rule$assessMethod <- ctlList$mp$assess$methodId
      rule$t            <- t
      rule$biomass      <- stockAssessment$biomass
      rule$ccBiomass    <- mp$data$Itg[1:(t-1),5]
      rule$Dt           <- om$Ct[1:(t-1)]
      
      # ccRule lifted from previous LRP paper
      if( ctlList$gui$mpLabel == "ccRule" )
      {
        targetHarv <- .calcHCR_ccRule( rule )

      } else {
        # Calculate catch limit using herring 
        # cutoff/targHR rule
        targetHarv   <- .calcHarvRuleHerringDFOMP( rule )  

      }
      
      

      
  }
  else
  {
    # Build harvest control rule object, need to check that legalBiomass input.
    # Most recent survey spawning biomass estimate (i.e.,from t-1)
    if( ctlList$mp$hcr$rule == "herring")
    {
      rule                  <- mp$hcr
      rule$assessMethod     <- ctlList$mp$assess$methodId
      rule$slowUpYears      <- ctlList$mp$hcr$nYearsAboveCutoff
      rule$t                <- t
      rule$sbt              <- stockAssessment$sbt
      rule$biomass          <- stockAssessment$mpdPars$projExpBio
      rule$maxF             <- log(1/(1-ctlList$mp$hcr$targHRHerring))
      rule$targHR           <- ctlList$mp$hcr$targHRHerring
      rule$cutoff           <- ctlList$mp$hcr$herringCutoffVal
      rule$cutoffType       <- ctlList$mp$hcr$herringCutoffType
      rule$assessFailed     <- FALSE

      # Calculate catch limit
      targetHarv   <- .calcHarvRuleHerringDFOMP( rule )      
    }
    if(ctlList$mp$hcr$rule == "linear")
    {
      rule                  <- mp$hcr
      rule$assessMethod     <- ctlList$mp$assess$methodId
      rule$slowUpYears      <- ctlList$mp$hcr$nYearsAboveCutoff
      rule$t                <- t
      rule$SBt              <- stockAssessment$SBt
      rule$biomass          <- stockAssessment$mpdPars$projExpBio
      rule$maxF             <- mp$hcr$remRate[t]
      rule$targHR           <- ctlList$mp$hcr$targHRHerring
      rule$assessFailed     <- FALSE

      # Calculate catch limit
      targetHarv   <- .calcLegalHarvRule( rule )        
    }
      
  }

  # Apply % limit on TAC change as independent floor and ceiling, as long
  # as maxDeltaTAC != NULL
  tmpTAC0     <- targetHarv$catchLimit
  # initialize floor and ceiling to current TAC 
  tacFloor    <- tmpTAC0
  tacCeiling  <- ctlList$mp$hcr$catchCeiling

  # # do the floor?
  # if( is.null(ctlList$mp$hcr$maxDeltaTACf) )
  # {
  #   # no limit on TAC change
  #   targetHarv$catchLimit <- tmpTAC0
  # }
  # else
  # {
  #   # if maxDeltaTACf==0, then use constant TAC
  #   if( ctlList$mp$hcr$maxDeltaTACf==0 )
  #   {
  #     targetHarv$catchLimit <- ctlList$mp$hcr$consTAC      
  #   }
  #   if(ctlList$mp$hcr$maxDeltaTACf > 0 )
  #   {
  #     deltaTACf   <- ctlList$mp$hcr$maxDeltaTACf      
  #     tacFloor   <- lastCatch*(1.-deltaTACf)
  #   }
  # }
  # # do the ceiling
  # if( is.null(ctlList$mp$hcr$maxDeltaTACc) )
  # {
  #   # no limit on TAC change
  #   targetHarv$catchLimit <- tmpTAC0
  # }
  # else
  # {
  #   # if maxDeltaTACc==0, then use constant TAC
  #   if( ctlList$mp$hcr$maxDeltaTACc==0 )
  #   {
  #     targetHarv$catchLimit <- ctlList$mp$hcr$consTAC      
  #   }
  #   if(ctlList$mp$hcr$maxDeltaTACc > 0 )
  #   {
  #     if( ctlList$mp$hcr$deltaTACcType == "absolute" )
  #       tacCeiling   <- ctlList$mp$hcr$catchCeiling
  #     else
  #     {
  #       deltaTACc   <- ctlList$mp$hcr$maxDeltaTACc      
  #       tacCeiling <- lastCatch*(1.+deltaTACc)
  #     }
  #   }
  # }
  tmpTAC     <- max( tmpTAC0, tacFloor )
  tmpTAC     <- min( tmpTAC, tacCeiling )

  lastTAC <- sum(om$Ctg[t-1,])

  if( tmpTAC > lastTAC & tmpTAC < sum(lastTAC + ctlList$mp$hcr$minDeltaTACup) )
    tmpTAC <- sum(om$Ctg[t-1,])

  if(tmpTAC < 0) tmpTAC <- 0

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
  {
    .FISHERYCLOSED <<- TRUE
    tmpCatch[1] <- 0
  }

  
  # Set catch limit for the year
  om$Ctg[ t, ]  <- rep( 0, ncol(om$Ctg) )
  om$Ctg[t,]    <- ctlList$opMod$allocProp * min(tmpCatch)

  # Add test fishery catch - maybe we move this inside the OM
  # om$Ctg[t,]    <- om$Ctg[t,] + ctlList$opMod$testFishery

 
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
  obj <- ageLenOpMod(obj,t )

  if ( obj$om$Bt[t] < (0.05*ctlList$opMod$B0) )
  {
    .DEADFLAG <<- FALSE
  }
 
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
  if( !ctlList$gui$quiet )
    cat( "\nMSG (.writePinDat) Calculating reference points, please wait...\n" )

  refPts <- calcRefPoints( opMod )

  if( !ctlList$gui$quiet )
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

# Try a sampling system for h and SSB
.quantileStratSample <- function( seed = NULL,
                                  post = SOGtvMpost, par1 = "m", par2 = "sbo",
                                  nBreaks = 10 )
{
  if( !is.null(seed) )
    set.seed(seed)

  pctiles <- seq(1/nBreaks,1,length = nBreaks)

  par1Quant <- quantile( x = post[,par1], probs = pctiles )

  par1Breaks <- c(0,par1Quant)

  samples <- numeric(length = length(pctiles)^2 )

  for( k in 1:nBreaks )
    {
      LB <- par1Breaks[k]
      UB <- par1Breaks[k+1]

      rowIdx1 <- which(post[,par1] > LB & post[,par1] <= UB  )
      par2CondQuants <- quantile( x = post[rowIdx1,par2], probs = pctiles )
      
      par2CondBreaks <- c( 0, par2CondQuants )

      for( j in 1:nBreaks )
      {
        condLB <- par2CondBreaks[j]
        condUB <- par2CondBreaks[j+1]

        rowIdx2 <- which( post[,par2] > condLB & post[,par2] <= condUB )

        rowIdx <- intersect(rowIdx1, rowIdx2)
        if(length(rowIdx) == 0) browser()
        samples[j + (k-1)*nBreaks ] <- sample( x = rowIdx, size = 1 )
      }

    }  

  samples
}

# makeFWpars()
# Takes vonB and allometric length-weight parameters 
# and outputs inflection age in the weight-at-age curve (model),
# then estimates FW pars for the branch of the curve
# above that age. Returns pars estimated from the 
# integer ages above (ceiling) and below (floor) the
# inflection point.
# inputs: spec = 1-char species name root of data files.
# outputs: out = list of DDpars.lo (floor) and DDpars.hi (ceiling)
.makeFWpars <- function( vonK, Linf, L1,
                        c1.lw, c2.lw, nAges = 35,
                        plot = FALSE )
{
  # von bertalanffy growth function
  vonB <- function( age, K, Linf, L1 )
  {
    Lt <- Linf + (L1 - Linf) * ( exp( -K * (age - 1)))
    Lt
  }

  vonBcurve <- vonB(  age = 1:nAges, 
                      K = vonK, Linf = Linf, L1 = L1 )

  # Make wtAge curve
  wtAge   <- c1.lw * vonBcurve ^ c2.lw

  ages <- 1:nAges

  # Make a spline, then solve for inflection point
  wtAgeSpline     <- splinefun( x = ages, y = wtAge )
  inflectionAge   <- uniroot( f = wtAgeSpline, interval = c(1,max(ages)),
                              deriv = 2)$root

  # Take the ceiling and floor of inflection
  kage_lo <- floor(inflectionAge)
  kage_hi <- ceiling(inflectionAge)

  # Get max age
  A <- max(ages)
  # Now estimate FW pars
  dat_lo <- data.frame( w1 = wtAge[kage_lo:(A-1)], w2 = wtAge[kage_hi:(A)]  )
  dat_hi <- data.frame( w1 = wtAge[kage_hi:(A-1)], w2 = wtAge[(kage_hi+1):(A)]  )

  floorFW <- lm( w2 ~ w1, data = dat_lo)
  ceilFW <- lm( w2 ~ w1, data = dat_hi)

  FW_lo <- coef(floorFW)
  FW_hi <- coef(ceilFW)


  

  DDpars.lo <- c(kage = kage_lo, alpha = FW_lo[1], rho = FW_lo[2])
  DDpars.hi <- c(kage = kage_hi, alpha = FW_hi[1], rho = FW_hi[2])


  DDpars <- list( DDpar.lo = DDpars.lo, DDpar.hi = DDpars.hi)

  out <- list()
  out$DDpars    <- DDpars
  out$lenAge    <- vonBcurve
  out$wtAge     <- wtAge

  return( out )

}
