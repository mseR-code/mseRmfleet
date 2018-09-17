#------------------------------------------------------------------------------#
#-- Life History and Reference Points (some HIDDEN, e.g., .foo)              --#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#-- Main Reference Point Function (PUBLIC)                                   --#
#------------------------------------------------------------------------------#
# calcRefPoints  ( Calculate reference points )
# .calcSalg      ( Calculate selectivity at age a, growth-group l, gear g)
# .calcPalg      ( Calculate proportion discarded at age a and growth-group l)
# .calcMa        ( Calculate maturity at age ogive )
# .calcLal       ( Calculate von Bertanlanffy length at age by growth-group )
# .calcWtAge     ( Calculate weight-at-age by growth-group )
# .calcSchedules ( Calculate life history schedules )

# calcRefPoints ( Calculate reference points )
# Purpose:      Primary function to (i) compute life history schedules,
#               (ii) equilibrium relationships to F, and (iii) equilibrium
#               reference points.
# Parameters:   obj, a list of all operating model parameters.
# Returns:      obj, a list with all life history schedules (vectors),
#               equilibrium relationships to F (vectors), and equilibrium
#               reference points (scalars).
# Source:       S.P. Cox
calcRefPoints <- function( opModList )
{
  # ARK (10-Nov-10) Removed to diminish whining to screen...
  #t1 <- proc.time()
  
  obj <- deref( opModList )
  # use lisread to extract the vector parameters below
  #tmp <- lisread( obj$lisReadFile, quiet=TRUE )
  #obj <- refPars

  # Indices
  A           <- obj$nAges
  obj$piOne   <- 1

  # salgPars: used to compute selectivity for age a, growth-group l, by gear g.
  obj$L50Cg1  <- obj$L50Cg1
  obj$L95Cg1  <- obj$L95Cg1
  obj$L95Cg2  <- obj$L95Cg2
  obj$L50Cg2  <- obj$L50Cg2

  # palgPars: used to compute proportion of age a, growth-group l discarded.
  obj$sizeLim <- obj$sizeLim
  obj$L50Dg   <- obj$L50Dg
  obj$volReleaseSize <- obj$volReleaseSize

  # Relative Fs
  obj$fg      <- rep(0,obj$nGear)

  # discard mortality rate
  obj$dg      <- obj$dg

  # Add life history schedules to parameters.
  obj <- .calcSchedules( obj )
  # Add SPR and YPR.
  obj <- deref( .calcPerRecruit( f=0, objRef=as.ref(obj) ) )

  # Add Beverton-Holt stock-recruit parameters.
  B0         <- obj$B0              # Unfished female biomass.
  rSteepness <- obj$rSteepness      # Steepness.
  obj$R0     <- B0/obj$ssbpr        # Unfished recruitment.

  # Beverton-Holt stock-recruitment parameters
  obj$rec.a  <- 4.*rSteepness*obj$R0 / ( B0*(1.-rSteepness) )
  obj$rec.b  <- (5.*rSteepness-1.) / ( B0*(1.-rSteepness) )


  # Initialise population at unfished equilibrium.
  a <- obj$ages[c(-1,-A)]
  obj$numAgeYr1    <- matrix ( NA, ncol = obj$nGrps, nrow=A )
  obj$numAgeYr1[1,] <- obj$R0*obj$piOne
  for ( g in 1:obj$nGrps )
    obj$numAgeYr1[a,g] <- obj$numAgeYr1[1,g]*exp( -obj$M[g]*(a-1) )
  obj$numAgeYr1[A,] <- obj$numAgeYr1[1,]*exp( -obj$M[1:obj$nGrps]*(A-1) )/(1.-exp(-obj$M[1:obj$nGrps]))

  # Calculate reference curves.
  obj <- deref( .calcRefCurves( objRef=as.ref(obj) ) )


  # Recruitment calculations for reference points/steepness plot.
  B20  <- 0.2*B0
  R20  <- obj$rec.a*B20 / ( 1.+obj$rec.b*B20 )

  obj$B20  <- B20
  obj$R20  <- R20

  # Calculate reference points.
  hrSplineFun <- splinefun( obj$F, obj$legalHR )
  # Unfished F0
  tmp               <- deref( .calcEquil( f=0, objRef=as.ref(obj) ) )
  
  obj$F0            <- 0
  obj$yprLF0        <- tmp$yprL
  obj$yprDF0        <- tmp$yprD
  obj$yprF0         <- sum(tmp$yprL)
  obj$ssbprF0       <- tmp$ssbpr
  obj$landedF0      <- tmp$landed
  obj$yieldF0       <- tmp$landed
  obj$discardedF0   <- tmp$discarded
  obj$ssbF0         <- tmp$ssb
  obj$recruitsF0    <- tmp$recruits
  obj$U0            <- 0.
  obj$legbprF0      <- tmp$legbpr
  obj$legalHRF0     <- tmp$legalHR
  obj$sublegalHRF0  <- tmp$sublegalHR


  # F0.1
  tmp               <- .getF01( obj )
  obj$F01           <- tmp$F01
  obj$U01           <- hrSplineFun( x=tmp$F01 )
  obj$yprLF01       <- tmp$yprLF01
  obj$yprDF01       <- tmp$yprDF01
  obj$yprF01        <- sum(tmp$yprLF01)
  obj$ssbprF01      <- tmp$ssbprF01
  obj$landedF01     <- tmp$landedF01
  obj$yieldF01      <- tmp$landedF01
  obj$discardedF01  <- tmp$discardedF01
  obj$ssbF01        <- tmp$ssbF01
  obj$recruitsF01   <- tmp$recruitsF01

  obj$legbprF01     <- tmp$legbprF01
  obj$legalHRF01    <- tmp$legalHRF01
  obj$sublegalHRF01 <- tmp$sublegalHRF01


  # Fmsy
  tmp               <- .getFmsy( obj )
  obj$Fmsy          <- tmp$Fmsy
  obj$Umsy          <- hrSplineFun( x=tmp$Fmsy )
  obj$yprLFmsy      <- tmp$yprLFmsy
  obj$yprDFmsy      <- tmp$yprDFmsy
  obj$yprFmsy       <- sum(tmp$yprLFmsy)
  obj$ssbprFmsy     <- tmp$ssbprFmsy
  obj$landedFmsy    <- tmp$landedFmsy
  obj$yieldFmsy     <- tmp$landedFmsy
  obj$discardedFmsy <- tmp$discardedFmsy
  obj$ssbFmsy       <- tmp$ssbFmsy
  obj$recruitsFmsy  <- tmp$recruitsFmsy

  obj$legbprFmsy     <- tmp$legbprFmsy
  obj$legalHRFmsy    <- tmp$legalHRFmsy
  obj$sublegalHRFmsy <- tmp$sublegalHRFmsy


  # F40%
  tmp               <- .getF40( obj )
  obj$F40           <- tmp$F40
  obj$U40           <- hrSplineFun( x=tmp$F40 )
  obj$yprLF40       <- tmp$yprLF40
  obj$yprDF40       <- tmp$yprDF40
  obj$yprF40        <- sum(tmp$yprLF40)
  obj$ssbprF40      <- tmp$ssbprF40
  obj$landedF40     <- tmp$landedF40
  obj$yieldF40      <- tmp$landedF40
  obj$discardedF40  <- tmp$discardedF40
  obj$ssbF40        <- tmp$ssbF40
  obj$recruitsF40   <- tmp$recruitsF40

  obj$legbprF40     <- tmp$legbprF40
  obj$legalHRF40    <- tmp$legalHRF40
  obj$sublegalHRF40 <- tmp$sublegalHRF40


  # Fmax
  tmp                <- .getFmax( obj )
  obj$Fmax           <- tmp$Fmax
  obj$Umax           <- hrSplineFun( x=tmp$Fmax )
  obj$yprLFmax       <- tmp$yprLFmax
  obj$yprDFmax       <- tmp$yprDFmax
  obj$yprFmax        <- sum(tmp$yprLFmax)
  obj$ssbprFmax      <- tmp$ssbprFmax
  obj$landedFmax     <- tmp$landedFmax
  obj$yieldFmax      <- tmp$landedFmax
  obj$discardedFmax  <- tmp$discardedFmax
  obj$ssbFmax        <- tmp$ssbFmax
  obj$recruitsFmax   <- tmp$recruitsFmax

  obj$legbprFmax     <- tmp$legbprFmax
  obj$legalHRFmax    <- tmp$legalHRFmax
  obj$sublegalHRFmax <- tmp$sublegalHRFmax


  # Fcrash
  tmp               <- .getFcra( obj )
  obj$Fcra          <- tmp$Fcra
  obj$Ucra          <- hrSplineFun( x=tmp$Fcra )
  obj$yprLFcra      <- tmp$yprLFcra
  obj$yprDFcra      <- tmp$yprDFcra
  obj$yprFcra       <- sum(tmp$yprLFcra)
  obj$ssbprFcra     <- tmp$ssbprFcra
  obj$landedFcra    <- tmp$landedFcra
  obj$yieldFcra     <- tmp$landedFcra
  obj$discardedFcra <- tmp$discardedFcra
  obj$ssbFcra       <- tmp$ssbFcra
  obj$recruitsFcra  <- tmp$recruitsFcra

  obj$legbprFcra      <- tmp$legbprFcra
  obj$legalHRFcra     <- tmp$legalHRFcra
  obj$sublegalHRFcra  <- tmp$sublegalHRFcra


  
  #cat("calcRefPoints took ",proc.time()-t1," seconds\n")
  return( as.ref(obj) )
}

# .calcSalg   ( Calculate selectivity-at-age)
# Purpose:    Calculate selectivity-at-age for nGrps length groups.
# Parameters: salgPars, a list of selectivity params, A=max age,
#               Lal=length-at-age for nGrps length groups.
# Returns:    Salg, array of (0,1) selectivities for each age-/length-group.
# Source:     S.P. Cox
.calcSalg <- function( salgPars, A=25, Lal )
{
  nGrps  <- salgPars$nGrps
  nGear  <- salgPars$nGear

  L50Cg1 <- salgPars$L50Cg1
  L95Cg1 <- salgPars$L95Cg1
  L95Cg2 <- salgPars$L95Cg2
  L50Cg2 <- salgPars$L50Cg2

  selAge <- salgPars$selAge


  # From sableOpmod.tpl:
  #for( g=1; g<=nFisheries; g++ )
  #{
  #    if( selType[g] == 2 )   // use dome-shaped function 
  #    { 
  #      tmp1 = exp( (-1.)*log(19.0)*(tmpLal-L50_1[g])/(L95_1[g] - L50_1[g]) );
  #      tmp2 = exp( (-1.)*log(19.0)*(tmpLal-L50_2[g])/(L95_2[g] - L50_2[g]) );
  #      Salg[g] = elem_prod( (1./(1.+tmp1)), (1./(1.+tmp2)) );
  #      Salg[g] /= matrixMAX( Salg[g] ); 
  #    }
  #    else                    // use asymptotic function
  #    { 
  #      tmp1 = exp( (-1.)*log(19.0)*(tmpLal-L50_1[g])/(L95_1[g] - L50_1[g]) );
  #      Salg[g] = 1./(1.+tmp1); 
  #    }      
  #}
  selType <- salgPars$selType
  Salg <- array( data=NA, dim=c(A,nGrps,nGear) )
  for( g in 1:nGear )
  {
    if( selAge )
    {
      for( l in 1:ncol(Lal) ) Lal[,l] <- 1:nrow(Lal)
    } 
    if ( selType[g] == 2 )     # Use dome-shaped function (normal).
    {
      Salg[,,g] <- exp(-(L50Cg1[g] - Lal)^2/2/L95Cg1[g]/L95Cg1[g])
      # tmp1 <- exp( (-1.)*log(19.0)*(Lal-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
      # tmp2 <- exp( (-1.)*log(19.0)*(Lal-L50Cg2[g])/(L95Cg2[g] - L50Cg2[g]) )
      # tmpS <- (1./(1.+tmp1))*(1./(1.+tmp2))
      # Salg[,,g] <- tmpS/max( tmpS )
    }
    if ( selType[g] == 1 )   # Use asymptotic function.
    {
      tmp1 <- exp( (-1.)*log(19.0)*(Lal-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
      Salg[,,g] <- 1.0 / ( 1.0+tmp1 )
      Salg[,,g] <- Salg[,,g]/max(Salg[,,g])
    }

  }
  return( Salg )
}

# .calcPalg   ( Calculate proportion discarded at age a and growth-group l)
# Purpose:    Calculate proportion discarded for A ages and nGrps length groups.
# Parameters: palgPars, a list of discard ogive params, A=max age,
#               Lal=length-at-age for nGrps length groups.
# Returns:    Palg, array of (0,1) discard probs for each age-/length-group.
# Source:     S.P. Cox
.calcPalg <- function( palgPars, A=25, Lal )
{
  nGrps <- palgPars$nGrps
  nGear <- palgPars$nGear

  L50Dg <- palgPars$L50Dg
  L95Dg <- palgPars$L95Dg

  Palg <- array( data=NA, dim=c(A,nGrps,nGear) )
  for( g in 1:nGear )
  {
    tmp  <- exp( (-1.)*log(19)*(Lal-L50Dg[g])/(L95Dg[g] - L50Dg[g]) )
    tmpP <- (1./(1.+ tmp))
    tmpP[ Lal < palgPars$sizeLim[g] ] <- 1.0
    Palg[,,g] <- tmpP
  }
  return( Palg )
}

# .calcMa      ( Calculate maturity at age ogive )
# Purpose:     Calculates maturity-at-age ogive
# Parameters:  A, max age, A50(95), age-at-50%(95%) maturity.
# Returns:     Ma, a vector of length A with proportion mature at age.
# Source:      S.P. Cox
.calcMa <- function( A, A50=5,A95=8 )
{
  a <- c(1:A)
  g <- log(19.)/( A95 - A50 )
  Ma <- 1./( 1. + exp(-g*( a - A50 ) ) )
  return( Ma )
}

# .calcLal      ( Calculate von Bertanlanffy length at age by growth-group )
# Purpose:      Calculate von B length-at-age for multiple growth-groups.
# Parameters:   A, max age; Linf, asymptotic length(s); L1, length-at-age 1;
#                 vonK, growth rate.
# Returns:      Lal, a matrix of lengths-at-age by growth group.
# Source:       S.P. Cox
.calcLal <- function( Linf=80., L1=35.0, vonK=0.465, A=25 )
{
  age <- 1:A
  t0 <- log ( (Linf-L1)/Linf)/vonK + 1
  if( length(Linf)==1 )
  {
    Lal <- Linf*(1.-exp(-vonK*(age-t0)))
    Lal <- matrix(Lal,nrow = length(Lal), ncol = length(Linf))
  }
  else # vector of Linf for length-group
  {
    Lal <- matrix( NA, nrow=A,ncol=length(Linf) )
    for( l in 1:ncol(Lal) )
      Lal[,l] <- Linf[l]*(1.-exp( -vonK[l]*(age-t0[l]) ))
  }
  return( Lal )
}

# .setLegal     ( Set legal indicator (0 or 1) for each age/length)
# Purpose:      Use length/age matrix to set legal (1) or sub-legal (0)
# Parameters:   l = length/age matrix and sizeLim = legal size limit
# Returns:      legal - a matrix of indicators (0 or 1) 
# Source:       S.P. Cox
.setLegal <- function( l, sizeLim )
{
  legal <- matrix( 0, nrow=nrow(l), ncol=ncol(l) )
  legal[ l>sizeLim[1] ] <- 1
  return(legal)
}

# .calcWtAge     ( Calculate weight-at-age by growth-group )
# Purpose:       Calculates weight-at-age for multiple growth-groups.
# Parameters:    Lal, length-at-age by group; c1, scalar; c2, power
# Returns:       Wal, matrix of weights-at-age by growth-group.
# Source:        S.P. Cox
.calcWal <- function( Lal, c1=1.e-5,c2=3.0 )
{
  # Weight-length relationship including bias correction
  if ( length(c1)==1) Wal <- c1 * Lal^c2
  else {
    Wal <- Lal
    for ( i in 1:length(c1) ) Wal[,i] <- c1[i]*Lal[,i]^c2[i]
  }
  Wal  ###Path Specific scaling here???
}

# .calcSchedules ( Calculate life history schedules )
# Purpose:       Calculate length-, weight-, and maturity-at-age vectors.
# Parameters:    obj, a list of operating model parameters; 
# Returns:       lifeScheds, a list with vectors for each life history schedule.
# Source:        S.P. Cox
.calcSchedules <- function( obj )
{
  # Extract life history parameters for setting up population dynamics model.
  M         <- obj$M
  Linf      <- obj$Linf
  sigmaLinf <- NULL #obj$sigmaLinf
  L1        <- obj$L1
  vonK      <- obj$vonK
  c1        <- obj$c1
  c2        <- obj$c2

  A50       <- obj$aMat50
  A95       <- obj$aMat95



  if(!is.null(obj$selAge))
    selAge <- obj$selAge
  else selAge <- FALSE

  salgPars <- list( L50Cg1 = obj$L50Cg1,
                    L95Cg1 = obj$L95Cg1,
                    L95Cg2 = obj$L95Cg2,
                    L50Cg2 = obj$L50Cg2,
                    selType = obj$selType,
                    nGrps  = obj$nGrps,
                    nGear  = obj$nGear,
                    selAge = selAge
                  )

  palgPars <- list( sizeLim  = obj$sizeLim,
                    L50Dg    = obj$L50Dg,
                    L95Dg    = obj$L95Dg,
                    nGrps    = obj$nGrps,
                    nGear    = obj$nGear
                  )

  # Indices
  A       <- obj$nAges
  nGrps   <- obj$nGrps
  nGear   <- obj$nGear

  # SPC, HACK: hardwired limits to distribution of Linfs here.
  # Might consider optional log-normal distribution.
  minProbL <- 0.025
  maxProbL <- 0.975
  pWidth   <- (maxProbL-minProbL)/nGrps
  probs    <- minProbL + pWidth*c( 0:(nGrps-1) )

  # Compute life history schedules.
  
  obj$ages <- c(1:A)
  lifeScheds       <- obj
  lifeScheds$Linfl <- Linf #qnorm( p=probs, mean=Linf, sd=sigmaLinf )
  lifeScheds$Lal   <- .calcLal( Linf=lifeScheds$Linfl[1:nGrps], vonK=vonK[1:nGrps], L1=L1[1:nGrps], A=A )
  lifeScheds$Legal <- .setLegal( l=lifeScheds$Lal, sizeLim=obj$sizeLim )
  lifeScheds$Wal   <- .calcWal( c1=c1[1:nGrps],c2=c2[1:nGrps],Lal=lifeScheds$Lal )
  lifeScheds$Ma    <- .calcMa( A50=A50,A95=A95,A=A )
  lifeScheds$Salg  <- .calcSalg( salgPars=salgPars, A=A, Lal=lifeScheds$Lal )
  lifeScheds$Palg  <- .calcPalg( palgPars=palgPars, A=A, Lal=lifeScheds$Lal )
  lifeScheds$genTime <- .calcGenTime( M=obj$M, A50=A50,A95=A95,A=A )
  # cat("Generation time for M =",obj$recM," is: ", lifeScheds$genTime, "\n", sep = "" )
  lifeScheds
}

# .calcGenTime
# Purpose:     Calculate generation time as average age of mature stock
# Parameters:  natural mortality and maturity
# Returns:     generation time
# Source:      S.P. Cox
.calcGenTime <- function( M=0.08, A50=5, A95=8, A=30 )
{
  a <- c(1:A)
  surv <- vector(length=A)
  surv[1:(A-1)] <- exp(-M[1]*(c(1:(A-1))-1.))
  surv[A]       <- surv[A-1]*exp(-M[1])/(1.-exp(-M[1]))
  g    <- log(19.0)/(A95-A50)
  mat  <- .calcMa( A=A, A95=A95, A50=A50 )
  genTime <- sum( a*surv*mat )/sum( surv*mat )
  genTime
}

# .calcPerRecruit
# Purpose:     Calculate all equilibrium per-recruit quantities of interest for an
#              input fishing mortality.
# Parameters:  f=scalar input fishing mortality rate; obj=list of all operating
#              model parameters.
# Returns:     a list with equilibrium quantities - (i) spawning stock biomass-per-recruit
#              and (ii) yield-per-recruit (ypr)
# Source:      S.P. Cox
.calcPerRecruit <- function( f=0, objRef )
{

  # Compute equilibrium spawning biomass per recruit given f and parameters.
  obj <- deref( objRef )
  A <- obj$nAges
  M <- obj$M
  recM <- obj$recM
  nGrps <- obj$nGrps
  nGear <- obj$nGear


  Ma    <- obj$Ma
  Ma[1] <- 0
  Lal   <- obj$Lal
  Wal   <- obj$Wal
  Legal <- obj$Legal

  Salg  <- obj$Salg
  Palg  <- obj$Palg
  dg    <- obj$dg
  if( any( obj$fg < 1.e-10 ) ) obj$fg <- rep(0,nGear)
  Fg    <- f[1]*obj$fg

  # Create survivorship and total mortality matrices.
  Nal <- matrix( NA, nrow=A, ncol=nGrps )
  Zal <- matrix( NA, nrow=A, ncol=nGrps )

  # initialize age-1 growth groups
  # piOne=proportion in each growth-group
  Nal[1,] <- rep( 1, nGrps )

  # calc total fishing mortality including both
  # landed and discarded by age-/growth-group
  Falg <- array( data=NA, dim=c(A,nGrps,nGear) )

  #for( g in 1:nGear )
   # Falg[,,g] <- Salg[,,g]*Fg[g]*(dg[g]*Palg[,,g] - Palg[,,g] + 1.)
  # compute Zal by summing M and Fg over gear types
  #Zal <- M + matrixsum( Falg )
  Zal <- matrix ( M, nrow = A, ncol = nGrps, byrow = TRUE ) 
  for( g in 1:nGear )
    Zal <- Zal + Salg[,,g]*Fg[g]
    # Zal <- Zal + Salg[,,g]*Fg[g]*(dg[g]*Palg[,,g] - Palg[,,g] + 1.)
  # compute Zal by summing M and Fg over gear types
  #Zal <- M + matrixsum( Falg )

  # recursive calculation of number-at-age/growth-group
  for( a in 2:(A-1) )
    Nal[a,] <- Nal[a-1,]*exp( -Zal[a-1,] )
  Nal[A,] <- Nal[A-1,]*exp(-Zal[A-1,])/(1.0-exp(-Zal[A,]))

  # calc ypr for both landed and discarded by age-/growth-group
  Calg <- array( data=NA, dim=c(A,nGrps,nGear) )# landings
  Dalg <- array( data=NA, dim=c(A,nGrps,nGear) )# dead discards

  yprL <- numeric(nGear)
  yprD <- numeric(nGear)
  yprLeg <- numeric(nGear)
  for( g in 1:nGear )
  {
    Calg[,,g] <- Nal*Wal*Salg[,,g]*Fg[g]*(1.-exp(-Zal))/Zal    
    Dalg[,,g] <- 0
    # Calg[,,g] <- Nal*Wal*Salg[,,g]*Fg[g]*(1.-Palg[,,g])*(1.-exp(-Zal))/Zal
    # Dalg[,,g] <- Nal*Wal*Salg[,,g]*Fg[g]*dg[g]*Palg[,,g]*(1.-exp(-Zal))/Zal

    yprL[g]   <- sum( Calg[,,g] )
    yprD[g]   <- sum( Dalg[,,g] )
  }

  if ( any(is.na(Calg)) )
    browser()
    
  if ( any(is.na(Dalg)) )
    browser()

  # legal ypr - this must sum over the gear types.
  #yprLeg    <- sum( Legal*matrixsum( Calg+Dalg ) ) 
  #yprSubLeg <- sum( (1.-Legal)*matrixsum( Calg+Dalg ) )
  
  yprLeg    <- sum( Legal*apply( Calg+Dalg,c(1,2),sum ) ) 
  yprSubLeg <- sum( (1.-Legal)*apply( Calg+Dalg,c(1,2),sum ) ) 

  # spawning and legal biomass per recruit.
  # Only use femlaes for ssbpr
  # Added: depletion by total mortality for end of year spawning before
  # graduating to the following year class (Herring/ISCAM)
  ssbpr  <- sum( Nal[,1]*Wal[,1]*Ma*exp(-1.*Zal[,1]) )
  legbpr <- sum( Nal*Legal*Wal )
  sublegbpr <- sum( Nal*(1.-Legal)*Wal )

  # compile return list
  phi <- obj
    phi$ssbpr  <- ssbpr
    phi$legbpr <- legbpr
    phi$sublegbpr <- sublegbpr
    phi$yprL   <- yprL # these ypr are vectors (gear)
    phi$yprD   <- yprD
    phi$ypr    <- yprL + yprD
    phi$yprLeg <- yprLeg # legal yrp landed+discarded dead
    phi$yprSubLeg <- yprSubLeg
  return( as.ref(phi) )
}

# ARK (12-Aug=12) Documentation needed here.
.getYPRvals <- function( lnfg, objRef, f=0 )
{
  obj    <- deref( objRef )
  obj$fg <- exp( lnfg )
  tmp    <- deref( .calcPerRecruit( f=f, objRef=as.ref(obj) ) )

  propYPR <- tmp$yprL/sum(tmp$yprL)
  funcVal <- sum((propYPR-obj$allocProp)^2.)
  funcVal
}

# .calcEquil
# Purpose:     Calculate all equilibrium quantities of interest for an
#              input fishing mortality.
# Parameters:  f=scalar input fishing mortality rate; obj=list of all operating
#              model parameters.
# Returns:     a list with equilibrium quantities - (i) total recruits,spawning
#              biomass (ssb) and yield; (ii) spawning stock biomass-per-recruit
#              (ssbpr), and (iii) yield-per-recruit (ypr)
# Source:      S.P. Cox
.calcEquil <- function( f=0, objRef )
{
  obj <- deref( objRef )
  obj$fg <- .FGINIT


  
  # Compute yield and biomass per recruit function values
  tmp <- deref( .calcPerRecruit( f=f,objRef=as.ref(obj) ) )
  # Beverton-Holt sr model parameters
  rec.a <- obj$rec.a
  rec.b <- obj$rec.b

  # Compute equilibrium recruitment, biomass and yield
  # Multiply be 2 for males and females
  recruits <- (rec.a*tmp$ssbpr - 1.0) / (rec.b*tmp$ssbpr)

  equil <- list()
    equil$recruits <- recruits
    equil$ssbpr    <- tmp$ssbpr
    equil$ssb      <- recruits * tmp$ssbpr
    equil$legb     <- recruits * tmp$legbpr
    equil$sublegb  <- recruits * tmp$sublegbpr
    equil$yprL     <- tmp$yprL
    equil$yprD     <- tmp$yprD
    equil$ypr      <- sum(tmp$yprL + tmp$yprD)
    equil$yprLeg   <- tmp$yprLeg
    equil$yprSubLeg<- tmp$yprSubLeg
    equil$landed   <- recruits*sum( tmp$yprL )
    equil$discarded<- recruits*sum( tmp$yprD )
    equil$legal    <- recruits*tmp$yprLeg
    equil$sublegal <- recruits*tmp$yprSubLeg
    equil$fg       <- obj$fg
    equil$legalHR  <- equil$landed/(equil$legb + equil$landed)

  if(!is.finite(equil$sublegb)) browser()
    
  return( as.ref(equil) )
}

# .calcRefCurves
# Purpose:     Calculate all equilibrium relationships to fishing mortality.
# Parameters:  obj=list of all operating model parameters; .nFVALS=the number of
#              fishing mortality points over which to compute the functions
# Returns:     a list with vectors of fishing mortality (f) and equilibrium
#              functions (these are later fitted with splines for finding
#              references points via root-finding algorithms)
# Source:      S.P. Cox
.calcRefCurves <- function( objRef )
{
  obj <- deref( objRef )
  
  f <- seq( from=0.0, to=.MAXF*max(obj$M), length=.nFVALS )

  recruits   <- rep( NA, length=.nFVALS )
  ssbpr      <- rep( NA, length=.nFVALS )
  ssb        <- rep( NA, length=.nFVALS )
  legalb     <- rep( NA, length=.nFVALS )
  sublegalb  <- rep( NA, length=.nFVALS )
  ypr        <- rep( NA, length=.nFVALS )
  yprLeg     <- rep( NA, length=.nFVALS )
  yprSubLeg  <- rep( NA, length=.nFVALS )
  yprL       <- rep( NA, length=.nFVALS )
  yprD       <- rep( NA, length=.nFVALS )
  landed     <- rep( NA, length=.nFVALS )
  discarded  <- rep( NA, length=.nFVALS )
  legal      <- rep( NA, length=.nFVALS )
  legalHR    <- rep( NA, length=.nFVALS )
  sublegal   <- rep( NA, length=.nFVALS )
  sublegalHR <- rep( NA, length=.nFVALS )
  fg         <- matrix(NA, nrow=.nFVALS, ncol=obj$nGear )

  optF <- optim(  par=rep(-1,obj$nGear), fn=.getYPRvals, method="BFGS", 
                  control=list(maxit=.MAXIT), f=obj$M, objRef=objRef )
  .FGINIT <<- exp( optF$par )

  for( i in 1:length(f) )
  {
    tmp          <- deref( .calcEquil( f=f[i],objRef=objRef ) )
    recruits[i]  <- tmp$recruits
    ssbpr[i]     <- tmp$ssbpr
    ssb[i]       <- tmp$ssb
    legalb[i]    <- tmp$legb
    sublegalb[i] <- tmp$sublegb           # ARK (12-Aug-13) Added by ARK.
    yprL[i]      <- sum( tmp$yprL )
    yprD[i]      <- sum( tmp$yprD )
    ypr[i]       <- yprL[i] + yprD[i]
    yprLeg[i]    <- tmp$yprLeg
    yprSubLeg[i] <- tmp$yprSubLeg
    landed[i]    <- tmp$landed
    discarded[i] <- tmp$discarded
    legal[i]     <- tmp$legal
    sublegal[i]  <- tmp$sublegal
    legalHR[i]   <- tmp$legal/(tmp$legb + tmp$legal)
    

    if( tmp$sublegb > 0. )
      sublegalHR[i] <- tmp$sublegal/tmp$sublegb
    else
      sublegalHR[i] <- 0.
      
    fg[i,]     <- tmp$fg
  }

  refCurves <- obj
    refCurves$F          <- f
    refCurves$ssbpr      <- ssbpr
    refCurves$ssb        <- ssb
    refCurves$legalb     <- legalb
    refCurves$sublegalb  <- sublegalb
    refCurves$recruits   <- recruits
    refCurves$ypr        <- ypr
    refCurves$yprL       <- yprL
    refCurves$yprD       <- yprD
    refCurves$yprLeg     <- yprLeg
    refCurves$yprSubLeg  <- yprSubLeg
    refCurves$yield      <- landed + discarded
    refCurves$landed     <- landed
    refCurves$discarded  <- discarded
    refCurves$legal      <- legal
    refCurves$sublegal   <- sublegal
    refCurves$legalHR    <- legalHR
    refCurves$sublegalHR <- sublegalHR
    refCurves$fg         <- fg

  return( as.ref(refCurves) )
}     # .calcRefCurves function


# .getF01
# Purpose:     fit a spline function to f vs ypr, then use a root finder to get F0.1. Note
#              this function can be easily modified to get any F0.X by changing target
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for F0.1
# Source:      S.P. Cox
.getF01 <- function( obj )
{
  maxF <- max( obj$F )
  # create the spline function: this is not a number, it is a function
  # that can be called like any other function, except it only has one
  # argument, in this case F. Spline functions below are similar
  fyprSplineFun <- splinefun( x=obj$F,y=obj$yprL )
  slopeAtOrigin <- fyprSplineFun( x=0, deriv=1 )

  yprRatio <- function( fin ){
    f2     <- fyprSplineFun( x=fin, deriv=1 )
    ratio  <- f2/slopeAtOrigin
    target <- 0.1
    return(ratio - target)
  }
  if( fyprSplineFun( x=maxF,deriv=1 ) > 0 )
    obj$F01 <- maxF
  else
    obj$F01 <- uniroot( f=yprRatio,interval=c(0,maxF) )$root

  tmp             <- deref( .calcEquil( f=obj$F01, objRef=as.ref(obj)  ) )
  obj$yprLF01     <- tmp$yprL
  obj$yprDF01     <- tmp$yprD
  obj$yprLeg      <- tmp$yprLeg
  obj$ssbprF01    <- tmp$ssbpr
  obj$landedF01   <- tmp$landed
  obj$discardedF01<- tmp$discarded
  obj$ssbF01      <- tmp$ssb
  obj$recruitsF01 <- tmp$recruits
  obj
}

# .getFmsy     ()
# Purpose:     fit a spline function to f vs yield, then use a root finder to get Fmsy.
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fmsy
# Source:      S.P. Cox
.getFmsy <- function( obj )
{

  # Attempt to handle failed uniroot.
  # Two strategies (1) increase intervals, (2) decrease maxF.
  # However, this is so embedded that it is not possible to deal with it here.
  # So, simply flag the event and set Fmsy==Fmax to allow other calculations
  # to proceed, then reset to 9999 to flag in mcmcStats.
  F <- as.vector(obj$F)
  L <- as.vector(obj$landed)
  tmp1 <- data.frame( cbind(F=F,L=L) )
  tmp  <- subset( tmp1, L>0, select=c(F,L) )

  maxF              <- max( tmp$F )
  fySplineFun       <- splinefun( x=tmp$F,y=tmp$L )
  val0              <- fySplineFun(x=0,deriv=1)
  valMaxF           <- fySplineFun(x=maxF,deriv=1)
  
  Fmsy <- try( uniroot( f=fySplineFun,interval=c(0,maxF), deriv=1 )$root )
 
  if ( class(Fmsy)=="try-error" )
  {
    cat( "\n(.getFmsy) Error in uniroot...\n" )
    print( Fmsy )
    Fmsy <- 9999
  }
  
  #Fmsy              <- uniroot( f=fySplineFun,interval=c(0,maxF), deriv=1 )$root
  
  obj$Fmsy          <- min( Fmsy, maxF )
  tmp               <- deref(.calcEquil( f=obj$Fmsy, objRef=as.ref(obj) ) )
  obj$yprLFmsy      <- tmp$yprL
  obj$yprDFmsy      <- tmp$yprD
  obj$ssbprFmsy     <- tmp$ssbpr
  obj$landedFmsy    <- tmp$landed
  obj$discardedFmsy <- tmp$discarded
  obj$ssbFmsy       <- tmp$ssb
  obj$recruitsFmsy  <- tmp$recruits
  obj$legalHRFmsy   <- tmp$legalHR
  obj$Fmsy <- Fmsy
  
  obj
}

# .getF40     ()
# Purpose:     fit a spline function to f vs ssbpr, then use a root finder to get F40%. Can
#              get any FX% by changing the value of "target"
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for F40%
# Source:      S.P. Cox
.getF40 <- function( obj )
{
  maxF <- max( obj$F )
  fssbprSplineFun <- splinefun( x=obj$F,y=obj$ssbpr )
  ssbprAtOrigin   <- fssbprSplineFun( x=0 )
  ssbprRatio <- function( fin ){
    f2 <- fssbprSplineFun( fin )
    ratio <- f2/ssbprAtOrigin
    target <- 0.4
    return(ratio - target)
  }
  F40 <- uniroot( f=ssbprRatio,interval=c(0,maxF) )$root
  obj$F40 <- min( F40, maxF )

  tmp             <- deref(.calcEquil( f=obj$F40, objRef=as.ref(obj) ) )
  obj$yprLF40     <- tmp$yprL
  obj$yprDF40     <- tmp$yprD
  obj$ssbprF40    <- tmp$ssbpr
  obj$landedF40   <- tmp$landed
  obj$discardedF40<- tmp$discarded
  obj$ssbF40      <- tmp$ssb
  obj$recruitsF40 <- tmp$recruits
  obj
}

# .getFmax     ()
# Purpose:     fit a spline function to f vs ypr, then use a root finder to get Fmax.
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fmax
# Source:      S.P. Cox
.getFmax <- function( obj )
{
  maxF          <- max( obj$F )
  fyprSplineFun <- splinefun( x=obj$F,y=obj$yprL )

  if( fyprSplineFun( x=maxF,deriv=1 ) > 0 )
    obj$Fmax <- maxF
  else
    obj$Fmax <- uniroot( f=fyprSplineFun,interval=c(0,maxF),deriv=1 )$root

  tmp              <- deref( .calcEquil( f=obj$Fmax, objRef=as.ref(obj) ) )
  obj$yprLFmax     <- tmp$yprL
  obj$yprDFmax     <- tmp$yprD
  obj$ssbprFmax    <- tmp$ssbpr
  obj$landedFmax   <- tmp$landed
  obj$discardedFmax<- tmp$discarded
  obj$ssbFmax      <- tmp$ssb
  obj$recruitsFmax <- tmp$recruits
  obj
}


# .getFcra
# Purpose:     fit a spline function to f vs ssb, then use a root finder to get Fcra(sh).
# Parameters:  obj=list of all operating model parameters, schedules, equilibrium functions
# Returns:     a list with all equilibrium quantities for Fcra(sh)...only really matters
#              for per-recruit quantities
# Source:      S.P. Cox
.getFcra <- function( obj )
{
  maxF          <- max( obj$F )
  fssbSplineFun <- splinefun( x=obj$F,y=obj$ssb )

  if( fssbSplineFun( x=maxF ) > 0 )
    obj$Fcra <- maxF
  else
    obj$Fcra <- uniroot( f=fssbSplineFun,interval=c(0,maxF) )$root

  tmp              <- deref(.calcEquil( f=obj$Fcra, objRef=as.ref(obj) ) )
  obj$yprLFcra     <- tmp$yprL
  obj$yprDFcra     <- tmp$yprD
  obj$ssbprFcra    <- tmp$ssbpr
  obj$landedFcra   <- tmp$landed
  obj$discardedFcra<- tmp$discarded
  obj$ssbFcra      <- tmp$ssb
  obj$recruitsFcra <- tmp$recruits
  obj
}

# Notes: need to modify .calcEquil to add legal and sub-legal categories
# Then, need to decide how to use legal-biomass. One option is to calc
# (i) totalCatch = fn(legBiomass, F); (ii) gearCatch = allocation*totalCatch;
# (iii) fg = solveBaranov( gearCatch, obj ); (iv) discard = fn( fg,Palg,Salg,dg )
# check lit (e.g., Kell et al, Punt...) for how others have simulated discarding
# for multiple fleets.
