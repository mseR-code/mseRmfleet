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
calcRefPoints <- function( refPars )
{
  #  obj <- readSimPars( parFile )
  # use lisread to extract the vector parameters below
  #tmp <- lisread( obj$lisReadFile, quiet=TRUE )

  obj <- refPars

  # Indices
  A           <- obj$nAges
  obj$piOne   <- 1./obj$nGrps

  # salgPars: used to compute selectivity for age a, growth-group l, by gear g.
  obj$L50Cg1  <- obj$L50Cg1
  obj$L95Cg1  <- obj$L95Cg1
  obj$L95Cg2  <- obj$L95Cg2
  obj$L50Cg2  <- obj$L50Cg2

  # palgPars: used to compute proportion of age a, growth-group l discarded.
  obj$sizeLim <- obj$sizeLim
  obj$L95Dg   <- obj$L95Dg
  obj$L50Dg   <- obj$L50Dg

  # Relative Fs
  obj$fg      <- obj$fg

  # discard mortality rate
  obj$dg      <- obj$dg

  # Add life history schedules to parameters.
  obj <- .calcSchedules( obj )

  # Add SPR and YPR.
  obj <- .calcPerRecruit( f=0, obj=obj )

  # Add Beverton-Holt stock-recruit parameters.
  B0         <- obj$B0              # Unfished biomass.
  rSteepness <- obj$rSteepness      # Steepness.
  obj$R0     <- B0/obj$ssbpr        # Unfished recruitment.

  # Beverton-Holt stock-recruitment parameters
  obj$rec.a  <- 4.*rSteepness*obj$R0 / ( B0*(1.-rSteepness) )
  obj$rec.b  <- (5.*rSteepness-1.) / ( B0*(1.-rSteepness) )

  # Initialise population at unfished equilibrium.
  #SPC: this needs to be replaced with age,growth-group matrix
  a <- obj$ages[c(-1,-A)]
  obj$numAgeYr1    <- numeric( length=A )
  obj$numAgeYr1[1] <- obj$R0
  obj$numAgeYr1[a] <- obj$R0*exp( -obj$M*(a-1) )
  obj$numAgeYr1[A] <- obj$R0*exp( -obj$M*(A-1) )/(1.-exp(-obj$M))

  # Calculate reference curves.
  obj <- .calcRefCurves( obj, nFs=100 )

  # Recruitment calculations for reference points/steepness plot.
  B20  <- 0.2*B0
  R20  <- obj$rec.a*B20/( 1.+obj$rec.b*B20 )

  obj$B20  <- B20
  obj$R20  <- R20

  # Calculate reference points.

  # Unfished F0
  tmp               <- .calcEquil( f=0, obj )
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

  # F0.1
  tmp               <- .getF01( obj )
  obj$F01           <- tmp$F01
  obj$yprLF01       <- tmp$yprLF01
  obj$yprDF01       <- tmp$yprDF01
  obj$yprF01        <- sum(tmp$yprLF01)
  obj$ssbprF01      <- tmp$ssbprF01
  obj$landedF01     <- tmp$landedF01
  obj$yieldF01      <- tmp$landedF01
  obj$discardedF01  <- tmp$discardedF01
  obj$ssbF01        <- tmp$ssbF01
  obj$recruitsF01   <- tmp$recruitsF01

  # Fmsy
  tmp               <- .getFmsy( obj )
  obj$Fmsy          <- tmp$Fmsy
  obj$yprLFmsy      <- tmp$yprLFmsy
  obj$yprDFmsy      <- tmp$yprDFmsy
  obj$yprFmsy       <- sum(tmp$yprLFmsy)
  obj$ssbprFmsy     <- tmp$ssbprFmsy
  obj$landedFmsy    <- tmp$landedFmsy
  obj$yieldFmsy     <- tmp$landedFmsy
  obj$discardedFmsy <- tmp$discardedFmsy
  obj$ssbFmsy       <- tmp$ssbFmsy
  obj$recruitsFmsy  <- tmp$recruitsFmsy

  # F40%
  tmp               <- .getF40( obj )
  obj$F40           <- tmp$F40
  obj$yprLF40       <- tmp$yprLF40
  obj$yprDF40       <- tmp$yprDF40
  obj$yprF40        <- sum(tmp$yprLF40)
  obj$ssbprF40      <- tmp$ssbprF40
  obj$landedF40     <- tmp$landedF40
  obj$yieldF40      <- tmp$landedF40
  obj$discardedF40  <- tmp$discardedF40
  obj$ssbF40        <- tmp$ssbF40
  obj$recruitsF40   <- tmp$recruitsF40

  # Fmax
  tmp               <- .getFmax( obj )
  obj$Fmax          <- tmp$Fmax
  obj$yprLFmax      <- tmp$yprLFmax
  obj$yprDFmax      <- tmp$yprDFmax
  obj$yprFmax       <- sum(tmp$yprLFmax)
  obj$ssbprFmax     <- tmp$ssbprFmax
  obj$landedFmax    <- tmp$landedFmax
  obj$yieldFmax     <- tmp$landedFmax
  obj$discardedFmax <- tmp$discardedFmax
  obj$ssbFmax       <- tmp$ssbFmax
  obj$recruitsFmax  <- tmp$recruitsFmax

  # Fcrash
  tmp               <- .getFcra( obj )
  obj$Fcra          <- tmp$Fcra
  obj$yprLFcra      <- tmp$yprLFcra
  obj$yprDFcra      <- tmp$yprDFcra
  obj$yprFcra       <- sum(tmp$yprLFcra)
  obj$ssbprFcra     <- tmp$ssbprFcra
  obj$landedFcra    <- tmp$landedFcra
  obj$yieldFcra     <- tmp$landedFcra
  obj$discardedFcra <- tmp$discardedFcra
  obj$ssbFcra       <- tmp$ssbFcra
  obj$recruitsFcra  <- tmp$recruitsFcra

  obj
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

  Salg <- array( data=NA, dim=c(A,nGrps,nGear) )
  for( g in 1:nGear )
  {
    tmp1 <- exp( (-1.)*log(19)*(Lal-L50Cg1[g])/(L95Cg1[g] - L50Cg1[g]) )
    tmp2 <- exp( (-1.)*log(19)*(Lal-L50Cg2[g])/(L95Cg2[g] - L50Cg2[g]) )
    tmpS <- (1./(1.+tmp1))*(1./(1.+tmp2))
    Salg[,,g] <- tmpS/max( tmpS )
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
    tmp <- exp( (-1.)*log(19)*(Lal-L50Dg[g])/(L95Dg[g] - L50Dg[g]) )
    tmpP <- (1./(1.+ tmp))
    tmpP[ Lal < palgPars$sizeLim ] <- 1.0
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
  if( length(Linf)==1 )
  {
    Lal <- Linf + (L1-Linf)*exp(-vonK*(age-1.))
  }
  else # vector of Linf for length-group
  {
    Lal <- matrix( NA, nrow=A,ncol=length(Linf) )
    for( l in 1:ncol(Lal) )
      Lal[,l] <- Linf[l] + ( L1 - Linf[l] )*exp( -vonK*(age-1.) )
  }
  return( Lal )
}

# .calcWtAge     ( Calculate weight-at-age by growth-group )
# Purpose:       Calculates weight-at-age for multiple growth-groups.
# Parameters:    Lal, length-at-age by group; c1, scalar; c2, power
# Returns:       Wal, matrix of weights-at-age by growth-group.
# Source:        S.P. Cox
.calcWal <- function( Lal, c1=1.e-5,c2=3.0 )
{
  # Weight-length relationship including bias correction
  Wal <- c1*Lal^c2
  Wal
}

# .calcSchedules ( Calculate life history schedules )
# Purpose:       Calculate length-, weight-, and maturity-at-age vectors.
# Parameters:    obj, a list of operating model parameters; nFs, the number of
#                  fishing mortality points over which to compute the functions.
# Returns:       lifeScheds, a list with vectors for each life history schedule.
# Source:        S.P. Cox
.calcSchedules <- function( obj )
{
  # Extract life history parameters for setting up population dynamics model.
  M         <- obj$M
  Linf      <- obj$Linf
  sigmaLinf <- obj$sigmaLinf
  L1        <- obj$L1
  vonK      <- obj$vonK
  c1        <- obj$c1
  c2        <- obj$c2

  A50       <- obj$A50
  A95       <- obj$A95

  salgPars <- list( L50Cg1 = obj$L50Cg1,
                    L95Cg1 = obj$L95Cg1,
                    L95Cg2 = obj$L95Cg2,
                    L50Cg2 = obj$L50Cg2,
                    nGrps  = obj$nGrps,
                    nGear  = obj$nGear
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
  lifeScheds$Linfl <- qnorm( p=probs, mean=Linf, sd=sigmaLinf )
  lifeScheds$Lal   <- .calcLal( Linf=lifeScheds$Linfl, vonK=vonK, L1=L1, A=A )
  lifeScheds$Wal   <- .calcWal( c1=c1,c2=c2,Lal=lifeScheds$Lal )
  lifeScheds$Ma    <- .calcMa( A50=A50,A95=A95,A=A )
  lifeScheds$Salg  <- .calcSalg( salgPars=salgPars, A=A, Lal=lifeScheds$Lal )
  lifeScheds$Palg  <- .calcPalg( palgPars=palgPars, A=A, Lal=lifeScheds$Lal )
  lifeScheds
}

# .calcPerRecruit
# Purpose:     Calculate all equilibrium per-recruit quantities of interest for an
#              input fishing mortality.
# Parameters:  f=scalar input fishing mortality rate; obj=list of all operating
#              model parameters.
# Returns:     a list with equilibrium quantities - (i) spawning stock biomass-per-recruit
#              and (ii) yield-per-recruit (ypr)
# Source:      S.P. Cox
.calcPerRecruit <- function( f=0, obj )
{
  # Compute equilibrium spawning biomass per recruit given f and parameters.

  A <- obj$nAges
  M <- obj$M
  nGrps <- obj$nGrps
  nGear <- obj$nGear

  Ma    <- obj$Ma
  Lal   <- obj$Lal
  Wal   <- obj$Wal

  Salg  <- obj$Salg
  Palg  <- obj$Palg
  dg    <- obj$dg
  Fg    <- f*obj$fg

  # Create survivorship vector.
  Nal <- matrix( NA, nrow=A, ncol=nGrps )
  Zal <- matrix( NA, nrow=A, ncol=nGrps )

  # initialize age-1 growth groups
  # piOne=proportion in each growth-group
  Nal[1,] <- rep( obj$piOne, nGrps )

  # calc total fishing mortality including both
  # landed and discarded by age-/growth-group
  Falg <- array( data=NA, dim=c(A,nGrps,nGear) )

  for( g in 1:nGear )
    Falg[,,g] <- Salg[,,g]*Fg[g]*(dg[g]*Palg[,,g] - Palg[,,g] + 1.)
  # compute Zal by summing M and Fg over gear types
  Zal <- M + apply( Falg, MARGIN=c(1:2), sum )

  # age-2 to age-(A-1)
  for( a in 2:(A-1) ){
    # loop over growth-groups
    for( l in 1:nGrps ){
      Nal[a,l] <- Nal[a-1,l]*exp( -Zal[a-1,l] )
    }
  }
  for( l in 1:nGrps )
    Nal[A,l] <- Nal[A-1,l]*exp(-Zal[A-1,l])/(1.0-exp(-Zal[A,l]))

  # calc ypr for both landed and discarded by age-/growth-group
  Calg <- array( data=NA, dim=c(A,nGrps,nGear) )# landings
  Dalg <- array( data=NA, dim=c(A,nGrps,nGear) )# dead discards
  for( g in 1:nGear ){
    Calg[,,g] <- Nal*Wal*Salg[,,g]*Fg[g]*(1.-Palg[,,g])*(1.-exp(-Zal))/Zal
    Dalg[,,g] <- Nal*Wal*Salg[,,g]*Fg[g]*dg[g]*Palg[,,g]*(1.-exp(-Zal))/Zal
  }
  # gear-specific ypr L=landed, D=discarded
  yprL   <- apply(Calg,MARGIN=3,sum)
  yprD   <- apply(Dalg,MARGIN=3,sum)

  # spawning biomass per recruit.
  ssbpr <- sum( rowSums(Nal*Wal)*Ma )

  # compile return list
  phi <- obj
    phi$ssbpr <- ssbpr
    phi$yprL  <- yprL # these ypr are vectors (gear)
    phi$yprD  <- yprD
    phi$ypr   <- yprL + yprD
  phi
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
.calcEquil <- function( f=0, obj )
{
  # Compute yield and biomass per recruit function values
  tmp <- .calcPerRecruit( f=f,obj=obj )

  # Beverton-Holt sr model parameters
  rec.a <- obj$rec.a
  rec.b <- obj$rec.b

  # Compute equilibrium recruitment, biomass and yield
  recruits <- (rec.a*tmp$ssbpr - 1.0) / (rec.b*tmp$ssbpr)
  ssb      <- recruits * tmp$ssbpr
  landed   <- recruits*sum( tmp$yprL )
  discarded<- recruits*sum( tmp$yprD )

  equil <- list()
    equil$recruits <- recruits
    equil$ssbpr    <- tmp$ssbpr
    equil$ssb      <- ssb
    equil$yprL     <- tmp$yprL
    equil$yprD     <- tmp$yprD
    equil$ypr      <- sum(tmp$yprL + tmp$yprD)
    equil$landed   <- landed
    equil$discarded<- discarded
  equil
}

# .calcRefCurves
# Purpose:     Calculate all equilibrium relationships to fishing mortality.
# Parameters:  obj=list of all operating model parameters; nFs=the number of
#              fishing mortality points over which to compute the functions
# Returns:     a list with vectors of fishing mortality (f) and equilibrium
#              functions (these are later fitted with splines for finding
#              references points via root-finding algorithms)
# Source:      S.P. Cox
.calcRefCurves <- function( obj, nFs=400 )
{
  f <- seq( from=0.0, to=.MAXF*obj$M, length=nFs )

  recruits <- rep( NA, length=nFs )
  ssbpr    <- rep( NA, length=nFs )
  ssb      <- rep( NA, length=nFs )
  ypr      <- rep( NA, length=nFs )
  yprL     <- rep( NA, length=nFs )
  yprD     <- rep( NA, length=nFs )
  landed   <- rep( NA, length=nFs )
  discarded<- rep( NA, length=nFs )

  for( i in 1:length(f) )
  {
    tmp        <- .calcEquil( f=f[i],obj=obj )
    recruits[i]<- tmp$recruits
    ssbpr[i]   <- tmp$ssbpr
    ssb[i]     <- tmp$ssb
    yprL[i]    <- sum( tmp$yprL )
    yprD[i]    <- sum( tmp$yprD )
    ypr[i]     <- yprL[i]
    landed[i]  <- tmp$landed
    discarded[i]<-tmp$discarded
  }

  refCurves <- obj
    refCurves$F        <- f
    refCurves$ssbpr    <- ssbpr
    refCurves$ssb      <- ssb
    refCurves$recruits <- recruits
    refCurves$ypr      <- ypr
    refCurves$yprL     <- yprL
    refCurves$yprD     <- yprD
    refCurves$yield    <- landed
    refCurves$landed   <- landed
    refCurves$discarded<- discarded
  refCurves
}


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

  tmp             <- .calcEquil( f=obj$F01, obj=obj  )
  obj$yprLF01     <- tmp$yprL
  obj$yprDF01     <- tmp$yprD
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
  F <- as.vector(obj$F)
  L <- as.vector(obj$landed)
  tmp1 <- data.frame( cbind(F=F,L=L) )
  tmp <- subset( tmp1, L>0,select=c(F,L) )

  maxF <- max( tmp$F )
  fySplineFun <- splinefun( x=tmp$F,y=tmp$L )
  Fmsy <- uniroot( f=fySplineFun,interval=c(0,maxF), deriv=1 )$root
  obj$Fmsy <- min( Fmsy, maxF )

  tmp              <- .calcEquil( f=obj$Fmsy, obj=obj )
  obj$yprLFmsy     <- tmp$yprL
  obj$yprDFmsy     <- tmp$yprD
  obj$ssbprFmsy    <- tmp$ssbpr
  obj$landedFmsy   <- tmp$landed
  obj$discardedFmsy<- tmp$discarded
  obj$ssbFmsy      <- tmp$ssb
  obj$recruitsFmsy <- tmp$recruits
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

  tmp             <- .calcEquil( f=obj$F40, obj=obj )
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

  tmp              <- .calcEquil( f=obj$Fmax, obj=obj )
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

  tmp              <- .calcEquil( f=obj$Fcra, obj=obj )
  obj$yprLFcra     <- tmp$yprL
  obj$yprDFcra     <- tmp$yprD
  obj$ssbprFcra    <- tmp$ssbpr
  obj$landedFcra   <- tmp$landed
  obj$discardedFcra<- tmp$discarded
  obj$ssbFcra      <- tmp$ssb
  obj$recruitsFcra <- tmp$recruits
  obj
}
