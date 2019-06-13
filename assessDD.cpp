// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>
// assessDD.cpp
// 
// A multi-survey Schnute-Deriso state-space Delay-Difference model.
// 
// Author: Samuel D. N. Johnson
// Initial Date: Sep 27, 2018
// 
// Last revised: Oct 8, 2018
// 
// Purpose: As an assessment model for a multigear fishery in mseR_mfleet. 
// 
// Features/Options:
//    - Pope's approximation for F
//    - Multiple surveys
// 
// Notes on variables:
//    - Arrays/vectors are named as X_ijk, where X is the array name and ijk are 
//      the indices of each dimension (row,column,slice,...)
//    - Most arrays are named as a single capital letter (B,N,I etc.) but in
//      some cases there may be a letter followed by an accent (Chat, wbar) or
//      if two words/abbreviations are concatenated, then camelCase is used
//      (e.g. initBioCode_s, lnM_s)
//    - Occasionally the camelCase convention is broken, when the variable in question 
//      is lower case by convention, e.g. lnq_os vs lnB0_s
//    - Greek letter tau is reserved for observation error/model variances,
//      while sigma is reserved for process error/model variances
// 
// Possible additional features:
//    - (auto-)correlated omega_st series
//
// 
// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>

#include <TMB.hpp>                                // Links in the TMB libraries
#include <iostream>

// invLogit
template<class Type>
Type invLogit(Type x, Type scale, Type trans)
{
  return scale/(Type(1.0) + exp(-Type(1.0)*x)) + trans;
}

// invLogit
template<class Type>
Type square(Type x )
{
  return pow(x,2);
}

// posfun
template<class Type>
Type posfun(Type x, Type eps, Type &pen)
{
  pen += CppAD::CondExpLt(x, eps, Type(0.01) * pow(x-eps,2), Type(0));
  return CppAD::CondExpGe(x, eps, x, eps/(Type(2)-x/eps));
}


// objective function
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Call namespaces //
  using namespace density;

  /*data section*/
  // Data Structures
  DATA_ARRAY(I_tg);             // Observation indices
  DATA_VECTOR(C_t);             // Catch data
  DATA_VECTOR(wbar_t);          // average weight observations
  
  
  // Model dimensions
  int nG = I_tg.dim(1);          // No. of surveys g
  int nT = I_tg.dim(0);          // No. of time steps t

  // Growth model fixed pars
  DATA_INTEGER(kage);           // age at recruitment
  DATA_SCALAR(alpha);           // FW alpha
  DATA_SCALAR(rho);             // FW rho
  DATA_SCALAR(wk);              // weight at age k (is this wbar again? - no, this is for recruitment)

  // Switches
  DATA_INTEGER(initBioCode);    // initialise at unfished (0) or fished eqbm (1)
  DATA_INTEGER(useWbar);        // Use average weight obs
  DATA_IVECTOR(survType_g);     // Type of index (0 = vuln bio, 1 = vuln numbers)
  DATA_IVECTOR(indexType_g);    // Type of survey (0 = relative, 1 = absolute)

  // Biomass positive value penalty multiplier
  DATA_SCALAR(pospenScale);     // Scalar multiple of pospen added to obj fun
  DATA_INTEGER(firstRecDev);    // First year of recruitment deviations to be estimated

  /* parameter section */
  // Biological parameters //
  PARAMETER(logith);     // logit steepness
  PARAMETER(lnB0);       // log-scale unfished biomass
  PARAMETER(lnM);        // log-scale natural mortality rate

  // Obs error Sd
  PARAMETER_VECTOR(lntauObs_g);   // Observation error SD
  PARAMETER_VECTOR(lnq_g);        // Observation model catchability scalar
  
  // Random Effects //
  // Recruitment
  PARAMETER_VECTOR(recDevs_t);  // Recruitment process errors (t = 2:nT)
  PARAMETER(lnsigmaR);          // Recruitment proc error sd
  PARAMETER(gammaR);            // Recruitment proc error auto-corr

  // Fishing Mortality
  PARAMETER(lnFinit);           // initialisation fishing mortality rate

  // Prior distributions //
  // process error variance prior
  PARAMETER_VECTOR(sig2RPrior); // IG prior on recruitment dev variance

  // Update the following priors
  // to be single level
  // M prior
  PARAMETER(mM);                  // Average M prior mean
  PARAMETER(sdM);                 // Average M prior sd
  // h prior
  PARAMETER_VECTOR(hPrior);       // Steepness prior pars
  // q prior
  PARAMETER_VECTOR(mq);           // Average q prior mean
  PARAMETER_VECTOR(sdq);          // Average q prior sd

  // Transform pars //
  Type ySteep   = invLogit(logith, Type(1.), Type(0.));
  Type h        = invLogit(logith, Type(1./1.3), Type(.3/1.3));
  Type B0       = exp(lnB0);
  Type M        = exp(lnM);
  Type sigmaR   = exp(lnsigmaR);
  Type Finit    = exp(lnFinit);

  vector<Type>  tauObs_g(nG);     // obs error SD
  vector<Type>  q_g(nG);          // conditional MLE of Catchability (survey, species)
                
  tauObs_g  = exp(lntauObs_g);
  q_g       = exp(lnq_g);
  
  // Define derived variables //
  // Eqbm values
  Type  S0;             // unfished survival rate   
  Type  wbar0;          // unfished average weight
  Type  N0;             // unfished numbers
  Type  R0;             // unfished recruitment
  
  // BH Recruitment pars
  Type  reca;           // BH a parameter
  Type  recb;           // BH b parameter
  
  // State variables
  vector<Type>   B_t(nT+1);        // biomass state variable
  vector<Type>   N_t(nT+1);        // Numbers state variable
  vector<Type>   R_t(nT+1);        // Recruitment
  vector<Type>   wbarhat_t(nT+1);  // estimated mean weight
  vector<Type>   S_t(nT);          // Survival
  vector<Type>   F_t(nT);          // Fishing mortality rate
  vector<Type>   Z_t(nT);          // Total mortality
  vector<Type>   D_t(nT);          // Depletion
  vector<Type>   U_t(nT);          // Exploitation rate
  vector<Type>   omegaR_t(nT+1);   // Recruitment deviations
  vector<Type>   numC_t(nT);       // Catch in numbers
  
  // objective function contributions
  Type          objFun    = 0.0;   // Objective function value
  Type          nllProc   = 0.0;   // Process error likelihood
  Type          nllWbar   = 0.0;   // observed average weight nll
  Type          nlpM      = 0.0;   // M joint neg log prior density
  Type          nlph      = 0.0;   // h joint neg log prior density
  Type          nlpq      = 0.0;   // q joint neg log prior density
  vector<Type>  nllObs_g(nG);      // Observation error likelihood by gear
  vector<Type>  nlptau2_g(nG);     // Observation error var prior

  // Observation model quantities
  vector<Type>   ss_g(nG);         // Sum of squared residuals
  vector<Type>   validObs_g(nG);   // # of valid observations
  vector<Type>   zSum_g(nG);       // sum of residuals
  vector<Type>   SSR_g(nG);        // sum of squared residuals
  array<Type>    z_tg(nT,nG);      // yearly resid by gear
  

  // Avg weight observations
  Type    wSSR    = 0.0;
  int     nObsW   = 0;
  Type    tauWbar = 0.0;

  // Biomass positive penalty for Pope's approx
  Type    pospen  = 0.0;
  
  /* Estimation Procedure */
  // First, calculate recruitment and equilibrium parameters
  S0     = exp( -M );
  wbar0  = (alpha * S0 + wk * (Type(1) - S0)) / (Type(1) - rho * S0);
  N0     = B0 / wbar0;
  R0     = N0 * (Type(1) - S0);
  reca   = Type(4) * h * R0 / ( B0 * ( Type(1) - h ) );
  recb   = ( Type(5) * h - Type(1) ) / ( B0 * ( Type(1) -h ) );


  // Fill omegaR_t with 0s
  omegaR_t.fill(0.0);
  // Now starting from firstRecDev + kage
  for(int t = firstRecDev + kage ; t < nT; t++ )
    omegaR_t(t) = gammaR * omegaR_t(t-1) + (1 - gammaR) * recDevs_t(t - firstRecDev - kage );


  // Population Dynamics (process model) //
  // Initialise population
  if( initBioCode == 0 )
  {
    Z_t(0) = M;
    S_t(0) = exp( - Z_t(0) );
    // Set all initial state variables to unfished eqbm values
    wbarhat_t(0) = wbar0;
    B_t(0) = B0;
    N_t(0) = N0;
    R_t(0) = R0;
  } else {
    // Compute total mortality and initial survivorship
    // from Finit (borrow Zt and St vectors for now)
    Z_t(0) = M + Finit;
    S_t(0) = exp( - Z_t(0) );
    // Solve for fished equilibrium assuming 
    // initial F value is long-term eq value
    // Average weight
    wbarhat_t(0) = ( S_t(0) * alpha + wk*(1. - S_t(0) ) );
    wbarhat_t(0) /= 1. - rho*S_t(0);
    // Biomass
    B_t(0) = (S_t(0)*(alpha + rho*wbarhat_t(0)) + wbarhat_t(0)*(reca*wk - 1));
    B_t(0) /= recb * ( wbarhat_t(0) - rho*S_t(0)*wbarhat_t(0) - alpha * S_t(0));
    // Numbers
    N_t(0) = B_t(0) / wbarhat_t(0);
    // Recruitment
    R_t(0) = reca * B_t(0) / (1. + recb * B_t(0));
  }

  // Calculate depletion and exploitation rate
  D_t(0) = B_t(0) / B0;
  U_t(0) = C_t(0) / B_t(0);

  // Now loop over time for the population dynamics
  for( int t = 1; t <= nT; t++ )
  {
    // Now update the F estimate based on Pope's
    // approximation - will have to change the pop
    // dynamics below
    // First, save the last year's Nt to a temporary variable
    Type tmpN_t = N_t(t-1);

    // Now convert catch to catch in numbers
    // by dividing by the average weight
    numC_t(t-1) = C_t(t-1) / wbarhat_t(t-1);

    // Now reduce tmpN_t by half of mortality and the catch
    tmpN_t *= exp(-M/2);
    tmpN_t -= numC_t(t-1);

    // Apply posfun to avoid negative numbers and apply
    // remaining mortality
    tmpN_t = posfun( Type(tmpN_t), Type(1e-4), pospen );
    tmpN_t *= exp(-M/2);

    // Use tmpN_t to estimate F
    F_t(t-1) = log(N_t(t-1)/tmpN_t) - M;

    // Total mortality and survivorship
    Z_t(t-1) = M + F_t( t-1 );
    S_t(t-1) = exp( - Z_t( t-1 ) );

    // Recruitment
    // Compute expected recruitment
    if( t < int(kage) ) 
      R_t(t) = R_t(0);
    else
      R_t(t) = reca * B_t(t-kage) / (1. + recb * B_t(t-kage));
    // Now apply process errors
    R_t(t) *= exp( omegaR_t(t) );

    // Update biomass and numbers
    N_t(t) = tmpN_t + R_t(t);
    // There's a modeling choice to be made here:
    // do we use tmpN_t and remove the survival from 
    // the first term in the following eqn, or
    // keep it as survival using the pope's approx?
    // Needs testing!!
    B_t(t) = alpha*tmpN_t + rho * wbarhat_t(t-1) * tmpN_t + wk*R_t(t);
    // B_t(t) = S_t(t-1) * (alpha * N_t(t-1) + rho * B_t(t-1) ) + wk * R_t(t);

    // Compute average weight
    wbarhat_t(t) = B_t(t) / N_t(t);

    // Calculate depletion and exploitation rate
    if( t < nT )
    {
      D_t(t) = B_t(t) / B0;
      U_t(t) = C_t(t) / B_t(t); 
    }
  }

  // Observation Model //
  // Calculate observation model likelihood, including catch
  // Fill with 0s
  validObs_g.fill(0.0);
  ss_g.fill(0.0);
  z_tg.fill(0.0);
  SSR_g.fill(0.0);
  vector<Type> idxState(nT+1);

  // Loop over surveys
  for( int g = 0; g < nG; g++ )
  {
    // Select between numbers or biomass
    // for survey
    if( survType_g(g) == 0 )
      idxState = B_t;
    if( survType_g(g) == 1 )
      idxState = N_t;

    // years/timesteps
    for( int t = 0; t < nT; t++ )
      // only add a contribution if the data exists (It < 0 is missing)
      if (I_tg(t,g) > 0) 
        z_tg(t,g) = log( I_tg( t, g ) ) - log( idxState( t ) ) - lnq_g(g);
    
    
    // Add contribution to nllObs
    // nllObs_g(g) += SSR_g(g)/0.01;
    vector<Type> tmpVec = z_tg.col(g);
    nllObs_g(g) -= dnorm( tmpVec, Type(0), tauObs_g(g), true).sum();

  }

  // Now add mean weight observations
  for( int t = 0; t < nT; t++ )
  {
    // As with indices, only add mean weight contributions 
    // if that data is provided
    if( wbar_t(t) > 0 & useWbar == 1 )
    {
      wSSR += square( wbarhat_t(t) - wbar_t(t) );
      nObsW += int(1);
    }
  }
  if( useWbar == 1)
  {
    tauWbar =  wSSR / nObsW;
    nllWbar += 0.5*(nObsW * log(tauWbar) + nObsW);
  }

  // Add observation model likelihood to objective function
  objFun += nllObs_g.sum() + nllWbar;

  // Priors //
  // Calculate recruitment (process error) prior - correlated later??
  // Process errors
  int nRecDevs = recDevs_t.size();
  for( int t = 0; t < nRecDevs; t++ )
    nllProc +=  lnsigmaR + Type(0.5) * square( recDevs_t(t)/sigmaR );

  // Regularise the mean recruitment residual
  nllProc += .5*square(omegaR_t.sum()/nT/0.05);

  // proc err variance IG prior
  nllProc += (sig2RPrior(0)+Type(1))*2*lnsigmaR + sig2RPrior(1)/square(sigmaR);

  objFun += nllProc;

  // Now leading parameter prior
  // First, M prior
  nlpM += .5 * square( (M - mM ) / sdM );

  // Now do a beta prior on steepness
  // Steepness prior pars passed in as a 2ple
  // of mean and sd, use moment matching to convert
  // to beta parameters. There's some scaling here
  // I don't fully understand, requires some thought
  // Type muB            = 1.3*hPrior(0)-0.3;
  // Type tauB           = muB*(1.-muB)/( 1.5625*pow(hPrior(1),2) )-1.; 
  // Type aB             = tauB*muB; 
  // Type bB             = tauB*(1.-muB); 
  // Type steepness_nlp  = ( (1. - aB)*log(ySteep) + (1. - bB)*log(1.-ySteep) );    
  // nlph               += steepness_nlp;
  nlph += (1 - hPrior(0)) * log(h) + (1 - hPrior(1)) * log(1-h);

  // Finally, q prior
  for( int g = 0; g < nG; g++ )
    nlpq += .5* square( ( q_g(g) - mq(g) ) / sdq(g) );  


  // // Apply Jeffreys prior to B0
  Type nlpB0 = 0.;
  
  // Add priors to objective function
  objFun += nlpq + nlph + nlpM + nlptau2_g.sum() + nlpB0;

  // Add biomass positive penalty
  objFun += pospenScale*pospen;
  
  // Reference points?? //
  // Might be able to code a version of refPts.R  
  // in C++ to call here

  // Convert numbers and biomass
  // to log scale for reporting with SEs
  vector<Type> lnB_t = log(B_t);
  vector<Type> lnN_t = log(N_t);

  /* Reporting Section */

  // First, ADREPORT all variables that we want
  // SDs for, on their natural scale
  ADREPORT(lnB0);
  ADREPORT(lnM);
  ADREPORT(logith);
  ADREPORT(lnFinit);
  ADREPORT(lnq_g);
  ADREPORT(tauObs_g);
  ADREPORT(lnN_t);
  ADREPORT(lnB_t);


  // Now, all model quantities //
  // Data
  REPORT( I_tg );           // Survey indices
  REPORT( C_t );            // Observed Catch
  REPORT( wbar_t );         // Observed mean weight
  
  // Model dimensions
  REPORT( nG );             // No. of observational surveys g
  REPORT( nT );             // No. of time steps t
  
  // Growth model fixed pars
  REPORT( kage );           // age at recruitment
  REPORT( alpha );          // FW alpha
  REPORT( rho );            // FW rho
  REPORT( wk );             // weight at age of recruitment
  
  // Switches
  REPORT( initBioCode );  // initialise at unfished (0) or fished eqbm (1)

    
  // Biological parameters
  REPORT( h );            // logit steepness
  REPORT( B0 );           // log-scale unfished biomass
  REPORT( M );            // log-scale natural mortality rate

  // Eqbm values
  REPORT( S0 );           // unfished survival rate   
  REPORT( wbar0 );        // unfished average weight
  REPORT( N0 );           // unfished numbers
  REPORT( R0 );           // unfished recruitment
  
  // BH Recruitment pars
  REPORT( reca );         // BH a parameter
  REPORT( recb );         // BH b parameter
  
  // State variables
  REPORT( B_t );          // biomass state variable
  REPORT( N_t );          // Numbers state variable
  REPORT( R_t );          // Recruitment
  REPORT( S_t );          // Survival
  REPORT( numC_t);        // catch in numbers
  REPORT( wbarhat_t );    // estimated mean weight
  REPORT( F_t );          // Fishing mortality
  REPORT( Z_t );          // Total mortality
  REPORT( D_t );          // Depletion
  REPORT( U_t );          // Exploitation rate
  REPORT( Finit );        // CR estimates of initial total mortality
  
  // Observation model
  REPORT( q_g);           // Survey catchability
  REPORT( tauObs_g );     // Survey observation error
  REPORT( tauWbar );      // Mean weight observation error sd
  REPORT( validObs_g );   // Number of valid observations in each survey
  REPORT( SSR_g );        // Sum of squared resids
  REPORT( z_tg );         // matrix of residuals

  // Random Effects
  REPORT( recDevs_t);     // Recruitment deviations
  REPORT( omegaR_t );     // Recruitment process errors
  REPORT( sigmaR );       // Recruitment proc error sd
  REPORT( gammaR );       // Recruitment proc error auto corr

  // Multilevel M prior
  REPORT( mM );           // Average M prior mean
  REPORT( sdM );          // Average M prior sd

  // Multilevel h prior
  REPORT( hPrior );       // Average steepness beta prior shape1
  
  // Multilevel q prior
  REPORT( mq );           // Average q prior mean
  REPORT( sdq );          // Average q prior sd

  // Switches
  REPORT(survType_g);     // Survey type

  // Objective Function quantities
  REPORT( objFun );
  REPORT( nllObs_g );
  REPORT( nllProc );
  REPORT( nlph );
  REPORT( nlpq );
  REPORT( nlpM );
  REPORT( nlpB0 );
  // REPORT( nlpRt );
  REPORT( pospen );

  
  return objFun;
}
