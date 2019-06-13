// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>
// refPtsDD.cpp
// 
// Computes DD model reference points quickly
// 
// Author: Samuel D. N. Johnson
// Initial Date: June 13, 2019
// 
//
// ><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><>><><>><><>><><>><>

#include <TMB.hpp>                                // Links in the TMB libraries
#include <iostream>


// objective function
template<class Type>
Type objective_function<Type>::operator() ()
{
  // Call namespaces //
  using namespace density;

  /*data section*/
  // Data Structures
  DATA_SCALAR(rec_a);
  DATA_SCALAR(rec_b);
  DATA_SCALAR(M);
  DATA_SCALAR(alpha);
  DATA_SCALAR(rho);
  DATA_SCALAR(w_k);

  /* parameter section */
  PARAMETER(Fmsy);

  Type objFun   = 0;
  Type Z        = 0;
  Type S        = 0;
  Type wbarhat  = 0;
  Type Be       = 0;
  Type MSY      = 0;
  Type Bmsy     = 0;


  // Now Compute equilibirium dynamics
  Z = M + Fmsy;
  S = exp( -Z );
  
  // Average weight
  wbarhat = ( S * alpha + w_k*(1. - S ) );
  wbarhat /= 1. - rho*S;
  // Biomass
  Be     = (S*(alpha + rho*wbarhat) + wbarhat*(rec_a*w_k - 1));
  Be    /= rec_b * ( wbarhat - rho*S*wbarhat - alpha * S);
  

  MSY     = Fmsy * (1 - S) * Be / Z;
  objFun -= log(MSY);
  Bmsy    = Be;

  REPORT( MSY );
  REPORT( Bmsy );
  REPORT( Fmsy );

  return(objFun);
}
