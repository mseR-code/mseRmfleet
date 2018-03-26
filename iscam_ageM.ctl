## CTL file written by herringM sim-est procedure## _______________________________________________________________________________________
##                         GENERIC HERRING CONTROLS
<<<<<<< HEAD
##                            written: Mon Mar 26 13:09:16 2018 
=======
##                            written: Mon Mar 26 12:57:36 2018 
>>>>>>> ebc93314a8670f39933a68736c018d525cda50f9
## ___________________________CONTROLS FOR ESTIMATED PARAMETERS__________________________
##   Prior descriptions:
##                    -0 uniform (0,0)
##                    -1 normal (p1=mu,p2=sig)
##                    -2 lognormal (p1=log(mu),p2=sig)
##                    -3 beta (p1=alpha,p2=beta)
##                    -4 gamma(p1=alpha,p2=beta)
## ______________________________________________________________________________________
7  # npar 
##    ival  lb   ub  phz  prior  p1  p2  parameter name 
## ______________________________________________________________________________________
7.028008 -5 15 4 0 -5 15  # log_r0 
0.732133 0.2 1 4 3 10 4.92  # steepness 
-0.5425262 -5 5 3 1 -0.79 0.4  # log.m 
7.028008 -5 15 1 0 -5 15  # log_avgrec 
7.028008 -5 15 1 0 -5 15  # log_recinit 
0.3043478 0.001 0.999 3 3 17.08696 39.0559  # rho 
0.8695652 0.01 5 3 4 25 28.75  # kappa (precision) 
## ______________________________________________________________________________________
## ____________________________________________________________
## _________________________SELECTIVITY PARAMETERS_______________________________________
##  OPTIONS FOR SELECTIVITY:
##              1) logistic selectivity parameters
##              2) selectivity coefficients
##              3) a constant cubic spline with age-nodes
##              4) a time varying cubic spline with age-nodes
##              5) a time varying bicubic spline with age & year nodes
##              6) fixed logistic (set isel_type=6, and estimation phase to -1)
##              7) logistic function of body weight.
##              sig=0.05 0.10 0.15 0.20 0.30 0.40 0.50
##              wt =200. 50.0 22.2 12.5 5.56 3.12 2.00
## Gear 1:3 fishery:       Gear 4-5 survey
## isel_typ 
1 1 1 6 6 
## Age at 50% selectivity (logistic) 
3 3 4 2.055 2.055 
## STD at 50% selectivity (logistic) 
0.25 0.25 0.25 0.05 0.05 
## No. of age nodes for each gear (0 to ignore). 
5 5 5 0 0 
## No. of year nodes for each gear (0 to ignore).
12 3 10 0 0 
## Estimation phase
2 2 2 -1 -1 
## Penalty weight for 2nd differences w=1/(2*sig^2)
125 125 12.5 12.5 12.5 
## Penalty weight for dome-shaped selectivity 1=1/(2*sig^2)
50 50 200 200 200 
##_________________________________________##
##_________________________________________##
##           Priors for Survey q           ##
##_________________________________________##
## nits  #number of surveys
2 
## priors 0=uniform density   1=normal density
0 1 
## prior log(mean)
0 0 
## prior sd
1 0.01 
##______________________________________________________________________________##
##_______OTHER MISCELLANEOUS CONTROL_______##
0   ## 1 verbose ADMB output (0=off, 1=on) 
1   ## 2 recruitment model (1=beverton-holt, 2=ricker) 
0.1   ## 3 std in observed catches in first phase 
0.0707   ## 4 std in observed catches in last phase 
0           ## 5 Assume unfished in first year (0=FALSE, 1=TRUE) 
0.02           ## 6 Minimum proportion to consider in age-proportions for dmvlogistic 
0.2    ## 7 Mean fishing mortality for regularizing the estimates of Ft 
0.01   ## 8 std in mean fishing mortality in first phase 
2   ## 9 std in mean fishing mortality in last phase 
3   ## 10 phase for estimating m_deviations (use -1 to turn off mdevs) 
0.1   ## 11 std in deviations for natural mortality 
12   ## 12 number of estimated nodes for deviations in natural mortality 
1   ## 13 fraction of total mortality that takes place prior to spawning 
1   ## 14 switch for age-composition likelihood (1=dmvlogistic,2=dmultinom) 
2   ## 15 switch for selectivity renormalisation factor (0=mean,1=max,2=none) 
##_________________________________________##
# mDevPhz 
3 
# mAges 
10 
##operational control points for harvest control rule
0.25     ##cutoff_fraction prop of prefishery biomass to unfished biomass used to calculate cutoff  
0.2     ##target_hr 
#fixed_cutoff (tonnes)
21.2 
## eofc
999 
