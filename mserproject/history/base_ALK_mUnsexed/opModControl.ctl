## mseR2010 Operating model control settings written Thu Apr 10 16:12:39 2014 
# verbose
0
## this was working but now broken!
# useBaranov
0
# baranovIter
 10 
# baranovSteps
 0.1 0.1 0.2 0.4 0.4
# objFunc_scale
1.
# objFunc_scale_last 
1.
# objFunc_scale_sd
1.
# sizeLim
55
# sizeLimYear
1
# dM
 0.16 0.35 1.6 0 0 
# Linf 
68 72 
# vonK
#0.504 0.390
0.29 0.25
#0.39 0.35
# sigmaL
0.12 0.12

# L1
32.5 32.5
# wt_a
1.04e-5 1.04e-5
# wt_b
3.08 3.06

## Maturity - test 2.63 and 3.14
# aMat50
5
# aMat95
12

## Estimation phases 
# ph_log_avgR
-1 
# ph_logit_h
1
# lb_logit_h
-5.
# ub_logit_h
5.
# prior_h
40 20 
# ph_log_SSB0
1
# lb_log_SSB0
2.5
# ub_log_SSB0
5
# ph_log_M
2
# ph_log_q
1
# phRecDevs
3
# firstRecDev
10
# lastRecDev
52
## Selectivity options - time-varying for trawl
## Selectivity form: 1=asym, 2=norm, 3=gamma
# selType
 2 2 3 1 1

## alpha param is: normal mode for selType(2)
## alpha param is: L50 for selType(1)
# ph_log_alpha_g
4 4 4 4 4
#-4 -4 -4 -4 -4
# lb_log_alpha_g
1.0 1.0 -10 1.6 1.6
# ub_log_alpha_g
4.5 4.5 4.5 4.5 4.5

## beta param is: normal sd for selType(1)
## beta param is: L95_step for selType(2)

# ph_log_beta_g
5 5 5 5 5
#-5 -5 -5 -5 -5
# lb_log_beta_g
0 0 0 0 0
# ub_log_beta_g
5.0 3.0 4.3 4.0 4.0


# useHighgrading
0 0 0

# ph_log_hg50
-7

## Type of age comp likelihood: 1=MVL, 2=MF
# ageLikeType
1
## Prior distributions
## Process error in recruitment
# sigma_R
1.0
## Selectivity parameters
# sigma_alpha
0.1 0.1 0.2 0.001 0.001
# sigma_beta
0.1 0.1 0.5 0.001 0.001
# tauRelLambda
1 1 1
# tauIndexLambda
1 1 1
# tauAgeLambda
1 1 1 1


# Natural mortality
# prior_mean_M
0.1 0.1
# prior_sd_M
0.01 0.01
# priorSD_F
0.3
# catError
0.01

## Fixed recruitment deviations
# nFixed
2
# tFixedRecDev
53 54
# logFixedRecDev
0. 0.

# fracYrFleet
.5 .5 .5 .75 .75

# Check
999