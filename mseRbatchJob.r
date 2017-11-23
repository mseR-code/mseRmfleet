## mseR batch file written: Fri Nov 26 14:27:14 2010 
#
parameter value
#
#--------------------------------------------------------------------# 
# Scenarios for reference set:
#
# S1 - Res. rel. mort. rates, M est., K=0.3             - ResMort_Mest_K3
# S2 - Res. rel. mort. rates, M=0.08, K=0.3             - ResMort_M08_K3
#
# S3 - Res. rel. mort. rates, M=est., K=0.25            - ResMort_Mest_K25
# S4 - Res. rel. mort. rates. M=0.08, K=0.25            - ResMort_M08_K25
#
# S5 - Res. rel. mort. rates. M=est., K=0.3, gammaR=0.4 - ResMort_Mest_K3_Gam4
# S6 - Res. rel. mort. rates. M=0.08, K=0.3, gammaR=0.4 - ResMort_M08_K3_Gam4
#
# To be done if time permits:
#
# S7 - Res. rel. mort. rates. M est, K=0.3, Posterior mean - ResMort_Mest_meanMCMC
# S8 - Res. rel. mort. rates. M fix, K=0.3, 10-th Percentile of Posterior MSY - ResMort_M08_K3_MCMCQ40
#
# Added 10-Jan-11:
#
# S9 - Baseline_Mest_HiMort
#
# allocProp: c(0.417027,0.475627,0.099525,0.002995,0.004826)
#--------------------------------------------------------------------# 
#
# Scenario  S1-ResMort_Mest_K3 
#
scenario$scenario1$scenarioName “S1-ResMort_Mest_K3”
scenario$scenario1$pars$tMP 47
scenario$scenario1$pars$nT 83
scenario$scenario1$pars$nReps 100
scenario$scenario1$pars$nGear 5
scenario$scenario1$pars$nAges 35
scenario$scenario1$pars$nGrps 12
scenario$scenario1$pars$B0 114.774
scenario$scenario1$pars$rSteepness 0.875183
scenario$scenario1$pars$gammaR 0.0
scenario$scenario1$pars$sigmaR 0.6
scenario$scenario1$pars$A50 5
scenario$scenario1$pars$A95 8
scenario$scenario1$pars$M 0.057089
scenario$scenario1$pars$c1 8.4835e-06
scenario$scenario1$pars$c2 3.05118
scenario$scenario1$pars$Linf 71.0
scenario$scenario1$pars$L1 32.5
scenario$scenario1$pars$vonK 0.30
scenario$scenario1$pars$sigmaLinf 7.8
scenario$scenario1$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario1$pars$powerg rep(1,5)
scenario$scenario1$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario1$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario1$pars$sizeLim c(55,55,55,20,20)
scenario$scenario1$pars$selType c(1,1,2,1,1)
scenario$scenario1$pars$L50Cg1 c(58.7974,59.2866,38.9977,51.7101,54.2988)
scenario$scenario1$pars$L95Cg1 c(73.0292,69.8392,39.9977,54.3781,55.2988)
scenario$scenario1$pars$L95Cg2 c(70.7974,73.2866,47.1322,83.7101,86.2988)
scenario$scenario1$pars$L50Cg2 c(73.7974,74.2866,67.2177,90.7101,93.2988)
scenario$scenario1$pars$L95Dg c(56,55,50,20,20)
scenario$scenario1$pars$L50Dg c(58,59,54,21,21)
scenario$scenario1$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario1$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario1$pars$baranovIter 4
scenario$scenario1$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario1$pars$initDepTarg 0.3
scenario$scenario1$pars$rseed 1234
scenario$scenario1$pars$tauIndexg c(0.289508,0.48599,0.160111)
scenario$scenario1$pars$tauAgeg c(2.84149,1.27236,1.38996)
scenario$scenario1$pars$tauReleaseg c(0.521071,0.264597,0.874196,0,0)
scenario$scenario1$pars$qg c(0.558504,0.401029,0.897919)
scenario$scenario1$opModData$catch lisread('opModCatch.dat')
scenario$scenario1$opModData$index lisread('opModIndex.dat')
scenario$scenario1$opModData$ages lisread('opModAges.dat')
scenario$scenario1$opModEst$recDevs lisread('sableOpModmodelDevs06122010102835.dat')
scenario$scenario1$opModEst$qDevs lisread('sableOpModmodelDevs06122010102835.dat')
#
# Scenario  S2-ResMort_M08_K3
#
scenario$scenario2$scenarioName “S2-ResMort_M08_K3”
scenario$scenario2$pars$tMP 47
scenario$scenario2$pars$nT 83
scenario$scenario2$pars$nReps 100
scenario$scenario2$pars$nGear 5
scenario$scenario2$pars$nAges 35
scenario$scenario2$pars$nGrps 12
scenario$scenario2$pars$B0 147.729
scenario$scenario2$pars$rSteepness 0.505651
scenario$scenario2$pars$gammaR 0.0
scenario$scenario2$pars$sigmaR 0.6
scenario$scenario2$pars$A50 5
scenario$scenario2$pars$A95 8
scenario$scenario2$pars$M 0.08
scenario$scenario2$pars$c1 8.4835e-06
scenario$scenario2$pars$c2 3.05118
scenario$scenario2$pars$Linf 71.0
scenario$scenario2$pars$L1 32.5
scenario$scenario2$pars$vonK 0.30
scenario$scenario2$pars$sigmaLinf 7.8
scenario$scenario2$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario2$pars$powerg rep(1,5)
scenario$scenario2$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario2$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario2$pars$sizeLim c(55,55,55,20,20)
scenario$scenario2$pars$selType c(1,1,2,1,1)
scenario$scenario2$pars$L50Cg1 c(58.7748,59.2346,36.599,51.3189,54.2077)
scenario$scenario2$pars$L95Cg1 c(73.127,69.923,37.5991,54.2615,55.2077)
scenario$scenario2$pars$L95Cg2 c(70.7748,73.2346,46.6183,83.3189,86.2077)
scenario$scenario2$pars$L50Cg2 c(73.7748,74.2346,66.7038,90.3189,93.2077)
scenario$scenario2$pars$L95Dg c(56,55,50,20,20)
scenario$scenario2$pars$L50Dg c(58,59,54,21,21)
scenario$scenario2$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario2$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario2$pars$baranovIter 4
scenario$scenario2$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario2$pars$initDepTarg 0.3
scenario$scenario2$pars$rseed 1234
scenario$scenario2$pars$tauIndexg c(0.241585,0.496763,0.156945)
scenario$scenario2$pars$tauAgeg c(2.8836,1.27914,1.39808)
scenario$scenario2$pars$tauReleaseg c(0.332505,0.455038,0.930503,0,0)
scenario$scenario2$pars$qg c(0.366982,0.26669,0.57439)
scenario$scenario2$opModData$catch lisread('opModCatch.dat')
scenario$scenario2$opModData$index lisread('opModIndex.dat')
scenario$scenario2$opModData$ages lisread('opModAges.dat')
scenario$scenario2$opModEst$recDevs lisread('sableOpModmodelDevs06122010111552.dat')
scenario$scenario2$opModEst$qDevs lisread('sableOpModmodelDevs06122010111552.dat')
#
# Scenario  S3-ResMort_Mest_K25 
#
scenario$scenario3$scenarioName “S3-ResMort_Mest_K25”
scenario$scenario3$pars$tMP 47
scenario$scenario3$pars$nT 83
scenario$scenario3$pars$nReps 100
scenario$scenario3$pars$nGear 5
scenario$scenario3$pars$nAges 35
scenario$scenario3$pars$nGrps 12
scenario$scenario3$pars$B0 122.103
scenario$scenario3$pars$rSteepness 0.851291
scenario$scenario3$pars$gammaR 0.0
scenario$scenario3$pars$sigmaR 0.6
scenario$scenario3$pars$A50 5
scenario$scenario3$pars$A95 8
scenario$scenario3$pars$M 0.0596283
scenario$scenario3$pars$c1 8.4835e-06
scenario$scenario3$pars$c2 3.05118
scenario$scenario3$pars$Linf 71.0
scenario$scenario3$pars$L1 32.5
scenario$scenario3$pars$vonK 0.25
scenario$scenario3$pars$sigmaLinf 7.8
scenario$scenario3$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario3$pars$powerg rep(1,5)
scenario$scenario3$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario3$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario3$pars$sizeLim c(55,55,55,20,20)
scenario$scenario3$pars$selType c(1,1,2,1,1)
scenario$scenario3$pars$L50Cg1 c(58.7476,59.3318,37.7146,49.26,52.0384)
scenario$scenario3$pars$L95Cg1 c(73.1246,69.6923,38.7146,53.8313,54.6136)
scenario$scenario3$pars$L95Cg2 c(70.7476,73.3318,46.9546,81.26,84.0384)
scenario$scenario3$pars$L50Cg2 c(73.7476,74.3318,67.0401,88.26,91.0384)
scenario$scenario3$pars$L95Dg c(56,55,50,20,20)
scenario$scenario3$pars$L50Dg c(58,59,54,21,21)
scenario$scenario3$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario3$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario3$pars$baranovIter 4
scenario$scenario3$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario3$pars$initDepTarg 0.3
scenario$scenario3$pars$rseed 1234
scenario$scenario3$pars$tauIndexg c(0.268933,0.495537,0.16358)
scenario$scenario3$pars$tauAgeg c(2.91134,1.27632,1.3974)
scenario$scenario3$pars$tauReleaseg c(0.655596,0.197805,0.802553,0,0)
scenario$scenario3$pars$qg c(0.517652,0.34184,0.738299)
scenario$scenario3$opModData$catch lisread('opModCatch.dat')
scenario$scenario3$opModData$index lisread('opModIndex.dat')
scenario$scenario3$opModData$ages lisread('opModAges.dat')
scenario$scenario3$opModEst$recDevs lisread('sableOpModmodelDevs06122010124337.dat')
scenario$scenario3$opModEst$qDevs lisread('sableOpModmodelDevs06122010124337.dat')
#
# Scenario  S4-ResMort_M08_K25 
#
scenario$scenario4$scenarioName “S4-ResMort_M08_K25”
scenario$scenario4$pars$tMP 47
scenario$scenario4$pars$nT 83
scenario$scenario4$pars$nReps 100
scenario$scenario4$pars$nGear 5
scenario$scenario4$pars$nAges 35
scenario$scenario4$pars$nGrps 12
scenario$scenario4$pars$B0 152.646
scenario$scenario4$pars$rSteepness 0.507453
scenario$scenario4$pars$gammaR 0.0
scenario$scenario4$pars$sigmaR 0.6
scenario$scenario4$pars$A50 5
scenario$scenario4$pars$A95 8
scenario$scenario4$pars$M 0.08
scenario$scenario4$pars$c1 8.4835e-06
scenario$scenario4$pars$c2 3.05118
scenario$scenario4$pars$Linf 71.0
scenario$scenario4$pars$L1 32.5
scenario$scenario4$pars$vonK 0.25
scenario$scenario4$pars$sigmaLinf 7.8
scenario$scenario4$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario4$pars$powerg rep(1,5)
scenario$scenario4$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario4$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario4$pars$sizeLim c(55,55,55,20,20)
scenario$scenario4$pars$selType c(1,1,2,1,1)
scenario$scenario4$pars$L50Cg1 c(58.7393,59.2069,36.5983,49.0401,52.0554)
scenario$scenario4$pars$L95Cg1 c(73.156,70.0179,37.5984,53.3926,54.7634)
scenario$scenario4$pars$L95Cg2 c(70.7393,73.2069,46.5944,81.0401,84.0554)
scenario$scenario4$pars$L50Cg2 c(73.7393,74.2069,66.6799,88.0401,91.0554)
scenario$scenario4$pars$L95Dg c(56,55,50,20,20)
scenario$scenario4$pars$L50Dg c(58,59,54,21,21)
scenario$scenario4$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario4$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario4$pars$baranovIter 4
scenario$scenario4$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario4$pars$initDepTarg 0.3
scenario$scenario4$pars$rseed 1234
scenario$scenario4$pars$tauIndexg c(0.239751,0.502908,0.163075)
scenario$scenario4$pars$tauAgeg c(2.94276,1.28203,1.40332)
scenario$scenario4$pars$tauReleaseg c(0.497891,0.261034,0.829162,0,0)
scenario$scenario4$pars$qg c(0.36947,0.248597,0.52939)
scenario$scenario4$opModData$catch lisread('opModCatch.dat')
scenario$scenario4$opModData$index lisread('opModIndex.dat')
scenario$scenario4$opModData$ages lisread('opModAges.dat')
scenario$scenario4$opModEst$recDevs lisread('sableOpModmodelDevs06122010140712.dat')
scenario$scenario4$opModEst$qDevs lisread('sableOpModmodelDevs06122010140712.dat')
#
# Scenario  S5-ResMort_Mest_K3_GAM4 
#
scenario$scenario5$scenarioName “S5-ResMort_Mest_K3_GAM4”
scenario$scenario5$pars$tMP 47
scenario$scenario5$pars$nT 83
scenario$scenario5$pars$nReps 100
scenario$scenario5$pars$nGear 5
scenario$scenario5$pars$nAges 35
scenario$scenario5$pars$nGrps 12
scenario$scenario5$pars$B0 114.774
scenario$scenario5$pars$rSteepness 0.875183
scenario$scenario5$pars$gammaR 0.4
scenario$scenario5$pars$sigmaR 0.6
scenario$scenario5$pars$A50 5
scenario$scenario5$pars$A95 8
scenario$scenario5$pars$M 0.057089
scenario$scenario5$pars$c1 8.4835e-06
scenario$scenario5$pars$c2 3.05118
scenario$scenario5$pars$Linf 71.0
scenario$scenario5$pars$L1 32.5
scenario$scenario5$pars$vonK 0.30
scenario$scenario5$pars$sigmaLinf 7.8
scenario$scenario5$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario5$pars$powerg rep(1,5)
scenario$scenario5$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario5$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario5$pars$sizeLim c(55,55,55,20,20)
scenario$scenario5$pars$selType c(1,1,2,1,1)
scenario$scenario5$pars$L50Cg1 c(58.7974,59.2866,38.9977,51.7101,54.2988)
scenario$scenario5$pars$L95Cg1 c(73.0292,69.8392,39.9977,54.3781,55.2988)
scenario$scenario5$pars$L95Cg2 c(70.7974,73.2866,47.1322,83.7101,86.2988)
scenario$scenario5$pars$L50Cg2 c(73.7974,74.2866,67.2177,90.7101,93.2988)
scenario$scenario5$pars$L95Dg c(56,55,50,20,20)
scenario$scenario5$pars$L50Dg c(58,59,54,21,21)
scenario$scenario5$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario5$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario5$pars$baranovIter 4
scenario$scenario5$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario5$pars$initDepTarg 0.3
scenario$scenario5$pars$rseed 1234
scenario$scenario5$pars$tauIndexg c(0.289508,0.48599,0.160111)
scenario$scenario5$pars$tauAgeg c(2.84149,1.27236,1.38996)
scenario$scenario5$pars$tauReleaseg c(0.521071,0.264597,0.874196,0,0)
scenario$scenario5$pars$qg c(0.558504,0.401029,0.897919)
scenario$scenario5$opModData$catch lisread('opModCatch.dat')
scenario$scenario5$opModData$index lisread('opModIndex.dat')
scenario$scenario5$opModData$ages lisread('opModAges.dat')
scenario$scenario5$opModEst$recDevs lisread('sableOpModmodelDevs06122010102835.dat')
scenario$scenario5$opModEst$qDevs lisread('sableOpModmodelDevs06122010102835.dat')
#
# Scenario  S6-ResMort_M08_K3_GAM4 
#
scenario$scenario6$scenarioName “S6-ResMort_M08_K3_GAM4”
scenario$scenario6$pars$tMP 47
scenario$scenario6$pars$nT 83
scenario$scenario6$pars$nReps 100
scenario$scenario6$pars$nGear 5
scenario$scenario6$pars$nAges 35
scenario$scenario6$pars$nGrps 12
scenario$scenario6$pars$B0 147.729
scenario$scenario6$pars$rSteepness 0.505651
scenario$scenario6$pars$gammaR 0.4
scenario$scenario6$pars$sigmaR 0.6
scenario$scenario6$pars$A50 5
scenario$scenario6$pars$A95 8
scenario$scenario6$pars$M 0.08
scenario$scenario6$pars$c1 8.4835e-06
scenario$scenario6$pars$c2 3.05118
scenario$scenario6$pars$Linf 71.0
scenario$scenario6$pars$L1 32.5
scenario$scenario6$pars$vonK 0.30
scenario$scenario6$pars$sigmaLinf 7.8
scenario$scenario6$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario6$pars$powerg rep(1,5)
scenario$scenario6$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario6$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario6$pars$sizeLim c(55,55,55,20,20)
scenario$scenario6$pars$selType c(1,1,2,1,1)
scenario$scenario6$pars$L50Cg1 c(58.7748,59.2346,36.599,51.3189,54.2077)
scenario$scenario6$pars$L95Cg1 c(73.127,69.923,37.5991,54.2615,55.2077)
scenario$scenario6$pars$L95Cg2 c(70.7748,73.2346,46.6183,83.3189,86.2077)
scenario$scenario6$pars$L50Cg2 c(73.7748,74.2346,66.7038,90.3189,93.2077)
scenario$scenario6$pars$L95Dg c(56,55,50,20,20)
scenario$scenario6$pars$L50Dg c(58,59,54,21,21)
scenario$scenario6$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario6$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario6$pars$baranovIter 4
scenario$scenario6$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario6$pars$initDepTarg 0.3
scenario$scenario6$pars$rseed 1234
scenario$scenario6$pars$tauIndexg c(0.241585,0.496763,0.156945)
scenario$scenario6$pars$tauAgeg c(2.8836,1.27914,1.39808)
scenario$scenario6$pars$tauReleaseg c(0.332505,0.455038,0.930503,0,0)
scenario$scenario6$pars$qg c(0.366982,0.26669,0.57439)
scenario$scenario6$opModData$catch lisread('opModCatch.dat')
scenario$scenario6$opModData$index lisread('opModIndex.dat')
scenario$scenario6$opModData$ages lisread('opModAges.dat')
scenario$scenario6$opModEst$recDevs lisread('sableOpModmodelDevs06122010111552.dat')
scenario$scenario6$opModEst$qDevs lisread('sableOpModmodelDevs06122010111552.dat')
#
# Scenario S7- MCMC Mean of S1-ResMort_Mest_K3
#
scenario$scenario7$scenarioName “S7-Mest_K3_mcmcMean”
scenario$scenario7$pars$tMP 47
scenario$scenario7$pars$nT 83
scenario$scenario7$pars$nReps 100
scenario$scenario7$pars$nGear 5
scenario$scenario7$pars$nAges 35
scenario$scenario7$pars$nGrps 12
scenario$scenario7$pars$B0 120.049437
scenario$scenario7$pars$rSteepness 0.747435836
scenario$scenario7$pars$gammaR 0.0
scenario$scenario7$pars$sigmaR 0.6
scenario$scenario7$pars$A50 5
scenario$scenario7$pars$A95 8
scenario$scenario7$pars$M 0.06044607935
scenario$scenario7$pars$c1 8.4835e-06
scenario$scenario7$pars$c2 3.05118
scenario$scenario7$pars$Linf 71.0
scenario$scenario7$pars$L1 32.5
scenario$scenario7$pars$vonK 0.30
scenario$scenario7$pars$sigmaLinf 7.8
scenario$scenario7$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario7$pars$powerg rep(1,5)
scenario$scenario7$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario7$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario7$pars$sizeLim c(55,55,55,20,20)
scenario$scenario7$pars$selType c(1,1,2,1,1)
scenario$scenario7$pars$L50Cg1 c(58.78913615,59.25536885,39.8795772,51.33091985,54.06682815)
scenario$scenario7$pars$L95Cg1 c(73.10686105,69.9296774,40.98804825,54.4046139,55.0854149)
scenario$scenario7$pars$L95Cg2 c(70.78914085,73.2553646,47.0209435,83.33092375,86.0668315)
scenario$scenario7$pars$L50Cg2 c(73.78913995,74.2553646,67.09413395,90.3309226,93.06683015)
scenario$scenario7$pars$L95Dg c(56,55,50,20,20)
scenario$scenario7$pars$L50Dg c(58,59,54,21,21)
scenario$scenario7$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario7$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario7$pars$baranovIter 4
scenario$scenario7$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario7$pars$initDepTarg 0.3
scenario$scenario7$pars$rseed 1234
scenario$scenario7$pars$tauIndexg c(0.2779614835,0.490409178,0.1609119645)
scenario$scenario7$pars$tauAgeg c(2.859495785,1.284673115,1.403378685)
scenario$scenario7$pars$tauReleaseg c(0.474260843,0.315751863,0.9334105595,0,0)
scenario$scenario7$pars$qg c(0.514442493,0.369832949,0.8187370975)
scenario$scenario7$opModData$catch lisread('opModCatch.dat')
scenario$scenario7$opModData$index lisread('opModIndex.dat')
scenario$scenario7$opModData$ages lisread('opModAges.dat')
scenario$scenario7$opModEst$recDevs lisread('sableOpModmodelDevsMCMCmean16122010073148.dat')
scenario$scenario7$opModEst$qDevs lisread('sableOpModmodelDevsMCMCmean16122010073148.dat')
#
# Scenario S8 - MCMC @ 10-th percentile of MSY of S1-ResMort_Mest_K3
#
scenario$scenario8$scenarioName "S8-Mest_K3_mcmcQ40"
scenario$scenario8$pars$tMP 47
scenario$scenario8$pars$nT 83
scenario$scenario8$pars$nReps 100
scenario$scenario8$pars$nGear 5
scenario$scenario8$pars$nAges 35
scenario$scenario8$pars$nGrps 12
scenario$scenario8$pars$B0 121.076
scenario$scenario8$pars$rSteepness 0.586963
scenario$scenario8$pars$gammaR 0.0
scenario$scenario8$pars$sigmaR 0.6
scenario$scenario8$pars$A50 5
scenario$scenario8$pars$A95 8
scenario$scenario8$pars$M 0.0632784
scenario$scenario8$pars$c1 8.4835e-06
scenario$scenario8$pars$c2 3.05118
scenario$scenario8$pars$Linf 71.0
scenario$scenario8$pars$L1 32.5
scenario$scenario8$pars$vonK 0.30
scenario$scenario8$pars$sigmaLinf 7.8
scenario$scenario8$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario8$pars$powerg rep(1,5)
scenario$scenario8$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario8$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario8$pars$sizeLim c(55,55,55,20,20)
scenario$scenario8$pars$selType c(1,1,2,1,1)
scenario$scenario8$pars$L50Cg1 c(58.7034,59.4867,39.9587,53.1057,53.4025)
scenario$scenario8$pars$L95Cg1 c(73.7676,71.0461,41.0901,60.762,54.4261)
scenario$scenario8$pars$L95Cg2 c(70.7034,73.4867,46.5429,85.1057,85.4025)
scenario$scenario8$pars$L50Cg2 c(73.7034,74.4867,66.6157,92.1057,92.4025)
scenario$scenario8$pars$L95Dg c(56,55,50,20,20)
scenario$scenario8$pars$L50Dg c(58,59,54,21,21)
scenario$scenario8$pars$dg c(0.15,0.3,0.8,0,0)
scenario$scenario8$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario8$pars$baranovIter 4
scenario$scenario8$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario8$pars$initDepTarg 0.3
scenario$scenario8$pars$rseed 1234
scenario$scenario8$pars$tauIndexg c(0.256829,0.489055,0.147451)
scenario$scenario8$pars$tauAgeg c(2.86399,1.30001,1.41681)
scenario$scenario8$pars$tauReleaseg c(0.492681,0.324397,0.883639,0,0)
scenario$scenario8$pars$qg c(0.502358,0.380169,0.771782)
scenario$scenario8$opModData$catch lisread('opModCatch.dat')
scenario$scenario8$opModData$index lisread('opModIndex.dat')
scenario$scenario8$opModData$ages lisread('opModAges.dat')
scenario$scenario8$opModEst$recDevs lisread('sableOpModmodelDevsMCMCquantile16122010073148.dat')
scenario$scenario8$opModEst$qDevs lisread('sableOpModmodelDevsMCMCquantile16122010073148.dat')
#
#
# Scenario S9 - Baseline_Mest_HiMort 
#
scenario$scenario9$scenarioName “S9:Baseline+HiMort”
scenario$scenario9$pars$tMP 47
scenario$scenario9$pars$nT 83
scenario$scenario9$pars$nReps 100
scenario$scenario9$pars$nGear 5
scenario$scenario9$pars$nAges 35
scenario$scenario9$pars$nGrps 12
scenario$scenario9$pars$B0 129.096
scenario$scenario9$pars$rSteepness 0.789677
scenario$scenario9$pars$gammaR 0.0
scenario$scenario9$pars$sigmaR 0.6
scenario$scenario9$pars$A50 5
scenario$scenario9$pars$A95 8
scenario$scenario9$pars$M 0.063442
scenario$scenario9$pars$c1 8.4835e-06
scenario$scenario9$pars$c2 3.05118
scenario$scenario9$pars$Linf 71.0
scenario$scenario9$pars$L1 32.5
scenario$scenario9$pars$vonK 0.30
scenario$scenario9$pars$sigmaLinf 7.8
scenario$scenario9$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario9$pars$powerg rep(1,5)
scenario$scenario9$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario9$pars$allocProp c(0.417027,0.475627,0.099525,0.002995,0.004826)
scenario$scenario9$pars$sizeLim c(55,55,55,20,20)
scenario$scenario9$pars$selType c(1,1,2,1,1)
scenario$scenario9$pars$L50Cg1 c(58.7841,59.2476,38.8095,51.3671,54.2292)
scenario$scenario9$pars$L95Cg1 c(73.082,69.9216,39.8095,54.2252,55.2292)
scenario$scenario9$pars$L95Cg2 c(70.7841,73.2476,46.847,83.3671,86.2292)
scenario$scenario9$pars$L50Cg2 c(73.7841,74.2476,66.9326,90.3671,93.2292)
scenario$scenario9$pars$L95Dg c(56,55,50,20,20)
scenario$scenario9$pars$L50Dg c(58,59,54,21,21)
scenario$scenario9$pars$dg c(0.198,0.357,1.61,0,0)
scenario$scenario9$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario9$pars$baranovIter 4
scenario$scenario9$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario9$pars$initDepTarg 0.3
scenario$scenario9$pars$rseed 1234
scenario$scenario9$pars$tauIndexg c(0.249975,0.494772,0.159379)
scenario$scenario9$pars$tauAgeg c(2.88213,1.27335,1.39202)
scenario$scenario9$pars$tauReleaseg c(0.350756,0.420093,0.938417,0,0)
scenario$scenario9$pars$qg c(0.435868,0.317534,0.687802)
scenario$scenario9$opModData$catch lisread('opModCatch.dat')
scenario$scenario9$opModData$index lisread('opModIndex.dat')
scenario$scenario9$opModData$ages lisread('opModAges.dat')
scenario$scenario9$opModEst$recDevs lisread('sableOpModmodelDevs29122010085129.dat')
scenario$scenario9$opModEst$qDevs lisread('sableOpModmodelDevs29122010085129.dat')
#
#--------------------------------------------------------------------#
# Management procedures for each scenario.
#
# MP1 - PerfectHCR46
#       Data - NA
#       Assess - True Umsy, True LegalB.
#       HCR - LBM=0.4, UBM=0.6
#
# MP2 - StrsHiTuneHCR46
#       Data - StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=1.7, muUmsy=0.08, sdUmsy=0.04)
#       HCR - LBM=0.4, UBM=0.6
#
# MP3 - AvoidStrsHiTuneHCR46
#       futureOption = 1
#       Data - StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=1.7, muUmsy=0.08, sdUmsy=0.04)
#       HCR - LBM=0.4, UBM=0.6
#
# MP4 - RetainStrsHiTuneHCR46
#       futureOption = 2
#       Data - StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=1.7, muUmsy=0.08, sdUmsy=0.04)
#       HCR - LBM=0.4, UBM=0.6
#
# MP5 - StrsLowTuneHCR46
#       Data - StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=5.1, muUmsy=0.08, sdUmsy=0.12)
#       HCR - LBM=0.4, UBM=0.6
#
# MP6 - StdStrsLowTuneH46
#       Data - Std., StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=5.1, muUmsy=0.08, sdUmsy=0.12)
#       HCR - LBM=0.4, UBM=0.6
#
# MP7 - StdStRSLowTuneHCR48
#       Data - Std, StRS
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=5.1, muUmsy=0.08, sdUmsy=0.12)
#       HCR - LBM=0.4, UBM=0.8
#
#--------------------------------------------------------------------#
#
# Management Procedure 1, MP1 - PerfectHCR46
#
mp$mp1$mp$data$minAge 3
mp$mp1$mp$data$t1Index c(15,26,39)
mp$mp1$mp$data$t2Index c(47,47,47)
mp$mp1$mp$data$k1Index c(1,1,1)
mp$mp1$mp$data$k2Index c(0,0,1)
mp$mp1$mp$data$useIndex c(1,4,5)
mp$mp1$mp$data$IndexType c(1,1,1)
mp$mp1$mp$data$useAges c(1,4,5)
mp$mp1$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp1$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp1$mp$data$inputAges lisread('opModAges.dat')
mp$mp1$mpName "PerfectHCR46"
mp$mp1$mp$assess$kfGain 0.5
mp$mp1$mp$assess$assessMethod "perfect"
mp$mp1$mp$assess$rhoEIV 0.95
mp$mp1$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp1$mp$assess$muPriorMSY 3.4
mp$mp1$mp$assess$sdPriorMSY 1.7
mp$mp1$mp$assess$muPriorFmsy 0.08
mp$mp1$mp$assess$sdPriorFmsy 0.04
mp$mp1$mp$assess$skipFails "FALSE"
mp$mp1$mp$assess$futureOption 0
mp$mp1$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp1$mp$hcr$limitRefMult 0.4
mp$mp1$mp$hcr$upperRefMult 0.6
mp$mp1$mp$hcr$lam1 0
mp$mp1$mp$hcr$ssbRefTyp "Bmsy"
mp$mp1$mp$hcr$BrefSource "Est"
mp$mp1$mp$hcr$BrefMethod "Rel"
mp$mp1$mp$hcr$remRefTyp "Fmsy"
mp$mp1$mp$hcr$remRefSource "Est"
mp$mp1$mp$hcr$remRefMethod "Rel"
mp$mp1$mp$hcr$remRateInput 0.2
mp$mp1$mp$hcr$refEstFreq 1
mp$mp1$mpCol "green"
mp$mp1$mpLty 1
mp$mp1$mpLwd 1
mp$mp1$mpSym 16
#
# Management Procedure 2, MP2 - StrsHiTuneHCR46
#
mp$mp2$mp$mp$data$minAge 3
mp$mp2$mp$data$t1Index c(15,26,39)
mp$mp2$mp$data$t2Index c(47,47,47)
mp$mp2$mp$data$k1Index c(1,1,1)
mp$mp2$mp$data$k2Index c(0,0,1)
mp$mp2$mp$data$useIndex c(1,4,5)
mp$mp2$mp$data$IndexType c(1,1,1)
mp$mp2$mp$data$useAges c(1,4,5)
mp$mp2$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp2$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp2$mp$data$inputAges lisread('opModAges.dat')
mp$mp2$mpName "StrsHiTuneHCR46"
mp$mp2$mp$assess$kfGain 0.5
mp$mp2$mp$assess$assessMethod "pMod"
mp$mp2$mp$assess$rhoEIV 0.95
mp$mp2$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp2$mp$assess$muPriorMSY 3.4
mp$mp2$mp$assess$sdPriorMSY 1.7
mp$mp2$mp$assess$muPriorFmsy 0.08
mp$mp2$mp$assess$sdPriorFmsy 0.04
mp$mp2$mp$assess$skipFails "FALSE"
mp$mp2$mp$assess$futureOption 0
mp$mp2$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp2$mp$hcr$limitRefMult 0.4
mp$mp2$mp$hcr$upperRefMult 0.6
mp$mp2$mp$hcr$lam1 0
mp$mp2$mp$hcr$ssbRefTyp "Bmsy"
mp$mp2$mp$hcr$BrefSource "Est"
mp$mp2$mp$hcr$BrefMethod "Rel"
mp$mp2$mp$hcr$remRefTyp "Fmsy"
mp$mp2$mp$hcr$remRefSource "Est"
mp$mp2$mp$hcr$remRefMethod "Rel"
mp$mp2$mp$hcr$remRateInput 0.2
mp$mp2$mp$hcr$refEstFreq 1
mp$mp2$mpCol "black"
mp$mp2$mpLwd 1
mp$mp2$mpLty 1
mp$mp2$mpSym 1
#
# Management Procedure 3, MP3 - AvoidStrsHiTuneHCR46
#
mp$mp3$mp$data$minAge 3
mp$mp3$mp$data$t1Index c(15,26,39)
mp$mp3$mp$data$t2Index c(47,47,47)
mp$mp3$mp$data$k1Index c(1,1,1)
mp$mp3$mp$data$k2Index c(0,0,1)
mp$mp3$mp$data$useIndex c(1,4,5)
mp$mp3$mp$data$IndexType c(1,1,1)
mp$mp3$mp$data$useAges c(1,4,5)
mp$mp3$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp3$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp3$mp$data$inputAges lisread('opModAges.dat')
mp$mp3$mpName "AvoidStrsHiTuneHCR46"
mp$mp3$mp$assess$kfGain 0.5
mp$mp3$mp$assess$assessMethod "pMod"
mp$mp3$mp$assess$rhoEIV 0.95
mp$mp3$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp3$mp$assess$muPriorMSY 3.4
mp$mp3$mp$assess$sdPriorMSY 1.7
mp$mp3$mp$assess$muPriorFmsy 0.08
mp$mp3$mp$assess$sdPriorFmsy 0.04
mp$mp3$mp$assess$skipFails "FALSE"
mp$mp3$mp$assess$futureOption 1
mp$mp3$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp3$mp$hcr$limitRefMult 0.4
mp$mp3$mp$hcr$upperRefMult 0.6
mp$mp3$mp$hcr$lam1 0
mp$mp3$mp$hcr$ssbRefTyp "Bmsy"
mp$mp3$mp$hcr$BrefSource "Est"
mp$mp3$mp$hcr$BrefMethod "Rel"
mp$mp3$mp$hcr$remRefTyp "Fmsy"
mp$mp3$mp$hcr$remRefSource "Est"
mp$mp3$mp$hcr$remRefMethod "Rel"
mp$mp3$mp$hcr$remRateInput 0.2
mp$mp3$mp$hcr$refEstFreq 1
mp$mp3$mpCol "red"
mp$mp3$mpLty 1
mp$mp3$mpLwd 1
mp$mp3$mpSym 16
#
# Management Procedure 4, MP4 - RetainStrsHiTuneHCR46
#
mp$mp4$mp$data$minAge 3
mp$mp4$mp$data$t1Index c(15,26,39)
mp$mp4$mp$data$t2Index c(47,47,47)
mp$mp4$mp$data$k1Index c(1,1,1)
mp$mp4$mp$data$k2Index c(0,0,1)
mp$mp4$mp$data$useIndex c(1,4,5)
mp$mp4$mp$data$IndexType c(1,1,1)
mp$mp4$mp$data$useAges c(1,4,5)
mp$mp4$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp4$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp4$mp$data$inputAges lisread('opModAges.dat')
mp$mp4$mpName "RetainStrsHiTuneHCR46"
mp$mp4$mp$assess$kfGain 0.5
mp$mp4$mp$assess$assessMethod "pMod"
mp$mp4$mp$assess$rhoEIV 0.95
mp$mp4$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp4$mp$assess$muPriorMSY 3.4
mp$mp4$mp$assess$sdPriorMSY 1.7
mp$mp4$mp$assess$muPriorFmsy 0.08
mp$mp4$mp$assess$sdPriorFmsy 0.04
mp$mp4$mp$assess$skipFails "FALSE"
mp$mp4$mp$assess$futureOption 2
mp$mp4$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp4$mp$hcr$limitRefMult 0.4
mp$mp4$mp$hcr$upperRefMult 0.6
mp$mp4$mp$hcr$lam1 0
mp$mp4$mp$hcr$ssbRefTyp "Bmsy"
mp$mp4$mp$hcr$BrefSource "Est"
mp$mp4$mp$hcr$BrefMethod "Rel"
mp$mp4$mp$hcr$remRefTyp "Fmsy"
mp$mp4$mp$hcr$remRefSource "Est"
mp$mp4$mp$hcr$remRefMethod "Rel"
mp$mp4$mp$hcr$remRateInput 0.2
mp$mp4$mp$hcr$refEstFreq 1
mp$mp4$mpCol "cyan"
mp$mp4$mpLty 1
mp$mp4$mpLwd 1
mp$mp4$mpSym 16
#
# Management Procedure 5, MP5 - StrsLowTuneHCR46
#
mp$mp5$mp$data$minAge 3
mp$mp5$mp$data$t1Index c(15,26,39)
mp$mp5$mp$data$t2Index c(47,47,47)
mp$mp5$mp$data$k1Index c(1,1,1)
mp$mp5$mp$data$k2Index c(0,0,1)
mp$mp5$mp$data$useIndex c(1,4,5)
mp$mp5$mp$data$IndexType c(1,1,1)
mp$mp5$mp$data$useAges c(1,4,5)
mp$mp5$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp5$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp5$mp$data$inputAges lisread('opModAges.dat')
mp$mp5$mpName "StrsLowTuneHCR46"
mp$mp5$mp$assess$kfGain 0.5
mp$mp5$mp$assess$assessMethod "pMod"
mp$mp5$mp$assess$rhoEIV 0.95
mp$mp5$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp5$mp$assess$muPriorMSY 3.4
mp$mp5$mp$assess$sdPriorMSY 5.1
mp$mp5$mp$assess$muPriorFmsy 0.08
mp$mp5$mp$assess$sdPriorFmsy 0.12
mp$mp5$mp$assess$skipFails "FALSE"
mp$mp5$mp$assess$futureOption 0
mp$mp5$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp5$mp$hcr$limitRefMult 0.4
mp$mp5$mp$hcr$upperRefMult 0.6
mp$mp5$mp$hcr$lam1 0
mp$mp5$mp$hcr$ssbRefTyp "Bmsy"
mp$mp5$mp$hcr$BrefSource "Est"
mp$mp5$mp$hcr$BrefMethod "Rel"
mp$mp5$mp$hcr$remRefTyp "Fmsy"
mp$mp5$mp$hcr$remRefSource "Est"
mp$mp5$mp$hcr$remRefMethod "Rel"
mp$mp5$mp$hcr$remRateInput 0.2
mp$mp5$mp$hcr$refEstFreq 1
mp$mp5$mpCol "lightgreen"
mp$mp5$mpLty 1
mp$mp5$mpLwd 1
mp$mp5$mpSym 16
#
# Management Procedure 6, MP6 - StdStrsLowTuneH46
#
mp$mp6$mp$data$minAge 3
mp$mp6$mp$data$t1Index c(15,26,39)
mp$mp6$mp$data$t2Index c(47,47,47)
mp$mp6$mp$data$k1Index c(1,1,1)
mp$mp6$mp$data$k2Index c(0,1,1)
mp$mp6$mp$data$useIndex c(1,4,5)
mp$mp6$mp$data$IndexType c(1,1,1)
mp$mp6$mp$data$useAges c(1,4,5)
mp$mp6$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp6$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp6$mp$data$inputAges lisread('opModAges.dat')
mp$mp6$mpName "StdStrsLowTuneH46"
mp$mp6$mp$assess$kfGain 0.5
mp$mp6$mp$assess$assessMethod "pMod"
mp$mp6$mp$assess$rhoEIV 0.95
mp$mp6$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp6$mp$assess$muPriorMSY 3.4
mp$mp6$mp$assess$sdPriorMSY 5.1
mp$mp6$mp$assess$muPriorFmsy 0.08
mp$mp6$mp$assess$sdPriorFmsy 0.12
mp$mp6$mp$assess$skipFails "FALSE"
mp$mp6$mp$assess$futureOption 0
mp$mp6$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp6$mp$hcr$limitRefMult 0.4
mp$mp6$mp$hcr$upperRefMult 0.6
mp$mp6$mp$hcr$lam1 0
mp$mp6$mp$hcr$ssbRefTyp "Bmsy"
mp$mp6$mp$hcr$BrefSource "Est"
mp$mp6$mp$hcr$BrefMethod "Rel"
mp$mp6$mp$hcr$remRefTyp "Fmsy"
mp$mp6$mp$hcr$remRefSource "Est"
mp$mp6$mp$hcr$remRefMethod "Rel"
mp$mp6$mp$hcr$remRateInput 0.2
mp$mp6$mp$hcr$refEstFreq 1
mp$mp6$mpCol "blue"
mp$mp6$mpLty 1
mp$mp6$mpLwd 1
mp$mp6$mpSym 16
#
# Management Procedure 7, MP7 - StdStrsLowTuneH48
#
mp$mp7$mp$data$minAge 3
mp$mp7$mp$data$t1Index c(15,26,39)
mp$mp7$mp$data$t2Index c(47,47,47)
mp$mp7$mp$data$k1Index c(1,1,1)
mp$mp7$mp$data$k2Index c(0,1,1)
mp$mp7$mp$data$useIndex c(1,4,5)
mp$mp7$mp$data$IndexType c(1,1,1)
mp$mp7$mp$data$useAges c(1,4,5)
mp$mp7$mp$data$inputCatch lisread('opModCatch.dat')
mp$mp7$mp$data$inputIndex lisread('opModIndex.dat')
mp$mp7$mp$data$inputAges lisread('opModAges.dat')
mp$mp7$mpName "StdStrsLowTuneH48"
mp$mp7$mp$assess$kfGain 0.5
mp$mp7$mp$assess$assessMethod "pMod"
mp$mp7$mp$assess$rhoEIV 0.95
mp$mp7$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp7$mp$assess$muPriorMSY 3.4
mp$mp7$mp$assess$sdPriorMSY 5.1
mp$mp7$mp$assess$muPriorFmsy 0.08
mp$mp7$mp$assess$sdPriorFmsy 0.12
mp$mp7$mp$assess$skipFails "FALSE"
mp$mp7$mp$assess$futureOption 0
mp$mp7$mp$hcr$ruleName "calcLegalHarvRule"
mp$mp7$mp$hcr$limitRefMult 0.4
mp$mp7$mp$hcr$upperRefMult 0.8
mp$mp7$mp$hcr$lam1 0
mp$mp7$mp$hcr$ssbRefTyp "Bmsy"
mp$mp7$mp$hcr$BrefSource "Est"
mp$mp7$mp$hcr$BrefMethod "Rel"
mp$mp7$mp$hcr$remRefTyp "Fmsy"
mp$mp7$mp$hcr$remRefSource "Est"
mp$mp7$mp$hcr$remRefMethod "Rel"
mp$mp7$mp$hcr$remRateInput 0.2
mp$mp7$mp$hcr$refEstFreq 1
mp$mp7$mpCol "gray"
mp$mp7$mpLty 1
mp$mp7$mpLwd 1
mp$mp7$mpSym 16
# File Ends <not run>.