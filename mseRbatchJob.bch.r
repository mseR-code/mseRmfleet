## mseR batch file written: Fri Nov 26 14:27:14 2010 
#
parameter value
#
#--------------------------------------------------------------------# 
# Scenarios for reference set:
#
# S1 - Res. release mortality rates, asym. sel. M est. - ResMort_ASYM
# S2 - Res. release mortality rates, asym. sel. M=0.08 - ResMort_ASYM_M08
# S3 - Res. release mortality rates, asym. sel. M est., gam=0.5 - ResMort_ASYM_GAM5
# S4 - Res. release mortality rates, asym. sel. M=0.08, gam=0.5 - ResMort_ASYM_M08_GAM5
#--------------------------------------------------------------------# 
#
# Scenario  ResMort_ASYM 
#
scenario$scenario1$scenarioName “ResMort_ASYM”
scenario$scenario1$pars$tMP 47
scenario$scenario1$pars$nT 96
scenario$scenario1$pars$nReps 50
scenario$scenario1$pars$nGear 5
scenario$scenario1$pars$nAges 35
scenario$scenario1$pars$nGrps 12
scenario$scenario1$pars$B0 119.89
scenario$scenario1$pars$rSteepness 0.833749
scenario$scenario1$pars$gammaR 0.0
scenario$scenario1$pars$sigmaR 0.6
scenario$scenario1$pars$A50 5
scenario$scenario1$pars$A95 8
scenario$scenario1$pars$M 0.0597591
scenario$scenario1$pars$c1 8.4835e-06
scenario$scenario1$pars$c2 3.05118
scenario$scenario1$pars$Linf 71.0
scenario$scenario1$pars$L1 32.5
scenario$scenario1$pars$vonK 0.3
scenario$scenario1$pars$sigmaLinf 7.8
scenario$scenario1$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario1$pars$powerg rep(1,5)
scenario$scenario1$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario1$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario1$pars$sizeLim c(55,55,55,20,20)
scenario$scenario1$pars$selType c(1,1,2,1,1)
scenario$scenario1$pars$L50Cg1 c(58.7969,59.2602,38.7671,51.5192,54.2622)
scenario$scenario1$pars$L95Cg1 c(73.0311,69.9042,39.7671,54.2968,55.2622)
scenario$scenario1$pars$L95Cg2 c(70.7969,73.2602,46.9603,83.5192,86.2622)
scenario$scenario1$pars$L50Cg2 c(73.7969,74.2602,67.0459,90.5192,93.2622)
scenario$scenario1$pars$L95Dg c(56,55,52,20,20)
scenario$scenario1$pars$L50Dg c(58,59,54,21,21)
scenario$scenario1$pars$dg c(0.2,0.35,0.9,0,0)
scenario$scenario1$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario1$pars$baranovIter 4
scenario$scenario1$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario1$pars$initDepTarg 0.3
scenario$scenario1$pars$rseed 1234
scenario$scenario1$pars$tauIndexg c(0.268183,0.490303,0.157613)
scenario$scenario1$pars$tauAgeg c(2.8601,1.27323,1.39154)
scenario$scenario1$pars$tauReleaseg c(0.43706,0.320128,0.976961,0,0)
scenario$scenario1$pars$qg c(0.501936,0.36186,0.797895)
scenario$scenario1$opModData$catch lisread('opModCatch.dat')
scenario$scenario1$opModData$index lisread('opModIndex.dat')
scenario$scenario1$opModData$ages lisread('opModAges.dat')
scenario$scenario1$opModEst$recDevs lisread('sableOpModmodelDevs23112010112110.dat')
scenario$scenario1$opModEst$qDevs lisread('sableOpModmodelDevs23112010112110.dat')
#
# Scenario  ResMort_ASYM_M08 
#
scenario$scenario2$scenarioName “ResMort_ASYM_M08”
scenario$scenario2$pars$tMP 47
scenario$scenario2$pars$nT 96
scenario$scenario2$pars$nReps 50
scenario$scenario2$pars$nGear 5
scenario$scenario2$pars$nAges 35
scenario$scenario2$pars$nGrps 12
scenario$scenario2$pars$B0 149.643
scenario$scenario2$pars$rSteepness 0.509646
scenario$scenario2$pars$gammaR 0.0
scenario$scenario2$pars$sigmaR 0.6
scenario$scenario2$pars$A50 5
scenario$scenario2$pars$A95 8
scenario$scenario2$pars$M 0.08
scenario$scenario2$pars$c1 8.4835e-06
scenario$scenario2$pars$c2 3.05118
scenario$scenario2$pars$Linf 71.0
scenario$scenario2$pars$L1 32.5
scenario$scenario2$pars$vonK 0.3
scenario$scenario2$pars$sigmaLinf 7.8
scenario$scenario2$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario2$pars$powerg rep(1,5)
scenario$scenario2$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario2$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario2$pars$sizeLim c(55,55,55,20,20)
scenario$scenario2$pars$selType c(1,1,2,1,1)
scenario$scenario2$pars$L50Cg1 c(58.7732,59.237,36.5989,51.2875,54.2049)
scenario$scenario2$pars$L95Cg1 c(73.1377,69.9132,37.599,54.2668,55.2049)
scenario$scenario2$pars$L95Cg2 c(70.7732,73.237,46.5872,83.2875,86.2049)
scenario$scenario2$pars$L50Cg2 c(73.7732,74.237,66.6728,90.2875,93.2049)
scenario$scenario2$pars$L95Dg c(56,55,52,20,20)
scenario$scenario2$pars$L50Dg c(58,59,54,21,21)
scenario$scenario2$pars$dg c(0.2,0.35,0.9,0,0)
scenario$scenario2$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario2$pars$baranovIter 4
scenario$scenario2$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario2$pars$initDepTarg 0.3
scenario$scenario2$pars$rseed 1234
scenario$scenario2$pars$tauIndexg c(0.238249,0.498169,0.155925)
scenario$scenario2$pars$tauAgeg c(2.88809,1.27909,1.39784)
scenario$scenario2$pars$tauReleaseg c(0.324052,0.473999,1.00235,0,0)
scenario$scenario2$pars$qg c(0.356892,0.259609,0.55685)
scenario$scenario2$opModData$catch lisread('opModCatch.dat')
scenario$scenario2$opModData$index lisread('opModIndex.dat')
scenario$scenario2$opModData$ages lisread('opModAges.dat')
scenario$scenario2$opModEst$recDevs lisread('sableOpModmodelDevs23112010141730.dat')
scenario$scenario2$opModEst$qDevs lisread('sableOpModmodelDevs23112010141730.dat')
#
# Scenario  ResMort_ASYM_GAM5 
#
scenario$scenario3$scenarioName “ResMort_ASYM_GAM5”
scenario$scenario3$pars$tMP 47
scenario$scenario3$pars$nT 96
scenario$scenario3$pars$nReps 50
scenario$scenario3$pars$nGear 5
scenario$scenario3$pars$nAges 35
scenario$scenario3$pars$nGrps 12
scenario$scenario3$pars$B0 119.89
scenario$scenario3$pars$rSteepness 0.833749
scenario$scenario3$pars$gammaR 0.5
scenario$scenario3$pars$sigmaR 0.6
scenario$scenario3$pars$A50 5
scenario$scenario3$pars$A95 8
scenario$scenario3$pars$M 0.0597591
scenario$scenario3$pars$c1 8.4835e-06
scenario$scenario3$pars$c2 3.05118
scenario$scenario3$pars$Linf 71.0
scenario$scenario3$pars$L1 32.5
scenario$scenario3$pars$vonK 0.3
scenario$scenario3$pars$sigmaLinf 7.8
scenario$scenario3$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario3$pars$powerg rep(1,5)
scenario$scenario3$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario3$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario3$pars$sizeLim c(55,55,55,20,20)
scenario$scenario3$pars$selType c(1,1,2,1,1)
scenario$scenario3$pars$L50Cg1 c(58.7969,59.2602,38.7671,51.5192,54.2622)
scenario$scenario3$pars$L95Cg1 c(73.0311,69.9042,39.7671,54.2968,55.2622)
scenario$scenario3$pars$L95Cg2 c(70.7969,73.2602,46.9603,83.5192,86.2622)
scenario$scenario3$pars$L50Cg2 c(73.7969,74.2602,67.0459,90.5192,93.2622)
scenario$scenario3$pars$L95Dg c(56,55,52,20,20)
scenario$scenario3$pars$L50Dg c(58,59,54,21,21)
scenario$scenario3$pars$dg c(0.2,0.35,0.9,0,0)
scenario$scenario3$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario3$pars$baranovIter 4
scenario$scenario3$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario3$pars$initDepTarg 0.3
scenario$scenario3$pars$rseed 1234
scenario$scenario3$pars$tauIndexg c(0.268183,0.490303,0.157613)
scenario$scenario3$pars$tauAgeg c(2.8601,1.27323,1.39154)
scenario$scenario3$pars$tauReleaseg c(0.43706,0.320128,0.976961,0,0)
scenario$scenario3$pars$qg c(0.501936,0.36186,0.797895)
scenario$scenario3$opModData$catch lisread('opModCatch.dat')
scenario$scenario3$opModData$index lisread('opModIndex.dat')
scenario$scenario3$opModData$ages lisread('opModAges.dat')
scenario$scenario3$opModEst$recDevs lisread('sableOpModmodelDevs23112010112110.dat')
scenario$scenario3$opModEst$qDevs lisread('sableOpModmodelDevs23112010112110.dat')
#
# Scenario  ResMort_ASYM_M08_GAM5 
#
scenario$scenario4$scenarioName “ResMort_ASYM_M08_GAM5”
scenario$scenario4$pars$tMP 47
scenario$scenario4$pars$nT 96
scenario$scenario4$pars$nReps 50
scenario$scenario4$pars$nGear 5
scenario$scenario4$pars$nAges 35
scenario$scenario4$pars$nGrps 12
scenario$scenario4$pars$B0 149.643
scenario$scenario4$pars$rSteepness 0.509646
scenario$scenario4$pars$gammaR 0.5
scenario$scenario4$pars$sigmaR 0.6
scenario$scenario4$pars$A50 5
scenario$scenario4$pars$A95 8
scenario$scenario4$pars$M 0.08
scenario$scenario4$pars$c1 8.4835e-06
scenario$scenario4$pars$c2 3.05118
scenario$scenario4$pars$Linf 71.0
scenario$scenario4$pars$L1 32.5
scenario$scenario4$pars$vonK 0.3
scenario$scenario4$pars$sigmaLinf 7.8
scenario$scenario4$pars$tagRepRate c(1,1,1,1,1)
scenario$scenario4$pars$powerg rep(1,5)
scenario$scenario4$pars$gNames c("Trap","Hook","Trawl","Std","StRs")
scenario$scenario4$pars$allocProp c(0.4179,0.4745,0.0997,0.0030,0.0049)
scenario$scenario4$pars$sizeLim c(55,55,55,20,20)
scenario$scenario4$pars$selType c(1,1,2,1,1)
scenario$scenario4$pars$L50Cg1 c(58.7732,59.237,36.5989,51.2875,54.2049)
scenario$scenario4$pars$L95Cg1 c(73.1377,69.9132,37.599,54.2668,55.2049)
scenario$scenario4$pars$L95Cg2 c(70.7732,73.237,46.5872,83.2875,86.2049)
scenario$scenario4$pars$L50Cg2 c(73.7732,74.237,66.6728,90.2875,93.2049)
scenario$scenario4$pars$L95Dg c(56,55,52,20,20)
scenario$scenario4$pars$L50Dg c(58,59,54,21,21)
scenario$scenario4$pars$dg c(0.2,0.35,0.9,0,0)
scenario$scenario4$pars$fg c(1.0,0.8,0.3,0.1,0.1)
scenario$scenario4$pars$baranovIter 4
scenario$scenario4$pars$baranovSteps c(0.8,0.5,0.8,0.4,0.4)
scenario$scenario4$pars$initDepTarg 0.3
scenario$scenario4$pars$rseed 1234
scenario$scenario4$pars$tauIndexg c(0.238249,0.498169,0.155925)
scenario$scenario4$pars$tauAgeg c(2.88809,1.27909,1.39784)
scenario$scenario4$pars$tauReleaseg c(0.324052,0.473999,1.00235,0,0)
scenario$scenario4$pars$qg c(0.356892,0.259609,0.55685)
scenario$scenario4$opModData$catch lisread('opModCatch.dat')
scenario$scenario4$opModData$index lisread('opModIndex.dat')
scenario$scenario4$opModData$ages lisread('opModAges.dat')
scenario$scenario4$opModEst$recDevs lisread('sableOpModmodelDevs23112010141730.dat')
scenario$scenario4$opModEst$qDevs lisread('sableOpModmodelDevs23112010141730.dat')
#
#--------------------------------------------------------------------#
# Management procedures for each scenario.
#
# MP1 - StrsTune1HCR46
#       Data - (StRS)
#       Assess - pMod(rel., est., muMSY=3.3, sdMSY=0.8, muUmsy=0.06, sdUmsy=0.02)
#       HCR - LBM=0.4, UBM=0.6
#
# MP2 - StrsTune2HCR46
#       Data - (StRS)
#       Assess - pMod(rel., est., muMSY=3.4, sdMSY=0.8, muUmsy=0.08, sdUmsy=0.02)
#       HCR - LBM=0.4, UBM=0.6
#--------------------------------------------------------------------#
#
# Management Procedure 1, MP1 - StrsTune1HCR46
#
mp$mp1$mp$mp$data$minAge 3
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
mp$mp1$mpName "StrsTune1HCR46"
mp$mp1$mp$assess$kfGain 0.5
mp$mp1$mp$assess$assessMethod "pMod"
mp$mp1$mp$assess$rhoEIV 0.95
mp$mp1$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp1$mp$assess$muPriorMSY 3.3
mp$mp1$mp$assess$sdPriorMSY 0.8
mp$mp1$mp$assess$muPriorFmsy 0.06
mp$mp1$mp$assess$sdPriorFmsy 0.02
mp$mp1$mp$assess$skipFails "FALSE"
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
mp$mp1$mpCol "black"
mp$mp1$mpLwd 1
mp$mp1$mpLty 1
mp$mp1$mpSym 1
#
# Management Procedure 2, MP2 - StrsTune2HCR46
#
mp$mp2$mp$data$minAge 3
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
mp$mp2$mpName "StrsTune2HCR46"
mp$mp2$mp$assess$kfGain 0.5
mp$mp2$mp$assess$assessMethod "pMod"
mp$mp2$mp$assess$rhoEIV 0.95
mp$mp2$mp$assess$idxLikeWeight "rep(1,4)"
mp$mp2$mp$assess$muPriorMSY 3.4
mp$mp2$mp$assess$sdPriorMSY 0.8
mp$mp2$mp$assess$muPriorFmsy 0.08
mp$mp2$mp$assess$sdPriorFmsy 0.02
mp$mp2$mp$assess$skipFails "FALSE"
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
mp$mp2$mpCol "green"
mp$mp2$mpLty 1
mp$mp2$mpLwd 1
mp$mp2$mpSym 16
# File Ends <not run>.