DATA_SECTION
  // Year for retrospective analysis. This just determines the
  // ending year for likelihood calcs. The whole model still runs
  // and generates variables for all years.
  init_int likeYear;
  // Catch biomass data -- switch to catch data file
  !! ad_comm::change_datafile_name("opModCatch.dat");
  // Total length of catch data.
  init_int nT;
  // Number of fisheries.
  init_int nFisheries;
  // first year of catch data
  init_ivector firstYearCatch(1,nFisheries-2);
  // Matrix of landed catch biomass (tonnes) values.
  init_matrix landCatchMatrix(1,nT,1,nFisheries+2);
  // Matrix of landed catch biomass (tonnes) values.
  init_matrix releaseMatrix(1,nT,1,nFisheries+2);
  // Total landed catch 
  vector totLandCatch(1,nT);
  // Catch proportions by fishery
  vector propCatchFish(1,nFisheries);

  // Abundance index data -- switch to index data file
  !! ad_comm::change_datafile_name("opModIndex.dat");
  // Number of stock indices
  init_int nIndexSeries;
  //  Series index numbers.
  init_ivector idxIndex(1,nIndexSeries);
  // Index weights in overall likelihood
  init_vector idxLikeWeight(1,nIndexSeries);
  // Vector indicating first year for each index.
  init_ivector idxFirstYear(1,nIndexSeries);
  // Vector indicating last year for each index.
  init_ivector idxLastYear(1,nIndexSeries);
  // Fraction of year expired prior to index 
  init_vector fracYearSurvey(1,nIndexSeries);
  // Index series data
  init_matrix idxSeries(1,nIndexSeries,1,nT);

  // Create vector of max index series lengths.
  ivector nobs(1,nIndexSeries);
  !! nobs=idxLastYear-idxFirstYear+1;
  vector validObs(1,nIndexSeries);


  // Time varying catchability
  !! ad_comm::change_datafile_name("opModTVq.dat");
  // Vector indicating first year for each indexes time-varying q
  init_ivector firstqdev_g(1,nIndexSeries);
  // Vector indicating last year for each indexes time-varying q.
  init_ivector lastqdev_g(1,nIndexSeries);
  // phase of q devs
  init_ivector ph_log_qdev_g(1,nIndexSeries);
  init_vector priorSD_qdev(1,nIndexSeries);

  init_number checkTVq;
  !! if(checkTVq != 999) cout << "Bad Check in TVq!" << endl;
  !! if(checkTVq != 999) exit(1);


  // Age proportion data -- switch to age composition file
  !! ad_comm::change_datafile_name("opModAges.dat");
  // Number of age series
  init_int nAgeSeries;
  !! cout << "nAgeSeries == " << nAgeSeries << endl;

  // plusGroupAge for model
  init_int plusGroupAge;  

  // Make vector of ages
  vector ages(1,plusGroupAge);
  !! ages.fill_seqadd(1,1);

  // First age class 
  init_ivector minAge(1,nAgeSeries);
  // Max age by fishery
  init_ivector maxAge(1,nAgeSeries);
  // Fishery indices for each age-comp
  init_ivector fisheryAgeID(1,nAgeSeries);

  // Age likelihood weights
  init_vector ageLikeWeight_m(1,nAgeSeries);
  init_vector ageLikeWeight_f(1,nAgeSeries);

  // fraction of year when samples taken
  init_vector fracYearAges(1,nAgeSeries);

  // Ageing error switch
  init_ivector useAgeError_m(1,nAgeSeries);
  init_ivector useAgeError_f(1,nAgeSeries);


  // Matrix of first age props > 1%: males
  init_matrix firstAge_m(1,nAgeSeries,1,nT);
  // Matrix of first age props > 1%: females
  init_matrix firstAge_f(1,nAgeSeries,1,nT);

  // Matrix of last age props > 1%: males
  init_matrix lastAge_m(1,nAgeSeries,1,nT);
  // Matrix of last age props > 1%: females
  init_matrix lastAge_f(1,nAgeSeries,1,nT);

  // Matrix of age sample sizes: males
  init_matrix nObsAge_m(1,nAgeSeries,1,nT);
  // Matrix of age sample sizes: females
  init_matrix nObsAge_f(1,nAgeSeries,1,nT);

  //Observed proportions-at-age: males
  init_3darray ageObsProp_m(1,nAgeSeries,1,nT,minAge,maxAge);
  //Observed proportions-at-age: females
  init_3darray ageObsProp_f(1,nAgeSeries,1,nT,minAge,maxAge);

  number n_residuals_m; 
  number n_residuals_f;

  // low prop threshold
  number prop_threshold;
  !!  prop_threshold = 0.005;
  // accumulator bins
  matrix accBin_m(1,nAgeSeries,1,nT);
  matrix accBin_f(1,nAgeSeries,1,nT);

  init_int eof;
  !! cout << "opModAges/eof = " << eof << endl;

  // Ageing error matrix
  !! ad_comm::change_datafile_name("ageErrorMatrix.dat");
  init_matrix Q(2,35,2,plusGroupAge);

  // Selectivity parameter priors from tag release-recovery data
  !! ad_comm::change_datafile_name("opModSelPriors.dat");
  
  // Pull gear specific priors
  // gear specific alpha parameter prior means
  ivector firstSelPrior_g(1,nFisheries-2);
  ivector lastSelPrior_g(1,nFisheries-2);
  // Pull first/last time steps for sel priors individually, then
  // combine into an ivector for the vector_vector initialisation later
  init_int firstSelPrior_1;
  !! firstSelPrior_g(1) = firstSelPrior_1;
  init_int firstSelPrior_2;
  !! firstSelPrior_g(2) = firstSelPrior_2;
  init_int firstSelPrior_3;
  !! firstSelPrior_g(3) = firstSelPrior_3;
  init_int lastSelPrior_1;
  !! lastSelPrior_g(1) = lastSelPrior_1;
  init_int lastSelPrior_2;
  !! lastSelPrior_g(2) = lastSelPrior_2;
  init_int lastSelPrior_3;
  !! lastSelPrior_g(3) = lastSelPrior_3;

  // Phase for deviations in commercial selectivity
  // mode
  init_ivector ph_log_alpha_dev_g(1,nFisheries-2);
  init_ivector ph_log_beta_dev_g(1,nFisheries-2);
  // Process std deviations for tv selectivity
  init_number priorSD_alpha_dev;
  init_number priorSD_beta_dev;
  // deviations for sex dependent sel at length 
  init_ivector ph_alpha_sexdev_g(1,nFisheries);
  init_ivector ph_beta_sexdev_g(1,nFisheries);
  init_number priorSD_alpha_sexdev;
  init_number priorSD_beta_sexdev;

  // Read in tv selectivity parameter priors
  // alpha prior means
  init_vector p_alpha_1t(firstSelPrior_1,lastSelPrior_1);
  init_vector p_alpha_2t(firstSelPrior_2,lastSelPrior_2);
  init_vector p_alpha_3t(firstSelPrior_3,lastSelPrior_3);
  // gear specific beta parameter prior means
  init_vector p_beta_1t(firstSelPrior_1,lastSelPrior_1);
  init_vector p_beta_2t(firstSelPrior_2,lastSelPrior_2);
  init_vector p_beta_3t(firstSelPrior_3,lastSelPrior_3);
  // gear specific alpha parameter prior sd
  init_vector sd_alpha_1t(firstSelPrior_1,lastSelPrior_1);
  init_vector sd_alpha_2t(firstSelPrior_2,lastSelPrior_2);
  init_vector sd_alpha_3t(firstSelPrior_3,lastSelPrior_3);
  // gear specific beta parameter prior sd
  init_vector sd_beta_1t(firstSelPrior_1,lastSelPrior_1);
  init_vector sd_beta_2t(firstSelPrior_2,lastSelPrior_2);
  init_vector sd_beta_3t(firstSelPrior_3,lastSelPrior_3);
  // arrange into a matrix for easy pulling later (in procedure section)

  init_number checkSelPriors;
  !! if(checkSelPriors != 999) cout << "Bad Check in selPriors!" << endl;
  !! if(checkSelPriors != 999) exit(1);


  // Selectivity parameter priors from tag release-recovery data
  !! ad_comm::change_datafile_name("opModSelBlocks.dat");
  // years that priors cover (33, 51)
  init_int nSelBlocks_g1;
  init_int nSelBlocks_g2;
  init_int nSelBlocks_g3;
  ivector nSelBlocks(1,3);
  !! nSelBlocks(1) = nSelBlocks_g1;
  !! nSelBlocks(2) = nSelBlocks_g2;
  !! nSelBlocks(3) = nSelBlocks_g3;
  // Need some way to control for zero blocks, but still keep this
  // somewhat general...
  // set a phase vector
  init_ivector ph_alpha_blockDev(1,3);
  init_ivector ph_beta_blockDev(1,3);

  // prior SDs
  init_vector blockDevPriorSd(1,3);

  // gear specific selectivity block start and end time steps
  // Trap
  init_ivector selBlockStart_g1(1,nSelBlocks_g1);
  init_ivector selBlockEnd_g1(1,nSelBlocks_g1);

  // LL
  init_ivector selBlockStart_g2(1,nSelBlocks_g2);
  init_ivector selBlockEnd_g2(1,nSelBlocks_g2);

  // LL
  init_ivector selBlockStart_g3(1,nSelBlocks_g3);
  init_ivector selBlockEnd_g3(1,nSelBlocks_g3);

  init_number checkSelBlock;
  !! if(checkSelBlock != 999) cout << "Bad Check in selBlocks!" << endl;
  !! if(checkSelBlock != 999) exit(1);

  // Switch to proportion directed ctl file
  !! ad_comm::change_datafile_name("opModPropDirected.dat");
  // Model pDirected?
  init_ivector pDirOn(1,nFisheries-2);

  // Times that proportion directed is estimated
  init_ivector pDir_t1_in(1,nFisheries-2);
  ivector pDir_t1(1,nFisheries-2);
  !! pDir_t1 = pDir_t1_in;
  init_ivector pDir_t2_in(1,nFisheries-2);
  ivector pDir_t2(1,nFisheries-2);
  !! pDir_t2= pDir_t2_in;
  // scale parameter for reducing pDir
  init_number pDirScale;
  init_vector selAlphaDirDev_base(1,nFisheries-2);

  // estimation phases
  init_ivector ph_logit_pDir_in(1,nFisheries-2);
  ivector ph_logit_pDir(1,nFisheries-2);
  !! ph_logit_pDir = ph_logit_pDir_in;
  init_ivector ph_log_selAlphaDir_in(1,nFisheries-2);
  ivector ph_log_selAlphaDirDev(1,nFisheries-2);
  !! ph_log_selAlphaDirDev = ph_log_selAlphaDir_in;
  init_ivector ph_log_selBetaDir_in(1,nFisheries-2);
  ivector ph_log_selBetaDirDev(1,nFisheries-2);
  !! ph_log_selBetaDirDev = ph_log_selBetaDir_in;

  // Prior SDs for deviations
  init_number priorSD_logit_pDir;
  init_number priorSD_seldev_pDir;

  // Penalty for positive undirected catch
  // penaliseUndir (switch for penalising undirected catch)
  init_int penaliseUndir;
  // Undirected catch penalty SD
  init_number undirK;

  init_number checkPDir;
  !! if(checkPDir != 999) cout << "Bad Check in pDir!" << endl;
  !! if(checkPDir != 999) exit(1);

  // Reporting rate by fishery
  !! ad_comm::change_datafile_name("opModReportRate.dat");
  // Length at 50% reporting
  init_vector lenRep95(1,nFisheries-2);
  init_vector lenRep50(1,nFisheries-2);
  init_ivector phzLenRep50_g(1,nFisheries-2);
  init_ivector phzLenRep95_g(1,nFisheries-2);

  init_number checkRepRate;
  !! if(checkPDir != 999) cout << "Bad Check in report rates!" << endl;
  !! if(checkPDir != 999) exit(1);


  // Control parameters -- switch to estimation control file
  !! ad_comm::change_datafile_name("opModControl.ctl");
  // Bark to screen?
  init_int verbose;
  // Use Baranov solution for F (1) or treat Fs as parameters (0)
  init_int useBaranov;
  int ph_logF;
  !! ph_logF = 1;
  !! if( useBaranov ) ph_logF = -1;
  // Baranov iterations
  init_int baranovIter;
  int baranov_time;
  init_vector baranovSteps(1,nFisheries);
  // Objective function scaling for all but last phase
  init_number objFunc_scale;
  // Objective function scaling for last phase
  init_number objFunc_scale_last;
  // ...for sd_phase
  init_number objFunc_scale_sd;
  // Size limits
  init_number sizeLim;
  init_int sizeLimYear;
  init_vector dM(1,nFisheries);

  // Growth parameters: size-age calculated in PRELIM_CALCS
  init_number lInf_m;
  init_number lInf_f;
  init_number vonK_m;
  init_number vonK_f;
  init_number sigmaL_m;
  init_number sigmaL_f;
  number sigmaL;

  init_number L1_m;
  init_number L1_f;
  init_vector wt_a(1,2);
  init_vector wt_b(1,2);

  // Maturity parameter
  init_number aMat50;
  init_number aMat95;

  // # Estimation phase numbers for model parameters
  // Unfished biomass
  init_int ph_log_avgR;
  // Steepness
  init_int ph_logit_h;
  init_number lb_logit_h;
  init_number ub_logit_h;
  // beta prior: alpha, beta for steepness
  init_vector prior_h(1,2);
  // Unfished female SSB (only one can be active, avgR or SSB0)
  init_int ph_log_SSB0;
  init_number lb_log_SSB0;
  init_number ub_log_SSB0;

  // Natural mortality rate
  init_int ph_log_M;
  // Catchability
  init_int ph_log_q;
  //!! cout << "ph_log_M = " << ph_log_M << endl;
  // Recruitment deviations
  init_int phRecDevs;
  init_int firstRecDev;
  // Last rec devs to estimate
  init_int lastRecDev;

  // Selectivity 
  // Selectivity types: 1=ASYM, 2=NORM, 3=GAMMA
  init_ivector selType(1,nFisheries);

  // selectivity parameter phases
  init_ivector ph_log_alpha_g(1,nFisheries);
  init_vector lb_log_alpha_g(1,nFisheries);
  init_vector ub_log_alpha_g(1,nFisheries);

  init_ivector ph_log_beta_g(1,nFisheries);
  init_vector lb_log_beta_g(1,nFisheries);
  init_vector ub_log_beta_g(1,nFisheries);

  // Highgrading switch
  init_ivector useHighgrading(1,nFisheries-2);
  // Phase for high-grading function
  init_int ph_log_hg50;

  // Age comp likelihood type
  init_int ageLikeType;

  // Std dev on rec and random walks in selectivity pars
  init_number sigma_R;
  init_vector sigma_alpha(1,nFisheries);
  init_vector sigma_beta(1,nFisheries);

  // Lambda par for exponential prior on tauRel and tauIndex
  init_vector tauRelLambda(1,nFisheries-2);
  init_vector tauIndexLambda(1,nIndexSeries);
  init_vector tauAgeLambda(1,nFisheries-1);

  // Prior mean and sd on natural mortality
  init_vector prior_mean_M(1,2);
  init_vector prior_sd_M(1,2);

  // random walk std dev on Fs
  init_number priorSD_F;
  // Error tolerance on landed catch
  init_number catError;

  // Fixed recruitment deviations to
  // try to follow discard data
  init_int nFixedRecDev;
  init_vector tFixedRecDev(1,nFixedRecDev);
  init_vector logFixedRecDev(1,nFixedRecDev);

  init_vector fracYearFleet(1,nFisheries);

  init_number checkCtl;
  !! if(checkCtl != 999) cout << "Bad Check in opModControl!" << endl;
  !! if(checkCtl != 999) exit(1);

  !! ad_comm::change_datafile_name("opModRelDevs.dat");

  // a penalty on the deviations from
  // release observations in the trawl fishery
  init_vector relDevInitYr(1,nFisheries-2);
  init_vector relDevLastYr(1,nFisheries-2);
  init_number relDevPenalty;
  //!! cout << relDevInitYr << endl;
  //!! cout << relDevPenalty << endl;
  //!! exit(1);
  // Check
  init_number checkRelDevs;
  !! if(checkRelDevs != 999) cout << "Bad Check in relDevs!" << endl;
  !! if(checkRelDevs != 999) exit(1);


  //  ------- END CONTROL FILE INPUT ------------//
  
  // Maturity-at-age
  vector matAge_f(1,plusGroupAge);
  vector lenAge_m(1,plusGroupAge);
  vector lenAge_f(1,plusGroupAge);
  vector wtAge_m(1,plusGroupAge);
  vector wtAge_f(1,plusGroupAge);

  // Catch by year/fishery
  matrix Ctg(1,nT,1,nFisheries); 
  // discards by fisher/year
  matrix obs_relCtg(1,nT,1,nFisheries); 
  matrix sublegalDtg(1,nFisheries,1,nT);

  // Proportion retained-at-age
  matrix pRet_m(1,nT,1,plusGroupAge);
  matrix pRet_f(1,nT,1,plusGroupAge);

  vector validObs_idx(1,nIndexSeries);
  vector validObs_rel(1,nFisheries-2);

  int nSteps;
  // tracking number of function evaluations
  int neval;
  
//*******************************************************************/
PARAMETER_SECTION
  objective_function_value objFunc;
  // Unfished female spawning biomass, log-scale
  init_bounded_number log_SSB0(lb_log_SSB0,ub_log_SSB0,ph_log_SSB0);
  !! cout << "log_SSB0 = " << log_SSB0 << endl;
  // Recruitment steepness
  init_bounded_number logit_h(lb_logit_h,ub_logit_h,ph_logit_h);
  !! cout << "logit_h = " << logit_h << endl;
  // Avg recruitment
  init_bounded_number log_avgR(0.7,2.,ph_log_avgR);
  !! cout << "log_avgR = " << log_avgR << endl;
  // Natural mortality
  init_bounded_vector log_M(1,2,-5,-1,ph_log_M);
  !! cout << "log_M = " << log_M << endl;
  
  // Fs by fishery in first year
  init_bounded_vector log_Fg1(1,nFisheries-2,-10.0,-0.7,ph_logF);
  !! cout << "log_Fg1 = " << log_Fg1 << endl;
  // F deviations for random walk
  init_bounded_vector log_Fdevs_1t(10,nT,-5.,5.,ph_logF);
  init_bounded_vector log_Fdevs_2t(2,nT,-5.,5.,ph_logF);
  init_bounded_vector log_Fdevs_3t(2,nT,-5.,5.,ph_logF);
  init_bounded_vector logit_rho(1,nFisheries-2,-10.,10.,-1);
  // Recruitment deviations
  init_bounded_vector recDevs(firstRecDev,lastRecDev,-3.,3.,phRecDevs);
  !! cout << "recDevs = " << recDevs << endl;
  // Selectivity parameters
  init_bounded_number_vector log_alpha_g(1,nFisheries,lb_log_alpha_g,ub_log_alpha_g,ph_log_alpha_g);
  //!! cout << "log_alpha_g = " << log_alpha_g << endl;
  init_bounded_number_vector log_beta_g(1,nFisheries,lb_log_beta_g,ub_log_beta_g,ph_log_beta_g);
  //!! cout << "log_beta_g = " << log_beta_g << endl;
  init_bounded_vector_vector log_alpha_dev_gt(1,nFisheries-2,firstSelPrior_g,lastSelPrior_g,-2,2,ph_log_alpha_dev_g);
  init_bounded_vector_vector log_beta_dev_gt(1,nFisheries-2,firstSelPrior_g,lastSelPrior_g,-1.,1.,ph_log_beta_dev_g);
  !! cout << "ph_log_alpha_dev_g = " << ph_log_alpha_dev_g << endl;
  !! cout << "ph_log_beta_dev_g = " << ph_log_beta_dev_g << endl;
  // Time varying q
  init_bounded_vector lnq(1,nIndexSeries,-2,2,ph_log_q);
  init_bounded_vector_vector log_qdev_gt(1,nIndexSeries,firstqdev_g,lastqdev_g,-1.,1.,ph_log_qdev_g);
  // High-grading function parameters

  init_bounded_vector log_hg50(1,nFisheries-2,4.0,4.1,ph_log_hg50);
  !! cout << "log_hg50 = " << log_hg50 << endl;
  
  // Selectivity block deviations
  // alpha (L50 or mean)
  init_bounded_vector_vector log_alpha_blockDev_g(1,nFisheries-2,1,nSelBlocks,-2,2,ph_alpha_blockDev);
  //!! cout << "log_alpha_blockDev_g = " << log_alpha_blockDev_g << endl;
  init_bounded_vector_vector log_beta_blockDev_g(1,nFisheries-2,1,nSelBlocks,-2,2,ph_beta_blockDev);
  
  // sex structured selectivity
  init_bounded_number_vector log_alpha_sexdev_g(1,nFisheries,-2,2,ph_alpha_sexdev_g);
  init_bounded_number_vector log_beta_sexdev_g(1,nFisheries,-2,2,ph_beta_sexdev_g);

  // Proportion directed in each year
  init_bounded_number_vector logit_pDir_g(1,nFisheries-2,-10.,10.,ph_logit_pDir);
  init_bounded_number_vector log_selAlphaDirDev_g(1,nFisheries-2,-5,5,ph_log_selAlphaDirDev);
  init_bounded_number_vector log_selBetaDirDev_g(1,nFisheries-2,-5,5,ph_log_selBetaDirDev);

  !! cout << "logit_pDir_g = " << logit_pDir_g << endl;
  !! cout << "log_selAlphaDirDev_g = " << log_selAlphaDirDev_g << endl;
  !! cout << "log_selBetaDirDev_g = " << log_selBetaDirDev_g << endl;

  init_bounded_number_vector log_epsLenRep50_g(1,nFisheries-2,-10.,10.,phzLenRep50_g);
  init_bounded_number_vector log_epsLenRep95_g(1,nFisheries-2,-10.,10.,phzLenRep95_g);

  // Matrix of reporting rates
  matrix reportRate_ga_m(1,nFisheries,1,plusGroupAge);
  matrix reportRate_ga_f(1,nFisheries,1,plusGroupAge);


  // Derived variables
  number B0;
  sdreport_number SSB0;
  sdreport_number R0;
  sdreport_number D2015; // depletion 2015
  sdreport_number B2015;
  // likeprof_number SSB0;
  //!! SSB0.set_stepnumber(20);
  //!! SSB0.set_stepsize(0.2);
  sdreport_number h;
  sdreport_number M_m;
  sdreport_number M_f;
  sdreport_number LHR2015;
  sdreport_number SLHR2015;

  // legal and sublegal biomass
  sdreport_vector legalBt(1,nT+1);
  sdreport_vector subLegalBt(1,nT+1);
  // landed catch (input)
  matrix landedCtg(1,nT,1,nFisheries);
  matrix undirLandCtg(1,nT,1,nFisheries);
  matrix dirLandCtg(1,nT,1,nFisheries);
  matrix undirRelCtg(1,nT,1,nFisheries);
  matrix dirRelCtg(1,nT,1,nFisheries);

  // Exploitation rates
  vector subLegalHR(1,nT); 
  vector legalHR(1,nT);

  matrix log_Fdevs_gt(1,nFisheries-2,1,nT);
  vector rho(1,nFisheries-2);
  number avgR;
  number phiSSB;
  number bh_a;
  number bh_b;
  matrix ch_sigmaF(1,3,1,3);//Choelesky decomp of sigmaF
  matrix sigmaF(1,3,1,3);
  vector row_means_F(1,3);

  // Selectivity parameters - gear, time, gender
  matrix alpha_gt_m(1,nFisheries,1,nT);
  matrix beta_gt_m(1,nFisheries,1,nT);
  matrix alpha_gt_f(1,nFisheries,1,nT);
  matrix beta_gt_f(1,nFisheries,1,nT);
  // Selectivity alpha pars for undirected Fs
  matrix alphaUndir_gt_m(1,nFisheries,1,nT);
  matrix alphaUndir_gt_f(1,nFisheries,1,nT);
  matrix betaUndir_gt_m(1,nFisheries,1,nT);
  matrix betaUndir_gt_f(1,nFisheries,1,nT);
  vector alpha_g1(1,nFisheries);
  vector beta_g1(1,nFisheries);
  

  // Selectivity by gear, time, age, and gender
  3darray sel_gta_m(1,nFisheries,1,nT,1,plusGroupAge);
  3darray sel_gta_f(1,nFisheries,1,nT,1,plusGroupAge);

  3darray selUndir_gta_m(1,nFisheries,1,nT,1,plusGroupAge);
  3darray selUndir_gta_f(1,nFisheries,1,nT,1,plusGroupAge);

  // Directed and undirected catch


  // Time-varying catchability
  matrix  lnq_it(1,nIndexSeries,1,nT);

  sdreport_vector  SSBt(1,nT+1);
  vector  Rt(1,nT+1);
  vector  omega_t(1,nT);
  matrix  Nta_m(1,nT+1,1,plusGroupAge);
  matrix  Nta_f(1,nT+1,1,plusGroupAge);
  matrix  Bta_m(1,nT+1,1,plusGroupAge);
  matrix  Bta_f(1,nT+1,1,plusGroupAge);
  
  matrix  Zta_m(1,nT+1,1,plusGroupAge);
  matrix  Zta_f(1,nT+1,1,plusGroupAge);
  matrix  pCtg(1,nT,1,nFisheries-2);

  matrix  log_Ftg(1,nT,1,nFisheries-2);
  matrix  Ftg(1,nT,1,nFisheries-2);
  // True age proportions
  3darray uCgta_m(1,nAgeSeries,1,nT,minAge,plusGroupAge);
  3darray uCgta_f(1,nAgeSeries,1,nT,minAge,plusGroupAge);
  // Predicted age proportions allowing for ageing error
  3darray predObsProp_m(1,nAgeSeries,1,nT,minAge,maxAge);
  3darray predObsProp_f(1,nAgeSeries,1,nT,minAge,maxAge);

  // predicted releases by fishery
  matrix relCtg(1,nT,1,nFisheries); 
  // Proportion released-at-age by fishery/year
  3darray pR_gta_m(1,nFisheries,1,nT,1,plusGroupAge);
  3darray pR_gta_f(1,nFisheries,1,nT,1,plusGroupAge);
  // matrix of proportions for directed F and undirected 
  // F in commercial fisheries
  matrix pDir_tg(1,nT,1,nFisheries-2);

  // Objective function quantities
  matrix expBit(1,nFisheries,1,nT);
  vector indexLikelihood(1,nIndexSeries);
  number ssQ;
  number ssR;
  vector ss(1,nIndexSeries);
  vector tauSquareIndex(1,nIndexSeries);
  
  vector ageLikelihood_m(1,nAgeSeries);
  vector tauSquareAges_m(1,nAgeSeries);

  vector ageLikelihood_f(1,nAgeSeries);
  vector tauSquareAges_f(1,nAgeSeries);

  vector releaseLikelihood(1,nFisheries);
  vector tauSquareReleases(1,nFisheries);

  number mnLL_m;
  number mnLL_f;

  number dataLikelihood;
  number mPrior;
  number ssbPrior;
  number selPrior;
  number recPrior;
  number fLike;
  number catLike;
  number dirCatPrior;
  number undirCatPrior;
  number hPrior;
  number pDirPrior;
  number repRatePrior;
  vector qdevPrior(1,nIndexSeries);

  number idxLam;

  // Variables for pRet integration
  number x;
  number lim1;
  number lim2;
  
  // Matrix for collecting sel priors
  !! int firstSelPrior = min(firstSelPrior_g);
  !! int lastSelPrior = max(lastSelPrior_g);
  matrix p_alpha_gt(1,3,firstSelPrior,lastSelPrior);
  matrix p_beta_gt(1,3,firstSelPrior,lastSelPrior);
  matrix sd_alpha_gt(1,3,firstSelPrior,lastSelPrior);
  matrix sd_beta_gt(1,3,firstSelPrior,lastSelPrior);
  
  // mcmc declarations
  
  // matrix to hold indicators to tell whether the prop in that 
  // bin is too low and needs to be added to accumulator bin.
  3darray iz_m(1,nAgeSeries,1,nT,minAge,maxAge);
  //3darray iz_m(1,nAgeSeries,1,nT,3,35);
  3darray iz_f(1,nAgeSeries,1,nT,minAge,maxAge);
  // accumulator bin for predicted props
  matrix pred_accBin_m(1,nAgeSeries,1,nT);
  matrix pred_accBin_f(1,nAgeSeries,1,nT);

  !! cout << "Parameter Section Complete" << endl;
  
//*******************************************************************/
RUNTIME_SECTION
      //convergence_criteria .1, .1, .001
//*******************************************************************/
GLOBALS_SECTION
  #include <admodel.h>
  #include <time.h>

  void solveBaranov( const int& t, const int& iter);
  // Flag to control whether header written to mcmc output file.
  int mcmcHeader = 0;
  
  // Flag to control which parameters are done in MCMC phase.
  int mcmcFlag = 1;

  //ofstream mcout("mcout.dat");

  time_t start,finish;
  long hour,minute,second;
  double elapsed_time;

//*******************************************************************/    
TOP_OF_MAIN_SECTION
  time(&start);
  arrmblsize=20000000;
  gradient_structure::set_CMPDIF_BUFFER_SIZE(25000000);
  gradient_structure::set_GRADSTACK_BUFFER_SIZE(1000000);
//*******************************************************************/   
PRELIMINARY_CALCS_SECTION
  for ( int t=1;t<=landCatchMatrix.rowmax();t++ )
  {
    // katch: Rows are different fisheries.  Columns are tSteps.
    for ( int i=3;i<=landCatchMatrix.colmax();i++ )
    {
        Ctg(t,i-2) = landCatchMatrix(landCatchMatrix(t,2),i)/1000.;
      // converted catch units (t) to model units (1,000 t)
        obs_relCtg(t,i-2) = releaseMatrix(releaseMatrix(t,2),i)/1000.;      
    }
  }

  // Length-at-age
  lenAge_m = lInf_m+(L1_m-lInf_m)*exp( -vonK_m*(ages-1.));
  lenAge_f = lInf_f+(L1_m-lInf_f)*exp( -vonK_f*(ages-1.));
  // Weight-at-age
  wtAge_m  = wt_a(1)*pow(lenAge_m,wt_b(1));
  wtAge_f  = wt_a(2)*pow(lenAge_f,wt_b(2));
  
  // Maturity-at-age females
  matAge_f = 1./(1.+ exp( -log(19.)*(ages-aMat50)/(aMat95-aMat50)));

  // Proportion released-at-age function
  pR_gta_m.initialize(); pR_gta_f.initialize();
  pRet_m.initialize(); pRet_f.initialize();
  pRet_m = 1.; pRet_f = 1.;

  nSteps = 5;
  // Implement 55 cm comm. size limit in sizeLimYear
  for( int t=sizeLimYear; t<=nT; t++ )
  {
    for( int a=1; a<=plusGroupAge; a++ )
    {
      // Proportion retained-at-age by integrating the normal
      // pdf of length for each age and taking the proportion
      // above the size limit
      funnel_dvariable pL;
      funnel_dvariable pU;
      ad_begin_funnel();
      x      = lenAge_m(a);
      sigmaL = sigmaL_m;
      lim1 = 0.;
      lim2 = sizeLim;
      pL    = adromb( &model_parameters::fz, lim1, lim2, nSteps);
      lim1 = sizeLim;
      lim2 = lInf_m*(1. + 4.*sigmaL_m);
      pU    = adromb( &model_parameters::fz, lim1, lim2, nSteps);
      pRet_m(t,a) = value( pU/(pL+pU) );

      x    = lenAge_f(a);
      sigmaL = sigmaL_f;
      lim1 = 0.;
      lim2 = sizeLim;
      pL    = adromb( &model_parameters::fz, lim1, lim2, nSteps);
      lim1 = sizeLim;
      lim2 = lInf_f*(1. + 4.*sigmaL_f);
      pU    = adromb( &model_parameters::fz, lim1, lim2, nSteps);
      pRet_f(t,a) = value( pU/(pL+pU) );
    }
  }
  //cout << row(pRet_m,sizeLimYear+1) << endl;
  //cout << row(pR_gta_m(1),sizeLimYear+1) << endl;
  //cout << row(pR_gta_f(1),sizeLimYear+1) << endl;
  //exit(1);
  iz_m.initialize();
  iz_f.initialize();
  for( int i=1; i<=nAgeSeries; i++ )
  {
    iz_f(i) = 1.; iz_m(i) = 1.;
    for( int t=1; t<=nT; t++ )
    {
      // MALES--------------------------
      // check whether this year has data
      accBin_m(i,t) = 0.;
      if( firstAge_m(i,t) > 0 )
      {
        // vector of all age props for this year
        dvector obsPat_m  = row(ageObsProp_m(i),t)(firstAge_m(i,t),lastAge_m(i,t));
        for( int a=firstAge_m(i,t); a<=lastAge_m(i,t); a++ )
        {
          if( obsPat_m(a) <= prop_threshold )
          {
            // add obsPat to accBin
            accBin_m(i,t) += obsPat_m(a);
            // set indicator to 0 so (1) this bin is added to accumulator in predicted
            // and (2) multiplying obs by iz will eliminate these values in the 
            // likelihood and residual count
            iz_m(i,t,a) = 0.;
          }

        } // end a-loop
      }
      // FEMALES--------------------------
      // check whether this year has data
      accBin_f(i,t) = 0.;
      if( firstAge_f(i,t) > 0 )
      {
        // vector of all age props for this year
        dvector obsPat_f  = row(ageObsProp_f(i),t)(firstAge_f(i,t),lastAge_f(i,t));
        for( int a=firstAge_f(i,t); a<=lastAge_f(i,t); a++ )
        {
          if( obsPat_f(a) < prop_threshold )
          {
            // add obsPat to accBin
            accBin_f(i,t) += obsPat_f(a);
            // set indicator to 0 so (1) this bin is added to accumulator in predicted
            // and (2) multiplying obs by iz will eliminate these values in the 
            // likelihood and residual count
            iz_f(i,t,a) = 0;
          }

        } // end a-loop
      }
    }     // end t-loop
  }       // end i-loop

  

  neval = 0;

//*******************************************************************/
PROCEDURE_SECTION
  
  // make sel prior matrix
  makeSelPriorMtx();
  if (verbose) cout << "makeSelPriorMtx() done..." << endl;

  // Calculate proportion directed
  calc_pDir_tg();
  if (verbose) cout << "calc_pDir_tg() done..." << endl;

  // Reporting rate functions
  calcRepRate();
  calcRepRatePrior();

  initModelParameters();
  if (verbose) cout << "initModelParameters done..." << endl;
  popInit_SSB0();
  if (verbose) cout << "popInit done..." << endl;
  popDynamics_MF();
  if (verbose) cout << "popDynamics done..." << endl;  
  calc_age_likelihood();
  if (verbose) cout << "age_likelihood = " <<ageLikelihood_m<<" "<<ageLikelihood_f<< endl;
  calc_index_likelihood();
  if (verbose) cout << "index_likelihood = " <<indexLikelihood<< endl;
  calc_release_likelihood();
  if (verbose) cout << "release_likelihood = " <<releaseLikelihood<<endl;
  if( active(recDevs) )
    calc_rec_prior();
  if (verbose) cout << "rec_prior = " <<recPrior<< endl;
  
  calc_sel_prior();
  if (verbose) cout << "sel_prior = " <<selPrior<< endl;
  if( !useBaranov )
  {
    calc_F_likelihood();
    calc_catch_likelihood();
    if (verbose) cout << "catLike = " <<catLike<< endl;
  }
  if (verbose) cout << "fLike = " <<fLike<< endl;

  if( active(log_M) )
    calc_M_prior();
  
  calc_pDirPrior();
  
  calc_SSB0_prior();
  
  for(int i = 1; i<= nIndexSeries; i++)
  {
    qdevPrior(i) = 0;
    if(active(log_qdev_gt(i)))
      for( int t = firstqdev_g(i); t <= lastqdev_g(i); t++)
        if( log_qdev_gt(i,t) != 0 )
          qdevPrior(i) += 0.5*pow( log_qdev_gt(i,t)/priorSD_qdev(i), 2 );
    
  } // end i loop
  
  objFunc  = 0.;  
  objFunc  = sum( elem_prod(idxLikeWeight,indexLikelihood ) );
  objFunc += sum( elem_prod(ageLikeWeight_m,ageLikelihood_m) ) 
          +  sum( elem_prod(ageLikeWeight_f,ageLikelihood_f) );
  objFunc += sum(releaseLikelihood);
  // Save data-only likelihood
  dataLikelihood = objFunc;
  // add priors to objFunc
  objFunc += fLike;
  objFunc += catLike;
  objFunc += mPrior + recPrior + selPrior;
  objFunc += 100.*ssbPrior;
  objFunc += hPrior;
  objFunc += sum(qdevPrior);
  objFunc += pDirPrior;
  objFunc += repRatePrior;
  if( last_phase() )
    objFunc *= objFunc_scale_last;
  else
    objFunc *= objFunc_scale;
  //cout << objFunc << endl;

  if (verbose) cout << "obj_func done..." << endl;
  // mcmc output when in mceval phase  
  if ( mceval_phase() )
  {
    ofstream mcout("mcout.dat", ios::app );
    // On first call, build header for MCMC file output 
    if ( mcmcHeader == 0 )
    {
      int i = 1;
      
      mcout << "SSB0 h M_m M_f";
      mcout << " projSSB projLegalB projSublegalB projDep legalHR.T sublegalHR.T";
      
      for ( i=1; i<=nT+1; i++ )
        mcout << " spawnB" << i;
        
      for ( i=firstRecDev; i<=lastRecDev; i++ )
        mcout << " recDev" << i;
        
      for ( i=1; i<=nIndexSeries; i++ )
        mcout << " tauIndex" << i;
        
      for ( i=1; i<=nAgeSeries; i++ )
        mcout << " tauAges_m" << i;
        
      for ( i=1; i<=nAgeSeries; i++ )
        mcout << " tauAges_f" << i;

      for ( i=1; i<=nFisheries; i++ )
        mcout << " tauReleases" << i;

      for ( i=1; i<=nIndexSeries; i++ )
        mcout << " qg" << i;      
      
      mcout << endl;      
      
      mcmcHeader = 1;
    }
    mcout << SSB0 << " " << h << " " << M_m << " " << M_f << " ";
    mcout << SSBt(nT+1) << " " << legalBt(nT+1) << " " << subLegalBt(nT+1) << " "
          << SSBt(nT+1)/SSBt(1) << " " << legalHR(nT) << " " << subLegalHR(nT) << " " ;
    mcout << SSBt;
    mcout << recDevs;
    mcout << sqrt(tauSquareIndex);
    mcout << sqrt(tauSquareAges_m);
    mcout << sqrt(tauSquareAges_f);
    mcout << sqrt(tauSquareReleases);
    mcout << exp(lnq);   
    mcout << endl; 
  }
//*******************************************************************/
BETWEEN_PHASES_SECTION

//*******************************************************************/
FUNCTION dvariable fz(const dvariable& z)
  double tmp1; double tmp;
  double mx;
  tmp1 = value( 1./sqrt(2.*3.141593*pow(x*sigmaL,2.)) );
  mx   = value(z);
  tmp  = value( mfexp( -0.5*pow(x-mx,2)/pow(x*sigmaL,2)) );
  //tmp  *= tmp1;
  return tmp;
//*******************************************************************/
//*******************************************************************/ 
FUNCTION calc_SSB0_prior
  ssbPrior = -log(1./SSB0);
FUNCTION calc_M_prior
  mPrior = 0.;
  mPrior += pow(M_m - prior_mean_M(1),2.)/pow(prior_sd_M(1),2) + 
            pow(M_f - prior_mean_M(2),2.)/pow(prior_sd_M(2),2);     

FUNCTION calcRepRate
  for( int g = 1; g <= nFisheries - 2; g ++ )
  {
    dvariable tmpLenRep50;
    dvariable tmpLenRep95;
    dvariable step;
    
    tmpLenRep50 = lenRep50(g) * mfexp(log_epsLenRep50_g(g));
    tmpLenRep95 = lenRep95(g) * mfexp(log_epsLenRep95_g(g));
    step=  tmpLenRep50 - tmpLenRep95 ;
    
    for( int a = 1; a <= plusGroupAge; a++)
    {
      reportRate_ga_m(g,a) = 1. / (1. + exp( -(log(19)/step) * ( tmpLenRep50 - lenAge_m(a) ) ));
      reportRate_ga_f(g,a) = 1. / (1. + exp( -(log(19)/step) * ( tmpLenRep50 - lenAge_f(a) ) ));
    }
  }

FUNCTION calcRepRatePrior
  repRatePrior = sum(pow( log_epsLenRep95_g,2 ) + pow( log_epsLenRep50_g,2 ) );



FUNCTION calc_pDirPrior
  pDirPrior = 0.;
  // pDirPrior is centred at -3 in first year (5% directed)
  for( int g = 1; g <= nFisheries-2; g++ )
  {
    if( (ph_logit_pDir(g) > 0) &  (logit_pDir_g(g) != 0) )
      pDirPrior += 0.5*pow(((logit_pDir_g(g))/priorSD_logit_pDir),2);

    if( (ph_log_selAlphaDirDev(g) > 0) & (log_selAlphaDirDev_g(g) != 0) )
      pDirPrior += 0.5 * pow((log_selAlphaDirDev_g(g))/priorSD_seldev_pDir,2.0);

    if( (ph_log_selBetaDirDev(g) > 0) & (log_selBetaDirDev_g(g) != 0) )
      pDirPrior += 0.5 * pow((log_selBetaDirDev_g(g))/priorSD_seldev_pDir,2.0);
  }
  //for( int t = pDir_t1 + 1; t <= pDir_t2; t++)
  //{
    //// Take difference of subsequent points
    //dvariable tmpDiff = logit_propDirected_t(t) - logit_propDirected_t(t-1);
    //// treat logit pDir as a random walk in logit space
    //if(active(logit_propDirected_t) & tmpDiff > 0)
      //pDirPrior += 0.5 * pow((tmpDiff)/priorSD_logit_pDir,2.0);
    //// And now add selAlphapDirDevs
    //if(active(log_selAlphaDirDev_t) & (log_selAlphaDirDev_t(t) > 0) )
      //pDirPrior += 0.5 * pow((log_selAlphaDirDev_t(t))/priorSD_seldev_pDir,2.0);
    //if(active(log_selBetaDirDev_t) & (log_selBetaDirDev_t(t) > 0))
      //pDirPrior += 0.5 * pow((log_selBetaDirDev_t(t))/priorSD_seldev_pDir,2.0);
  //}

//*******************************************************************/ 

FUNCTION makeSelPriorMtx
  //cout << "Pulling together sel priors" << endl;
  // Loop to pull together the sel priors
  for( int gIdx = 1; gIdx <= 3; gIdx ++)
    for( int tIdx = firstSelPrior_g(gIdx); tIdx <= lastSelPrior_g(gIdx); tIdx ++)
    {
      //cout << "Looping, tIdx == " << tIdx << endl;
      switch( gIdx )
      { 
        case 1: // Trap
          p_alpha_gt(gIdx,tIdx) = p_alpha_1t(tIdx);
          p_beta_gt(gIdx,tIdx) = p_beta_1t(tIdx);
          sd_alpha_gt(gIdx,tIdx) = sd_alpha_1t(tIdx);
          sd_beta_gt(gIdx,tIdx) = sd_beta_1t(tIdx);
        break;
        case 2: // LL 
          p_alpha_gt(gIdx,tIdx) = p_alpha_2t(tIdx);
          p_beta_gt(gIdx,tIdx) = p_beta_2t(tIdx);
          sd_alpha_gt(gIdx,tIdx) = sd_alpha_2t(tIdx);
          sd_beta_gt(gIdx,tIdx) = sd_beta_2t(tIdx);
        break;
        case 3: // Trawl
          p_alpha_gt(gIdx,tIdx) = p_alpha_3t(tIdx);
          p_beta_gt(gIdx,tIdx) = p_beta_3t(tIdx);
          sd_alpha_gt(gIdx,tIdx) = sd_alpha_3t(tIdx);
          sd_beta_gt(gIdx,tIdx) = sd_beta_3t(tIdx);
        break;
      }
    }

FUNCTION calc_pDir_tg
  // Computes the proportion directed for each gear type at
  // each time step
  pDir_tg=1.0;

  // We can improve this with a sub-vector of the pDir mtx
  for( int g = 1; g <= nFisheries-2; g++)
    if( pDirOn(g) == 1 )
      for( int t = pDir_t1(g); t <= pDir_t2(g); t++)
      {
        pDir_tg(t,g) = pDirScale / ( 1 + exp(-1. * logit_pDir_g(g)));
      }

  // cout << "pDir_tg = " << pDir_tg << endl;

FUNCTION calc_pR_gta
  // Computes the proportion of catch-at-age released due to combination
  // of size limit and high-grading. hg50 is the high-grading curve length
  // at 50% release
  for( int t=sizeLimYear; t<=nT; t++ )
  {
    for( int a=1; a<=plusGroupAge; a++ )
    {
      // Get pR_gta_m and ..._f here by adjusting for high-grade
      // function
      for( int g=1; g<=nFisheries-2; g++ )
      {
        pR_gta_m(g,t,a) = pRet_m(t,a);
        pR_gta_f(g,t,a) = pRet_f(t,a);

        if( useHighgrading(g) == 1)
        {
          pR_gta_m(g,t,a) *= 1./(1.+exp( -log(19.)*(lenAge_m(a)-exp(4+log_hg50(g))) ));
          pR_gta_f(g,t,a) *= 1./(1.+exp( -log(19.)*(lenAge_f(a)-exp(4+log_hg50(g))) ));
        }
      }
    }
  }
//*******************************************************************/ 
FUNCTION calc_sel_gta
  // Adding undirected selectivity, identified with
  // directed selectivity, for non-trawl fleets, in case
  // we want to add an undirected/bycatch portion of F
  // for other fleets later on. U/Undir suffix used to 
  // denote variables
  int g;
  dvar_vector tmp(1,plusGroupAge); // temp vars for orderly computations
  dvar_vector tmp_lenAge(1,plusGroupAge); // temp vars for orderly computations
  dvar_matrix tmp_alpha_gt(1,nFisheries,1,nT); // temp vars for orderly computations
  dvar_matrix tmp_beta_gt(1,nFisheries,1,nT); // temp vars for orderly computations
  dvar_matrix tmp_alphaU_gt(1,nFisheries,1,nT); // temp vars for orderly computations of undirected sel
  dvar_matrix tmp_betaU_gt(1,nFisheries,1,nT); // temp vars for orderly computations of undirected sel
  dvar_vector tmpUndir(1,plusGroupAge);
  dvar_vector log_alpha_t(1,nT); alpha_gt_m.initialize();
  dvar_vector log_beta_t(1,nT);  beta_gt_m.initialize();

  // Loop over fisheries
  for( int g=1; g<=nFisheries; g++ )
  {
    if( verbose ) cout << "Calculating sel for fleet " << g << endl;
    // Re-initialize temp vectors
    log_alpha_t.initialize();
    log_beta_t.initialize();

    // Loop over years
    for( int t=1; t<=nT; t++ )
    {
      if( t==1 )
      {
        // Initialize first value (in log-cm units)
        alpha_g1(g)     = exp(log_alpha_g(g));
        beta_g1(g)      = exp(log_beta_g(g));
        //if( selType(g) != 3 )
        alpha_g1(g) += L1_f;
        log_alpha_t(1) = log_alpha_g(g);
        log_beta_t(1)  = log_beta_g(g);        
      }
      else
      {
        // Let selectivity vary for years with prior info
        // in the commercial fleets
        if( g <= 3 )
        {
          if( (t>=firstSelPrior_g(g)) && (t<=lastSelPrior_g(g)) )
          {
            log_alpha_t(t) = log_alpha_t(t-1) + log_alpha_dev_gt(g,t);
            log_beta_t(t)  = log_beta_t(t-1) + log_beta_dev_gt(g,t);                        
          }
          else
          {
            log_alpha_t(t) = log_alpha_t(t-1);
            log_beta_t(t)  = log_beta_t(t-1);            
          }
        }
        else
        {
          // Params are constant
          log_alpha_t(t) = log_alpha_t(t-1);
          log_beta_t(t)  = log_beta_t(t-1);
        }  
      }

      // add block deviations for commercial fisheries
      if( g <= nFisheries - 2)
      {
        // cout << "In block dev calculation, g = " << g << endl;
        // Check if phz is positive, if so, add block devs
        if( ph_alpha_blockDev(g) > 0)
        {
          // cout << "phz positive for g = "<< g << endl;
          // add block deviations
          for(int b = 1; b <= nSelBlocks(g); b ++ )
          {
            // cout << "block b = "<< b << " of nB = " << nSelBlocks(g) << endl;
            // Check gear type
            // Trap
            if(g == 1)
            {
              if( t == selBlockStart_g1(b) )
              {
                log_alpha_t(t) += log_alpha_blockDev_g(g,b);
                log_beta_t(t) += log_beta_blockDev_g(g,b);
              }
              if( t == selBlockEnd_g1(b) + 1 )
              {
                log_alpha_t(t) -= log_alpha_blockDev_g(g,b);
                log_beta_t(t) -= log_beta_blockDev_g(g,b); 
              }
            }

            // Hook
            if(g == 2)
            {
              if( t == selBlockStart_g2(b) )
              {
                log_alpha_t(t) += log_alpha_blockDev_g(g,b);
                log_beta_t(t) += log_beta_blockDev_g(g,b);
              }
              if( t == selBlockEnd_g2(b) + 1 )
              {
                log_alpha_t(t) -= log_alpha_blockDev_g(g,b);
                log_beta_t(t) -= log_beta_blockDev_g(g,b); 
              }
            }

            // Trawl
            if(g == 3)
            {
              if( t == selBlockStart_g3(b) )
              {
                log_alpha_t(t) += log_alpha_blockDev_g(g,b);
                log_beta_t(t) += log_beta_blockDev_g(g,b);
              }
              if( t == selBlockEnd_g3(b) + 1 )
              {
                log_alpha_t(t) -= log_alpha_blockDev_g(g,b);
                log_beta_t(t) -= log_beta_blockDev_g(g,b); 
              }
            }

          }
        }
      }

      if( verbose) cout << "Finished with block dev calcs... " << endl;

      // Final selectivity parameters (now in cm units)
      alpha_gt_m(g,t) = mfexp( log_alpha_t(t) );
      alpha_gt_m(g,t) += L1_m;
      alphaUndir_gt_m(g,t) = alpha_gt_m(g,t);
      alpha_gt_f(g,t) = mfexp( log_alpha_t(t) + log_alpha_sexdev_g(g) );
      alpha_gt_f(g,t) += L1_f;
      alphaUndir_gt_f(g,t) = alpha_gt_f(g,t);
      
      switch( selType(g) )
      {
        case 1: // beta is asymptotic L95_step
          beta_gt_m(g,t)  = alpha_gt_m(g,t) - mfexp( log_beta_t(t) );
          beta_gt_f(g,t)  = alpha_gt_f(g,t) - mfexp( log_beta_t(t) + log_beta_sexdev_g(g) );
        break;
        case 2: // beta is normal sd
          beta_gt_m(g,t)  = mfexp( log_beta_t(t) );
          beta_gt_f(g,t)  = mfexp( log_beta_t(t) + log_beta_sexdev_g(g) );
        break;
        case 3: // beta is gamma scale parameter
          beta_gt_m(g,t)  = mfexp( log_beta_t(t) );
          beta_gt_f(g,t)  = mfexp( log_beta_t(t) + log_beta_sexdev_g(g) );
        break;

      }
      // save beta as betaUndir, might need updating later
      betaUndir_gt_m(g,t) = beta_gt_m(g,t);
      betaUndir_gt_f(g,t) = beta_gt_f(g,t);

      if( verbose ) cout << "Finished with sex dev calcs for fleet " << g << endl;
      if( g <= nFisheries-2 )
        if( (t >= pDir_t1(g)) & (t <= pDir_t2(g)) )
        {
          if( verbose ) cout << "Computing undirected sel parameters for fleet " << g << endl;
          // Modify alpha parameter
          if( (pDirOn(g) == 1) )
          {
            alphaUndir_gt_m(g,t) *= exp(log_selAlphaDirDev_g(g) + selAlphaDirDev_base(g));
            alphaUndir_gt_f(g,t) *= exp(log_selAlphaDirDev_g(g) + selAlphaDirDev_base(g));
            // Modify beta parameter
            betaUndir_gt_m(g,t) *= exp(log_selBetaDirDev_g(g));
            betaUndir_gt_f(g,t) *= exp(log_selBetaDirDev_g(g));
          }
        }
      // Loops over sexes and age, compute selectivity      
      for( int l=1; l<=2; l++ )
      {      
        if( l==1 ) 
        {
          tmp_lenAge    = lenAge_m;
          tmp_alpha_gt  = alpha_gt_m;
          tmp_beta_gt   = beta_gt_m;
          tmp_alphaU_gt = alphaUndir_gt_m;
          tmp_betaU_gt  = betaUndir_gt_m;
        }
        else
        {
          tmp_lenAge    = lenAge_f;
          tmp_alpha_gt  = alpha_gt_f;
          tmp_beta_gt   = beta_gt_f;
          tmp_alphaU_gt = alphaUndir_gt_f;
          tmp_betaU_gt  = betaUndir_gt_f;
        }
        tmp.initialize();
        tmpUndir.initialize();
        for( int a=1; a<=plusGroupAge; a++ )
        {

          switch( selType(g) )
          {
            if( verbose ) cout << "Computing undirected sel curves for fleet " << g << endl;
            case 1: // asymptotic
              tmp(a) = exp( (-1.)*log(19.)*(tmp_lenAge(a)-tmp_beta_gt(g,t))/(tmp_alpha_gt(g,t) - tmp_beta_gt(g,t) ) );
              tmp(a) = 1./(1.+tmp(a));
              tmpUndir(a) = exp( (-1.)*log(19.)*(tmp_lenAge(a)-tmp_betaU_gt(g,t))/(tmp_alphaU_gt(g,t) - tmp_betaU_gt(g,t) ) );
              tmpUndir(a) = 1/(1 + tmpUndir(a));
              
            break;
            case 2: // normal
              tmp(a) = exp( (-0.5)*pow(tmp_lenAge(a)-tmp_alpha_gt(g,t),2.)/pow(tmp_beta_gt(g,t),2.) );               
              tmpUndir(a) = exp( (-0.5)*pow(tmp_lenAge(a)-tmp_alphaU_gt(g,t),2.)/pow(tmp_betaU_gt(g,t),2.) );               
            break;

            case 3: // gamma
              tmp(a)      = pow(tmp_lenAge(a),(tmp_alpha_gt(g,t) - 1)) * mfexp( -tmp_lenAge(a) / tmp_beta_gt(g,t));
              tmpUndir(a) = pow(tmp_lenAge(a),(tmp_alphaU_gt(g,t) - 1)) * mfexp( -tmp_lenAge(a) / tmp_betaU_gt(g,t));
          }

        } // end a-loop
        for( int a=1; a<=plusGroupAge; a++ )
        {
          if( g == 4 & a <= 2 )
          {
            tmp(a) = 0;
            tmpUndir(a) = 0;
          }
          // Adding undirected selectivity, identified with
          // directed selectivity for non-trawl fleets, in case
          // we want to add an undirected/bycatch portion of F
          // for other fleets later on
          // Normalise sel by maximum value
          if( l==1 )
          {
            sel_gta_m(g,t,a) = tmp(a)/max(tmp);
            selUndir_gta_m(g,t,a) = tmpUndir(a)/max(tmpUndir);
          }
          else
          {
            sel_gta_f(g,t,a) = tmp(a)/max(tmp);
            selUndir_gta_f(g,t,a) = tmpUndir(a)/max(tmpUndir);
          }
        
          //if( g==1) cout << "t = "<<t<< " a = "<<a<<" sel_gta_f = "<< sel_gta_f(g,t,a) << endl;
        }
      }   // end l-loop
    }     // end t-loop
  }       // end g-loop
  //exit(1);
//*******************************************************************/  
FUNCTION calc_sel_prior
  // Priors needed for
  // calc prior for years 32-48
  int g;
  selPrior = 0.;
  for( int g=1; g<=nFisheries-2; g++ )
  {

    for( int t=firstSelPrior_g(g); t<=lastSelPrior_g(g); t++ )
    {
      
      if(active(log_alpha_g(g)) )
      {
        // Males
        selPrior += 0.5*pow( alpha_gt_m(g,t)-p_alpha_gt(g,t),2. )/pow(sd_alpha_gt(g,t),2.);  
        // Females
        selPrior += 0.5*pow( alpha_gt_f(g,t)-p_alpha_gt(g,t),2. )/pow(sd_alpha_gt(g,t),2.);
      }
      
      if( active(log_beta_g(g))  )
      {
        // Males
        selPrior += 0.5*pow( beta_gt_m(g,t)-p_beta_gt(g,t),2. )/pow(sd_beta_gt(g,t),2.);  
        // Females
        selPrior += 0.5*pow( beta_gt_f(g,t)-p_beta_gt(g,t),2. )/pow(sd_beta_gt(g,t),2.);
      }
      
      // prior on selectivity std deviation deviations...
      if( active(log_alpha_dev_gt(g)) & (log_alpha_dev_gt(g,t) != 0) )
        selPrior += 0.5*pow(log_alpha_dev_gt(g,t)/priorSD_alpha_dev,2.);
    
      if( active(log_beta_dev_gt(g)) & (log_beta_dev_gt(g,t) != 0) )
        selPrior += 0.5*pow(log_beta_dev_gt(g,t)/priorSD_beta_dev,2.);

    }
    // Prior on sex-based selectivity devs
    if( (active(log_beta_sexdev_g(g))) & (log_beta_sexdev_g(g) != 0) )
      selPrior += 0.5*pow(log_beta_sexdev_g(g)/priorSD_beta_sexdev,2.);
    if( (active(log_beta_sexdev_g(g))) & (log_alpha_sexdev_g(g) != 0) )
      selPrior += 0.5*pow(log_alpha_sexdev_g(g)/priorSD_alpha_sexdev,2.);

    // Add block deviations to prior, with .1 sd
    if( active(log_alpha_blockDev_g(g) ) )
      selPrior  += 0.5*norm2(log_alpha_blockDev_g(g))/pow(blockDevPriorSd(g),2);
    if( active(log_beta_blockDev_g(g) ) )
      selPrior  += 0.5*norm2(log_beta_blockDev_g(g))/pow(blockDevPriorSd(g),2);
  }    

FUNCTION calc_sel_prior2
  // Priors needed for
  // calc prior for years 32-48
  int g;
  selPrior = 0.;
  for( int g=1; g<=nFisheries-2; g++ )
  {
    for( int t=firstSelPrior_g(g); t<=lastSelPrior_g(g); t++ )
    {
      if( active(log_alpha_g(g)) )
        selPrior += 0.5*pow( alpha_gt_m(g,t)-p_alpha_gt(g,t),2.)/pow(sd_alpha_gt(g,t),2.);
      if( active(log_beta_g(g)) )
        selPrior += 0.5*pow( beta_gt_m(g,t)-p_beta_gt(g,t),2.)/pow(sd_beta_gt(g,t),2.);
    }   
  }    

FUNCTION calc_sel_prior3
  // Priors for constant selectivity
  // in all three comm fisheries
  int g;
  selPrior = 0.;
  for( int g=1; g<=nFisheries-2; g++ )
  {
    if( active(log_alpha_g(g)) )
      selPrior += 0.5*pow( log(alpha_gt_m(g,1))-log(p_alpha_gt(g,firstSelPrior_g(g))),2. )/pow(sigma_alpha(g),2.);
    if( active(log_beta_g(g)) )
      selPrior += 0.5*pow( log(beta_gt_m(g,1))-log(p_beta_gt(g,firstSelPrior_g(g))),2. )/pow(sigma_beta(g),2.);
    // Add block deviations to prior, with .1 sd
    if( active(log_alpha_blockDev_g(g) ) )
      selPrior  += 0.5*norm2(log_alpha_blockDev_g(g))/pow(blockDevPriorSd(g),2);
    if( active(log_beta_blockDev_g(g) ) )
      selPrior  += 0.5*norm2(log_beta_blockDev_g(g))/pow(blockDevPriorSd(g),2);

  }    


  //exit(1);
//*******************************************************************/
FUNCTION calc_rec_prior
  recPrior  = 0.5*norm2(recDevs)/pow(sigma_R,2.);
//*******************************************************************/
FUNCTION calc_h_prior
  hPrior = 0.;
  hPrior = (prior_h(1)-1.)*log(h) + (prior_h(2)-1.)*log(1.-h);
  hPrior *= -1.;
//*******************************************************************/
FUNCTION calc_release_likelihood
  // Function computes likelihoods for at-sea releases for each gear type
  // using data from 1996+ (trawl) and 2006+ (trap, LL)
  dvariable res; validObs_rel.initialize();
  tauSquareReleases.initialize(); releaseLikelihood.initialize();
  int tMax = min(nT,likeYear);
  // cout << "Inside rel like" << endl;
  for( int g=1; g<=nFisheries-2; g++ )
  {
      ssR = 0.;
      for( int t=1; t<=tMax; t++ )
      {
        if( obs_relCtg(t,g) > 0. )
        {
          // Check if using the trawl fishery, or 
          //
          if(t < relDevInitYr(g) | t > relDevLastYr(g) )
          {
            res = log(obs_relCtg(t,g)) - log(relCtg(t,g));
            validObs_rel(g) +=1;
            ssR             += pow(res,2.);
          }
        } 
      }   
      if(validObs_rel(g) > 0)
      {
        tauSquareReleases(g) = ssR/validObs_rel(g);
        releaseLikelihood(g) = 0.5*(validObs_rel(g)-1.)*log(tauSquareReleases(g));
        // Added by SDNJ: an exponential prior on 
        // the release observation variance, attempting
        // to penalise bad fits - might have to play with
        // a weighting here.

        releaseLikelihood(g) += tauRelLambda(g) * sqrt(tauSquareReleases(g));
      }
      
      

      // Try adding a really stiff penalty for the residuals in the last 2 years
      // in the trawl fishery - this will be added to the likelihood in place
      // of the conditional normal MLEs above
      // cout << "Adding trawl release dev penalty in last 2 years" << endl;
      if( relDevInitYr(g) <= tMax )
      {
        for( int t = relDevInitYr(g); t <= relDevLastYr(g); t++)
        { 
          res = (log(obs_relCtg(t,g)) - log(relCtg(t,g)));
          if(res > 0)
            releaseLikelihood(g) += 0.5 * pow(res/relDevPenalty,2);
        }
      }
  }
  // cout << "Completed rel like" << endl;
//*******************************************************************/
FUNCTION calc_index_likelihood
  // Initialize likelihood function terms.
  int i,t,g;
  indexLikelihood.initialize(); validObs_idx.initialize();
  ss.initialize();
  dvariable zSum;       // cumulative residual
  dvar_vector z(1,nT);  // residuals

  // Calculations for MLEs of q.
  for ( i=1;i<=nIndexSeries;i++ )
  {
    z.initialize(); zSum=0.0; 
    for ( t=1;t<=likeYear;t++ )
    {
      if ( idxSeries(i,t) > 0. )
      { 
        g = idxIndex(i);
        z[t] = log(idxSeries(i,t)) - log( expBit(g,t)  );
        validObs_idx(i) += 1;
      }
    }
    // MLEe lnq 
    for( int t = 1; t <= nT; t++)
      lnq_it(i,t) = lnq(i);    

    // Sum-of-squares function.
    for( t=1; t<=likeYear; t++ )
    {
      // Add residual if asked for
      if( (t >= firstqdev_g(i)) & (t <= lastqdev_g(i)) & active(log_qdev_gt(i)))
        lnq_it(i,t) = lnq_it(i,t-1) + log_qdev_gt(i,t);

      if ( idxSeries(i,t) > 0. )
      {
        ss[i] += pow(z(t)-lnq_it(i,t),2.);        
      }
    }
    // Objective function value for this series.
    tauSquareIndex(i) = ss(i)/(validObs_idx(i)-1.); // index variance MLE
    indexLikelihood(i) = (validObs_idx(i)-1.)*log(tauSquareIndex(i)); // concentrated likelihood
    // Add a Jeffreys prior to sqrt(tauSquareIndex)
    indexLikelihood(i) += tauIndexLambda(i) * sqrt(tauSquareIndex(i));
  }
  //cout << "lnq_it = " << lnq_it << endl;
  //exit(1);
//*******************************************************************/
FUNCTION calc_age_likelihood
  
  dvariable meanDiff_m; dvariable meanDiff_f;
  //dvariable nYearsAges;
  dvariable etaSumSq_m; dvariable etaSumSq_f;
  n_residuals_m = 0; n_residuals_f = 0;  
  tauSquareAges_m.initialize(); tauSquareAges_f.initialize(); 
  ageLikelihood_m.initialize(); ageLikelihood_f.initialize();
  mnLL_m = 0.; mnLL_f = 0.;
  
  // If likeYear < nT, then use likeYear for retrospective
  int tMax = min(nT,likeYear);
  int g; 

  if(verbose == 1)
    cout << "Inside age likelihood" << endl;

  // Calculate predicted age-proportions for each index gear.
  for( int i=1;i<=nAgeSeries;i++ )
  {
    if(verbose == 1)
      cout << "Age series  = " << i << endl;
    g = fisheryAgeID(i);
    // males
    //cout<< "firstAge_m(i) = " <<  firstAge_m(i) << endl;
    etaSumSq_m = 0.; n_residuals_m = 0;
    for( int t=1; t<=nT; t++ )
    {
      if(verbose == 1)
        cout << "Time t = " << t << endl;
      // check whether this year has data
      dvariable accRes = 0.;
      if( firstAge_m(i,t) > 0 )
      {
        // extract temp vector of all age props for this year
        dvar_vector obsPat  = row(ageObsProp_m(i),t)(firstAge_m(i,t),lastAge_m(i,t));

        // cout << "t = " << t << endl;
        // cout << "firstAge_m(i,t) = " <<  firstAge_m(i,t) << endl;
        // cout << "lastAge_m(i,t) = " <<  lastAge_m(i,t) << endl;
        // cout << "obsPat = " << obsPat << endl;

        // NOTE: this needs to be summed up
        dvar_vector predPat = row(predObsProp_m(i),t);
        //dvar_vector predPat = row(uCgta_m(i),t)(firstAge_m(i,t),lastAge_m(i,t));

        pred_accBin_m(i,t) = 0.;
        for( int a=firstAge_m(i,t); a<=lastAge_m(i,t); a++ )
        {
          if( iz_m(i,t,a) == 0. )
          {
            // add predPat to accBin
            pred_accBin_m(i,t) += predPat(a);
            predPat(a) = 0.;
          }
        }
        // Likelihood calcs for males-------------------------
        // Residual function for this year
        int iRes = 0; dvariable sumRes = 0.;
        dvar_vector res(firstAge_m(i,t),lastAge_m(i,t));



        // Get residuals by age
        for( int a=firstAge_m(i,t); a<=lastAge_m(i,t); a++ )
        {
          if( iz_m(i,t,a) > 0. ) // Data exist and > prop_threshold
          {
            res(a)  = log(obsPat(a)) - log(predPat(a)); // residual
            sumRes   += res(a);
            iRes     += 1.;
          }
        }
        // If the accumulator bin was used, then get residual for that.
        if( pred_accBin_m(i,t) > 0. )
        {
          accRes = log(pred_accBin_m(i,t));
          sumRes += accRes; // add residual to total
          iRes   += 1.;     // add one more observation
        }
        // mean residual, SSQ, and n for MVN
        meanDiff_m     = sumRes/iRes;
        etaSumSq_m    += norm2( res - meanDiff_m );

        if( accRes > 0. ) 
          etaSumSq_m += pow(accRes - meanDiff_m,2.); // sumSq
        n_residuals_m += iRes;          // accumulate total n residuals    
        // cout << "etaSumSq_m = " << etaSumSq_m <<  endl;  
        // cout << "iRes = " << iRes <<  endl;  
        // cout << "sumRes = " << sumRes <<  endl;  
      }
    }
    if( verbose )
      cout << "Calculating tauSquareAges for males in gearid" << i << endl;
    // Final MLE of variance and likelihood
    tauSquareAges_m(i) = etaSumSq_m/n_residuals_m;
    ageLikelihood_m(i) = n_residuals_m*log(tauSquareAges_m(i))/2;
    ageLikelihood_m(i) += tauAgeLambda(i)*tauSquareAges_m(i);

    // cout << "i = " << i << endl;
    // cout << "etaSumSq_m = " << etaSumSq_m <<  endl;
    // cout << "n_residuals_m = " << n_residuals_m <<  endl;

    // exit(1);
  

    // females
    etaSumSq_f = 0.; n_residuals_f = 0;
    for( int t=1; t<=nT; t++ )
    {
      if(verbose)
        cout << "Female age observations in gear" << i << " at time " <<  t << endl;
      // check whether this year has data
      dvariable accRes = 0.;
      if( firstAge_f(i,t) > 0 )
      {
        // extract temp vector of all age props for this year
        dvar_vector obsPat  = row(ageObsProp_f(i),t)(firstAge_f(i,t),lastAge_f(i,t));
        // NOTE: this needs to be summed up
        dvar_vector predPat = row(predObsProp_f(i),t);
        //dvar_vector predPat = row(uCgta_f(i),t)(firstAge_f(i,t),lastAge_f(i,t));
        
        pred_accBin_f(i,t) = 0.;
        for( int a=firstAge_f(i,t); a<=lastAge_f(i,t); a++ )
        {
          if( iz_f(i,t,a) == 0. )
          {
            // add predPat to accBin
            pred_accBin_f(i,t) += predPat(a);
            predPat(a) = 0.;
          }
        }

        // Residual function for this year
        int iRes = 0; dvariable sumRes = 0.;
        dvar_vector res(firstAge_f(i,t),lastAge_f(i,t));
        // Get residuals by age in case some obsPat == 0
        for( int a=firstAge_f(i,t); a<=lastAge_f(i,t); a++ )
        {
          
          if( iz_f(i,t,a) > 0. ) // Data exist and > prop_threshold
          {
            res(a)  = log(obsPat(a)) - log(predPat(a)); // residual
            sumRes   += res(a);
            iRes     += 1.;
          }            
        }
        //cout<< "t = " << t << endl;
        //cout<< "res_f = " << res << endl;
        // If the accumulator bin was used, then get residual for that.
        if( pred_accBin_f(i,t) > 0. )
        {
          accRes = log(pred_accBin_f(i,t));
          sumRes += accRes; // add residual to total
          iRes   += 1.;     // add one more observation
        }
        // mean residual, SSQ, and n for MVN
        meanDiff_f     = sumRes/iRes;
        etaSumSq_f    += norm2( res - meanDiff_f );

        if( accRes > 0. ) 
          etaSumSq_f += pow(accRes - meanDiff_f,2.); // sumSq

        n_residuals_f += iRes;                    // total n residuals    
      }
    }
    if( verbose )
      cout << "Calculating tauSquareAges for females" << endl;
    // MLE of variance in female age-proportions.
    tauSquareAges_f(i) = etaSumSq_f/n_residuals_f;
    ageLikelihood_f(i) = n_residuals_f*log(tauSquareAges_f(i))/2;
    ageLikelihood_f(i) += tauAgeLambda(i)*tauSquareAges_f(i);
    //cout<< "i = " << i << endl;
    //cout<< "ageLike_f(i) = " << ageLikelihood_f(i) << endl;
  }


//*******************************************************************/
FUNCTION calc_F_likelihood
  fLike = 0.;
    
    /*dvariable ln_det;
    dvariable sign;
    dvar_vector rF  = column(log_Fdevs_gt,t)-row_means_F;
    dvar_vector tmp = solve(sigmaF,rF,ln_det,sign);
    
    fLike += (-.5)*ln_det-rF*tmp;
    */
    for( int g=1; g<=nFisheries-2; g++ )
    {
      for( int t=firstYearCatch(g)+1; t<=nT;t++ )
        fLike += 0.5*log_Fdevs_gt(g,t)*log_Fdevs_gt(g,t)/pow(priorSD_F,2.);
    
    /*if( last_phase() )
    {
      fLike += 1.*pow( mean(column(Ftg,g))-0.1,2. );
    }
    else
      fLike += 10000.*pow( mean(column(Ftg,g))-0.1,2. );
    */
    }
//*******************************************************************/
FUNCTION calc_catch_likelihood
  catLike = 0.;
  dvariable tmp;
  for( int t=1; t<=nT; t++ )
    for( int g=1; g<=nFisheries-2; g++ )
    {
      if( Ctg(t,g) > 0)
      {
        tmp = log(pCtg(t,g)) - log(Ctg(t,g));

        catLike += 0.5*pow(tmp/catError,2.); 
      }

    }

//*******************************************************************/
FUNCTION initModelParameters
  // Set likelihoods/priors to 0
  fLike = 0.; catLike = 0.; hPrior = 0.; ssbPrior = 0.;
  recPrior = 0.; selPrior = 0.;
  // Revise this order to match PARAMETER_SECTION
  // Natural mortality is constant over age and time
  // cout << "log_M = " <<  log_M << endl;
  M_m = mfexp( log_M(1) );
  M_f = mfexp( log_M(2) );

  // correlation in Fs
  rho = 1./(1.+exp(-logit_rho));
  // cout << "logit_rho = " <<  logit_rho << endl;
  // Random walk in Fs
  Ftg.initialize();
  if( !useBaranov )
  {
    // Ftg parameterized as random walk with estimated parameters
    //  log_Fg1,  log_Fg2, log_Fg3, log_Fdevs_1t,...3t
    log_Ftg.initialize(); 
    log_Fdevs_gt.initialize();
    // Fs in first year
    for(int g=1; g<=nFisheries-2; g++)
    {
      log_Ftg(firstYearCatch(g),g) = log_Fg1(g);
      // cout << "log_Fg1 = " << log_Fg1 << endl;
      Ftg(firstYearCatch(g),g)     = mfexp( log_Ftg(firstYearCatch(g),g) );
      switch( g )
      {

        case 1: // trap
          log_Fdevs_gt(g)(firstYearCatch(g)+1,nT) = log_Fdevs_1t;
        break;
        case 2: // normal
          log_Fdevs_gt(g)(firstYearCatch(g)+1,nT) = log_Fdevs_2t;
        break;
        case 3: // normal
          log_Fdevs_gt(g)(firstYearCatch(g)+1,nT) = log_Fdevs_3t;
        break;

      }
      for( int t=firstYearCatch(g)+1; t<=nT; t++ )
      {
        log_Ftg(t,g) = log_Ftg(t-1,g) + log_Fdevs_gt(g,t);
        Ftg(t,g)     = mfexp( log_Ftg(t,g) );
      }

    }
    /*// Fill ranwalk for remaining years
    for( int t=2; t<=nT; t++ )
    {
      for(int g=1; g<=nFisheries-2; g++)
      {
        log_Ftg(t,g) = log_Ftg(t-1,g) + log_Fdevs_gt(g,t);
        Ftg(t,g)     = mfexp( log_Ftg(t,g) );
      }
    }*/
    int nFs = log_Fdevs_gt.colmax()-log_Fdevs_gt.colmin();
    //cout << "rowmeans = " << rowsum(log_Fdevs_gt)/nFs << endl;
    row_means_F = rowsum(log_Fdevs_gt)/nFs;
  }

  /*ch_sigmaF.initialize();
  int ii=1;
  for (int i=1;i<=3;i++)
    for (int j=1;j<=i;j++)
      ch_sigmaF(i,j)=sigmaF_coeff(ii++);
  
  sigmaF = ch_sigmaF*trans(ch_sigmaF);
  for( int i=1; i<=3; i++ )
    sigmaF(i,i) += 0.001;
  */
  // get selectivity by gear and year
  calc_sel_gta();
  if( verbose ) cout << "calc_sel_gta() done..." << endl;
  // get proportion of catch-at-age released at sea due to
  // combination of size limit and high-grading 
  calc_pR_gta();
  //cout << "Done calc_sel_gta_mf " << endl;
  // get unfished spawning biomass per recruit for setting
  // up unfished recruitment and SR relationship
  calcPhi();
  // Average recruitment may be leading parameter
  avgR = mfexp( log_avgR );
  
  // Beverton-Holt steepness implementing 0.25-0.99 bounds
  // at logit_h=0, the default h=0.75
  h = 0.25 + (0.99-0.25)*mfexp(logit_h)/(1.+mfexp(logit_h));
  if( active(logit_h) )
    calc_h_prior();
  
  // Unfished SSB
  SSB0 = mfexp( log_SSB0 );
  // Unfished recruitment.
  R0 = SSB0/phiSSB;
  // Beverton-Holt a parameter.
  bh_a = 4.*h*R0/(SSB0*(1.-h));
  // Beverton-Holt b parameter.
  bh_b = (5.*h-1.)/(SSB0*(1.-h));
  // recruitment deviations
  omega_t.initialize(); // set all to 0
  // fill in the ones we are estimating
  for( int t=firstRecDev; t<=lastRecDev; t++ )
  {
    omega_t(t) = recDevs(t);
  } 
  // Overwrite recruitment deviations
  // for those years where they are fixed
  for( int i = 1; i <= nFixedRecDev; i ++)
    omega_t(tFixedRecDev(i)) = logFixedRecDev(i);
  //cout << R0 << endl;
//*******************************************************************/
FUNCTION calcPhi
  // Calculating unfished SSB-per-recruit. Borrowing
  // Nta and Bta for now. Initializing with 0.5 recruit
  // to get through calcs.
  Nta_m.initialize();SSBt.initialize();
  Nta_f.initialize();Bta_m.initialize();
  Bta_f.initialize();
  Nta_f(1,1) = 1.;
  for( int a=2; a<=(plusGroupAge-1); a++ )
  {
    Nta_f(1,a) = Nta_f(1,a-1)*mfexp( -M_f );
  }
  Nta_f(1,plusGroupAge) = Nta_f(1,plusGroupAge-1)*mfexp(-M_f)/(1.-mfexp(-M_f));
  
  Bta_f(1)(1,plusGroupAge) = elem_prod( row(Nta_f,1), wtAge_f );
  
  // Unfished spawning biomass-per-recruit
  phiSSB = sum( elem_prod( row(Bta_f,1),matAge_f ) );
  //cout << phiSSB << endl;
//*******************************************************************/
FUNCTION popInit_SSB0
  Bta_m.initialize();Bta_f.initialize(); SSBt.initialize();
  expBit.initialize(); legalBt.initialize(); subLegalBt.initialize();
  relCtg.initialize(); landedCtg.initialize(); 
  undirLandCtg.initialize(); undirRelCtg.initialize();
  dirLandCtg.initialize();   dirRelCtg.initialize();
  pCtg.initialize(); Nta_f.initialize(); Nta_m.initialize();

  //---------------------
  // Initialize from unfished
  Nta_m(1,1) = R0*mfexp( omega_t(1)  );    
  Nta_f(1,1) = Nta_m(1,1);        

  for( int a=2; a<=plusGroupAge-1; a++ )
  {
    Nta_m(1)(a) = Nta_m(1,a-1)*mfexp( -M_m );
    Nta_f(1)(a) = Nta_f(1,a-1)*mfexp( -M_f );
  }
  Nta_m(1)(plusGroupAge) = Nta_m(1)(plusGroupAge-1)*exp(-M_m);
  Nta_m(1)(plusGroupAge)/= 1.-exp(-M_m);

  Nta_f(1)(plusGroupAge) = Nta_f(1)(plusGroupAge-1)*exp(-M_f);
  Nta_f(1)(plusGroupAge)/= 1.-exp(-M_f);
  //cout << sum(row(Nta_f,1)) << endl;
  //exit(1);  
  
  // Biomass-at-age and SSB: SSBt(1) should match the value
  // at the end of calcPhi()
  Bta_m(1)(1,plusGroupAge) = elem_prod( row(Nta_m,1),wtAge_m );
  Bta_f(1)(1,plusGroupAge) = elem_prod( row(Nta_f,1),wtAge_f );
  SSBt(1)  = sum( elem_prod( row(Bta_f,1),matAge_f ) );
  Rt(1)    = Nta_f(1,1) + Nta_m(1,1);

  // Get total mortality rate
  Zta_m.initialize(); Zta_f.initialize();
  baranov_time = 1;
  if( useBaranov ) // solve catch equation to get Z each year
  {
    solveBaranov_multi_fleet();
  }
  else
  {
    // Fs are parameters: get all Zt
    for( int t=1; t<=nT; t++ )
    {
      // Initialize Z with only M 
      Zta_m(t)(1,plusGroupAge) = M_m; 
      Zta_f(t)(1,plusGroupAge) = M_f;
      // Add Fs
      for( int g=1; g<=nFisheries-2; g++ )
      {
        // subset the proportions released at age
        for( int a = 1; a <= plusGroupAge; a++)
        {
          // Directed portion of fishing mortality
          Zta_m(t,a) += pDir_tg(t,g) * sel_gta_m(g,t,a) * Ftg(t,g) * (pR_gta_m(g,t,a) + (1 - pR_gta_m(g,t,a)) * dM(g));
          Zta_f(t,a) += pDir_tg(t,g) * sel_gta_f(g,t,a) * Ftg(t,g) * (pR_gta_f(g,t,a) + (1 - pR_gta_f(g,t,a)) * dM(g));

          // Add undirected portion of fishing mortality
          Zta_m(t,a) += (1 - pDir_tg(t,g)) * selUndir_gta_m(g,t,a) * Ftg(t,g) * (pR_gta_m(g,t,a) + (1 - pR_gta_m(g,t,a)) * dM(g));
          Zta_f(t,a) += (1 - pDir_tg(t,g)) * selUndir_gta_f(g,t,a) * Ftg(t,g) * (pR_gta_f(g,t,a) + (1 - pR_gta_f(g,t,a)) * dM(g));
        }
        // cout << "t = "<<t<<" g = "<<g<< endl;
        // cout << "sel_gta(g,t) = " << row(sel_gta_f(g),t) << endl;
        //Zta_m(t)(1,plusGroupAge) += row(sel_gta_m(g),t)*Ftg(t,g);
        //Zta_f(t)(1,plusGroupAge) += row(sel_gta_f(g),t)*Ftg(t,g);
      }
    }
  // exit(1);

  }
  //-------------------------
  // End of Biomass dynamics
  //-------------------------
  
  //-------------------------------------------------
  // Observation models for indices and length comps
  calc_observation_models();
  //-------------------------------------------------
//*******************************************************************/
FUNCTION popDynamics_MF
  int g;
  //---------------------
  // Biomass dynamics
  //---------------------
  // Population abundances-at-age were initialized 
  // in popInit_SSB0 for year 1
  for( int t=2; t<=nT; t++ )
  {
    // Age-1 recruitment: m=male, f=female 
    // assumes 50:50 sex ratio at birth
    Nta_m(t,1) = bh_a*SSBt(t-1)*mfexp( omega_t(t) )/(1.+bh_b*SSBt(t-1)); 
    Nta_f(t,1) = Nta_m(t,1);             
    // Age-structured model equations: total mortality Zta calculated in popInit_SSB0
    // for all years by adding Ftg and M
    for( int a=2; a<=plusGroupAge-1; a++ )
    {
      Nta_m(t,a) = Nta_m(t-1,a-1)*mfexp( -Zta_m(t-1,a-1) );
      Nta_f(t,a) = Nta_f(t-1,a-1)*mfexp( -Zta_f(t-1,a-1) );
    }
    // The plus group:
    // Males
    Nta_m(t,plusGroupAge) = Nta_m(t-1,plusGroupAge-1)*mfexp( -Zta_m(t-1,plusGroupAge-1) ) + 
                            Nta_m(t-1,plusGroupAge)*mfexp(   -Zta_m(t-1,plusGroupAge)    );
    // Females
    Nta_f(t,plusGroupAge) = Nta_f(t-1,plusGroupAge-1)*mfexp( -Zta_f(t-1,plusGroupAge-1) ) + 
                            Nta_f(t-1,plusGroupAge)*mfexp(   -Zta_f(t-1,plusGroupAge)    );
    
    // Biomass-at-age and SSB
    Bta_m(t) = elem_prod( row(Nta_m,t), wtAge_m );
    Bta_f(t) = elem_prod( row(Nta_f,t), wtAge_f );
    SSBt(t)  = sum( elem_prod( row(Bta_f,t), matAge_f ) );
    Rt(t)    = Nta_f(t,1) + Nta_m(t,1);


    
    baranov_time = t;
    if( useBaranov ) // get Z for this year
    {
      solveBaranov_multi_fleet();
    }
    //---------------------
    // End biomass dynamics
    //---------------------
    //-----------------------------------------------------
    // Observation models
    //-----------------------------------------------------
    calc_observation_models();

  if( verbose == 1 )
    cout << "Done with observation models for time " << t << endl;

 
  } // end loop over year
  //exit(1);
  if( sd_phase() ) 
  {
    D2015 = SSBt(nT)/SSB0;
    B2015 = SSBt(nT);
  }
  

  if ( last_phase() ) neval+=1; 
  else neval=-2.;  // start neval counter at -2 at start of last phase so it equals admb screen output
//*******************************************************************/
FUNCTION calc_observation_models
  int t = baranov_time;
  int g;
  //-----------------------------------------------------
  // Observation models for indices
  //-----------------------------------------------------
  // predicted exploitable biomass by fishery (g=1)
  // and survey (g=2,3)
  for( int g=1; g<=nFisheries; g++ )
  {
    expBit(g,t)  = sum( elem_prod( elem_prod( row(Bta_m,t),row(sel_gta_m(g),t) ), 
                                              mfexp(-fracYearFleet(g)*Zta_m(t)) ) );      
    expBit(g,t) += sum( elem_prod( elem_prod( row(Bta_f,t),row(sel_gta_f(g),t) ), 
                                              mfexp(-fracYearFleet(g)*Zta_f(t)) ) );      
  }

  //-----------------------------------------------------
  // Observation models for age-composition
  //-----------------------------------------------------          
  dvar_vector tmp(1,plusGroupAge);
  dvariable sumP;
  dvariable fracYrSurvey;
  for( int i=1; i<=nAgeSeries; i++ )
  {
    if( verbose == 1 )
      cout << "Inside calc_observation_models for fleet " << i << endl;
    int g = fisheryAgeID(i);

    if( verbose)
      cout << "fisheryAgeID(" << i << ") = " << g << endl;
    //cout <<"t = "<< t <<" g = "<< g << endl;
    //cout <<  "sum_m = " << sum( row(ageObsProp_m(i),t) ) << endl;
    //cout <<  "sum_f = " << sum( row(ageObsProp_f(i),t) ) << endl;
    // Male age comps
    if( sum( row(ageObsProp_m(i),t) ) > 0. )
    {
      tmp.initialize();
      sumP=0.;
      // calc raw proportions-at-age
      for( int a=minAge(i); a<=plusGroupAge; a++ )
      {
        tmp(a) = Nta_m(t,a)*sel_gta_m(g,t,a)*exp(-fracYearAges(i)*Zta_m(t,a));
        if( (g < nFisheries-2) & last_phase() )
          tmp(a) *= reportRate_ga_m(g,a);
        sumP  += tmp(a);
      }
      // normalize proportions
      for( int a=minAge(i); a<=plusGroupAge; a++ )
      {
        uCgta_m(i,t,a) = tmp(a)/sumP;
      }

      if( useAgeError_m(i) )
        predObsProp_m(i,t)(minAge(i),maxAge(i)) = Q*row(uCgta_m(i),t);
      else
        predObsProp_m(i,t)(minAge(i),maxAge(i)) = row(uCgta_m(i),t);


      //cout<< "predObsProp_m = " <<  predObsProp_m(i,t) << endl;
      //cout<< "reportRate_ga_m = " <<  reportRate_ga_m(g) << endl;

      //if( g <= nFisheries-2 )
        //predObsProp_m(i,t) = predObsProp_m(i,t) * reportRate_ga_m(g);


    } // close if(obs>0)

    if( verbose == 1 )
      cout << "Done with male age obs" << endl;

    // Female age comps
    if( sum( row(ageObsProp_f(i),t) ) > 0. )
    {
      tmp.initialize();
      sumP=0.;
      for( int a=minAge(i); a<=plusGroupAge; a++ )
      {
        tmp(a) = Nta_f(t,a)*sel_gta_f(g,t,a)*exp(-fracYearAges(i)*Zta_f(t,a));
        if( (g < nFisheries-2) &  last_phase() )
          tmp(a) *= reportRate_ga_f(g,a);
        sumP  += tmp(a);
      }
      for( int a=minAge(i); a<=plusGroupAge; a++ )
      {
        uCgta_f(i,t,a) = tmp(a)/sumP;
      }
      if( useAgeError_f(i) )
        predObsProp_f(i,t)(minAge(i),maxAge(i)) = Q*row(uCgta_f(i),t);
      else
        predObsProp_f(i,t)(minAge(i),maxAge(i)) = row(uCgta_f(i),t);


      //cout<< "predObsProp_f = " <<  predObsProp_f(i,t) << endl;
      //cout<< "reportRate_ga_f = " <<  reportRate_ga_f(g) << endl;


      //if( g <= nFisheries-2 )
        //predObsProp_f(i,t) = predObsProp_f(i,t) * reportRate_ga_f(g);
      
    } // close if(obs>0)    
    if( verbose == 1 )
      cout << "Done with female age obs at time " << t << endl;
  }
  
  //-----------------------------------------------------
  // Observation models for sub-legal releases by gear
  //-----------------------------------------------------
  if( verbose == 1 )
      cout << "Calculating sublegal releases for year " << t << endl;
  dvar_vector tmpDirCatch_m(1,plusGroupAge);
  dvar_vector tmpDirCatch_f(1,plusGroupAge);
  dvar_vector tmpUndirCatch_m(1,plusGroupAge);
  dvar_vector tmpUndirCatch_f(1,plusGroupAge);
  dvar_vector tmpDirRel(1,plusGroupAge);
  dvar_vector tmpDirLand(1,plusGroupAge);
  dvar_vector tmpUndirRel(1,plusGroupAge);
  dvar_vector tmpUndirLand(1,plusGroupAge);


  for( int g=1;g<=nFisheries-2;g++ )
  {
    // Initialize released and landed catch
    relCtg(t,g) = 0.; landedCtg(t,g) = 0.; 
    undirLandCtg(t,g) = 0.;
    dirLandCtg(t,g) = 0.;
    undirRelCtg(t,g) = 0.;
    dirRelCtg(t,g) = 0.;
    // accumulate legal catch, discards, and sublegal total discards
    tmpDirCatch_m.initialize(); tmpDirCatch_f.initialize();
    tmpUndirCatch_m.initialize(); tmpUndirCatch_f.initialize();
    tmpDirRel.initialize(); tmpDirLand.initialize();
    tmpUndirRel.initialize(); tmpUndirLand.initialize();
    dvar_vector pR_m = row(pRet_m,t);
    dvar_vector pR_f = row(pRet_f,t);



    for( int a=1; a<=plusGroupAge; a++ )
    {
        // get onboard catch-at-age
        tmpDirCatch_m(a) = pDir_tg(t,g)*Bta_m(t,a)*sel_gta_m(g,t,a)*(Ftg(t,g))*(1.-exp(-Zta_m(t,a)));
        tmpDirCatch_m(a) /= Zta_m(t,a);

        tmpDirCatch_f(a) = pDir_tg(t,g)*Bta_f(t,a)*sel_gta_f(g,t,a)*Ftg(t,g)*(1.-exp(-Zta_f(t,a)));
        tmpDirCatch_f(a) /= Zta_f(t,a);

        // determine how much onboard catch is released at-sea
        // Directed catch
        tmpDirRel(a) = tmpDirCatch_m(a)*(1.-pR_m(a)) + tmpDirCatch_f(a)*(1.-pR_f(a));
        tmpDirLand(a) = value(tmpDirCatch_m(a)*pR_m(a) + tmpDirCatch_f(a)*pR_f(a) );
        
        // Undirected catch  
        //   Separate fleet F into a directed and undirected portion (as below)
        //   and deviate peak selectivity downwards in years where we have data
        //
        tmpUndirCatch_m(a) = (1-pDir_tg(t,g)) * Bta_m(t,a)*selUndir_gta_m(g,t,a)*Ftg(t,g)*(1.-exp(-Zta_m(t,a)));
        tmpUndirCatch_m(a) /= Zta_m(t,a);

        tmpUndirCatch_f(a) = (1-pDir_tg(t,g)) * Bta_f(t,a)*selUndir_gta_f(g,t,a)*Ftg(t,g)*(1.-exp(-Zta_f(t,a)));
        tmpUndirCatch_f(a) /= Zta_f(t,a);

        tmpUndirRel(a) = tmpUndirCatch_m(a)*(1.-pR_m(a)) + tmpUndirCatch_f(a)*(1.-pR_f(a));
        tmpUndirLand(a) = value(tmpUndirCatch_m(a)*pR_m(a) + tmpUndirCatch_f(a)*pR_f(a) );   
    }        
      
    landedCtg(t,g) = sum(tmpDirLand + tmpUndirLand);
    relCtg(t,g) = sum(tmpDirRel + tmpUndirRel);
  }
  
  //-----------------------------------------------------
  // Observation models for landed catch by year/gear
  //-----------------------------------------------------
  // males                               females
  dvar_vector Bprime_m(1,plusGroupAge);  dvar_vector Bprime_f(1,plusGroupAge);
  dvar_vector Bprime_mU(1,plusGroupAge); dvar_vector Bprime_fU(1,plusGroupAge);
  dvar_vector pR_m = row(pRet_m,t);      dvar_vector pR_f = row(pRet_f,t);
  dvar_vector Za_m = row(Zta_m,t);       dvar_vector Za_f = row(Zta_f,t);
  dvar_vector tmp_m(1,plusGroupAge);     dvar_vector tmp_f(1,plusGroupAge);
  dvar_vector tmp_mU(1,plusGroupAge);    dvar_vector tmp_fU(1,plusGroupAge);


  // Predicted combined male/female catch by fishery
  for( int g=1;g<=nFisheries-2;g++ )
  {
    // compute predicted total catch
    // Fully-selected total biomass of males and females
    Bprime_m = elem_prod( row(sel_gta_m(g),t),row(Bta_m,t) );
    Bprime_f = elem_prod( row(sel_gta_f(g),t),row(Bta_f,t) );

    Bprime_mU = elem_prod( row(selUndir_gta_m(g),t),row(Bta_m,t) );
    Bprime_fU = elem_prod( row(selUndir_gta_f(g),t),row(Bta_f,t) );

    tmp_m = elem_div( elem_prod( Bprime_m*Ftg(t,g),1.-mfexp(-Za_m) ), Za_m );
    tmp_m = pDir_tg(t,g) * elem_prod( tmp_m, row(pR_gta_m(g),t) );
    tmp_f = elem_div( elem_prod( Bprime_f*Ftg(t,g),1.-mfexp(-Za_f) ), Za_f );
    tmp_f = pDir_tg(t,g) * elem_prod( tmp_f, row(pR_gta_f(g),t) );

    tmp_mU = elem_div( elem_prod( Bprime_mU*Ftg(t,g),1.-mfexp(-Za_m) ), Za_m );
    tmp_mU = (1 - pDir_tg(t,g) ) * elem_prod( tmp_m, row(pR_gta_m(g),t) );
    tmp_fU = elem_div( elem_prod( Bprime_fU*Ftg(t,g),1.-mfexp(-Za_f) ), Za_f );
    tmp_fU = (1 - pDir_tg(t,g) ) * elem_prod( tmp_f, row(pR_gta_f(g),t) );



    pCtg(t,g) = sum( tmp_m + tmp_f + tmp_mU + tmp_fU );

    //if( g==2 )
      //cout << pCtg(t,2) << endl;
  }

  //cout << row(pCtg,t) << endl;
  //if( t==51 ) exit(1);
  // At end, get legal and sublegal biomasses
  if( last_phase() )
  {
    legalBt(t) = 0.; subLegalBt(t) = 0.;
    for( int a=1; a<=plusGroupAge; a++ )
    {
      //legalBt(t)     += value( Bta_m(t,a)*pRet_m(t,a) + Bta_f(t,a)*pRet_f(t,a) );
      //subLegalBt(t)  += value( Bta_m(t,a)*(1.-pRet_m(t,a)) + Bta_f(t,a)*(1.-pRet_f(t,a)) );
      legalBt(t)     += Bta_m(t,a)*pRet_m(t,a) + Bta_f(t,a)*pRet_f(t,a);
      subLegalBt(t)  += Bta_m(t,a)*(1.-pRet_m(t,a)) + Bta_f(t,a)*(1.-pRet_f(t,a));
      // save sdreport_numbers
      legalHR    = elem_div( rowsum(landedCtg), legalBt(1,nT) );
      LHR2015    = legalHR(nT);
      subLegalHR = elem_div( rowsum(relCtg), subLegalBt(1,nT) );
      SLHR2015   = subLegalHR(nT);        
    }
  }
//*******************************************************************/
FUNCTION solveBaranov_multi_fleet
  RETURN_ARRAYS_INCREMENT();
  int t = baranov_time;
  dvariable f;
  dvariable J;
  dvar_vector Bprime_m(1,plusGroupAge);
  dvar_vector Bprime_f(1,plusGroupAge);
  dvar_vector tmp_m(1,plusGroupAge);
  dvar_vector tmp_f(1,plusGroupAge);
  dvar_vector Za_m(1,plusGroupAge); 
  dvar_vector Za_f(1,plusGroupAge); 
  dvar_vector ZaNew_m(1,plusGroupAge);  
  dvar_vector ZaNew_f(1,plusGroupAge);  
  dvar_vector pR_m = row(pRet_m,t);
  dvar_vector pR_f = row(pRet_f,t);

  // Initialize Z to current vector of M
  ZaNew_m.initialize(); ZaNew_f.initialize(); 
  ZaNew_m = M_m;
  ZaNew_f = M_f; 
  // Initial approximation of F and Z...
  for( int g=1;g<=nFisheries-2;g++ )
  {  
    // Fully-selected total biomass of legal males and females
    Bprime_m = elem_prod(elem_prod( row(sel_gta_m(g),t),row(Bta_m,t) ), row(pR_gta_m(g),t) );
    Bprime_f = elem_prod(elem_prod( row(sel_gta_f(g),t),row(Bta_f,t) ), row(pR_gta_f(g),t) );

    /*cout << "g=" << g << endl;
    cout << Bprime_m << endl;
    cout << Bprime_f << endl;
    */
    Ftg(t,g) = Ctg(t,g)/(sum(Bprime_m) + sum(Bprime_f) );

    ZaNew_m += elem_prod(sel_gta_m(g,t)*Ftg(t,g),row(pR_gta_m(g),t)+(1.-row(pR_gta_m(g),t))*dM(g));
    ZaNew_f += elem_prod(sel_gta_f(g,t)*Ftg(t,g),row(pR_gta_f(g),t)+(1.-row(pR_gta_f(g),t))*dM(g));

    /*cout <<"t = "<< t <<" g = "<< g << endl;
    cout << " sum(Bprime_m) = "<< sum(Bprime_m) <<" sum(Bprime_f) = "<< sum(Bprime_f) << endl;   
    cout << " Ftg = "<< Ftg(t, g) <<" Ctg = "<< Ctg(t, g) << endl;   
    //cout << " ZaNew_m = " << ZaNew_m << endl;
    //cout << " ZaNew_f = " << ZaNew_f << endl;  
    cout << " pR_m = " << pR_m << endl;
    cout << " pR_f = " << pR_f << endl;
    */      
  }

  // refine F for major fisheries (surveys not critical)
  for( int i=1; i<=baranovIter; i++ )
  {
    // Save old Z value and then revise new
    Za_m = ZaNew_m; ZaNew_m = M_m;
    Za_f = ZaNew_f; ZaNew_f = M_f;

    for( int g=1;g<=nFisheries-2;g++ )
    {
      f = 0.;
      // compute predicted total catch
      // Fully-selected total biomass of legal males and females

      Bprime_m = elem_prod( row(sel_gta_m(g),t),row(Bta_m,t) );
      Bprime_f = elem_prod( row(sel_gta_f(g),t),row(Bta_f,t) );

      /*
      cout << "g=" << g << endl;
      cout << Bprime_m << endl;
      cout << Bprime_f << endl;
      */
      tmp_m = elem_div( elem_prod( Bprime_m*Ftg(t,g),1.-mfexp(-Za_m) ), Za_m );
      tmp_m = elem_prod( tmp_m, row(pR_gta_m(g),t) );
      tmp_f = elem_div( elem_prod( Bprime_f*Ftg(t,g),1.-mfexp(-Za_f) ), Za_f );
      tmp_f = elem_prod( tmp_f, row(pR_gta_f(g),t) );

      // Function value: difference of pred - obs catch
      f =  sum(tmp_m + tmp_f) - Ctg(t,g);
      pCtg(t,g) = sum( tmp_m + tmp_f );
      //cout <<"iter = "<< i << " Ftg = "<< Ftg(t, g) << " pCtg = " << pCtg(t,g) << endl;   
      
      tmp_m=0.;tmp_f=0.;

      // Jacobian: males
      dvar_vector tmp1 = elem_prod(row(pR_gta_m(g),t),Bprime_m);
      dvar_vector tmp2 = elem_prod(sel_gta_m(g,t),row(pR_gta_m(g),t)+(1.-row(pR_gta_m(g),t))*dM(g));
      dvar_vector tmp3 = 1.-mfexp(-Za_m);
      dvar_vector tmp4 = elem_prod(tmp1,tmp3);
      dvar_vector tmp5 = elem_div(elem_prod(elem_prod(Ftg(t,g)*tmp1,mfexp(-Za_m)),tmp2),Za_m);
      dvar_vector tmp6 = elem_div(elem_prod(elem_prod(Ftg(t,g)*tmp1,tmp3),tmp2),elem_prod(Za_m,Za_m));
      dvar_vector dfm  = elem_div(tmp4,Za_m) + tmp5 - tmp6;

      tmp1 = 0.; tmp2 = 0.; tmp3 = 0.; tmp4 = 0.; tmp5 = 0.; tmp6 = 0.;

      // Jacobian: females
      tmp1 = elem_prod(row(pR_gta_f(g),t),Bprime_f);
      tmp2 = elem_prod(sel_gta_f(g,t),row(pR_gta_f(g),t)+(1.-row(pR_gta_f(g),t))*dM(g));
      tmp3 = 1.-mfexp(-Za_f);
      tmp4 = elem_prod(tmp1,tmp3);
      tmp5 = elem_div(elem_prod(elem_prod(Ftg(t,g)*tmp1,mfexp(-Za_f)),tmp2),Za_f);
      tmp6 = elem_div(elem_prod(elem_prod(Ftg(t,g)*tmp1,tmp3),tmp2),elem_prod(Za_f,Za_f));
      dvar_vector dff  = elem_div(tmp4,Za_f) + tmp5 - tmp6;

      J = sum(dfm+dff);
      Ftg(t,g) -= f/J;
      tmp1 = 0.; tmp2 = 0.; tmp3 = 0.; tmp4 = 0.; tmp5 = 0.; tmp6 = 0.;

      ZaNew_m += elem_prod( row(sel_gta_m(g),t)*Ftg(t,g),row(pR_gta_m(g),t)+(1.-row(pR_gta_m(g),t))*dM(g));
      ZaNew_f += elem_prod( row(sel_gta_f(g),t)*Ftg(t,g),row(pR_gta_f(g),t)+(1.-row(pR_gta_f(g),t))*dM(g));
      
      /*cout <<"iter = "<< i <<" g = "<<g << " t = " << t << " phase = "<< current_phase() <<endl;
      cout << "pR_m = " << pR_m << endl;
      cout << "pR_f = " << pR_f << endl;
      cout << "Ftg = "<< Ftg(t, 1) << endl;   
      cout <<"f = "<< f << " J = "<< J << " dfm = " << sum(dfm) << " dff = " << sum(dff) << endl;   
      */
    }
  }
  //exit(1);
  for( int g=1;g<=nFisheries-2;g++ )
  {
    f = 0.;
    // compute predicted total catch
    // Fully-selected total biomass of males and females
    Bprime_m = elem_prod( row(sel_gta_m(g),t),row(Bta_m,t) );
    Bprime_f = elem_prod( row(sel_gta_f(g),t),row(Bta_f,t) );

    tmp_m = elem_div( elem_prod( Bprime_m*Ftg(t,g),1.-mfexp(-Za_m) ), Za_m );
    tmp_m = elem_prod( tmp_m, row(pR_gta_m(g),t) );
    tmp_f = elem_div( elem_prod( Bprime_f*Ftg(t,g),1.-mfexp(-Za_f) ), Za_f );
    tmp_f = elem_prod( tmp_f, row(pR_gta_f(g),t) );
    pCtg(t,g) = sum( tmp_m + tmp_f );    
    tmp_m=0.;tmp_f=0.;
  }
  Zta_m(t) = ZaNew_m;
  Zta_f(t) = ZaNew_f;
  
  RETURN_ARRAYS_DECREMENT();
//*******************************************************************/

//*******************************************************************/      
REPORT_SECTION
  time(&finish);
  elapsed_time=difftime(finish,start);
  hour=long(elapsed_time)/3600;
  minute=long(elapsed_time)%3600/60;
  second=(long(elapsed_time)%3600)%60;
  report<<endl<<endl<<"## *******************************************"<<endl;
  report<<"## --Start time: "<<ctime(&start)<<endl;
  report<<"## --Finish time: "<<ctime(&finish)<<endl;
  report<<"## --Runtime: ";
  report<<hour<<" hours, "<<minute<<" minutes, "<<second<<" seconds"<<endl;
  report<<"## *******************************************"<<endl;

  // Minimization performance details
  report << endl;
  report << "## Minimization performance" << endl;
  report << "# validObs_idx" << endl;           report << validObs_idx << endl;
  report << "# n_residuals_f" << endl;          report << n_residuals_f << endl;    
  report << "# n_residuals_m" << endl;          report << n_residuals_m << endl;    
  report << "# indexLikelihood" << endl;        report << indexLikelihood << endl;
  report << "# ageLikelihood_m" << endl;        report << ageLikelihood_m << endl;
  report << "# ageLikelihood_f" << endl;        report << ageLikelihood_f << endl;
  report << "# releaseLikelihood" << endl;      report << releaseLikelihood << endl;
  report << "# dataLikelihood" << endl;         report << dataLikelihood << endl;
  report << "# fLike" << endl;                 report << fLike << endl;
  report << "# catLike" << endl;                 report << catLike << endl;
  report << "# mPrior" << endl;                 report << mPrior << endl;
  report << "# ssbPrior" << endl;               report << ssbPrior << endl;
  report << "# selPrior" << endl;               report << selPrior << endl;
  report << "# recPrior"<< endl;                report << recPrior << endl;
  report << "# qdevPrior"<< endl;                report << qdevPrior << endl;
  report << "# hPrior"<< endl;                  report << hPrior << endl;
  report << "# pDirPrior"<< endl;                  report << pDirPrior << endl;
  report << "# dirCatPrior"<< endl;                  report << dirCatPrior << endl;
  report << "# undirCatPrior"<< endl;                  report << undirCatPrior << endl;
  report << "# objFun"  << endl;    report << *objective_function_value::pobjfun << endl;
  report << "# maxGrad" << endl;    report << objective_function_value::gmax << endl;
  report << "# exitCode"<< endl;    report << iexit << endl;
  report << "# funEvals"<< endl;    report << neval << endl;
  // Derived likelihood components
  report << "## Standard error estimates" << endl;
  report << "# tauIndex" << endl;   report << sqrt(tauSquareIndex) << endl;
  report << "# tauAge_m" << endl;   report << sqrt(tauSquareAges_m) << endl;
  report << "# tauAge_f" << endl;   report << sqrt(tauSquareAges_f) << endl;
  report << "# tauRel"   << endl;   report << sqrt(tauSquareReleases) << endl;

  // Parameter estimates
  report << "## Parameter estimates" << endl;
  report << "# avgR"   << endl;     report << avgR << endl;
  report << "# h"   << endl;        report << h << endl;
  report << "# SSB0"    << endl;    report << SSB0 << endl;
  report << "# D2015"    << endl;   report << D2015 << endl;
  report << "# R0"      << endl;    report << R0 << endl;
  report << "# M_m" << endl;        report << M_m << endl;
  report << "# M_f" << endl;        report << M_f << endl;
  report << "# sigma_R" << endl;    report << sigma_R << endl;
  report << "# rho" << endl;        report << rho << endl;
  report << "# recDevs" << endl;    report << omega_t << endl;
  report << "# log_Fg1"   << endl;      report << log_Fg1 << endl;
  report << "# log_Fdevs_gt"   << endl; report << log_Fdevs_gt << endl;
  
  // selectivity parameters in first year
  report << "# alpha_g1" << endl;    report << alpha_g1 << endl;
  report << "# beta_g1"  << endl;    report << beta_g1  << endl;
  // selectivity parameters for all years
  report << "# alpha_gt_m" << endl;    report << alpha_gt_m << endl;
  report << "# beta_gt_m"  << endl;    report << beta_gt_m  << endl;
  report << "# alpha_gt_f" << endl;    report << alpha_gt_f << endl;
  report << "# beta_gt_f"  << endl;    report << beta_gt_f  << endl;
  // Undirected selectivity parameters
  report << "# alphaU_gt_m" << endl;    report << alphaUndir_gt_m << endl;
  report << "# alphaU_gt_f" << endl;    report << alphaUndir_gt_f << endl;
  report << "# betaU_gt_m" << endl;     report << betaUndir_gt_m << endl;
  report << "# betaU_gt_f" << endl;     report << betaUndir_gt_f << endl;

  report << "# qg"       << endl;   report << mfexp( lnq ) << endl;
  for( int i=1; i <= nIndexSeries; i ++ )
  {
    int g=idxIndex[i];
    report << "# q_"<<g<<"t"<< endl; 
    report << exp(row(lnq_it,i))<< endl;
  }

  report << "# reportRate_gaf"  << endl;
  report << reportRate_ga_f << endl;
  

  report << "# reportRate_gam"  << endl;
  report << reportRate_ga_m << endl;

  // High-grading 50% length
  report << "# hg50" << endl;    report << mfexp(log_hg50) << endl;

  // Proportion directed
  report << "# pDir_t1" << endl;    report << pDir_t1 << endl;  
  report << "# pDir_t2" << endl;    report << pDir_t2 << endl;  
  report << "# pDir_tg" << endl;    report << pDir_tg << endl;  
  report << "# logit_pDir_g" << endl;    report << logit_pDir_g << endl;  
  report << "# ph_logit_pDir" << endl;    report << ph_logit_pDir << endl;  
  report << "# priorSD_logit_pDir" << endl;    report << priorSD_logit_pDir << endl;  
  report << "# priorSD_seldev_pDir" << endl;    report << priorSD_seldev_pDir << endl;  
  report << "# selAlphaDirDev_base " << endl;   report << selAlphaDirDev_base << endl;

  // Output concentrated obs error quantities
  report << "# validObs_idx" << endl; report << validObs_idx << endl;
  report << "# validObs_rel" << endl; report << validObs_rel << endl;

  // Derived variables
  report << endl;
  report << "## Derived variables" << endl;
  report << "# bh_a" << endl;      report << bh_a << endl;
  report << "# bh_b" << endl;      report << bh_b << endl;  
  report << "# projDep" << endl;    report << SSBt(nT+1)/SSBt(1) << endl;
  report << "# projSSB" << endl;    report << SSBt(nT+1) << endl;
  report << "# projLegalB" << endl; report << legalBt(nT+1) << endl;
  report << "# projSublegalB" << endl; report << subLegalBt(nT+1) << endl;  
  
  report << "# SSBt" << endl;       report << SSBt(1,nT) << endl;
  report << "# Rt" << endl;         report << Rt(1,nT) << endl;
  report << "# legalBt" << endl;    report << legalBt(1,nT) << endl;
  report << "# sublegalBt" << endl; report << subLegalBt(1,nT) << endl;
  
  // Legal and subLegal total harvest rates
  //legalHR    = elem_div( rowsum(landedCtg), legalBt(1,nT) );
  report << "# legalHR" << endl;    report << legalHR << endl;

  //subLegalHR = elem_div( value(rowsum(relCtg)), subLegalBt(1,nT) );
  report << "# subLegalHR" << endl; report << subLegalHR << endl;


  report <<"## Total Observed landings by fishery"<< endl;
  for( int g=1;g<=nFisheries-2;g++ )
  {
    report << "# Ctg"<<g<< endl; 
    report << column(Ctg,g)<< endl;
  }
 
  report <<"## Fishing mortality rates by fishery"<< endl;
  for( int g=1;g<=nFisheries-2;g++ )
  {
    report << "# Ftg"<<g<< endl; 
    report << column(Ftg,g)<< endl;
  }

  report <<"## Legal landings by fishery"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# landedCtg"<<g<< endl; 
    report << column(landedCtg,g)<< endl;
  }
  
  report <<"## Total model discards by fishery"<< endl;
  for( int g=1;g<=nFisheries-2;g++ )
  {
    report << "# relCtg"<<g<< endl; 
    report << column(relCtg,g)<< endl;
  }

  report <<"## Total observed discards by fishery"<< endl;
  for( int g=1;g<=nFisheries-2;g++ )
  {
    report << "# obs_relCtg"<<g<< endl; 
    report << column(obs_relCtg,g)<< endl;
  }

  report <<"## Undirected landings and releases by fishery"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# undirLandCtg"<<g<< endl; 
    report << column(undirLandCtg,g)<< endl;
  }

  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# undirRelCtg"<<g<< endl; 
    report << column(undirRelCtg,g)<< endl;
  }

  report <<"## Directed landings and releases by fishery"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# dirLandCtg"<<g<< endl; 
    report << column(dirLandCtg,g)<< endl;
  }
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# dirRelCtg"<<g<< endl; 
    report << column(dirRelCtg,g)<< endl;
  }

  report <<"## Predicted total landings by fishery"<< endl;
  for( int g=1;g<=nFisheries-2;g++ )
  {
    report << "# pCtg" << g << endl; 
    report << column(pCtg,g)<< endl;
  }
  

  /*report <<"## Legal (dead) discards by fishery"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# legalDtg"<<g<< endl; 
    report << row(legalDtg,g)<< endl;
  }
  */
  report <<"## SubLegal (dead) discards by fishery"<< endl;
  dvector tmp_D(1,nT);
  for( int g=1;g<=nFisheries-2;g++ )
  {
    tmp_D = value(column(relCtg,g)*(1.-exp(-dM(g))));

    report << "# sublegalDtg"<<g<< endl; 
    report << tmp_D << endl;
  }
  report <<"## Exploitable biomass by fishery (vals < 0 are missing...)"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# expBit"<<g<< endl; 
    report << row(expBit,g) << endl;
  }
  report <<"## TotalMortality-at-age for males and females"<< endl;
  report << "# Zta_m" << endl;      report << Zta_m << endl;
  report << "# Zta_f" << endl;      report << Zta_f << endl;

  report <<"## Numbers-at-age for males and females"<< endl;  
  report << "# Nta_m" << endl;      report << Nta_m << endl;
  report << "# Bta_m" << endl;      report << Bta_m << endl;

  report << "# Nta_f" << endl;      report << Nta_f << endl;
  report << "# Bta_f" << endl;      report << Bta_f << endl;

  report << "# pRet_m" << endl;      report << pRet_m << endl;
  report << "# pRet_f" << endl;      report << pRet_f << endl;

  // Selectivity
  report << "# selType" << endl;    report << selType << endl;
  
  report <<"## Selectivity-at-age by fishery: males"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# sel_gta_m"<<g<< endl;
    report << sel_gta_m(g) << endl;      
  }  
  report <<"## Selectivity-at-age by fishery: females"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# sel_gta_f"<<g<< endl; 
    report << sel_gta_f(g)<< endl;
  }

  report <<"## Undirected Selectivity-at-age by fishery: males"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# selU_gta_m"<<g<< endl;
    report << selUndir_gta_m(g) << endl;      
  }  
  report <<"## Selectivity-at-age by fishery: females"<< endl;
  for( int g=1;g<=nFisheries;g++ )
  {
    report << "# selU_gta_f"<<g<< endl; 
    report << selUndir_gta_f(g)<< endl;
  }

  report << "## Selectivity blocks start and end timesteps" << endl;
  report << "# selBlockStart_g1" << endl;
  report << selBlockStart_g1 << endl;

  report << "# selBlockEnd_g1" << endl;
  report << selBlockEnd_g1 << endl;

  report << "# selBlockStart_g2" << endl;
  report << selBlockStart_g2 << endl;

  report << "# selBlockEnd_g2" << endl;
  report << selBlockEnd_g2 << endl;

  report << "# selBlockStart_g3" << endl;
  report << selBlockStart_g3 << endl;

  report << "# selBlockEnd_g3" << endl;
  report << selBlockEnd_g3 << endl;

  // Life History Schedules
  report << "# ages "    << endl;   report << ages << endl;
  report << "# lenAge_m" << endl;   report << lenAge_m << endl;
  report << "# lenAge_f" << endl;   report << lenAge_f << endl;
  report << "# wtAge_m"  << endl;   report << wtAge_m << endl;
  report << "# wtAge_f"  << endl;   report << wtAge_f << endl;
  report << "# matAge_f" << endl;   report << matAge_f << endl;
  report << "# sizeLim"<< endl;   report << sizeLim << endl;
  report << "# sizeLimYear"<< endl; report << sizeLimYear << endl;

  report <<"## Scaled CPUE indices by fishery (vals < 0 are missing...)"<< endl;
  for( int i=1;i<=nIndexSeries;i++ )
  {
    int g=idxIndex[i];
    report << "# Itg"<<g<< endl; 
    report << elem_div(row(idxSeries,i),exp(row(lnq_it,i)))<< endl;
  }
  report <<"## Predicted male age proportions by fishery"<< endl;
  for( int i=1;i<=nAgeSeries;i++ )
  {
    report << "# uCgta_m"<<fisheryAgeID(i)<< endl; 
    report << predObsProp_m(i) << endl;
  }
  report <<"## Predicted female age proportions by fishery"<< endl;
  for( int i=1;i<=nAgeSeries;i++ )
  {
    report << "# uCgta_f"<<fisheryAgeID[i]<< endl; 
    report << predObsProp_f[i] << endl;
  }
  //--------------------------------------------------------------------------//
  // Begin echo of inputs for guiMOd()   ARK (03-Sep-10)                      //
  //--------------------------------------------------------------------------//

  report << "## sableOpMod Report" << endl;

  // Echo operating model parameters.

  // Echo data inputs from opModCatch.dat.
  report << "# nT" << endl;
  report << nT << endl;
  report << "# nFisheries" << endl;
  report << nFisheries << endl;

  report << "# landCatchMatrix" << endl;
  report << landCatchMatrix << endl;
  report << "# releaseMatrix" << endl;
  report << releaseMatrix << endl;

  // Echo data inputs from opModIndex.dat.
  report << "# nIndexSeries" << endl;
  report << nIndexSeries << endl;
  report << "# idxIndex" << endl;
  report << idxIndex << endl;
  report << "# idxLikeWeight" << endl;
  report << idxLikeWeight << endl;
  report << "# idxFirstYear" << endl;
  report << idxFirstYear << endl;
  report << "# idxLastYear" << endl;
  report << idxLastYear << endl;
  report << "# idxSeries" << endl;
  report << idxSeries << endl;

  // Echo data inputs from opModAges.dat.
  report << "# ages" << endl;
  report << ages << endl;
  report << "# minAge" << endl;
  report << minAge << endl;
  report << "# plusGroupAge" << endl;
  report << plusGroupAge << endl;
  report << "# nAgeSeries" << endl;
  report << nAgeSeries << endl;
  report << "# fisheryAgeID" << endl;
  report << fisheryAgeID << endl;
  report << "## ageObsProp_m (written as nAgeSeries slices)" << endl;
  for ( int i=1; i<=nAgeSeries;i++ )
  {
    report << "# ageObsProp_m" << i << endl;
    report << ageObsProp_m[i] << endl;
  }
  report << "## ageObsProp_f (written as nAgeSeries slices)" << endl;
  for ( int i=1; i<=nAgeSeries;i++ )
  {
    report << "# ageObsProp_f" << i << endl;
    report << ageObsProp_f[i] << endl;
  }
  // Echo data from opModLifeScheds.dat.
  report << "# sizeLim" << endl;
  report << sizeLim << endl;
  report << "# dM" << endl;
  report << dM << endl;
  report << "# lenAge_m" << endl;
  report << lenAge_m << endl;
  report << "# lenAge_f" << endl;
  report << lenAge_f << endl;
  report << "# wtAge_m" << endl;
  report << wtAge_m << endl;
  report << "# wtAge_f" << endl;
  report << wtAge_f << endl;
  report << "# matAge_f" << endl;
  report << matAge_f << endl;

  // Echo fixed inputs from control file

  // Echo selectivity priors
  report << "p_alpha_gt" << endl;
  report << p_alpha_gt << endl;
  report << "p_beta_gt" << endl;
  report << p_beta_gt << endl;
  report << "sd_alpha_gt" << endl;
  report << sd_alpha_gt << endl;
  report << "sd_beta_gt" << endl;
  report << sd_beta_gt << endl;

  // Echo ageing error matrix
  report << "# Q" << endl;
  report << Q << endl;

  //--------------------------------------------------------------------------//
  // End echo of inputs for guiMod()                                          //
  //--------------------------------------------------------------------------//  
