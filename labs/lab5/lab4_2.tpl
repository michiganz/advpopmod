//////////////////////////////////////////////
//  MAR 580: Advanced Population Modeling
//  Fall 2015
//  Steve Cadrin
//
//  Lab 4: Biomass Dynamics
//  time series observation error model assuming logistic growth
//////////////////////////////////////////////
DATA_SECTION
  init_int nyear;    // number of years for which any cpue is available
  init_int nyear_1;  // number of years for which fleet 1 cpue is available
  init_int nyear_2;  // number of years for which fleet 2 cpue is available
  init_int nyear_4;  // number of years for which fleet 4 cpue is available
  init_int nyear_7;  // number of years for which fleet 7 cpue is available
  init_int nfleet;   // number of fleets
  
  number styr;
  number endyr;
  number styr_1;
  number endyr_1;
  number styr_2;
  number endyr_2;
  number styr_4;
  number endyr_4;
  number styr_7;
  number endyr_7;

  // total yield
  init_vector year(1,nyear);
  !! styr = min(year);
  !! endyr = max(year);
  init_vector yield(styr,endyr);
  
  // cpue by fleet
  init_vector year_1(1,nyear_1);
  !! styr_1 = min(year_1);
  !! endyr_1 = max(year_1);
  init_vector yield_1(styr_1,endyr_1);
  init_vector cpue_1(styr_1,endyr_1);

  init_vector year_2(1,nyear_2);
  !! styr_2 = min(year_2);
  !! endyr_2 = max(year_2);
  init_vector yield_2(styr_2,endyr_2);
  init_vector cpue_2(styr_2,endyr_2);
  
  init_vector year_4(1,nyear_4);
  !! styr_4 = min(year_4);
  !! endyr_4 = max(year_4);
  init_vector yield_4(styr_4,endyr_4);
  init_vector cpue_4(styr_4,endyr_4);

  init_vector year_7(1,nyear_7);
  !! styr_7 = min(year_7);
  !! endyr_7 = max(year_7);
  init_vector yield_7(styr_7,endyr_7);
  init_vector cpue_7(styr_7,endyr_7);

  // end of file marker
  init_int eof;

 LOCAL_CALCS
  if(eof==42) cout<<"The data has been read correctly!";
  else { cout <<"What we have here is failure to communicate"<<endl;exit(1);  }

 END_CALCS

PARAMETER_SECTION
  init_number log_r;  // intrinsic growth rate
  init_number log_K;  // carrying capacity (same units as catch)
  init_number log_B1; // biomass in year 1 (same units as catch)
  
  // fleet-specific catchability/sigma
  init_bounded_number q_1(0,1);
  init_bounded_number q_2(0,1);
  init_bounded_number q_4(0,1);
  init_bounded_number q_7(0,1);
  // make phase -1 for least squares objective function
  init_number logsigma_1(-1);
  init_number logsigma_2(-1);
  init_number logsigma_4(-1);
  init_number logsigma_7(-1);
  
  number r;  
  number K;  
  number B1; 
  
  // dependent variables and transformed data
  vector biomass(styr,endyr);       // time series of biomass estimates (same units as catch)
  
  // predicted cpue by fleet
  vector logcpue_1(styr_1,endyr_1);       // Polachek et al. 1993 assume multiplicative observation error in cpue 
  vector logcpue_pred_1(styr_1,endyr_1);  // model predictions of log CPUE series
  vector resid_1(styr_1,endyr_1);          // log residual
  vector effort_1(styr_1,endyr_1);         // effort derived from catch and CPUE
  
  vector logcpue_2(styr_2,endyr_2);      
  vector logcpue_pred_2(styr_2,endyr_2); 
  vector resid_2(styr_2,endyr_2);          
  vector effort_2(styr_2,endyr_2);    

  vector logcpue_4(styr_4,endyr_4);      
  vector logcpue_pred_4(styr_4,endyr_4); 
  vector resid_4(styr_4,endyr_4);          
  vector effort_4(styr_4,endyr_4); 

  vector logcpue_7(styr_7,endyr_7);      
  vector logcpue_pred_7(styr_7,endyr_7); 
  vector resid_7(styr_7,endyr_7);          
  vector effort_7(styr_7,endyr_7);      

  // fishing mortality by fleet
  vector F_1(styr,endyr);             
  vector F_2(styr,endyr);  
  vector F_4(styr,endyr);  
  vector F_7(styr,endyr);  

  // management reference points
  vector F(styr,endyr);          // fishing mortality
  number MSY                     // maximum sustainable yield
  number Bmsy                    // biomass associated with MSY
  number Fmsy                    // F associated with MSY
  vector rel_biomass             // B/Bmsy
  vector rel_F                   // F/Fmsy

  // objective function 
  number like_1
  number like_2
  number like_4
  number like_7
  number test_obj_fun
  objective_function_value obj_fun;

 LOCAL_CALCS

  // log transform cpue
  logcpue_1 = log(cpue_1);
  logcpue_2 = log(cpue_2);
  logcpue_4 = log(cpue_4);
  logcpue_7 = log(cpue_7);

  // derive effort from catch and CPUE
  for (int i=styr_1; i<=endyr_1; i++){  
  effort_1(i) = yield_1(i)/cpue_1(i);}
  
  for (int i=styr_2; i<=endyr_2; i++){  
  effort_2(i) = yield_2(i)/cpue_2(i);}
  
  for (int i=styr_4; i<=endyr_4; i++){  
  effort_4(i) = yield_4(i)/cpue_4(i);}
  
  for (int i=styr_7; i<=endyr_7; i++){  
  effort_7(i) = yield_7(i)/cpue_7(i);}

 END_CALCS

INITIALIZATION_SECTION
  log_r -0.5;
  log_K  12;
  log_B1 11;
  
  q_1 0.0001;
  q_2 0.0001;
  q_4 0.0001;
  q_7 0.0001;

PROCEDURE_SECTION
  r = mfexp(log_r);
  K = mfexp(log_K);
  B1 = mfexp(log_B1); 

// begin time series of biomass estimates with estimated parameter B1
// if time series includes the beginning of the fishery, B1~K may be a reasonable assumption
  biomass(styr) = B1;
// for loop to derive time series of biomass estimates as a function of biomass in the previous year, catch in the previous year, r, and K
// with an if condition to avoid negative biomass values during iterative search
  for (int i=styr; i<=endyr-1; i++)
  if (biomass(i)+r*biomass(i)*(1-biomass(i)/K)-yield(i)<0) biomass(i+1) = 0.001;
  else biomass(i+1) = biomass(i)+r*biomass(i)*(1-biomass(i)/K)-yield(i);

// predicted values
  for (int i=styr_1; i<=endyr_1; i++){  
  logcpue_pred_1(i) = log(q_1 * biomass(i));}
  
  for (int i=styr_2; i<=endyr_2; i++){ 
  logcpue_pred_2(i) = log(q_2 * biomass(i));}
  
  for (int i=styr_4; i<=endyr_4; i++){  
  logcpue_pred_4(i) = log(q_4 * biomass(i));}
  
  for (int i=styr_7; i<=endyr_7; i++){  
  logcpue_pred_7(i) = log(q_7 * biomass(i));}

// calculate model residuals
  resid_1 = logcpue_1 - logcpue_pred_1;
  resid_2 = logcpue_2 - logcpue_pred_2;
  resid_4 = logcpue_4 - logcpue_pred_4;
  resid_7 = logcpue_7 - logcpue_pred_7;

// derive MSY reference points from logistic growth parameters
  MSY = (r * K) / 4;
  Fmsy = r / 2;
  Bmsy = K / 2;
  
// caldulate F and relative stock status
  F_1 = 0;
  for (int i=styr_1; i<=endyr_1; i++){
  F_1(i) = q_1 * effort_1(i);}

  F_2 = 0;
  for (int i=styr_2; i<=endyr_2; i++){
  F_2(i) = q_2 * effort_2(i);}

  F_4 = 0;
  for (int i=styr_4; i<=endyr_4; i++){
  F_4(i) = q_4 * effort_4(i);}

  F_7 = 0;
  for (int i=styr_7; i<=endyr_7; i++){
  F_7(i) = q_7 * effort_7(i);}

  F = F_1 + F_2 + F_4 + F_7;

  rel_F = F / Fmsy;
  rel_biomass = biomass / Bmsy;

// normal likelihood function
  like_1 = sum(logsigma_1 + (1/(2*exp(logsigma_1)*exp(logsigma_1)))*square(resid_1));
  like_2 = sum(logsigma_2 + (1/(2*exp(logsigma_2)*exp(logsigma_2)))*square(resid_2));  
  like_4 = sum(logsigma_4 + (1/(2*exp(logsigma_4)*exp(logsigma_4)))*square(resid_4));
  like_7 = sum(logsigma_7 + (1/(2*exp(logsigma_7)*exp(logsigma_7)))*square(resid_7));

  obj_fun += like_1;
  obj_fun += like_2;
  obj_fun += like_4;
  obj_fun += like_7;

REPORT_SECTION
  report << "#Year" << endl;
  report << year << endl;
  report << "#Biomass estimates" << endl;
  report << biomass << endl;
  report << "#Yield" << endl;
  report << yield << endl;

//  report model estimates
  report << "#CPUE" << endl;
  report << "#Fleet 1: Observed" << endl;
  report << cpue_1 << endl;
  report << "#Fleet 1: Predicted" << endl;
  report << exp(logcpue_pred_1) << endl;
  report << "#Fleet 2: Observed" << endl;
  report << cpue_2 << endl;
  report << "#Fleet 2: Predicted" << endl;
  report << exp(logcpue_pred_2) << endl;
  report << "#Fleet 4: Observed" << endl;
  report << cpue_4 << endl;
  report << "#Fleet 4: Predicted" << endl;
  report << exp(logcpue_pred_4) << endl;
  report << "#Fleet 7: Observed" << endl;
  report << cpue_7 << endl;
  report << "#Fleet 7: Predicted" << endl;
  report << exp(logcpue_pred_7) << endl;

  report << "#CPUE residuals" << endl;
  report << resid_1 << endl;
  report << resid_2 << endl;
  report << resid_4 << endl;
  report << resid_7 << endl;

  report << "#Fishing mortality estimates" << endl;
  report << F << endl;

  report << "#MSY" << endl;
  report << MSY << endl;

  report << "#Bmsy" << endl;
  report << Bmsy << endl;

  report << "#Fmsy" << endl;
  report << Fmsy << endl;

  report << "#Relative biomass estimates (B/Bmsy)" << endl;
  report << rel_biomass << endl;

  report << "#Relative F estimates (F/Fmsy)" << endl;
  report << rel_F << endl;

RUNTIME_SECTION
  maximum_function_evaluations 10000
