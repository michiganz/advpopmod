//////////////////////////////////////////////
//  MAR 580: Advanced Population Modeling
//  Fall 2015
//  Steve Cadrin
//
//  Lab 4: Biomass Dynamics
//  time series observation error model assuming logistic growth
//////////////////////////////////////////////
DATA_SECTION
  init_int nyear;
  init_vector year(1,nyear);  // calendar year
  init_vector yield(1,nyear); // yield (catch biomass)
  init_vector cpue(1,nyear);

PARAMETER_SECTION
  init_number r;     // intrinsic growth rate
  init_number K;  // carrying capacity (same units as catch)
  init_number B1;                 // biomass in year 1 (same units as catch)
  init_bounded_number q(0,1);     // catchability of CPUE fleet
  init_number logsigma;

  vector biomass(1,nyear);       // time series of biomass estimates (same units as catch)
  vector logcpue(1,nyear);         // Polachek et al. 1993 assume multiplicative observation error in cpue 
  vector logcpue_pred(1,nyear);  // model predictions of log CPUE series
  vector resid(1,nyear)          // log residual
  vector effort(1,nyear)         // effort derived from catch and CPUE
  vector F(1,nyear)              // fishing mortality
  number MSY                     // maximum sustainable yield
  number Bmsy                    // biomass associated with MSY
  number Fmsy                    // F associated with MSY
  vector rel_biomass             // B/Bmsy
  vector rel_F                   // F/Fmsy
  objective_function_value obj_fun;

PRELIMINARY_CALCS_SECTION
// output data series to screen
  cout << year << endl;
  cout << yield << endl;
  cout << cpue << endl;
// log transform cpue
  logcpue = log(cpue);
// output transformed data
  cout << logcpue << endl;
// derive effort from catch and CPUE
  effort = elem_div(yield, cpue);
// output effort series
  cout << effort << endl;

INITIALIZATION_SECTION
  r 0.5;
  K 200;
  B1 100;
  q 0.01;
  logsigma 10;

PROCEDURE_SECTION
// begin time series of biomass estimates with estimated parameter B1
// if time series includes the beginning of the fishery, B1~K may be a reasonable assumption
  biomass(1) = B1;
// for loop to derive time series of biomass estimates as a function of biomass in the previous year, catch in the previous year, r, and K
// with an if condition to avoid negative biomass values during iterative search
  for (int i=1; i<=nyear-1; i++)
  if (biomass(i)+r*biomass(i)*(1-biomass(i)/K)-yield(i)<0) biomass(i+1) = 0.001;
  else biomass(i+1) = biomass(i)+r*biomass(i)*(1-biomass(i)/K)-yield(i);
  logcpue_pred = log(q * biomass);
// calculate model residuals
  resid = logcpue - logcpue_pred;
// derive MSY reference points from logistic growth parameters
  MSY = (r * K) / 4;
  Fmsy = r / 2;
  Bmsy = K / 2;
// caldulate F and relative stock status
  F = q * effort;
  rel_biomass = biomass / Bmsy;
  rel_F = F / Fmsy;
// normal likelihood function
  obj_fun = sum(logsigma+(1/(2*exp(logsigma)*exp(logsigma)))*square(resid));
// least squares objective function
// obj_fun = norm2(resid);

REPORT_SECTION
//  report model estimates
  report << "CPUE residuals" << endl;
  report << resid << endl;
  report << "Biomass estimates" << endl;
  report << biomass << endl;
  report << "Fishing mortality estimates" << endl;
  report << F << endl;
  report << "MSY" << endl;
  report << MSY << endl;
  report << "Bmsy" << endl;
  report << Bmsy << endl;
  report << "Fmsy" << endl;
  report << Fmsy << endl;
  report << "Relative biomass estimates (B/Bmsy)" << endl;
  report << rel_biomass << endl;
  report << "Relative F estimates (F/Bmsy)" << endl;
  report << rel_F << endl;



