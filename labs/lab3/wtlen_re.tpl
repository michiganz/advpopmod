//////////////////////////////////////////////
//  MAR 580: Advanced Population Modeling
//  Fall 2015
//  Instructor: Gavin Fay
//  Student: Phil Ganz
//  Lab 1: Linear regression example
//////////////////////////////////////////////

DATA_SECTION
  //Number of observations
  init_int nobs;
  //Recognize data as matrix 
  init_matrix data(1,nobs,1,3);
  //Create vectors for data columns
  ivector subject(1,nobs);
  vector log_length(1,nobs);
  vector log_weight(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! subject=(ivector)column(data,1);
  !! log_length=log(column(data,2));
  !! log_weight=log(column(data,3));
  //Determine number of subjects (number of unique a params)
  number nsubs
  !! nsubs=max(subject);
  
  //Declare counting variable
  int i

PARAMETER_SECTION
  init_number b;
  init_number log_sigma;
  //init_number log_mu;
  init_number log_sigma_log_a(2);

  sdreport_number sigma;
  sdreport_number sigma_log_a;

  random_effects_vector log_a(1,nsubs,2);

  vector log_weight_pred(1,nobs);
  vector weight_pred(1,nobs);
  objective_function_value obj_fun;

PROCEDURE_SECTION
  //transformations
  sigma = mfexp(log_sigma);
  sigma_log_a = mfexp(log_sigma_log_a);

  //predictions
  //log_weight_pred = log_a(subject) + b*log_length;
  log_weight_pred = sigma_log_a*log_a(subject) + b*log_length;

  //distribution for the random effects
  //obj_fun = nobs*log_sigma_log_a + 0.5*norm2(log_a-log_mu)/square(sigma_log_a);
  // Normal (0,1) random variables  
  obj_fun = log(1.) + 0.5*norm2(log_a)/square(1.);  
  // we can also write this as obj_fun = 0.5*norm2(b);

  //Assuming normal error around logged values
  obj_fun += nobs*log_sigma + 0.5*norm2(log_weight-log_weight_pred)/square(sigma);
  
REPORT_SECTION
  report << "random effects (log_a)" << endl;
  report << sigma_log_a*log_a << endl;   
  report << "mean(log_a)" << endl;
  report << mean(sigma_log_a*log_a) << endl;   
  report<<"b"<<endl;
  report<<b<<endl;  
  report<<"predicted weights"<<endl;
  report<<mfexp(log_weight_pred)<<endl;
  report<<"obj_fun"<<endl;
  report<<obj_fun<<endl;
  