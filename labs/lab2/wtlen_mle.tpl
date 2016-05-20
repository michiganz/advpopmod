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
  vector subject(1,nobs);
  vector log_length(1,nobs);
  vector log_weight(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! subject=column(data,1);
  !! log_length=log(column(data,2));
  !! log_weight=log(column(data,3));
  //Determine number of subjects (number of unique a params)
  number nsubs
  !! nsubs=max(subject);
  
  //Declare counting variable
  int i

  //Read in desired number of a parameters from control (.ctl) file
  !! ad_comm::change_datafile_name("wtlen_mle.ctl");
  init_int num_apar;
  
PARAMETER_SECTION
  init_vector log_a(1,num_apar);
  init_number b;
  init_number log_sigma;
  sdreport_number sigma;

  vector log_weight_pred(1,nobs);
  vector weight_pred(1,nobs);
  objective_function_value obj_fun;

PROCEDURE_SECTION
  for (i=1;i<=nobs;i++) {
   log_weight_pred(i) = log_a(subject(i)) + b*log_length(i);}
  
  sigma = mfexp(log_sigma);
  
  //Assuming normal error around logged values
  obj_fun = nobs*log_sigma + 0.5*norm2(log_weight-log_weight_pred)/square(sigma);
  
  //Assuming lognormal error
  //weight = mfexp(log_weight);
  //length = mfexp(log_length);
  //a = mfexp(log_a);
  //for (i=1;i<=nobs;i++) {
  //
  //weight_pred(i) = a(subject(i))*pow(length(i),b)}
  //
  //obj_fun = nobs*log_sigma + sum(log_weight) + 0.5*norm2(log_weight-log(weight_pred))/square(sigma);

REPORT_SECTION
  report<<"a"<< endl;
  report<<log_a<<endl;
  report<<"b"<<endl;
  report<<b<<endl;  
  report<<"predicted weights"<<endl;
  report<<mfexp(log_weight_pred)<<endl;    
  report<<"obj_fun"<<endl;
  report<<obj_fun<<endl;
  