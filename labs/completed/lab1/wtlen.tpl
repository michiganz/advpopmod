//////////////////////////////////////////////
//  MAR 580: Advanced Population Modeling
//  Fall 2015
//  Gavin Fay
//  Phil Ganz
//  Lab 1: Linear regression example
//////////////////////////////////////////////
DATA_SECTION
  //Number of observations
  init_int nobs;
  //Recognize data as matrix 
  init_matrix data(1,nobs,1,3);
  //Create vectors for data columns
  vector subject(1,nobs);
  vector length(1,nobs);
  vector weight(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! subject=column(data,1);
  !! length=column(data,2);
  !! weight=column(data,3);
  //Determine number of subjects (number of unique a params)
  number nsubs
  !! nsubs=max(subject);

  int i

PARAMETER_SECTION
  init_bounded_vector a(1,nsubs,0.0001,50); //Bound a to be positive
  init_number b;
  
  vector ln_weight_pred(1,nobs);
  objective_function_value obj_fun;

PROCEDURE_SECTION
  //cout<<nsubs<<endl;
  //exit(42);

  for (i=1;i<=nobs;i++) {
   ln_weight_pred(i) = log(a(subject(i))) + b*log(length(i));}
  obj_fun = norm2(log(weight)-ln_weight_pred);

REPORT_SECTION
  report<<"a"<< endl;
  report<<a<<endl;
  report<<"b"<<endl;
  report<<b<<endl;  
  report<<"predicted weights"<<endl;
  report<<exp(ln_weight_pred)<<endl;    
  report<<"obj_fun"<<endl;
  report<<obj_fun<<endl;