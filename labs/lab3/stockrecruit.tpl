//////////////////////////////////////////////
//  MAR 580: Advanced Population Modeling
//  Fall 2015
//  Instructor: Gavin Fay
//  Student: Phil Ganz
//  Lab 3: stock recruit relationships, estimating steepness
//////////////////////////////////////////////
DATA_SECTION
  !! ad_comm::change_datafile_name("sr_new.dat");
  //Number of observations
  init_int nobs;
  //Number of stocks
  init_int nstocks;
  //Recognize data as matrix 
  init_matrix data(1,nobs,1,4);
  init_vector muRzero(1,nstocks);
  init_vector sdRzero(1,nstocks);

  //Create vectors for data columns
  vector  SSB(1,nobs);
  vector  rec(1,nobs);
  ivector stock(1,nobs);
  vector  SBPR(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! SSB = column(data,1);
  !! rec = column(data,2);
  !! stock = (ivector)column(data,3);
  !! SBPR = column(data,4);

  int i;

PARAMETER_SECTION
  init_number mu_beta;
  init_bounded_number log_sigma_h(-4.6,3.,2);
  init_number log_sigma(1);
  init_vector log_Rzero(1,nstocks);
  
  vector Rzero(1,nobs);
  vector beta(1,nobs);
  vector h(1,nobs);
  number sigma;
  number sigma_h;
  
  vector pred_rec(1,nobs);

  sdreport_number hbar;

  random_effects_vector h_devs(1,nstocks,2);

  objective_function_value obj_fun;

PROCEDURE_SECTION
  //transformations
  sigma = mfexp(log_sigma);
  sigma_h = mfexp(log_sigma_h);
  Rzero = mfexp(log_Rzero(stock));
  
  //distribution for the random effects: ~N(0,1)
  obj_fun = nstocks*log(sqrt(2*PI)) + 0.5*norm2(h_devs);

  beta = mu_beta + sigma_h*h_devs(stock);

  //steepness
  h = elem_div((mfexp(beta)+0.2),(1.+mfexp(beta)));

  pred_rec = elem_div(0.8*elem_prod(h,elem_prod(Rzero,SSB)),(0.2*elem_prod(elem_prod(SBPR,Rzero),(1.-h))+elem_prod(SSB,(h-0.2))));

  //Likelihood assuming lognormal error
  obj_fun += sum(log(sqrt(2*PI)) + log_sigma + log(rec) + 0.5*square(log(rec)-log(pred_rec)+0.5*square(sigma))/(square(sigma)));

  //mean steepness
  hbar = (0.2+mfexp(mu_beta))/(1.+mfexp(mu_beta));

REPORT_SECTION
  report << "mean steepness" << endl;
  report << hbar << endl;
  report << "" << endl;
  report << "RECRUITMENT" << endl;
  report << "observed" << endl;
  report << rec << endl;   
  report << "predicted" << endl;
  report << pred_rec << endl;   
  report << "residuals" << endl;
  report << pred_rec-rec << endl;

TOP_OF_MAIN_SECTION
  arrmblsize=500000;
