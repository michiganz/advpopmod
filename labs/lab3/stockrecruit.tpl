DATA_SECTION
  //Number of observations
  init_int nobs;
  //Number of stocks
  init_int nstocks;
  //Recognize data as matrix 
  init_matrix data(1,nobs,1,4);
  //Create vectors for data columns
  vector  SSB(1,nobs);
  vector  rec(1,nobs);
  ivector stock(1,nobs);
  vector  SBPR(1,nobs);
  //Assign values to vectors of interest using C++ code
  !! SSB=column(data,1);
  !! rec=column(data,2);
  !! stock=(ivector)column(data,3);
  !! SBPR=column(data,4);

  int i

PARAMETER_SECTION
  init_bounded_vector log_Rzero(1,nstocks,-3,8);
  init_number mu_beta;
  init_number log_sigma;
  init_number log_sigma_beta(2);

  sdreport_vector Rzero(1,nstocks);
  sdreport_vector h(1,nstocks);
  sdreport_number sigma;
  sdreport_number sigma_beta;
  
  vector beta(1,nstocks);
  random_effects_vector beta_devs(1,nstocks,2);

  vector pred_rec(1,nobs);

  objective_function_value obj_fun;

PROCEDURE_SECTION
  //transformations
  Rzero = mfexp(log_Rzero);
  sigma = mfexp(log_sigma);
  sigma_beta = mfexp(log_sigma_beta);
  
  //distribution for the random effects: ~N(mu,sigma^2)
  obj_fun = 0.5*norm2(beta_devs);
  
  for (i=1;i<=nobs;i++) {
  beta(stock(i)) = mu_beta + sigma_beta*beta_devs(stock(i));}

  h = elem_div(exp(beta)+0.2,1+exp(beta));

  for (i=1;i<=nobs;i++) {
  pred_rec(i)=(0.8*Rzero(stock(i))*h(stock(i))*SSB(i))/(0.2*SBPR(i)*Rzero(stock(i))*(1-h(stock(i)))+(h(stock(i))-0.2)*SSB(i));}
  
  //Assuming lognormal error
  obj_fun += nobs*log_sigma + 0.5*norm2(log(rec)-log(pred_rec))/square(sigma);

REPORT_SECTION
  report << "mean steepness" << endl;
  report << mean(h) << endl;
  report << "" << endl;
  report << "RECRUITMENT" << endl;
  report << "observed" << endl;
  report << rec << endl;   
  report << "predicted" << endl;
  report << pred_rec << endl;   
  report << "residuals" << endl;
  report << pred_rec-rec << endl;
