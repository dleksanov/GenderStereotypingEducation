//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> np; //number of (fake) participants
  int<lower=0> nt; //number of tradeoff questions asked
  int<lower=0> ns; //number of subjects
  int<lower=0,upper=1> choice[nt]; //the choice of left or right (left = 1) in each tradeoff question
  int<lower=0> subjl[nt]; //subject on the left
  int<lower=0> subjr[nt]; //subject on the right
  int<lower=1> incl[nt]; //increment on the left
  int<lower=1> incr[nt]; //increment on the right
  int<lower=0> id[nt]; //the ID of the participant in each tradeoff
  
  //vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
  vector[np] lambda; //bias towards the left (positive) or right (negative
  vector[np] logMU_subj[ns]; //matrix (num participants x num subjects) of log marginal utilities
  // vector<lower=0>[np] sigma_epsilon; //response error variance, which differs across participants
  // vector[np] sigma_epsilon; //response error variance, which differs across participants

  //hyperparameters
  real mu_lambda; //mean of the lambda parameters
  real<lower=0> sigma_lambda; //std dev of the lambda parameters
  vector[ns] mu_logMU_subj; //mean of the log marginal utilities for each subject
  real<lower=0> sigma_logMU; //std dev of the log marginal utilities, assumed to be common across subjects for simplicity
  // real mu_sigma_epsilon; //mean of response error variance
  // real<lower=0> sigma_sigma_epsilon; //std dev of response error variance
  
  //real mu;
  //real<lower=0> sigma;
}

//transformed parameters {
//  vector[nt] probit_expression; //lambda + m1 - m2 + log(inc1/inc2) -- expression for probit (then divide through by sigma_epsilon)
//  //probit_expression = (lambda + logMU_subj[subjl, id] - logMU_subj[subjr, id] + log(incl / incr)) / sigma_epsilon;
//  for (i in 1:nt)
//    //curr_id = id[i]
//    //curr_subjl = subjl[i]
//    //curr_subjr = subjr[i]
//    probit_expression[i] = (lambda[id[i]] + logMU_subj[subjl[i], id[i]] - logMU_subj[subjr[i], id[i]] + log(incl[i] * 1.0 / incr[i]));// / sigma_epsilon[id[i]];
//}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
  // in defining priors for logMU_subj matrix, use a FOR LOOP to loop over the subjects as appropriate
  //hyperpriors
  mu_lambda ~ normal(0, 10);
  sigma_lambda ~ cauchy(0, 2);
  // mu_sigma_epsilon ~ normal(0, 10);
  // sigma_sigma_epsilon ~ cauchy(0, 2);
  sigma_logMU ~ cauchy(0, 2);
  
  //priors
  lambda ~ normal(mu_lambda, sigma_lambda);
  // sigma_epsilon ~ lognormal(mu_sigma_epsilon, sigma_sigma_epsilon);
  for (j in 1:ns)
    logMU_subj[j] ~ normal(mu_logMU_subj[j], sigma_logMU);
  
  // define the log-likelihood for a SINGLE OBSERVATION
  // This is (r_ilr)*(Phi(...)) + (1-r_ilr)*(1-Phi(...))
  // choice ~ bernoulli(Phi_approx(probit_expression)); //likelihood function (vectorized)
  for (i in 1:nt)
    // choice[i] ~ bernoulli(Phi_approx((lambda[id[i]] + logMU_subj[subjl[i], id[i]] - logMU_subj[subjr[i], id[i]] + log(incl[i] * 1.0 / incr[i]) / sigma_epsilon[id[i]]))); //likelihood function (vectorized)
    // choice[i] ~ bernoulli(Phi_approx((lambda[id[i]] + logMU_subj[subjl[i], id[i]] - logMU_subj[subjr[i], id[i]] + log(incl[i] * 1.0 / incr[i]) / 1.0))); //likelihood function (vectorized)
    // choice[i] ~ bernoulli(Phi( (lambda[id[i]] + logMU_subj[subjl[i], id[i]] - logMU_subj[subjr[i], id[i]] + log(incl[i] * 1.0 / incr[i])) / sigma_epsilon[id[i]])); //likelihood function (vectorized)
    choice[i] ~ bernoulli(Phi( (lambda[id[i]] + logMU_subj[subjl[i], id[i]] - logMU_subj[subjr[i], id[i]] + log(incl[i] * 1.0 / incr[i])) / 1.0)); //likelihood function (vectorized)
  // Use approximate probit for now for speed; otherwise, just use Phi(...)
  
  //y ~ normal(mu, sigma);
}

