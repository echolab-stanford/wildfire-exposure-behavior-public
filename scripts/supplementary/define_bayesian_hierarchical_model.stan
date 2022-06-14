data {
  int<lower=0> J;          // number of units
  real tau_hat[J];         // estimated treatment effects
  real<lower=0> se_hat[J]; // s.e. of effect estimates 
}

parameters {
  real theta[J];        // unit treatment effect
  real tau;             // mean treatment effect
  real<lower=0> sigma;  // deviation of treatment effects
}

model{
  tau_hat ~ normal(theta, se_hat);
  theta ~ normal(tau, sigma);
  // Priors if used
  //tau ~ normal(0, 15); 
  //sigma ~ cauchy(0, 5);  
}
