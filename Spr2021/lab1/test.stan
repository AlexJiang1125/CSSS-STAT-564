data {
  int<lower=0> J;         // number of schools 
  real y[J];              // estimated treatment effects
  real<lower=0> sigma[J]; // standard error of effect estimates 
}
  parameters {
    real mu;                // population treatment effect
    real<lower=0> tau;      // standard deviation in treatment effects
}
  model {
    theta ~ normal(mu, tau); // prior
    y ~ normal(theta, sigma); // likelihood
}