data {
  int<lower=1> N;                    // Number of time steps
  int<lower=1> K;                    // Number of environmental covariates
  vector[2] y[N];                    // Observed displacements (2D)
  vector t;                          // Times of observed displacements
  matrix[N, K] X;                    // Environmental covariate matrix
  
  // Priors
  real beta0_prior_mean;             // Prior mean for intercept
  real<lower=0> beta0_prior_sd;      // Prior SD for intercept
  vector[K] beta_prior_mean;         // Prior mean for covariate effects
  vector<lower=0>[K] beta_prior_sd;  // Prior SD for covariate effects
  real<lower=0> sigma_gamma_prior;   // Prior for random walk SD
  real<lower=0> sigma_y_prior;       // Prior for displacement SD
  real<lower=0> sigma_logit_gamma_init; // Prior for logit gamma init SD
}

parameters {
  real beta0;                        // intercept on logit(gamma) bias
  vector[K] beta;                    // Covariate effects on logit(gamma) bias
  real<lower=0> sigma_gamma;         // Random walk standard deviation
  real<lower=0> sigma_y;             // Displacement standard deviation
  real tau;                          // Bias strength
  
  // Time-varying move persistence (on logit scale)
  vector[N-1] logit_gamma_raw;       // Raw random walk innovations
  real logit_gamma_init;             // initial gamma (logit scale)
}

transformed parameters {
  vector[N] logit_gamma;             // Move persistence on logit scale
  vector<lower=0, upper=1>[N] gamma; // Move persistence (0-1 scale)
  vector[N] mu_logit_gamma;          // Bias of logit(gamma) from covariates
  
  // Calculate covariate-driven mean for each time step
  for (t in 1:N) {
    mu_logit_gamma[t] = beta0 + X[t] * beta;
  }
  
  // Biased random walk: random walk with drift toward covariate-predicted value
  logit_gamma[1] = logit_gamma_init;
  
  for (t in 2:N) {
    // Drift toward environmentally-predicted value plus random innovation
    logit_gamma[t] = logit_gamma[t-1] + 
                     (mu_logit_gamma[t] - logit_gamma[t-1]) * tau;  // bias strength
  }
  
  // Transform to (0,1) scale
  gamma = inv_logit(logit_gamma);
}

model {
  // Priors
  beta0 ~ normal(beta0_prior_mean, beta0_prior_sd);
  beta ~ normal(beta_prior_mean, beta_prior_sd);
  sigma_gamma ~ exponential(1.0 / sigma_gamma_prior);
  sigma_y ~ exponential(1.0 / sigma_y_prior);
  
  // Random walk innovations
  logit_gamma_init ~ normal(0, sigma_logit_gamma_init);
  for (t in 2:N) {
    logit_gamma[t] ~ normal(logit_gamma[t-1], sigma_gamma);
  }
  
  // Movement model (Equation 1 from paper)
  for (t in 2:N) {
    vector[2] mu_y;
    matrix[2,2] Sigma_y;
    
    // Mean displacement based on previous displacement and persistence
    mu_y = gamma[t-1] * y[t-1];
    
    // Covariance matrix (diagonal, as specified in paper)
    Sigma_y = diag_matrix(rep_vector(sigma_y^2, 2));
    
    // Likelihood
    y[t] ~ multi_normal(mu_y, Sigma_y);
  }
}
