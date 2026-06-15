// errors_in_variables_growth.stan
data {
  int<lower=1> N;
  vector[N] y;
  vector[N] xmean;
  vector<lower=0>[N] sdx;
}
parameters {
  real<lower=0> a;
  real r;
  real b;
  real g0;
  real g1;
  vector<lower=0>[N] x_true;  // latent age
}
model {
  // priors — match prior_us_age from your brms fit
  a  ~ normal(100, 50);
  r  ~ normal(0.5, 0.3);
  b  ~ normal(80, 50);
  g0 ~ normal(2, 2);
  g1 ~ normal(0, 0.2);

  // measurement model
  xmean ~ normal(x_true, sdx);

  // weak prior on latent age (regularizes the tails)
  x_true ~ normal(mean(xmean), 3 * sd(xmean));

  // outcome
  vector[N] mu        = a * pow(x_true, r) + b;
  vector[N] log_sigma = g0 + g1 * x_true;
  y ~ normal(mu, exp(log_sigma));
}
generated quantities {
  vector[N] y_rep;
  for (n in 1:N) {
    real mu_n        = a * pow(x_true[n], r) + b;
    real sigma_n     = exp(g0 + g1 * x_true[n]);
    y_rep[n]         = normal_rng(mu_n, sigma_n);
  }
}
