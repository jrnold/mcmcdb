data {
  int<lower=1> N;
  real y[N];
  real x[N];
}
parameters {
  real beta[2];
  real <lower=0> sigma;
}
model {
  real mu[N];
  for (n in 1:N) {
    mu[n] <- beta[1] + beta[2] * x[n];
  }
  y ~ normal(mu, sigma);
}




