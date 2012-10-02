data {
     int<lower=1> N;
     real y[N];
     real x[N];
}
transformed data {
     real x_mean;
     real x_center[N];
     x_mean <- mean(x);
     for (n in 1:N) {
         x_center[n] <- x[n] - x_mean;
     }
}
parameters {
     real alpha;
     real beta;
     real <lower=0> sigma;
}
model {
     real mu[N];
     for (n in 1:N) {
         mu[n] <- alpha + beta * x_center[n];
     }
     y ~ normal(mu, sigma);
     alpha ~ cauchy(0, 2.5);
     beta ~ cauchy(0, 2.5);
     sigma ~ cauchy(0, 2.5) T[0,];
}




