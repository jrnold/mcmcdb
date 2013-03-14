library(rjags)
read_rdump <- rstan::read_rdump
set.seed(1234)

line_data <- read_rdump("line_data.R")
line_model <- jags.model("line.jag", data=line_data, n.adapt = 0, n.chains = 2)
line_mcmc_list <-
  coda.samples(line_model, c("beta", "tau"), n.iter = 10, thin = 2)
save(line_mcmc_list, file="line_mcmc_list.Rdata")

