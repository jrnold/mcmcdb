library("rstan")

line_data <- read_rdump("line_data.R")
line_stanfit <- stan("line.stan", data=line_data, seed=1234,
                     iter=100, chains=2, thin=1)
save(line_stanfit, file="line_stanfit.Rdata")
