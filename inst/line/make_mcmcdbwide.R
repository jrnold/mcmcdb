# create line_mcmcwide data for the package
library(mcmcdb)
load("line_mcmc_list.Rdata")
line_samples <- McmcdbWide(line_mcmc_list)
line_samples@metadata <- list(comment = "For use in the examples of package mcmcdb")
save(line_samples, file="data/line_samples.rda")
