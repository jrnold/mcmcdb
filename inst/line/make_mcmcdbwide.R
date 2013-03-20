# create line_mcmcwide data for the package
library(mcmcdb)
load("line_mcmc_list.Rdata")
line <- McmcdbWide(line_mcmc_list)
line <- list(comment = "For use in the examples of package mcmcdb")
save(line_mcmcdbwide, file="../../data/line.rda")
