# create line_mcmcwide data for the package
library(mcmcdb)
load("line_mcmc_list.Rdata")
line_mcmcdbwide <- McmcdbWide(line_mcmc_list)
save(line_mcmcdbwide, file="../../line_mcmcwide.Rdata")
