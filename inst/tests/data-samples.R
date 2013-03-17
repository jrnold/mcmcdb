## Example data used in creating McmcdbWide stuff

parnames <- paste("beta", 1:2, sep=".")
samples <- matrix(rnorm(16), ncol=2)
colnames(samples) <- parnames
chains <- McmcdbChains(data.frame(chain_id = 1:2,
                                niter = 4L,
                                thin=1L, start=1L, end=4L))
parameters <- mcmc_parse_parnames(parnames)
iters <- McmcdbIters(data.frame(chain_id = rep(1:2, each=4),
                              iter = rep(1:4, 2)))

flatpar_chains <- McmcdbFlatparChains(expand.grid(flatpar = names(parameters@flatpars),
                                       chain_id = 1:2))
metadata <- list()

