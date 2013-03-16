parnames <- paste("beta", 1:2, sep=".")
mcmc_parse_parnames(parnames)

samples <- matrix(rnorm(16), ncol=2)
colnames(samples) <- parnames

chains <- McmcChains(data.frame(chain_id = 1:2,
                                niter = 4L,
                                thin=1L, start=1L, end=4L))

parameters <- mcmc_parse_parnames(parnames)
iters <- McmcIters(data.frame(chain_id = rep(1:2, each=4),
                              iter = rep(1:4, 2)))

parchains <- McmcParChains(expand.grid(parname = names(parameters@flatpars),
                                        chain_id = 1:2))
metadata <- list()
fakedata <-new("McmcdbWide",
               samples = samples, parameters = parameters,
               chains = chains, iters = iters,
               parchains = parchains)


