## Example data used in tesing McmcdbWide data

parnames <- paste("beta", 1:2, sep=".")
parameters <- McmcdbParameters(parnames)
samples <- matrix(rnorm(16), ncol=2)
colnames(samples) <- parnames
chains <- McmcdbChains(chain_id = 1:2,
                       niter = 4L,
                       thin=1L, start=1L, end=4L)
flatpar_chains <-
  McmcdbFlatparChains(flatpar = rep(parnames, each=2),
                      chain_id = rep(1:2, 2),
                      init = NA_real_)
iters <- McmcdbIters(chain_id = rep(1:2, each=4),
                     iter = rep(1:4, 2))

