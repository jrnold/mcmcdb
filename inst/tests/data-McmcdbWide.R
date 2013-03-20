# dummy McmcWide object used for testing
source("data-samples.R")

test_wide1 <- 
  new("McmcdbWide",
      samples = samples,
      parameters = McmcdbParameters(parnames),
      chains = chains, iters = iters)

test_wide2 <- local({
  parnames <- paste("beta", 1:2, sep=".")
  samples <- matrix(1:8, ncol=2)
  colnames(samples) <- parnames
  chains <- McmcdbChains(data.frame(chain_id = rep(1:2)))
  iters <- McmcdbIters(data.frame(chain_id = rep(1:2, each=2),
                                  iter = rep(1:2, 2)))
  McmcdbWide(samples, chains = chains, iters = iters)
})

