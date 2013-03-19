context("method-extract")

test_McmcdbWide <-new("McmcdbWide",
                      samples = samples, parameters = parameters,
                      chains = chains, iters = iters)

parnames <- paste("beta", 1:2, sep=".")
samples <- matrix(1:8, ncol=2)
colnames(samples) <- parnames
chains <- McmcdbChains(data.frame(chain_id = rep(1:2, each = 2)))
iters <- McmcdbIters(data.frame(chain_id = rep(1:2, each=2),
                                iter = rep(1:2, 2)))
test_wide2 <- McmcdbWide(samples, chains = chains, iters = iters)
