context("mcmcdb_mcpar")

parnames <- paste("beta", 1:2, sep=".")
samples <- matrix(1:8, ncol=2)
colnames(samples) <- parnames
chains <- McmcdbChains(data.frame(chain_id = rep(1:2)))
iters <- McmcdbIters(data.frame(chain_id = rep(1:2, each=2),
                                iter = rep(1:2, 2)))
test_wide2 <- McmcdbWide(samples, chains = chains, iters = iters)

test_that("mcmcdb_mcpar,McmcdbWide works as expected", {
  mcpars <- mcmcdb_mcpar(test_wide2)
  expect_equal(mcmcdb_mcpars(test_wide2),
               data.frame(chain_id = 1:2,
                          n_iter = rep(2L, 2),
                          iter_start = rep(NA_integer_, 2),
                          iter_end = rep(NA_integer_, 2),
                          iter_thin = rep(NA_integer_, 2)))
})
