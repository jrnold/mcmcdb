context("test McmcdbWide")

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

par_chains <- McmcParChains(expand.grid(parname = names(parameters@flatpars),
                                        chain_id = 1:2))
metadata <- list()

test_that("McmcWide works", {
  foo <-new("McmcWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters,
            par_chains = par_chains)
  expect_equal(foo@samples, samples)
  expect_equal(foo@parameters, parameters)
  expect_equal(foo@chains, chains)
  expect_equal(foo@iters, iters)
  expect_equal(foo@par_chains, par_chains)
  expect_equal(foo@version, mcmcdb:::VERSION)
})

test_that("McmcdbWide works with par_chains = NULL", {
  foo <-new("McmcdbWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters)
  expect_equal(foo@par_chains, NULL)
})

test_that("McmcdbWide error if nrow(iters) != nrow(samples)", {
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = McmcIters(iters[1:2, ])),
               regexp="invalid class")
})

test_that("McmcdbWide error if colnames don't match parameters", {
  colnames(samples) <- c("alpha", "beta")
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters),
               regexp="invalid class")
})



