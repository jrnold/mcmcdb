context("test McmcdbWide")

##########
# Data

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

parchains <- McmcFlatparChains(expand.grid(flatpar = names(parameters@flatpars),
                                       chain_id = 1:2))
metadata <- list()

###########

test_that("McmcdbWide initialize works", {
  foo <-new("McmcdbWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters,
            parchains = parchains)
  expect_equal(foo@samples, samples)
  expect_equal(foo@parameters, parameters)
  expect_equal(foo@chains, chains)
  expect_equal(foo@iters, iters)
  expect_equal(foo@parchains, parchains)
  expect_equal(foo@version, mcmcdb:::VERSION)
})

test_that("McmcdbWide works with parchains = NULL", {
  foo <-new("McmcdbWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters)
  expect_equal(foo@parchains, NULL)
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


############

context("Function McmcdbWide")

test_that("McmcdbWide works with only required options", {
  foo <- McmcdbWide(samples)
  expect_is(foo, "McmcdbWide")
})

test_that("McmcdbWide works with parameters of class McmcParameters ", {
  foo <- McmcdbWide(samples, parameters=parameters)
  expect_is(foo, "McmcdbWide")
  expect_equal(foo@parameters, parameters)
})

test_that("McmcdbWide works with non-null parameters of class function", {
  f <- mcmc_parparser_stan
  parameters <- mcmc_parse_parnames(colnames(samples), f)
  foo <- McmcdbWide(samples, parameters=f)
  expect_is(foo, "McmcdbWide")
  expect_equal(foo@parameters, parameters)
})


#####################

parnames <- c("alpha", paste("beta", 1:2, sep="."))
mcmc_parse_parnames(parnames)

samples <- matrix(rnorm(2^5 * 3), ncol=3)
colnames(samples) <- parnames

chains <- McmcChains(data.frame(chain_id = 1:2))

parameters <- mcmc_parse_parnames(parnames)
iters <- McmcIters(data.frame(chain_id = rep(1:2, each=2^4),
                              iter = rep(1:2^4, 2)))
parchains <- McmcFlatparChains(expand.grid(flatpar = names(parameters@flatpars),
                                           chain_id = 1:2))
metadata <- list()


