context("method-mcmcdb_unflatten")

parnames <- c("alpha", "beta.1", "beta.2",
              "gamma.1.1", "gamma.1.2", "gamma.2.1", "gamma.2.2")
parameters <- McmcdbParameters(parnames, mcmc_parparser_stan)

############

test_that("mcmcdb_flatten,numeric,McmcdbParameters works as expected", {
  x <- runif(length(parnames))
  names(x) <- parnames
  foo <- mcmcdb_unflatten(x, parameters)
  expect_equal(lapply(foo, dim), list(alpha=1L, beta = 2L, gamma = c(2L, 2L)))
})

test_that("mcmcdb_flatten,numeric,function works as expected", {
  x <- runif(length(parnames))
  names(x) <- parnames
  foo <- mcmcdb_unflatten(x, mcmc_parparser_stan)
  expect_equal(lapply(foo, dim), list(alpha=1L, beta = 2L, gamma = c(2L, 2L)))
})

test_that("mcmcdb_flatten,numeric,missing works as expected", {
  x <- runif(length(parnames))
  names(x) <- parnames
  foo <- mcmcdb_unflatten(x)
  expect_equal(lapply(foo, dim), list(alpha=1L, beta = 2L, gamma = c(2L, 2L)))
})

test_that("mcmcdb_flatten,matrix,McmcdbParameters works as expected", {
  n <- 3L
  x <- matrix(runif(length(parnames) * n), nrow=n)
  colnames(x) <- parnames
  foo <- mcmcdb_unflatten(x, parameters)
  expect_equal(lapply(foo, dim), list(alpha=c(1L, n), beta = c(2L, n),
                                      gamma = c(2L, 2L, n)))
})

test_that("mcmcdb_flatten,matrix,function works as expected", {
  n <- 3L
  x <- matrix(runif(length(parnames) * n), nrow=n)
  colnames(x) <- parnames
  foo <- mcmcdb_unflatten(x, mcmc_parparser_stan)
  expect_equal(lapply(foo, dim), list(alpha=c(1L, n), beta = c(2L, n),
                                      gamma = c(2L, 2L, n)))
})

test_that("mcmcdb_flatten,matrix,missing works as expected", {
  n <- 3L
  x <- matrix(runif(length(parnames) * n), nrow=n)
  colnames(x) <- parnames
  foo <- mcmcdb_unflatten(x)
  expect_equal(lapply(foo, dim), list(alpha=c(1L, n), beta = c(2L, n),
                                      gamma = c(2L, 2L, n)))
})
