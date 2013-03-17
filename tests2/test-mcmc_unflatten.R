context("mcmc_unflatten")

parnames <- c("alpha", "beta.1", "beta.2",
              "gamma.1.1", "gamma.1.2", "gamma.2.1", "gamma.2.2")
parameters <- mcmc_parse_parnames(parnames, mcmc_parparser_stan)

test_that("mcmc_flatten,numeric,McmcParameters works as expected", {
  x <- runif(length(parnames))
  names(x) <- parnames
  foo <- mcmc_unflatten(x, parameters)
  expect_equal(lapply(foo, dim), list(alpha=1L, beta = 2L, gamma = c(2L, 2L)))
})

test_that("mcmc_flatten,matrix,McmcParameters works as expected", {
  n <- 3L
  x <- matrix(runif(length(parnames) * n), nrow=n)
  colnames(x) <- parnames
  foo <- mcmc_unflatten(x, parameters)
  expect_equal(lapply(foo, dim), list(alpha=c(1L, n), beta = c(2L, n),
                                      gamma = c(2L, 2L, n)))
})
