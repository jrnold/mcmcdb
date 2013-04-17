context("method-mcmcdb_flatten")

test_that("mcmcdb_flatten,array works", {
  foo <- array(runif(2), c(1,2))
  bar <- mcmcdb_flatten(foo, parname="beta", FUN=mcmc_parnames_bugs)
  expect_equivalent(bar, unlist(bar))
  expect_equal(names(bar), c("beta[1,1]", "beta[1,2]"))
})

test_that("mcmcdb_flatten,numeric works", {
  foo <- runif(2)
  bar <- mcmcdb_flatten(foo, parname="beta", FUN=mcmc_parnames_bugs)
  expect_equivalent(bar, unlist(bar))
  expect_equal(names(bar), c("beta[1]", "beta[2]"))
})

test_that("mcmcdb_flatten,list works", {
  foo <- list(alpha = array(runif(2)), beta = array(runif(2), c(1, 2)))
  bar <- mcmcdb_flatten(foo)
  expect_equivalent(bar, unlist(foo))
  expect_equal(names(bar), c("alpha.1", "alpha.2", "beta.1.1", "beta.1.2"))
})

test_that("mcmcdb_flatten,list works", {
  foo <- list(alpha = 1, beta = array(runif(4), c(2, 2)))
  bar <- mcmcdb_flatten(foo, FUN=mcmc_parnames_bugs)
  expect_equivalent(bar, unlist(foo))
  expect_equal(names(bar), c("alpha", "beta[1,1]", "beta[2,1]",
                             "beta[1,2]", "beta[2,2]"))
})

