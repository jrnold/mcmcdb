context("class-McmcdbWide")
source("data-samples.R")

test_that("McmcdbWide initialize works", {
  foo <-new("McmcdbWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters,
            flatpar_chains = flatpar_chains)
  expect_equal(foo@samples, samples)
  expect_equal(foo@parameters, parameters)
  expect_equal(foo@chains, chains)
  expect_equal(foo@iters, iters)
  expect_equal(foo@flatpar_chains, flatpar_chains)
  expect_equal(foo@version, mcmcdb:::VERSION)
})

test_that("McmcdbWide works with flatpar_chains = NULL", {
  foo <-new("McmcdbWide",
            samples = samples, parameters = parameters,
            chains = chains, iters = iters)
  expect_equal(foo@flatpar_chains, NULL)
})

test_that("McmcdbWide error if nrow(iters) != nrow(samples)", {
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = McmcdbIters(iters[1:2, ])),
               regexp="invalid class")
})

test_that("McmcdbWide error if colnames don't match parameters", {
  colnames(samples) <- c("alpha", "beta")
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters),
               regexp="invalid class")
})


