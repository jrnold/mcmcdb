context("class-McmcdbWide")
source("data-samples.R")

test_that("McmcdbWide initialize works", {
  foo <-new("McmcdbWide",
            samples = samples,
            parameters = parameters,
            chains = chains,
            iters = iters,
            flatpar_chains = flatpar_chains)
  expect_equal(foo@samples, samples)
  expect_equal(foo@parameters, parameters)
  expect_equal(foo@chains, chains)
  expect_equal(foo@iters, iters)
  expect_equal(foo@flatpar_chains, flatpar_chains)
  expect_equal(foo@version, as.character(packageVersion("mcmcdb")))
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

test_that("McmcdbWide error if colnames don't match parameters", {
  colnames(samples) <- c("alpha", "beta")
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters),
               regexp="invalid class")
})

test_that("McmcdbWide works with flatpar_chains", {
  foo <- new("McmcdbWide",
             samples = samples, parameters = parameters,
             chains = chains, iters = iters,
             flatpar_chains = flatpar_chains)
  expect_equal(foo@flatpar_chains, flatpar_chains)
})

test_that("McmcdbWide error if flatpar_chains has bad flatpar values", {
  expect_error(flatpar_chains$flatpar <- factor("alpha"),
               "invalid class")
})

test_that("McmcdbWide error if flatpar_chains has bad chain_id values", {
  expect_error(flatpar_chains$chain_id <- 3L, "invalid class")
})
