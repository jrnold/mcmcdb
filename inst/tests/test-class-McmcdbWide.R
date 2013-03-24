context("class-McmcdbWide")
source("data-samples.R")

test_that("McmcdbWide initialize works", {
  foo <-new("McmcdbWide",
            samples = samples,
            parameters = parameters,
            chains = chains,
            iters = iters)
  expect_equal(foo@samples, samples)
  expect_equal(foo@parameters, parameters)
  expect_equal(foo@chains, chains)
  expect_equal(foo@iters, iters)
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
  flatpar_chains$flatpar <- factor("alpha")
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters,
                   flatpar_chains = flatpar_chains),
               "invalid class")
})

test_that("McmcdbWide error if flatpar_chains has bad chain_id values", {
  flatpar_chains$chain_id <- 3L
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters,
                   flatpar_chains = flatpar_chains),
               "invalid class")
})

test_that("McmcdbWide works with non-NULL parinit", {
  parinit <- c(beta.1 = 0, beta.2 = 0)
  expect_is(new("McmcdbWide",
                samples = samples, parameters = parameters,
                chains = chains, iters = iters,
                parinit = parinit),
            "McmcdbWide")
})

test_that("McmcdbWide throws error if bad parinit names", {
  parinit <- c(alpha = 0, beta.2 = 0)
  expect_error(new("McmcdbWide",
                   samples = samples, parameters = parameters,
                   chains = chains, iters = iters,
                   parinit = parinit),
               "invalid class")
})

