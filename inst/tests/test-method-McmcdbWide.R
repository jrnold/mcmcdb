context("method-McmcdbWide")
source("data-samples.R")

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

