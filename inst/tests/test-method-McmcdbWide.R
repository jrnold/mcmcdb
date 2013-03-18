context("method-McmcdbWide")
source("data-samples.R")

test_that("McmcdbWide,matrix #1", {
  expect_is(McmcdbWide(samples, parameters = parameters, chains = chains, iters = iters),
           "McmcdbWide")
})

test_that("McmcdbWide,matrix no args", {
  expect_is(McmcdbWide(samples), "McmcdbWide")
})

test_that("McmcdbWide,matrix chains=NULL, iters != NULL", {
  expect_is(McmcdbWide(samples, iters=iters), "McmcdbWide")
})

test_that("McmcdbWide,matrix chains != NULL, iters == NULL error", {
  expect_error(McmcdbWide(samples, chains=chains), "iters cannot be NULL")
})

test_that("McmcdbWide,matrix with class(parameters) == \"McmcdbParameters\"", {
  expect_is(McmcdbWide(samples, parameters=parameters), "McmcdbWide")
})

test_that("McmcdbWide,matrix with class(parameters) == character", {
  expect_is(McmcdbWide(samples, parameters="mcmc_parparser_stan"), "McmcdbWide")
})

test_that("McmcdbWide,matrix with class(parameters) == function", {
  expect_is(McmcdbWide(samples, parameters=mcmc_parparser_stan), "McmcdbWide")
})

test_that("McmcdbWide,matrix with data != NULL works", {
  foo <- McmcdbWide(samples, model_data=list(a=1))
  expect_is(foo, "McmcdbWide")
  expect_equal(foo@model_data, as(list(a=1), "namedList"))
})

test_that("McmcdbWide,data.frame works", {
  expect_is(McmcdbWide(as.data.frame(samples)), "McmcdbWide")
})



