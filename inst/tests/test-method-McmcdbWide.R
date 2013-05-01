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

## foo <- array(c(rbind(1:3, 11:13), rbind(4:6, 14:16)), c(2, 3, 2))
## names(dim(foo)) <- c("", "iteration", "chain")
## class(foo) <- "mcarray"
## McmcdbWide.mcarray(foo, "foo")

## foobar <- McarrayList(foo = foo, bar = foo + 100)
## mcmcdb_flatten.McarrayList(foobar)
## McmcdbWide.McarrayList(foobar)

## test_that("McmcdbWide,mcmc.list works", {
##   foo <- McmcdbWide(line_mcmc_list)
##   expect_is(foo, "McmcdbWide")
##   expect_equal(nrow(foo@samples), sum(sapply(line_mcmc_list, nrow)))
## })

## test_that("McmcdbWide,mcmc works", {
##   foo <- McmcdbWide(line_mcmc_list[[1]])
##   expect_is(foo, "McmcdbWide")
##   expect_equal(nrow(foo@samples), nrow(line_mcmc_list[[1]]))
## })
