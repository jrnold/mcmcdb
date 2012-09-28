library(testthat)
library(stats)

context("Tests for mcmc class")

foo <- matrix(1:10)

test_that("initialization with no parameters works", {
    x <- new("mcmc", foo)
    expect_equal(x@.Data, foo)
    expect_equal(x@mcpar, c(1, 10, 1))
})

test_that("initialization with start works", {
    x <- new("mcmc", foo, start=6)
    expect_equal(x@mcpar, c(6, 15, 1))
})
test_that("initialization with end works", {
    x <- new("mcmc", foo, end=20)
    expect_equal(x@mcpar, c(1, 20, 2))
})
test_that("catches inconsistent mcpar", {
    expect_error(new("mcmc", foo, start=1, end=5))
    expect_error(new("mcmc", foo, start=1, end=10, thin=5))
})
test_that("throws error with non-numeric matrices", {
    expect_error(new("mcmc", matrix(letters)))
})

test_that("mcmc function works with signature numeric", {
    expect_is(mcmc(1:10), "mcmc")
})

test_that("mcmc function works with signature ts", {
    x <- mcmc(ts(foo))
    expect_identical(x@.Data, foo)
    expect_identical(x@mcpar, c(1, 10, 1))
})

test_that("Check mcmc generic functions", {
    x <- mcmc(matrix(rnorm(20), ncol=4))
    expect_that(length(mean(x)), is_identical_to(ncol(x)))
    expect_that(length(median(x)), is_identical_to(ncol(x)))
    expect_that(dim(quantile(x)), equals(c(5, ncol(x))))
    expect_that(length(coef(x)), is_identical_to(ncol(x)))
    expect_that(dim(vcov(x)), equals(rep(ncol(x), 2)))
})


