library(testthat)
library(stats)

context("Tests for mcmc class")

test_that("mcmc function works with matrices", {
    foo <- mcmc(matrix(1:10, ncol=2))
    expect_that(foo@.Data, is_identical_to(matrix(1:10, ncol=2)))
    expect_that(foo@mcpar, is_identical_to(c(1, 5, 1)))

    expect_that(mcmc(matrix(1:10, ncol=2), start=6, end=14, thin=2)@mcpar,
                is_identical_to(c(6, 14, 2)))
    expect_that(mcmc(matrix(1:10, ncol=2), start=1, thin=1)@mcpar,
                is_identical_to(c(1, 5, 1)))
    expect_that(mcmc(matrix(1:10, ncol=2), end=10, thin=1)@mcpar,
                is_identical_to(c(6, 10, 1)))
    expect_error(mcmc(matrix(1:10, ncol=2), start=1, end=10, thin=1)@mcpar)
    expect_error(mcmc(matrix(letters)))
})

test_that("mcmc function works with signature numeric", {
    expect_that(mcmc(1:10)@.Data, is_identical_to(matrix(1:10)))
})

test_that("mcmc function works with signature ts", {
    tsdata <- mcmc(ts(1:10, frequency = 4, start = c(1959, 2)))
    expect_that(tsdata@.Data, is_identical_to(matrix(1:9)))
    expect_that(tsdata@mcpar, is_identical_to(c(1959, 1967, 1)))
})

test_that("Check mcmc generic functions", {
    x <- mcmc(matrix(rnorm(20), ncol=4))
    expect_that(length(mean(x)), is_identical_to(ncol(x)))
    expect_that(length(median(x)), is_identical_to(ncol(x)))
    expect_that(dim(quantile(x)), equals(c(5, ncol(x))))
    expect_that(length(coef(x)), is_identical_to(ncol(x)))
    expect_that(dim(vcov(x)), equals(rep(ncol(x), 2)))
})
