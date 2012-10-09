library("coda")
context("Tests for mcmc class")

foo <- matrix(1:10)
data(line, package="mcmc4")

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
    expect_equivalent(x@.Data, foo)
    expect_identical(mcpar(x), c(1, 10, 1))
})


line1 <- line[[1]]

test_that("method mean works", {
    expect_that(length(mean(line1)), is_identical_to(ncol(line1)))
})

test_that("method quantile works", {
    expect_equal(dim(quantile(line1)),c(5, ncol(line1)))
    expect_equal(dim(quantile(line1, probs=c(0.25, 0.5))), c(2, ncol(line1)))
    expect_equal(dim(quantile(line1, type=1)), c(5, ncol(line1)))
})

test_that("method median works", {
    expect_equal(length(median(line1)), 3)
    expect_equal(names(median(line1)), colnames(line1))
})

test_that("method coef works", {
    expect_equal(coef(line1), mean(line1))
    expect_equal(coef(line1, FUN=median), median(line1))
})

test_that("method vcov works", {
    expect_equal(dim(vcov(line1)), rep(ncol(line1), 2))
})


test_that("melt works", {
    foo <- melt(line[[1]])
    expect_is(foo, "data.frame")
    expect_equal(nrow(foo), 600)
    expect_equal(rownames(foo)[1], "alpha.1")
    expect_equal(rownames(foo)[600], "sigma.200")
    expect_equal(colnames(foo), c("parameter", "chain", "iteration", "value"))
})

