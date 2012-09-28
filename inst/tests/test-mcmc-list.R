library(testthat)
library(mcmc4)

context("mcmc.list class and methods")

test_that("mcmc.list class definition", {
    ## Check that class is exported
    expect_true(isClass("mcmc.list"))

    ## Normal initialization
    ## Purposefully use chains of different lengths
    x <- lapply(seq(20,24,by=2),
                function(i) mcmc(matrix(rnorm(i), ncol=2)))
    expect_that(new("mcmc.list", x), is_a("mcmc.list"))

    ## Error if non-mcmc objects in list
    expect_error(new("mcmc.list", list(1, "a", 1:5)))
})

test_that("mcmc.list methods work", {
    x <- new("mcmc.list",
             lapply(seq(20,24,by=2),
                    function(i) mcmc(matrix(rnorm(i), ncol=2))))
    expect_that(length(mean(x)), equals(ncol(x[[1]])))
    expect_that(length(median(x)), equals(ncol(x[[1]])))
    expect_that(dim(quantile(x)), equals(c(5, ncol(x[[1]]))))
    expect_that(dim(vcov(x)), equals(rep(ncol(x[[1]]), 2)))
    expect_that(length(coef(x)), equals(ncol(x[[1]])))
})
