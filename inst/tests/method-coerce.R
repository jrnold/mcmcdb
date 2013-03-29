context("coerce methods")

local({
  source("data-McmcdbWide.R")
  
  test_that("coerce Mcmcdb -> mcmc.list works", {
    foo <- as(test_wide2, "mcmc.list")
    expected <- structure(list(structure(c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L), .Dim = c(3L, 3L), .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3")), class = "mcmc"), structure(c(4L, 5L, 6L, 10L, 11L, 12L, 16L, 17L, 18L), .Dim = c(3L, 3L), .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3")), class = "mcmc")), class = "mcmc.list")
    expect_equal(foo, expected)
  })

  test_that("coerce Mcmcdb -> matrix works", {
    foo <- as(test_wide2, "matrix")
    expected <- structure(1:18, .Dim = c(6L, 3L), .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3")))
    expect_equal(foo, expected)
  })

  test_that("coerce Mcmcdb -> list works", {
    foo <- as(test_wide2, "list")
    expected <- structure(list(beta = structure(c(1L, 7L, 13L, 2L, 8L, 14L, 3L, 9L, 15L, 4L, 10L, 16L, 5L, 11L, 17L, 6L, 12L, 18L), .Dim = c(3L, 6L))), .Names = "beta")
    expect_equal(foo, expected)
  })
  
})


