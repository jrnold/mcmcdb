context("method mcmcdb_samples_flatpars")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_flatpars passes test #1", {
    foo <- mcmcdb_samples_flatpars(test_wide2)
    expected <- structure(1:18, .Dim = c(6L, 3L),
                          .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3")))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #2", {
    foo <- mcmcdb_samples_flatpars(test_wide2, chain_id=1, flatpars="beta.1")
    expected <- structure(1:3, .Dim = c(3L, 1L), .Dimnames = list(NULL, "beta.1"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #3 (", {
    foo <- mcmcdb_samples_flatpars(test_wide2, chain_id=1, FUN = mean)
    expected <- c(beta.1 = 2, beta.2 = 8, beta.3 = 14)
    expect_equal(foo, expected)
  })
  
})


