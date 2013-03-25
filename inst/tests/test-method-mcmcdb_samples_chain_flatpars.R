context("method mcmcdb_samples_chain_flatpars")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_chain_flatpars passes test #1", {
    foo <- mcmcdb_samples_chain_flatpars(test_wide2)
    expected <- structure(list(`1` = structure(c(1L, 2L, 3L, 7L, 8L, 9L, 13L, 14L, 15L), .Dim = c(3L, 3L), .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3"))), `2` = structure(c(4L, 5L, 6L, 10L, 11L, 12L, 16L, 17L, 18L), .Dim = c(3L, 3L), .Dimnames = list(NULL, c("beta.1", "beta.2", "beta.3")))), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #2", {
    foo <- mcmcdb_samples_chain_flatpars(test_wide2, chain_id=1, flatpars="beta.1")
    expected <- structure(list(`1` = structure(1:3, .Dim = c(3L, 1L), .Dimnames = list(NULL, "beta.1"))), .Names = "1")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #3 (", {
    foo <- mcmcdb_samples_chain_flatpars(test_wide2, FUN = dim)
    expected <- structure(list(`1` = c(3L, 3L), `2` = c(3L, 3L)), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })
  
})


