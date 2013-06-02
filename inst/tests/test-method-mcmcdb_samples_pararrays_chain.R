context("method mcmcdb_samples_parameters_chain")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_parameters_chain passes test #1", {
    foo <- mcmcdb_samples_parameters_chain(test_wide2)
    expected <- structure(list(beta = structure(list(`1` = structure(c(1L, 7L, 13L, 2L, 8L, 14L, 3L, 9L, 15L), .Dim = c(3L, 3L)), `2` = structure(c(4L, 10L, 16L, 5L, 11L, 17L, 6L, 12L, 18L), .Dim = c(3L, 3L))), .Names = c("1", "2"))), .Names = "beta")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_parameters passes test #2", {
    foo <- mcmcdb_samples_parameters_chain(test_wide2, chain_id=1, parameters="beta")
    expected <- structure(list(beta = structure(list(`1` = structure(c(1L, 7L, 13L, 2L, 8L, 14L, 3L, 9L, 15L), .Dim = c(3L, 3L))), .Names = "1")), .Names = "beta")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_parameters passes test #3 (FUN)", {
    foo <- mcmcdb_samples_parameters_chain(test_wide2, FUN = length)
    expected <- structure(list(beta = 2L), .Names = "beta")
    expect_equal(foo, expected)
  })
  
})


