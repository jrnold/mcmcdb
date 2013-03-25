context("method mcmcdb_samples_chain_iter")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_chain_iter passes test #1", {
    foo <- mcmcdb_samples_chain_iter(test_wide2)
    expected <- structure(list(`1` = structure(list(`1` = structure(list(beta = structure(c(1L, 7L, 13L), .Dim = 3L)), .Names = "beta"), `2` = structure(list(beta = structure(c(2L, 8L, 14L), .Dim = 3L)), .Names = "beta"), `3` = structure(list(beta = structure(c(3L, 9L, 15L), .Dim = 3L)), .Names = "beta")), .Names = c("1", "2", "3")), `2` = structure(list(`1` = structure(list(beta = structure(c(4L, 10L, 16L), .Dim = 3L)), .Names = "beta"), `2` = structure(list(beta = structure(c(5L, 11L, 17L), .Dim = 3L)), .Names = "beta"), `3` = structure(list(beta = structure(c(6L, 12L, 18L), .Dim = 3L)), .Names = "beta")), .Names = c("1", "2", "3"))), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_chain_iter passes test #2 (FUN)", {
    foo <- mcmcdb_samples_chain_iter(test_wide2, FUN = function(x) length(x))
    expected <- structure(list(`1` = 3L, `2` = 3L), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })
  
})


