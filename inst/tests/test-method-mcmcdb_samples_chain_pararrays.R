context("method mcmcdb_samples_chain_pararrays")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_chain_pararrays test #1", {
    foo <- mcmcdb_samples_chain_pararrays(test_wide2)
    expected <- structure(list(`1` = structure(list(beta = structure(c(1L, 7L, 13L, 2L, 8L, 14L, 3L, 9L, 15L), .Dim = c(3L, 3L))), .Names = "beta"), `2` = structure(list(beta = structure(c(4L, 10L, 16L, 5L, 11L, 17L, 6L, 12L, 18L), .Dim = c(3L, 3L))), .Names = "beta")), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_chain_pararrays test #2 (FUN)", {
    foo <- mcmcdb_samples_chain_pararrays(test_wide2, FUN = length)
    expected <- structure(list(`1` = 1L, `2` = 1L), .Names = c("1", "2"))
    expect_equal(foo, expected)
  })

})
