context("method mcmcdb_samples_pararrays")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_pararrays test #1", {
    foo <- mcmcdb_samples_pararrays(test_wide2)
    expected <- structure(list(beta = structure(c(1L, 7L, 13L, 2L, 8L, 14L, 3L, 
                                 9L, 15L, 4L, 10L, 16L, 5L, 11L, 17L, 6L, 12L, 18L),
                                 .Dim = c(3L, 6L))), .Names = "beta")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_pararrays test #2 (FUN)", {
    foo <- mcmcdb_samples_pararrays(test_wide2, FUN = sum)
    expected <- structure(list(beta = 171L), .Names = "beta")
    expect_equal(foo, expected)
  })

})
