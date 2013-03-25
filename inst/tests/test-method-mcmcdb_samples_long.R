context("method mcmcdb_samples_long")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_long passes test #1", {
    foo <- mcmcdb_samples_long(test_wide2)
    expected <- structure(list(chain_id = c(1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L, 1L, 1L, 1L, 2L, 2L, 2L), iter = c(1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L, 1L, 2L, 3L), flatpar = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L), .Label = c("beta.1", "beta.2", "beta.3"), class = "factor"), val = 1:18), .Names = c("chain_id", "iter", "flatpar", "val"), row.names = c(NA, -18L), class = "data.frame")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_iter passes test #2", {
    foo <- mcmcdb_samples_long(test_wide2, chain_id = 1, flatpar=c("beta.1"))
    expected <- structure(list(chain_id = c(1L, 1L, 1L), iter = 1:3, flatpar = structure(c(1L, 1L, 1L), .Label = "beta.1", class = "factor"), val = 1:3), .Names = c("chain_id", "iter", "flatpar", "val"), row.names = c(NA, -3L), class = "data.frame")
    expect_equal(foo, expected)
  })
  
})


