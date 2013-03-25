context("method mcmcdb_samples_iter")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_iter passes test #1", {
    foo <- mcmcdb_samples_iter(test_wide2)
    expected <- structure(list(`1` = structure(list(beta = structure(c(1L, 7L, 13L), .Dim = 3L)),
                                 .Names = "beta"),
                               `2` = structure(list(beta = structure(c(2L, 8L, 14L), .Dim = 3L)),
                                 .Names = "beta"),
                               `3` = structure(list(beta = structure(c(3L, 9L, 15L), .Dim = 3L)),
                                 .Names = "beta"), 
                               `4` = structure(list(beta = structure(c(4L, 10L, 16L), .Dim = 3L)),
                                 .Names = "beta"), 
                               `5` = structure(list(beta = structure(c(5L, 11L, 17L), .Dim = 3L)),
                                 .Names = "beta"), 
                               `6` = structure(list(beta = structure(c(6L, 12L, 18L), .Dim = 3L)),
                                 .Names = "beta")),
                          .Names = c("1", "2", "3", "4", "5", "6"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_iter passes test #2", {
    foo <- mcmcdb_samples_iter(test_wide2, chain_id = 1)
    expected <-
      structure(list(`1` = structure(list(beta = structure(c(1L, 7L, 13L), .Dim = 3L)),
                       .Names = "beta"),
                     `2` = structure(list(beta = structure(c(2L, 8L, 14L), .Dim = 3L)),
                       .Names = "beta"),
                     `3` = structure(list(beta = structure(c(3L, 9L, 15L), .Dim = 3L)),
                       .Names = "beta")), .Names = c("1", "2", "3"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_iter passes test #3 (FUN)", {
    foo <- mcmcdb_samples_iter(test_wide2,
                               FUN = function(x) sum(x[["beta"]]))
    expected <-
      structure(list(`1` = 21L, `2` = 24L, `3` = 27L, `4` = 30L, `5` = 33L, 
                     `6` = 36L), .Names = c("1", "2", "3", "4", "5", "6"))
    expect_equal(foo, expected)
  })
  
})


