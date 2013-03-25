context("method mcmcdb_samples_flatpars_chain")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_flatpars_chain passes test #1", {
    foo <- mcmcdb_samples_flatpars_chain(test_wide2)
    expected <- structure(list(beta.1 = structure(list(`1` = c(1, 2, 3), `2` = c(4, 
5, 6)), .Names = c("1", "2")), beta.2 = structure(list(`1` = c(7, 
8, 9), `2` = c(10, 11, 12)), .Names = c("1", "2")), beta.3 = structure(list(
    `1` = c(13, 14, 15), `2` = c(16, 17, 18)), .Names = c("1", 
"2"))), .Names = c("beta.1", "beta.2", "beta.3"))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #2", {
    foo <- mcmcdb_samples_flatpars_chain(test_wide2, chain_id=1, flatpars="beta.1")
    expected <- structure(list(beta.1 = structure(list(`1` = c(1, 2, 3)), .Names = "1")), .Names = "beta.1")
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars passes test #3 (", {
    foo <- mcmcdb_samples_flatpars_chain(test_wide2, FUN = length)
    expected <- structure(list(beta.1 = 2L, beta.2 = 2L, beta.3 = 2L), .Names = c("beta.1", "beta.2", "beta.3"))
    expect_equal(foo, expected)
  })
  
})


