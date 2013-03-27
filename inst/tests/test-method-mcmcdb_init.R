context("method-mcmcdb_init")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_init,McmcdbWide-method works with flatten = TRUE", {
    expected <- structure(c(0, 0, 0, 0, 0, 0), .Dim = 2:3,
                          .Dimnames
                          = structure(list(chain_id = c("1", "2"),
                            c("beta.1", "beta.2", "beta.3")),
                            .Names = c("chain_id", "")))
    foo <- mcmcdb_init(test_wide2, flatten = TRUE)
    expect_equal(expected, foo)
  })
  
  test_that("mcmcdb_flatpars,McmcdbWide-method works with flatten = FALSE", {
    foo <- mcmcdb_init(test_wide2, flatten = FALSE)
    expected <- structure(list(`1` = structure(list(beta = structure(c(0, 0, 0), .Dim = 3L)), .Names = "beta"), `2` = structure(list(beta = structure(c(0, 0, 0), .Dim = 3L)), .Names = "beta")), .Names = c("1", "2"))
    expect_equal(expected, foo)
  })
})
