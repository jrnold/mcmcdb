context("mcmcdb_drop_pararray method")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_drop_pararray-method works", {
    foo <- mcmcdb_drop_pararray(test_wide3, "beta")
    expect_is(foo, "McmcdbWide")
    expect_equal(names(mcmcdb_parameters(foo)), "gamma")
  })
})
