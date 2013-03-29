context("mcmcdb_drop_chain method")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_drop_chain-method works", {
    foo <- mcmcdb_drop_chain(test_wide3, 2L)
    expect_is(foo, "McmcdbWide")
    expect_equal(mcmcdb_chains(foo, drop=TRUE), 1L)
  })
})
