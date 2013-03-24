context("method mcmcdb_samples_pararray")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_pararray test #1", {
    foo <- mcmcdb_samples_pararrays(test_wide2)
    expect_is(foo, "list")
    expect_equal(length(foo), 1L)
    expect_equal(names(foo), "beta")
  })

  ## test_that("mcmcdb_samples_pararray test #1", {
  ##   foo <- mcmcdb_samples_pararray(test_wide2, chain_id = 1)
  ##   expect_equal(length(foo), 3L)
  ## })
})
