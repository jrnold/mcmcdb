context("method mcmcdb_samples_iter")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_iter test #1", {
    foo <- mcmcdb_samples_iter(test_wide2)
    expect_is(foo, "list")
    expect_equal(length(foo), 6L)
  })

  test_that("mcmcdb_samples_iter test #1", {
    foo <- mcmcdb_samples_iter(test_wide2, chain_id = 1)
    expect_equal(length(foo), 3L)
  })
})


