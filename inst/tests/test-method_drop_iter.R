context("mcmcdb_drop_iter method")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_drop_iter-method works", {
    foo <- mcmcdb_drop_iters(test_wide3, 2L)
    expect_is(foo, "McmcdbWide")
    expect_equal(nrow(mcmcdb_iters(foo)), 4L)
  })
})
