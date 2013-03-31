context("mcmcdb_rename_chain method")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_rename_chain,McmcdbWide works", {
    wide1 <- create_McmcdbWide(1L, 2L, c("beta", "gamma"))
    wide1 <- mcmcdb_rename_chain(wide1, 1L, 2L)
    expect_is(wide1, "McmcdbWide")
    expect_equal(mcmcdb_chains(wide1, drop=TRUE), 2L)
  })
})
