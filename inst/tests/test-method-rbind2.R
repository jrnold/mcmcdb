context("rbind2 method")

local({
  source("data-McmcdbWide.R")
  test_that("rbind2,McmcdbWide,McmcdbWide works", {
    wide1 <- create_McmcdbWide(1L, 2L, c("beta", "gamma"))
    wide2 <- create_McmcdbWide(2L, 2L, c("beta", "gamma"))
    wide_1_2 <- rbind2(wide1, wide2)
    expect_is(wide_1_2, "McmcdbWide")
    expect_equal(mcmcdb_chains(wide_1_2), 1:2)
    expect_equal(nrow(wide_1_2@samples), 4)
  })
})
