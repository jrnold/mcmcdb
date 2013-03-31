context("method-c.R")

local({
  source("data-McmcdbWide.R")
  
  test_that("c,McmcdbWide works", {
    wide1 <- create_McmcdbWide(1L, 2L, c("beta", "gamma"))
    wide2 <- create_McmcdbWide(2L, 2L, c("beta", "gamma"))
    wide3 <- create_McmcdbWide(3L, 2L, c("beta", "gamma"))
    wide_c <- c(wide1, wide2, wide3)
    expect_is(wide_c, "McmcdbWide")
    expect_equal(mcmcdb_chains(wide_c), 1:3)
    expect_equal(nrow(wide_c@samples), 6)
  })
  
})
