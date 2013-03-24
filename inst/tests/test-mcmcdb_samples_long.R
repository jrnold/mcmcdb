context("method mcmcdb_samples_long")

local({
  source("data-McmcdbWide.R")
  
  test_that("McmcdbWide test #1", {
    foo <- mcmcdb_samples_long(test_wide2)
    expect_equal(colnames(foo), c("chain_id", "iter",
                                  "flatpar", "val"))
    expect_equal(nrow(foo), 18L)
  })

  test_that("McmcdbWide test #2", {
    foo <- mcmcdb_samples_long(test_wide2, chain_id=1, flatpars="beta.1")
    expect_equal(nrow(foo), 3L)
    expect_equal(foo[["chain_id"]], rep(1L, 3))
    expect_equal(foo[["iter"]], 1:3)
    expect_equal(foo[["val"]], 1:3)
    expect_equal(as.character(foo[["flatpar"]]), rep("beta.1", 3))
  })
})


