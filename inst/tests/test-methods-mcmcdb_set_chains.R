context("method mcmcdb_set_chains")

local({
  source("data-McmcdbWide.R")

  test_that("mcmcdb_set_iters,McmcdbWide,NULL works", {
    mcmcdb_set_iters(test_wide2) <- NULL
    expect_equal(nrow(test_wide2@samples), 0)
    expect_equal(nrow(test_wide2@iters), 0)
  })

  test_that("mcmcdb_set_iters,McmcdbWide,NULL with chain_id != NULL works", {
    mcmcdb_set_iters(test_wide2, chain_id = 1) <- NULL
    expect_equal(nrow(test_wide2@samples), 3L)
    expect_equal(nrow(test_wide2@iters), 3L)
    expect_equal(unique(test_wide2@iters[["chain_id"]]), 2L)
  })

  test_that("mcmcdb_set_iters,McmcdbWide,NULL with iter != NULL works", {
    mcmcdb_set_iters(test_wide2, iter = 1) <- NULL
    expect_equal(nrow(test_wide2@samples), 4L)
    expect_equal(unique(test_wide2@iters[["iter"]]), 2:3)
  })

  
  test_that("mcmcdb_set_iters,McmcdbWide,list works", {
    mcmcdb_set_iters(test_wide2) <-
      list(iters = McmcdbIters(chain_id = 2L, iter = 4L),
           samples = matrix(1:3, nrow=1,
             dimnames = list(NULL, paste("beta", 1:3, sep="."))))
    expect_equal(nrow(test_wide2@samples), 7L)
    expect_equivalent(test_wide2@samples[7, ], 1:3)
  })

  
})
