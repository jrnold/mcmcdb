context("mcmcdb_mcpar")
source("data-McmcdbWide.R")

test_that("mcmcdb_mcpar,McmcdbWide works as expected", {
  mcpars <- mcmcdb_mcpar(test_wide2)
  expect_equal(mcpars,
               data.frame(chain_id = 1:2,
                          n_iter = rep(3L, 2),
                          iter_start = rep(NA_integer_, 2),
                          iter_end = rep(NA_integer_, 2),
                          iter_thin = rep(NA_integer_, 2)))
})
