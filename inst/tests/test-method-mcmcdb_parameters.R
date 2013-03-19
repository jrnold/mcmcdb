context("method-mcmcdb_paridx")
source("data-McmcbWide.R")

test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
  expect_equal(mcmcdb_parameters(test_McmcdbWide),
               test_McmcdbWide@parameters)
})
