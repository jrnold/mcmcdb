context("method-mcmcdb_parameters")
source("data-McmcdbWide.R")

test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
  expect_equal(mcmcdb_parameters(test_wide2), test_wide2@parameters)
})
