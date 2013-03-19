context("method-mcmcdb_flatpars")

test_McmcdbParameters <-
  McmcdbParameters(list(alpha = "alpha", beta = c("beta.1", "beta.2")))

test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
  expect_equal(mcmcdb_flatpars(test_McmcdbParameters),
               structure(c("alpha", "beta", "beta"),
                         .Names = c("alpha", "beta.1", "beta.2")))

})

test_that("mcmcdb_flatpars,McmcdbWide-method works as expected", {
  expect_equal(mcmcdb_flatpars(test_McmcdbWide),
               mcmcdb_flatpars(test_McmcdbWide@parameters))
})
