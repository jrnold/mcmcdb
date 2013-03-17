context("method-mcmcdb_flatpars")

object <- McmcdbParameters(
  flatpars = McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
    beta.1 = McmcdbFlatpar("beta", 1L),
    beta.2 = McmcdbFlatpar("beta", 2L))),
  pararrays = McmcdbPararrayList(list(alpha = McmcdbPararray(1L, "alpha"),
    beta = McmcdbPararray(2L, c("beta.1", "beta.2")))))

test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
  expect_equal(mcmcdb_flatpars(object),
               structure(c("alpha", "beta", "beta"),
                         .Names = c("alpha", "beta.1", "beta.2")))
})
