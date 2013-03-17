context("method-mcmcdb_pararrays")

object <- McmcdbParameters(
  flatpars = McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
    beta.1 = McmcdbFlatpar("beta", 1L),
    beta.2 = McmcdbFlatpar("beta", 2L))),
  pararrays = McmcdbPararrayList(list(alpha = McmcdbPararray(1L, "alpha"),
    beta = McmcdbPararray(2L, c("beta.1", "beta.2")))))

test_that("mcmcdb_pararrays,McmcdbParameters-method works as expected", {
  pararrays <- mcmcdb_pararrays(object)
  expected <- list(alpha = "alpha",
                   beta = c("beta.1", "beta.2"))
  expect_equal(pararrays, expected)
})
