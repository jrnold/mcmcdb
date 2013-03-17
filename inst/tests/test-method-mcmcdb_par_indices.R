context("method-mcmcdb_par_indices")

object <- McmcdbParameters(
  flatpars = McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
    beta.1 = McmcdbFlatpar("beta", 1L),
    beta.2 = McmcdbFlatpar("beta", 2L))),
  pararrays = McmcdbPararrayList(list(alpha = McmcdbPararray(1L, "alpha"),
    beta = McmcdbPararray(2L, c("beta.1", "beta.2")))))

test_that("mcmcdb_par_indices,McmcdbParameters-method works as expected", {
  indices <- mcmcdb_par_indices(object)
  expected <-
    structure(list(alpha = structure(1L, .Dim = c(1L, 1L), .Dimnames = list(
    "alpha", "1")), beta = structure(1:2, .Dim = c(2L, 1L), .Dimnames = list(
    c("beta.1", "beta.2"), "1"))), .Names = c("alpha", "beta"))
  expect_equal(indices, expected)
})

