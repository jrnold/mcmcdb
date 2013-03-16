context("mcmc_parparser_scalar")

test_that("mcmc_parparser_scalar works", {
  x <- mcmc_parse_parnames(c("alpha", "beta.1", "gamma.1.1"),
                           mcmc_parparser_stan)
  y <- McmcParameters(
    flatpars = McmcFlatparList(list(alpha = McmcFlatpar("alpha", 1L),
      beta.1 = McmcFlatpar("beta", 1L),
      gamma.1.1 = McmcFlatpar("gamma", c(1L, 1L)))),
    pararrays = McmcPararrayList(list(alpha = McmcPararray(1L, "alpha"),
      beta = McmcPararray(1L, "beta.1"),
      gamma = McmcPararray(c(1L, 1L), "gamma.1.1"))))
  expect_equal(x, y)
})

context("McmcParameters methods work")

object <- McmcParameters(
  flatpars = McmcFlatparList(list(alpha = McmcFlatpar("alpha", 1L),
    beta.1 = McmcFlatpar("beta", 1L),
    beta.2 = McmcFlatpar("beta", 2L))),
  pararrays = McmcPararrayList(list(alpha = McmcPararray(1L, "alpha"),
    beta = McmcPararray(2L, c("beta.1", "beta.2")))))

test_that("mcmcdb_flatpars,McmcParameters-method works as expected", {
  expect_equal(mcmcdb_flatpars(object), c(alpha.1 = "alpha",
                                          beta.1 = "beta",
                                          beta.2 = "beta"))
})

test_that("mcmcdb_par_indices,McmcParameters-method works as expected", {
  indices <- mcmcdb_par_indices(object)
  expected <-
    structure(list(alpha = structure(1L, .Dim = c(1L, 1L), .Dimnames = list(
    "alpha", "1")), beta = structure(1:2, .Dim = c(2L, 1L), .Dimnames = list(
    c("beta.1", "beta.2"), "1"))), .Names = c("alpha", "beta"))
  expect_equal(indices, expected)
})

test_that("mcmcdb_pararrays,McmcParameters-method works as expected", {
  indices <- mcmcdb_par_indices(object)  
})
