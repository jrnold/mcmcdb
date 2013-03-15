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

