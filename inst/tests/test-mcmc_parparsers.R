context("mcmc_parparser_scalar")

test_that("mcmc_parparser_scalar works", {
  x <- mcmc_parparser_scalar(c("alpha", "beta.1", "gamma.1.1"))
  y <-
    McmcFlatparList(list(alpha = McmcFlatpar("alpha", 1L),
                         beta.1 = McmcFlatpar("beta.1", 1L),
                         gamma.1.1 = McmcFlatpar("gamma.1.1", 1L)))
  expect_equal(x, y)
})

context("mcmc_parparser_stan")

test_that("mcmc_parparser_stan works", {
  x <- mcmc_parparser_stan(c("alpha", "beta.1", "gamma.1.1"))
  y <-
    McmcFlatparList(list(alpha = McmcFlatpar("alpha", 1L),
                         beta.1 = McmcFlatpar("beta", 1L),
                         gamma.1.1 = McmcFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

context("mcmc_parparser_bugs")

test_that("mcmc_parparser_bugs works", {
  x <- mcmc_parparser_bugs(c("alpha", "beta[1]", "gamma[1,1]"))
  y <-
    McmcFlatparList(list(`alpha` = McmcFlatpar("alpha", 1L),
                         `beta[1]` = McmcFlatpar("beta", 1L),
                         `gamma[1,1]` = McmcFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})
