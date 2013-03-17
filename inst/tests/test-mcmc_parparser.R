context("mcmc_parparsers")

test_that("mcmc_parparser_scalar works", {
  x <- mcmc_parparser_scalar(c("alpha", "beta.1", "gamma.1.1"))
  y <-
    McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
                         beta.1 = McmcdbFlatpar("beta.1", 1L),
                         gamma.1.1 = McmcdbFlatpar("gamma.1.1", 1L)))
  expect_equal(x, y)
})

test_that("mcmc_parparser_stan works", {
  x <- mcmc_parparser_stan(c("alpha", "beta.1", "gamma.1.1"))
  y <-
    McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
                         beta.1 = McmcdbFlatpar("beta", 1L),
                         gamma.1.1 = McmcdbFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

test_that("mcmc_parparser_bugs works", {
  x <- mcmc_parparser_bugs(c("alpha", "beta[1]", "gamma[1,1]"))
  y <-
    McmcdbFlatparList(list(`alpha` = McmcdbFlatpar("alpha", 1L),
                         `beta[1]` = McmcdbFlatpar("beta", 1L),
                         `gamma[1,1]` = McmcdbFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

test_that("mcmc_parparser_guess works", {
  x <- mcmc_parparser_bugs(c("alpha", "beta[1]", "gamma[1,1]"))
  y <-
    McmcdbFlatparList(list(`alpha` = McmcdbFlatpar("alpha", 1L),
                         `beta[1]` = McmcdbFlatpar("beta", 1L),
                         `gamma[1,1]` = McmcdbFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

test_that("mcmc_parparser_guess identifies stan", {
  x <- mcmc_parparser_guess(c("alpha", "beta.1", "gamma.1.1"))
  y <-
    McmcdbFlatparList(list(`alpha` = McmcdbFlatpar("alpha", 1L),
                         `beta.1` = McmcdbFlatpar("beta", 1L),
                         `gamma.1.1` = McmcdbFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

test_that("mcmc_parparser_guess identifies bugs", {
  x <- mcmc_parparser_guess(c("alpha", "beta[1]", "gamma[1,1]"))
  y <-
    McmcdbFlatparList(list(`alpha` = McmcdbFlatpar("alpha", 1L),
                         `beta[1]` = McmcdbFlatpar("beta", 1L),
                         `gamma[1,1]` = McmcdbFlatpar("gamma", c(1L, 1L))))
  expect_equal(x, y)
})

test_that("mcmc_parparser_guess defaults to scalars if cannot match Stan or BUGS", {
  # + is not a valid character for Stan or BUGS parameter names
  x <- mcmc_parparser_guess(c("alpha+", "beta[1]", "gamma[1,1]"))
  y <-
    McmcdbFlatparList(list(`alpha+` = McmcdbFlatpar("alpha+", 1L),
                         `beta[1]` = McmcdbFlatpar("beta[1]", 1L),
                         `gamma[1,1]` = McmcdbFlatpar("gamma[1,1]", c(1L))))
  expect_equal(x, y)
})

test_that("mcmc_parse_parnames works as expected", {
  x <- mcmc_parse_parnames(c("alpha", "beta.1", "gamma.1.1"),
                           mcmc_parparser_stan)
  y <- McmcdbParameters(
    flatpars = McmcdbFlatparList(list(alpha = McmcdbFlatpar("alpha", 1L),
      beta.1 = McmcdbFlatpar("beta", 1L),
      gamma.1.1 = McmcdbFlatpar("gamma", c(1L, 1L)))),
    pararrays = McmcdbPararrayList(list(alpha = McmcdbPararray(1L, "alpha"),
      beta = McmcdbPararray(1L, "beta.1"),
      gamma = McmcdbPararray(c(1L, 1L), "gamma.1.1"))))
  expect_equal(x, y)
})

