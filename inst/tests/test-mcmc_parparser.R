context("mcmc_parparsers")

STAN <- c("alpha", "beta.1", "gamma.1.1")
BUGS <- c("alpha", "beta[1]", "gamma[1,1]")

test_that("mcmc_parparser_scalar works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = STAN,
                              pararray = STAN,
                              idx = "1",
                              stringsAsFactors = FALSE))
  expect_equal(mcmc_parparser_scalar(STAN), expected)
})

test_that("mcmc_parparser_stan works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = STAN,
                              pararray = c("alpha", "beta", "gamma"),
                              idx = c("1", "1", "1,1"),
                              stringsAsFactors = FALSE))
  expect_equivalent(mcmc_parparser_stan(STAN), expected)
})

test_that("mcmc_parparser_bugs works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = BUGS,
                              pararray = c("alpha", "beta", "gamma"),
                              idx = c("1", "1", "1,1"),
                              stringsAsFactors = FALSE))
  expect_equivalent(mcmc_parparser_bugs(BUGS), expected)
})

test_that("mcmc_parparser_guess with BUGS works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = BUGS,
                              pararray = c("alpha", "beta", "gamma"),
                              idx = c("1", "1", "1,1"),
                              stringsAsFactors = FALSE))
  expect_equivalent(mcmc_parparser_guess(BUGS), expected)
})

test_that("mcmc_parparser_guess with Stan works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = STAN,
                              pararray = c("alpha", "beta", "gamma"),
                              idx = c("1", "1", "1,1"),
                              stringsAsFactors = FALSE))
  expect_equivalent(mcmc_parparser_guess(STAN), expected)
})

