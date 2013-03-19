context("mcmc_parparsers")

# Not all valid parameter names
STAN <- c("alpha", "beta.1", "gamma.1.1", "foo.1.bar.1.2")
BUGS <- c("alpha", "beta[1]", "gamma[1,1]", "foo.[1,1].bar[1,2]")

stan_flatpars <-
  McmcdbFlatpars(data.frame(flatpar = STAN,
                            pararray = c("alpha", "beta", "gamma", "foo.1.bar"),
                            idx = c("1", "1", "1,1", "1,2"),
                            stringsAsFactors = FALSE))

bugs_flatpars <-
  McmcdbFlatpars(data.frame(flatpar = BUGS,
                            pararray = c("alpha", "beta", "gamma", "foo.[1,1].bar"),
                            idx = c("1", "1", "1,1", "1,2"),
                            stringsAsFactors = FALSE))


test_that("mcmc_parparser_scalar works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = STAN,
                              pararray = STAN,
                              idx = "1",
                              stringsAsFactors = FALSE))
  expect_equal(mcmc_parparser_scalar(STAN), expected)
})

test_that("mcmc_parparser_stan works", {
  expect_equivalent(mcmc_parparser_stan(STAN), stan_flatpars)
})

test_that("mcmc_parparser_bugs works", {
  expect_equivalent(mcmc_parparser_bugs(BUGS), bugs_flatpars)
})

test_that("mcmc_parparser_guess with BUGS works", {
  expect_equivalent(mcmc_parparser_guess(BUGS), bugs_flatpars)
})

test_that("mcmc_parparser_guess with Stan works", {
  expect_equivalent(mcmc_parparser_guess(STAN), stan_flatpars)
})

