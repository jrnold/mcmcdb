context("mcmc_parparsers")

# Not all valid parameter names
STAN <- c("alpha", "beta.1", "gamma.1.1", "foo.1.bar.1.2")
BUGS <- c("alpha", "beta[1]", "gamma[1,1]", "foo.[1,1].bar[1,2]")
UNDERSCORE <- c("alpha", "beta_1", "gamma_1_1", "foo_1_1_bar_1_2")

stan_flatpars <-
  McmcdbFlatpars(data.frame(flatpar = STAN,
                            pararray = c("alpha", "beta", "gamma", "foo.1.bar"),
                            idx = c("1", "1", "1,1", "1,2"),
                            scalar = c(TRUE, rep(FALSE, 3)),
                            stringsAsFactors = FALSE))

bugs_flatpars <-
  McmcdbFlatpars(data.frame(flatpar = BUGS,
                            pararray = c("alpha", "beta", "gamma", "foo.[1,1].bar"),
                            idx = c("1", "1", "1,1", "1,2"),
                            scalar = c(TRUE, rep(FALSE, 3)),
                            stringsAsFactors = FALSE))

underscore_flatpars <-
  McmcdbFlatpars(data.frame(flatpar = UNDERSCORE,
                            pararray = c("alpha", "beta", "gamma", "foo_1_1_bar"),
                            idx = c("1", "1", "1,1", "1,2"),
                            scalar = c(TRUE, rep(FALSE, 3)),
                            stringsAsFactors = FALSE))


test_that("mcmc_parparser_scalar works", {
  expected <-
    McmcdbFlatpars(data.frame(flatpar = STAN,
                              pararray = STAN,
                              idx = "1",
                              scalar = TRUE,
                              stringsAsFactors = FALSE))
  expect_equal(mcmc_parparser_scalar(STAN), expected)
})

test_that("mcmc_parparser_stan works", {
  expect_equivalent(mcmc_parparser_stan(STAN), stan_flatpars)
})

test_that("mcmc_parparser_bugs works", {
  expect_equivalent(mcmc_parparser_bugs(BUGS), bugs_flatpars)
})

test_that("mcmc_parparser_underscore works", {
  expect_equivalent(mcmc_parparser_underscore(UNDERSCORE), underscore_flatpars)
})

test_that("mcmc_parparser_pattern passes test #1", {
  parameters <- c("alpha", "beta<1>", "gamma<1;2>", "gamma<1;2>bar<1;2>")
  expected <-
    McmcdbFlatpars(data.frame(flatpar = parameters,
                              pararray = c("alpha", "beta", "gamma", "gamma<1;2>bar"),
                              idx = c("1", "1", "1,2", "1,2"),
                              scalar = c(TRUE, rep(FALSE, 3)),
                              stringsAsFactors = FALSE))
  foo <- mcmc_parparser_pattern(parameters, pre="<", sep=";", post=">")
  expect_equivalent(foo, expected)
})


test_that("mcmc_parparser_pattern passes test #1", {
  parameters <- c("alpha", "beta|1|", "gamma<1;2>", "bar<1,2>")
  expected <-
    McmcdbFlatpars(data.frame(flatpar = parameters,
                              pararray = c("alpha", "beta", "gamma", "bar"),
                              idx = c("1", "1", "1,2", "1,2"),
                              scalar = c(TRUE, rep(FALSE, 3)),
                              stringsAsFactors = FALSE))
  foo <- mcmc_parparser_pattern(parameters, pre="(?:[<|])", sep="[,;]", post="(?:[>|])")
  expect_equivalent(foo, expected)
})

test_that("mcmc_parparser_guess with BUGS works", {
  expect_equivalent(mcmc_parparser_guess(BUGS), bugs_flatpars)
})

test_that("mcmc_parparser_guess with Stan works", {
  expect_equivalent(mcmc_parparser_guess(STAN), stan_flatpars)
})

