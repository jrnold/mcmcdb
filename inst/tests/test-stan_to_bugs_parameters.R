context("(stan|bugs)_to_(stan|bugs)_parnames")

stan_parnames <- c("alpha", "beta.1", "gamma.1.1")
bugs_parnames <- c("alpha", "beta[1]", "gamma[1,1]")

test_that("bugs_to_stan_parnames works as expected", {
  expect_equal(bugs_to_stan_parnames(bugs_parnames), stan_parnames)
})

test_that("stan_to_bugs_parnames works as expected", {
  expect_equal(stan_to_bugs_parnames(stan_parnames), bugs_parnames)
})
