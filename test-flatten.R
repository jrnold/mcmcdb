context("mcmc_parnames_*")

parameters <- mcmc_parse_parnames(c("alpha", "beta.1", "beta.2",
                                    mcmc_parparser_stan))

test_that("mcmc_parnames_stan_idx works as expected", {
  expect_equal(mcmc_parnames_stan_idx("alpha", 1L, 1L), "alpha")
  expect_equal(mcmc_parnames_stan_idx("alpha", 1L, 2L), "alpha.1")
  expect_equal(mcmc_parnames_stan_idx("alpha", c(2L, 2L), c(2L, 2L)),
               "alpha.2.2")
})

test_that("mcmc_parnames_stan works as expected", {
  expect_equal(mcmc_parnames_stan("alpha", 1L), "alpha")
  expect_equal(mcmc_parnames_stan("alpha", 2L), "alpha.1")
  expect_equal(mcmc_parnames_stan("alpha", c(2L, 2L)),
               c("alpha.1.1", "alpha.2.1", "alpha.1.2", "alpha.2.2"))
})

test_that("mcmc_parnames_bugs_idx works as expected", {
  expect_equal(mcmc_parnames_bugs_idx("alpha", 1L, 1L), "alpha")
  expect_equal(mcmc_parnames_bugs_idx("alpha", 1L, 2L), "alpha[1]")
  expect_equal(mcmc_parnames_bugs_idx("alpha", c(2L, 2L), c(2L, 2L)),
               "alpha[2,2]")
})

test_that("mcmc_parnames_bugs_idx works as expected", {
  expect_equal(mcmc_parnames_bugs("alpha", 1L), "alpha")
  expect_equal(mcmc_parnames_bugs("alpha", 2L), c("alpha[1]", "alpha[2]"))
  expect_equal(mcmc_parnames_bugs("alpha", c(2L, 2L)),
               c("alpha[1,1]", "alpha[2,1]", "alpha[1,2]", "alpha[2,2]"))
})

context("(stan|bugs)_to_(stan|bugs)_parnames")

stan_parnames <- c("alpha", "beta.1", "gamma.1.1")
bugs_parnames <- c("alpha", "beta[1]", "gamma[1,1]")

test_that("bugs_to_stan_parnames works as expected", {
  expect_equal(bugs_to_stan_parnames(bugs_parnames), stan_parnames)
})

test_that("stan_to_bugs_parnames works as expected", {
  expect_equal(stan_to_bugs_parnames(stan_parnames), bugs_parnames)
})


