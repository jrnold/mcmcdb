context("valid_mcmc_parnames")

test_that("valid_mcmc_parnames work", {
  expect_equal(valid_mcmc_parnames(c("alpha", "beta[1]", "gamma[1,1]")), TRUE)
  expect_equal(valid_mcmc_parnames(c("alpha", "beta[1]", "gamma[1,1]"), "stan"), FALSE)
  expect_equal(valid_mcmc_parnames(c("alpha", "beta.1", "gamma.1.1"), "stan"), TRUE)
})

test_that("valid_mcmc_parnames throws error with bad option", {
  expect_error(valid_mcmc_parnames("a", "foo"))
})
