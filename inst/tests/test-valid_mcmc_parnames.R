context("valid_mcmc_parnames")

test_that("valid_mcmc_parnames work", {
  expect_equal(valid_mcmc_parnames(c("alpha", "beta[1]", "gamma[1,1]", "gamma_")),
               c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(valid_mcmc_parnames(c("alpha", "beta[1]", "gamma[1,1]", "beta.1", "beta.1.1"),
                                   "stan"),
               c(TRUE, FALSE, FALSE, TRUE, TRUE))
})

test_that("valid_mcmc_parnames throws error with bad option", {
  expect_error(valid_mcmc_parnames("a", "foo"))
})
