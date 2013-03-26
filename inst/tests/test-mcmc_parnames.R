context("mcmc_parnames")

test_that("mcmc_parnames_stan works as expected", {
  expect_equal(mcmc_parnames_stan("alpha", 1L), "alpha")
  expect_equal(mcmc_parnames_stan("alpha", 2L), c("alpha.1", "alpha.2"))
  expect_equal(mcmc_parnames_stan("alpha", c(2L, 2L)),
               c("alpha.1.1", "alpha.2.1", "alpha.1.2", "alpha.2.2"))
})

test_that("mcmc_parnames_bugs works as expected", {
  expect_equal(mcmc_parnames_bugs("alpha", 1L), "alpha")
  expect_equal(mcmc_parnames_bugs("alpha", 2L), c("alpha[1]", "alpha[2]"))
  expect_equal(mcmc_parnames_bugs("alpha", c(2L, 2L)),
               c("alpha[1,1]", "alpha[2,1]", "alpha[1,2]", "alpha[2,2]"))
})

test_that("mcmc_parnames_underscore works as expected", {
  expect_equal(mcmc_parnames_underscore("alpha", 1L), "alpha")
  expect_equal(mcmc_parnames_underscore("alpha", 2L), c("alpha_1", "alpha_2"))
  expect_equal(mcmc_parnames_underscore("alpha", c(2L, 2L)),
               c("alpha_1_1", "alpha_2_1", "alpha_1_2", "alpha_2_2"))
})

test_that("mcmc_parnames_pattern works as expected", {
  expect_equal(mcmc_parnames_pattern("alpha", 1L, "<", ";", ">"), "alpha")
  expect_equal(mcmc_parnames_pattern("alpha", 2L, "<", ";", ">"),
               c("alpha<1>", "alpha<2>"))
  expect_equal(mcmc_parnames_pattern("alpha", c(2L, 2L), "<", ";", ">"),
               c("alpha<1;1>", "alpha<2;1>", "alpha<1;2>", "alpha<2;2>"))
})
