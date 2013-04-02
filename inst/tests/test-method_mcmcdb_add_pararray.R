context("mcmcdb_add_parameters method")

local({
  source("data-McmcdbWide.R")

  test_that("mcmcdb_add_parameters-method works", {
    alpha <- matrix(1:6, dimnames=list(NULL, "alpha"))
    foo <- mcmcdb_add_parameters(test_wide3, alpha)
    expect_equal(colnames(foo@samples), c("beta", "gamma", "alpha"))
    expect_equal(names(mcmcdb_parameters(foo)),
                 c("beta", "gamma", "alpha"))
  })
})
