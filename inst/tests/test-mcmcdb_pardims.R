context("method-mcmcdb_pardims")

local({
  test_McmcdbParameters <-
    McmcdbParameters(list(alpha = "alpha", beta = c("beta.1", "beta.2")))
  
  test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
    expect_equal(mcmcdb_pardims(test_McmcdbParameters),
                 list(alpha = 1L, beta = 2L))
  })
})
