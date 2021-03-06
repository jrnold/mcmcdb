context("method-mcmcdb_paridx")

local({
  source("data-McmcdbWide.R")
  
  test_McmcdbParameters <-
    McmcdbParameters(list(alpha = "alpha", beta = c("beta.1", "beta.2")))
  
  test_that("mcmcdb_flatpars,McmcdbParameters-method works as expected", {
    expected <- 
      structure(list(alpha = structure(1L, .Dim = c(1L, 1L),
                       .Dimnames = list("alpha", "Var1")),
                     beta = structure(1:2, .Dim = c(2L, 1L),
                       .Dimnames = list(c("beta.1", "beta.2"), "Var1"))),
                .Names = c("alpha", "beta"))
    expect_equal(mcmcdb_paridx(test_McmcdbParameters), expected)
  })
  
  test_that("mcmcdb_flatpars,McmcdbWide-method works as expected", {
    expect_equal(mcmcdb_paridx(test_wide2),
                 mcmcdb_paridx(test_wide2@parameters))
  })
})
