context("method mcmcdb_samples_flatpars")

local({
  source("data-McmcdbWide.R")
  
  test_that("mcmcdb_samples_flatpars", {
    expect_equal(mcmcdb_samples_flatpars(test_wide2),
                 array(1:18, c(6, 3),
                       dimnames=list(NULL, paste("beta", 1:3, sep="."))))
  })

  test_that("mcmcdb_samples_flatpars", {
    foo <- mcmcdb_samples_flatpars(test_wide2,
                                   chain_id=1, flatpars="beta.1")
    expected <- array(1:3, c(3, 1), dimnames=list(NULL, paste("beta.1")))
    expect_equal(foo, expected)
  })

  test_that("mcmcdb_samples_flatpars", {
    foo <- mcmcdb_samples_flatpars(test_wide2,
                                   chain_id=1, flatpars="beta.1", drop=TRUE)
    expected <- 1:3
    expect_equal(foo, expected)
  })

})


