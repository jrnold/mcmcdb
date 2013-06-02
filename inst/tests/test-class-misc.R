context("class-misc")

test_that("McmcdbFlatpars works as expected", {
  foo <- McmcdbFlatpars(flatpar = "beta.1.1",
                        parameter = "beta",
                        idx = "1,1",
                        scalar = FALSE,
                        stringsAsFactors = FALSE)
  expect_is(foo, "McmcdbFlatpars")
})

test_that("McmcdbFlatpars error if bad idx column", {
  expect_error(McmcdbFlatpars(data.frame(flatpar = "beta.1.1",
                                         parameter = "beta",
                                         idx = "a",
                                         scalar = FALSE,
                                         stringsAsFactors = FALSE)),
               "invalid class")
})

test_that("McmcdbIters works", {
  foo <- McmcdbIters(chain_id = 1L, iter = 1:5)
  expect_is(foo, "McmcdbIters")
})

test_that("McmcdbIters error if duplicates", {
  expect_error(McmcdbIters(chain_id = 1L, iter = c(1L, 1L)),
               "invalid class")
})

######

test_that("McmcdbFlatparChains works", {
  foo <- McmcdbFlatparChains(chain_id = 1L,
                             flatpar = paste("beta", 1:2, sep="."),
                             init = 0)
  expect_is(foo, "McmcdbFlatparChains")
})

test_that("McmcdbFlatparChains error if duplicates", {
  expect_error(McmcdbFlatparChains(chain_id = 1L,
                                   flatpar = rep("beta", 2),
                                   init = 0),
               "invalid class")
})


