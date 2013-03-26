context("class-misc")

test_that("McmcdbFlatpars works as expected", {
  foo <- McmcdbFlatpars(flatpar = "beta.1.1",
                        pararray = "beta",
                        idx = "1,1",
                        scalar = FALSE,
                        stringsAsFactors = FALSE)
  expect_is(foo, "McmcdbFlatpars")
})

test_that("McmcdbFlatpars error if bad idx column", {
  expect_error(McmcdbFlatpars(data.frame(flatpar = "beta.1.1",
                                         pararray = "beta",
                                         idx = "a",
                                         scalar = FALSE,
                                         stringsAsFactors = FALSE)),
               "invalid class")
})
