context("class-misc")

test_that("McmcdbFlatpars works as expected", {
  foo <- McmcdbFlatpars(data.frame(flatpar = "beta.1.1",
                                   pararray = "beta",
                                   idx = "1,1",
                                   stringsAsFactors = FALSE))
  expect_is(foo, "McmcdbFlatpars")
})

test_that("McmcdbFlatpars error if bad idx column", {
  expect_error(McmcdbFlatpars(data.frame(flatpar = "beta.1.1",
                                         pararray = "beta",
                                         idx = "a",
                                         stringsAsFactors = FALSE)),
               "invalid class")
})
