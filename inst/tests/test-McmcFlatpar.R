context("McmcFlatpar")

test_that("McmFlatpar works", {
  foo <- McmcFlatpar(pararray = "beta", index = 1L)
  expect_is(foo, "McmcFlatpar")
  expect_equal(foo@pararray, "beta")
  expect_equal(foo@index, 1L)
})

test_that("McmcFlatparList works", {
  foo <- McmcFlatparList(list(beta = McmcFlatpar(pararray = "beta", index = 1L)))
  expect_is(foo, "McmcFlatparList")
  expect_is(foo[["beta"]], "McmcFlatpar")
})

          
