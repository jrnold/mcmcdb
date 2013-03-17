context("class-McmcdbFlatpar")

test_that("McmcdbFlatpar initialize works", {
  foo <- McmcdbFlatpar(pararray = "beta", index = 1L)
  expect_is(foo, "McmcdbFlatpar")
  expect_equal(foo@pararray, "beta")
  expect_equal(foo@index, 1L)
})

test_that("McmcdbFlatparList initialize works", {
  foo <- McmcdbFlatparList(list(beta = McmcdbFlatpar(pararray = "beta", index = 1L)))
  expect_is(foo, "McmcdbFlatparList")
  expect_is(foo[["beta"]], "McmcdbFlatpar")
})

          
