context("class-McmcdbParameters")

test_that("Class ParnameArray works as expected", {
  expect_is(mcmcdb:::ParnameArray(array(letters)), "ParnameArray")
})

test_that("Class ParnameArray converts non-character data", {
  expect_is(mcmcdb:::ParnameArray(array(1:10)), "ParnameArray")
})

test_that("Class ParnameArray throws error if NA", {
  expect_error(mcmcdb:::ParnameArray(c("a", NA)), "invalid class")
})


foo <- list(alpha="alpha.1", beta=array(c("beta.1.1", "beta.1.2"), c(1, 2)))

test_that("McmcdbParameters works as expected", {
  expect_is(McmcdbParameters(foo), "McmcdbParameters")
})

test_that("McmcdbParameters thows error if missing names", {
  expect_error(McmcdbParameters(list("alpha.1"), "invalid class"))
  foo <- McmcdbParameters(list(a = "alpha.1"))
  foo@names[1] <- NA_character_
  expect_error(validObject(foo), "invalid class")
  foo@names[1] <- ""
  expect_error(validObject(foo), "invalid class")
})

test_that("McmcdbParameters thows error if missing names", {
  expect_error(McmcdbParameters(list(alpha=c("alpha.1", NA))),
               "invalid class")
})

######## methods

foo <- list(alpha=array(c("alpha.1.1", "alpha.2.1",
              "alpha.1.2", "alpha.2.2"),
              c(2L, 2L)),
            beta = "beta")
foo <- McmcdbParameters(foo)
