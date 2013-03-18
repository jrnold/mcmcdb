context("class-McmcdbParameters")

test_that("Class CharacterArray works as expected", {
  expect_is(CharacterArray(array(letters)), "CharacterArray")
})

test_that("Class CharacterArray throws error with non-character data", {
  expect_error(CharacterArray(array(1:10)), "invalid class")
})

test_that("ListOfCharArrays works as expected", {
  expect_is(ListOfCharArrays(llply(list(a=letters, b=LETTERS),
                                   CharacterArray)),
            "ListOfCharArrays")
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

######## methods

foo <- McmcdbParameters(list(alpha=array(
                               c("alpha.1.1", "alpha.2.1",
                                 "alpha.1.2", "alpha.2.2"),
                               c(2L, 2L)),
                             beta = "beta"))

test_that("dim,McmcdbParameters works as expected", {
  expect_equal(dim(foo),
               list(alpha = c(2L, 2L), beta = 1L))
})

test_that("dim,McmcdbParameters works as expected", {
  expect_equal(dimnames(foo),
               list(alpha = c("alpha.1.1", "alpha.2.1", "alpha.1.2", "alpha.2.2"),
                    beta = "beta"))
})



