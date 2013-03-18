context("class-McmcdbParameters")

test_that("Class CharacterArray works as expected") {
  expect_is(CharacterArray(array(letters)), "CharacterArray")
}

test_that("Class CharacterArray throws error with non-character data") {
  expect_error(CharacterArray(array(1:10)), "invalid class")
}
