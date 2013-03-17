context("utilities")

test_that("expand_grid_dim works", {
  x <- mcmcdb:::expand_grid_dim(c(2, 3))
  expect_equal(x,
               structure(c(1L, 2L, 1L, 2L, 1L, 2L, 1L, 1L, 2L, 2L, 3L, 3L),
                         .Dim = c(6L, 2L),
                         .Dimnames = list(NULL, c("Var1", "Var2"))))
})
