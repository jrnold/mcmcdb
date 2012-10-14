context("extract-methods")

data("line", package="mcmc4")

line_long <- McmcLong(line)

test_that("extract,McmcLong,missing,missing-method", {
    expect_equal(nrow(line_long[]), nrow(line_long@samples))
})

# ---------------------------------

test_that("extract,McmcLong,numeric,missing-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[1]), 400)
})

test_that("extract,McmcLong,character,missing-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long["alpha"]), 400)
})

test_that("extract,McmcLong,function,missing-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[function(x) x == "alpha"]), 400)
})

test_that("extract,McmcLong,logical,missing-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[c(rep(TRUE, 400), rep(FALSE, 800))]), 400)
})

# ---------------------------------

test_that("extract,McmcLong,missing,numeric-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[ , 1]), 600)
})

test_that("extract,McmcLong,missing,function-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[ , function(x) x == 1]), 600)
})

test_that("extract,McmcLong,missing,logical-method k=Missing, value=Missing", {
    expect_equal(nrow(line_long[ , c(rep(TRUE, 200), rep(FALSE, 1000))]), 200)
})

# ---------------------------------


test_that("extract,McmcLong,missing,missing-method k=numeric, value=Missing", {
    expect_equal(nrow(line_long[ , , 1]), 6)
})

test_that("extract,McmcLong,missing,missing-method k=function, value=Missing", {
    expect_equal(nrow(line_long[ , , function(x) x == 1]), 6)
})

test_that("extract,McmcLong,missing,missing-method k=logical, value=Missing", {
    expect_equal(nrow(line_long[ , , c(rep(TRUE, 200), rep(FALSE, 1000))]), 200)
})

# ---------------------------------

test_that("extract,McmcLong,missing,missing-method k=missing, value=function", {
    expect_equal(nrow(line_long[ , , , function(x) x > 2.5]), 400)
})

# -----------------------------------

test_that("[[,McmcLong works", {
    expect_equal(nrow(line_long[["alpha"]]), 400)
})

test_that("$,McmcLong works", {
    expect_equal(nrow(line_long$alpha), 400)
})

# -----------------------------------

test_that("[[,McmcParameters works", {
    expect_equal(line_long@parameters[["beta"]], "beta")
})
