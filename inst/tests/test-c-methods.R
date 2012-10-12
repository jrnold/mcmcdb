context("c-methods")

data(line, package="mcmc4")
line_long <- McmcLong(line2)

test_that("signature x=McmcLong works", {
    ret <- c(line_long, line3)
    expect_is(ret, "McmcLong")
    expect_equal(nrow(ret@samples), 2*nrow(line3@samples))
})


