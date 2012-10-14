context("c-methods")

data(line, package="mcmc4")
line_long <- McmcLong(line)

## test_that("signature x=McmcLong works", {
##     ret <- c(line_long, line_long)
##     expect_is(ret, "McmcLong")
##     expect_equal(nrow(ret@samples), 2*nrow(line_long@samples))
## })


