context("median methods")

data(line, package="mcmc4")

test_that("x=\"mcmc.list\"", {
    expect_equal(length(mean(line)), 3)
})

test_that("x=\"mcmc\"", {
    expect_equal(length(mean(line[[1]])), 3)
})

test_that("x=\"McmcLong\"", {
    x <- median(as(McmcList2(line), "McmcLong"))
    expect_is(x, "data.frame")
    expect_equal(as.character(x$parameter), c("alpha", "beta", "sigma"))
    expect_is(x$median, "numeric")
})

