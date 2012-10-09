context("quantile methods")

data(line, package="mcmc4")
line1 <- line[[1]]

test_that("x=\"mcmc\"", {
    expect_equal(dim(quantile(line1)),c(5, ncol(line1)))
    expect_equal(dim(quantile(line1, probs=c(0.25, 0.5))), c(2, ncol(line1)))
    expect_equal(dim(quantile(line1, type=1)), c(5, ncol(line1)))
})

test_that("x=\"mcmc.list\"", {
    expect_that(dim(quantile(line)), equals(c(5, ncol(line[[1]]))))
})

test_that("x=\"McmcLong\"", {
    ret <- quantile(as(McmcList2(line), "McmcLong"))
    expect_is(ret, "data.frame")
    expect_equal(colnames(ret), c("parameter", "2.5%", "25%", "50%", "75%", "97.5%"))
    expect_equal(as.character(ret$parameter), c("alpha", "beta", "sigma"))
})

