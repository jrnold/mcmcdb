context("mcmcByParameter-methods")

data(line, package="mcmc4")
line_long <- McmcLong(line)

test_that("signature object=McmcLong works", {
    ret <- mcmcByParameter(line_long)
    expect_is(ret, "list")
    expect_equal(length(ret), 3)
    expect_equal(names(ret), c("alpha", "beta", "sigma"))
    expect_equal(unname(sapply(ret, length)), rep(400, 3))
})

test_that("signature object=McmcLong, fun=mean works", {
    ret <- mcmcByParameter(line_long, .fun=mean)
    expect_is(ret, "list")
    expect_equal(length(ret), 3)
    expect_equal(names(ret), c("alpha", "beta", "sigma"))
    expect_equal(unname(sapply(ret, length)), rep(1, 3))
})


