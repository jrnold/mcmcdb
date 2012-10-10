context("c-methods")

data(line, package="mcmc4")
line2 <- McmcList2(line)
line3 <- McmcLong(line2)

test_that("signature x=McmcList2 works", {
    ret <- c(line2, line2)
    expect_is(ret, "McmcList2")
    expect_equal(length(ret), 2*length(line2))
})

test_that("signature x=McmcLong works", {
    ret <- c(line3, line3)
    expect_is(ret, "McmcLong")
    expect_equal(nrow(ret), 2*nrow(line3))
})


