context("mcmcByIteration-methods")

data(line, package="mcmc4")

line2 <- McmcList2(line)
linelong <- McmcLong(line2)

test_that("mcmcByIteration,McmcList2 works", {
    x <- mcmcByIteration(line2)
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("alpha", "beta", "sigma")))))
})

test_that("mcmcByIteration,McmcList2 works with function", {
    x <- mcmcByIteration(line2, FUN=function(x) list(foo=x$alpha + x$beta))
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("foo")))))
})

test_that("mcmcByIteration,McmcList2 works with data", {
    x <- mcmcByIteration(line2, data=list(a=100), FUN=function(x) list(foo=x$a + x$alpha))
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("foo")))))
    expect_true(all(sapply(x, function(y) y$foo > 95)))
})

#----------------------------------------------------

test_that("mcmcByIteration,McmcLong works", {
    x <- mcmcByIteration(linelong)
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("alpha", "beta", "sigma")))))
})

test_that("mcmcByIteration,McmcLong works with function", {
    x <- mcmcByIteration(linelong, FUN=function(x) list(foo=x$alpha + x$beta))
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("foo")))))
})

test_that("mcmcByIteration,McmcLong works with data", {
    x <- mcmcByIteration(linelong, data=list(a=100), FUN=function(x) list(foo=x$a + x$alpha))
    expect_is(x, "list")
    expect_equal(length(x), 400)
    expect_equal(names(x)[1], "1.1")
    expect_equal(names(x)[400], "2.200")
    expect_true(all(sapply(x, function(y) all(names(y) == c("foo")))))
    expect_true(all(sapply(x, function(y) y$foo > 95)))
})

