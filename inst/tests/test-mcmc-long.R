context("Testing McmcLong class")

data(line, package="mcmc4")

samples <- melt(line)
metadata <- McmcParameterMeta(unique(as.character(line_long$parameter)))
foo <- new("McmcLong", samples, parameters=metadata)

test_that("McmcLong works", {
    expect_is(foo, "McmcLong")
    expect_equivalent(foo@.Data, samples)
})

test_that("error if bad colnames", {
    colnames(samples)[1] <- "foo"
    expect_error(new("McmcLong", samples, parameters=metadata))
})

test_that("error if bad parameter names", {
    levels(samples$parameter) <- c("a", "b", "c")
    expect_error(new("McmcLong", samples, parameters=metadata))
})

test_that("error if chain doesn't match", {
    samples$chain[1] <- 5
    expect_error(new("McmcLong", samples, parameters=metadata))
})

test_that("coerce from McmcLong to McmcList2 works", {
    expect_is(as(foo, "McmcList2"), "McmcList2")
})

test_that("coerce from McmcList2 to McmcLong works", {
    expect_is(as(McmcList2(foo), "McmcLong"), "McmcLong")
})


