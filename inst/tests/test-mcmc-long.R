context("Testing McmcLong class")

data(line, package="mcmc4")

samples <- melt(line)
metadata <- McmcParameterMeta(unique(as.character(samples$parameter)))
foo <- new("McmcLong", samples=samples[ , c("parameter", "chain", "iteration", "value")], parameters=metadata)

test_that("McmcLong works", {
    expect_is(foo, "McmcLong")
    expect_equivalent(foo@samples, samples)
})

test_that("error if bad colnames", {
    colnames(samples)[1] <- "foo"
    expect_error(new("McmcLong", samples=samples, parameters=metadata))
})

test_that("error if bad parameter names", {
    levels(samples$parameter) <- c("a", "b", "c")
    expect_error(new("McmcLong", samples, parameters=metadata))
})

test_that("error if column classes are incorrect", {
    expect_error(new("McmcLong",
                     samples=transform(samples, parameter=as.character(parameter)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, iter=as.numeric(iter + 0.5)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, chain=as.numeric(chain + 0.5)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, chain=as.character(value)),
                               parameters=metadata))
})

test_that("error if chain numbers doesn't match", {
    samples$chain[1] <- 5
    expect_error(new("McmcLong", samples=samples, parameters=metadata))
})

#############################

context("McmcLong-methods")

test_that("data=data.frame works", {
    expect_is(McmcLong(samples), "McmcLong")
})

test_that("data=data.frame works with parameter != NULL", {
    expect_is(McmcLong(samples, parameter_names=unique(as.character(samples$parameter))),
              "McmcLong")
})

test_that("data=data.frame fixes bad column types", {
    expect_is(McmcLong(transform(samples, parameter=as.character(parameter))), "McmcLong")
    expect_is(McmcLong(transform(samples, iteration=as.numeric(iteration))), "McmcLong")
    expect_is(McmcLong(transform(samples, iteration=as.character(iteration))), "McmcLong")
})

test_that("data=mcmc.list works", {
    expect_is(McmcLong(McmcList2(line)), "McmcLong")
})

#############################

context("McmcLong coercion methods")

test_that("coerce from=McmcLong to=McmcList2 works", {
    expect_is(as(foo, "McmcList2"), "McmcList2")
})

test_that("coerce from=McmcList2 to=McmcLong works", {
    expect_is(as(McmcList2(line), "McmcLong"), "McmcLong")
})

test_that("coerce from=McmcLong,to=data.frame works", {
    expect_equal(as(foo, "data.frame"), foo@samples)
})


