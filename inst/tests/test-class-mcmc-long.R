context("Testing McmcLong class")
library(plyr)

data(line, package="mcmc4")

samples <- melt(line)
parameters <- McmcParameters(unique(as.character(samples$parname)))
chains <- new("McmcChains",
              ddply(samples, "chainid", summarise,
                    niter=length(iter),
                    thin=1L, start=1L, end=length(iter)))
foo <- new("McmcLong", samples=samples, chains=chains,
           parameters=parameters)

test_that("McmcLong works", {
    expect_is(foo, "McmcLong")
    expect_equivalent(foo@samples, samples)
    expect_equivalent(foo@chains, chains)
})

test_that("error if bad colnames", {
    colnames(samples)[1] <- "foo"
    expect_error(new("McmcLong", samples=samples, parameters=metadata))
})

test_that("error if bad chains", {
    samples[1 , "chainid"] <- 5
    expect_error(new("McmcLong", samples=samples, chains=chains, parameters=metadata))
})

test_that("error if bad parameter names", {
    levels(samples$parname) <- c("a", "b", "c")
    expect_error(new("McmcLong", samples, parameters=metadata))
})

test_that("error if column classes are incorrect", {
    expect_error(new("McmcLong",
                     samples=transform(samples, parname=as.character(parameter)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, iter=as.numeric(iter + 0.5)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, chainid=as.numeric(chain + 0.5)),
                     parameters=metadata))
    expect_error(new("McmcLong",
                     samples=transform(samples, chainid=as.character(value)),
                               parameters=metadata))
})

#############################

context("McmcLong-methods")

test_that("data=data.frame works without arguments", {
    expect_is(McmcLong(samples), "McmcLong")
})

test_that("data=data.frame works with parameter != NULL", {
    expect_is(McmcLong(samples, parnames=unique(as.character(samples$parname))),
              "McmcLong")
})

test_that("data=data.frame works with parameter != NULL", {
    expect_is(McmcLong(samples, chains=chains), "McmcLong")
})

test_that("data=data.frame fixes bad column types", {
    expect_is(McmcLong(transform(samples, parname=as.character(parname))), "McmcLong")
    expect_is(McmcLong(transform(samples, iter=as.numeric(iter))), "McmcLong")
    expect_is(McmcLong(transform(samples, iter=as.character(iter))), "McmcLong")
})

test_that("data=mcmc.list works", {
    expect_is(McmcLong(line), "McmcLong")
})

test_that("data=mcmc works", {
    expect_is(McmcLong(line[[1]]), "McmcLong")
})

#############################

context("McmcLong coercion methods")

test_that("coerce from=McmcLong to=mcmc.list works", {
    expect_is(as(foo, "mcmc.list"), "mcmc.list")
})

test_that("coerce from=McmcList2 to=mcmc.list works", {
    expect_is(as(line, "McmcLong"), "McmcLong")
})

test_that("coerce from=McmcLong,to=data.frame works", {
    expect_equal(as(foo, "data.frame"), foo@samples)
})


