context("Testing McmcLong class")
library(plyr)

data(line, package="mcmc4")

samples <- melt(line)

parnames <- new("McmcParnames",
                mcmc_parse_parname_default(unique(samples$parname)))
pararrays <- new("McmcPararrays",
                 mcmc4:::parnames_to_pararrays(parnames))

chains <- ddply(samples, "chain_id", summarise,
                niter=max(iter),
                thin=1L, start=1L, end=max(iter))
foo <- new("McmcLong",
           samples=new("McmcSamples", samples),
           chains=new("McmcChains", chains),
           pararrays=pararrays,
           parnames=parnames)

test_that("McmcLong works", {
    expect_is(foo, "McmcLong")
    expect_equivalent(foo@samples, samples)
    expect_equivalent(foo@chains, chains)
})

test_that("error if bad colnames", {
    colnames(samples)[1] <- "foo"
    expect_error(new("McmcLong", samples=samples, chains=chains,
                     pararrays=pararrays, parnames=parnames))
})

test_that("error if bad chains", {
    samples[1 , "chain_id"] <- 5
    expect_error(new("McmcLong", samples=samples, chains=chains,
                     pararrays=pararrays, parnames=parnames))
})

test_that("error if bad parameter names", {
    levels(samples$parname) <- c("a", "b", "c")
    expect_error(new("McmcLong", samples,
                     pararrays=pararrays, parnames=parnames))
})

test_that("error if column classes are incorrect", {
    expect_error(new("McmcLong",
                     samples=transform(samples, parname=as.character(parameter)),
                     pararrays=pararrays, parnames=parnames))
    expect_error(new("McmcLong",
                     samples=transform(samples, iter=as.numeric(iter + 0.5)),
                     pararrays=pararrays, parnames=parnames))
    expect_error(new("McmcLong",
                     samples=transform(samples, chain_id=as.numeric(chain + 0.5)),
                     pararrays=pararrays, parnames=parnames))
    expect_error(new("McmcLong",
                     samples=transform(samples, chain_id=as.character(value)),
                     pararrays=pararrays, parnames=parnames))

})

#############################

context("McmcLong-methods")

test_that("data=data.frame works without arguments", {
    expect_is(McmcLong(samples), "McmcLong")
})

test_that("data=data.frame works with parameters != NULL", {
    parnames <- mcmc_parse_parname_default(unique(as.character(samples$parname)))
    expect_is(McmcLong(samples, parnames=parnames), "McmcLong")
})

test_that("data=data.frame works with parameter != NULL", {
    expect_is(McmcLong(samples, chains=chains), "McmcLong")
})

test_that("data=data.frame chokes on bad column types", {
    expect_error(McmcLong(transform(samples, iter=as.numeric(iter))))
    expect_error(McmcLong(transform(samples, iter=as.numeric(iter))))
    expect_error(McmcLong(transform(samples, iter=as.character(iter))))
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
