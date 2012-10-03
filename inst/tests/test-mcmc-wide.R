context("Testing McmcWide class")
library(reshape2)

samples <-
    mcmc.list(replicate(2, mcmc(matrix(rnorm(20), ncol=2,
                                       dimnames=list(NULL, c("alpha", "beta")))),
                        simplify=FALSE))
samples <-
    dcast(melt(samples), chain + iteration ~ parameter, value.var="value")
metadata <- McmcParameterMeta(unique(as.character(colnames(samples)[3:4])))

test_that("McmcWide works okay with good data", {
    foo <- new("McmcWide", samples, parameters=metadata)
    expect_is(foo, "McmcWide")
    expect_equal(dim(foo), c(20, 4))
})

test_that("McmcWide throws error with bad columns", {
    colnames(samples)[1] <- "foo"
    expect_error(new("McmcWide", samples, parameters=metadata))
})

test_that("McmcWide throws error with parameters", {
    colnames(samples)[3] <- "foo"
    expect_error(new("McmcWide", samples, parameters=metadata))
})

test_that("McmcWide throws error with bad chain values", {
    samples$chains[1] <- 5
    expect_error(new("McmcWide", samples, parameters=metadata))
})

######


