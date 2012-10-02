## Data
process_parsed_parameters <- mcmc4:::process_parsed_parameters

context("McmcList2 objects ...")

COLUMNS <- list(alpha="alpha", beta=c("beta.1.1", "beta.1.2", "beta.2.1", "beta.2.2"))
COLUMN_STR <- unlist(COLUMNS)
PARAMETERS <- structure(c("alpha", rep("beta", 4)), names=unname(COLUMN_STR))
TEMPLATE <- list(alpha=0, beta=matrix(0, 2, 2))
INDICES <- list(alpha=matrix(1, 1, 1, dimnames=list("alpha", NULL)),
                   beta=matrix(c(rep(1:2, each=2), rep(1:2, 2)),
                   ncol=2, dimnames=list(COLUMNS$beta, NULL)))

data <- mcmc.list(replicate(2, mcmc(matrix(rnorm(5 * 10), ncol=5,
                                           dimnames=list(1:10, COLUMN_STR))),
                            simplify=FALSE))

test_that("McmcList2, signature=mcmc.list produces correct mcmc2 objects", {
    newdata <- McmcList2(data, fun=parse_parameter_names_stan)
    expect_is(newdata, "McmcList2")
    expect_equivalent(newdata@.Data, data)
    expect_equivalent(newdata@template, TEMPLATE)
    expect_equal(newdata@indices, INDICES)
    expect_equal(newdata@parameters, PARAMETERS)
})

test_that("McmcList2, signature=mcmc produces mcmc2 objects", {
    newdata <- McmcList2(data[[1]], fun=parse_parameter_names_stan)
    expect_is(newdata, "McmcList2")
})

test_that("mcmc_to_iterations works", {
    newdata <- McmcList2(data, fun=parse_parameter_names_stan)
    datalist <- mcmc_to_iterations(newdata)
    expect_is(datalist, "list")
    expect_equal(names(datalist[[1]]), c("alpha", "beta"))
    expect_equal(lapply(datalist[[1]], dim), list(alpha=NULL, beta=c(2, 2)))
})

test_that("mcmc_to_iterations with a function works", {
    newdata <- McmcList2(data, fun=parse_parameter_names_stan)
    datalist <- mcmc_to_iterations(newdata, FUN=function(x) with(x, sum(beta)))
    expect_is(datalist, "list")
    expect_true(all(sapply(datalist, is, class2="numeric")))
    expect_true(all(sapply(datalist, length) == 1L))
})

test_that("mcmc_to_iterations with a function and data works", {
    newdata <- McmcList2(data, fun=parse_parameter_names_stan)
    datalist <- mcmc_to_iterations(newdata, data=list(y=1), FUN=function(x) with(x, alpha + y))
    expect_is(datalist, "list")
    expect_true(all(sapply(datalist, is, class2="numeric")))
    expect_true(all(sapply(datalist, length) == 1L))
})



