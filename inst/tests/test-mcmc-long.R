context("Testing McmcLong class")

## .colnames <- c("parameter", "chain", "iteration", "value")
## samples1 <- replicate(2, matrix(rnorm(20), ncol=2,
##                                   dimnames=list(NULL, c("alpha", "beta"))),
##                              simplify=FALSE)))[ , .colnames]
## samples <-
##     melt(mcmc.list(
## metadata <- McmcParameterMeta(unique(as.character(samples$parameter)))

## test_that("McmcLong works", {
##     foo <- new("McmcLong", samples, parameters=metadata)
##     expect_is(foo, "McmcLong")
##     expect_equal(dim(foo), c(40, 4))
## })

## test_that("error if bad colnames", {
##     colnames(samples)[1] <- "foo"
##     expect_error(new("McmcLong", samples, parameters=metadata))
## })
## test_that("error if bad colnames", {
##     levels(samples$parameter) <- c("a", "b")
##     expect_error(new("McmcLong", samples, parameters=metadata))
## })
## test_that("error if bad parameters", {
##     samples$chain[1] <- 5
##     expect_error(new("McmcLong", samples, parameters=metadata))
## })

## test_that("mcmcByIteration works", {
##     foo <- new("McmcLong", samples, parameters=metadata)
##     expect_is(datalist, "list")
##     expect_equal(names(datalist[[1]]), c("alpha", "beta"))
##     expect_equal(lapply(datalist[[1]], dim), list(alpha=NULL, beta=c(2, 2)))

##     mcmcByIteration(foo)

## })



