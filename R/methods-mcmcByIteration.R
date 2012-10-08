##' Iterate through mcmc iterations
##'
##' @param object \code{McmcList2} object.
##' @param data \code{function} Data to combine with parameters on
##' each iteration.
##' @param FUN \code{function} Function to apply to each iteration.
##' @param ... Pass to \code{FUN}.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(object="mcmcList2")}}{Method for class \code{mcmcList}.}
##' \item{\code{signature(object="mcmcLong"}}{Method for class \code{mcmcLong}.}
##' \item{\code{signature(object="mcmcWide"}}{Method for class \code{mcmcWide}.}
##' }
##'
##' @name mcmcByIteration-methods
##' @rdname mcmcByIteration-methods
NULL

##' @export
setGeneric("mcmcByIteration",
           function(object, ...) {
               standardGeneric("mcmcByIteration")
           })
setMethod("mcmcByIteration", "McmcList2", mcmc_by_iteration_mcmc_list2)

mcmc_by_iteration_mcmc_long <- function(object, data=list(), FUN=identity) {
    do_iteration <- function(x, metadata, innerfun, data, ...) {
        values <- structure(x$value, names=as.character(x$parameter))
        innerfun(c(mcmcUnflatten(metadata, values), data))
    }
    listnames <- paste(object$chain, object$iteration, sep=".")
    ret <- dlply(object, c("chain", "iteration"),
                 .fun=do_iteration,
                 metadata=object@parameters,
                 data=data,
                 innerfun=FUN)
    names(ret) <- listnames
    strip_plyr_attr(ret)
}

setMethod("mcmcByIteration", "McmcLong", mcmc_by_iteration_mcmc_long)

mcmc_by_iteration_mcmc_wide <- function(object, data=list(), FUN=identity) {
    do_iteration <- function(x, metadata, innerfun, data, ...) {
        innerfun(c(mcmcUnflatten(metadata, x), data))
    }
    listnames <- paste(object$chain, object$iteration, sep=".")
    ret <- dlply(object, c("chain", "iteration"),
                 .fun=do_iteration,
                 metadata=object@parameters,
                 data=data,
                 innerfun=FUN)
    names(ret) <- listnames
    strip_plyr_attr(ret)
}

setMethod("mcmcByIteration", "McmcWide", mcmc_by_iteration_mcmc_wide)
