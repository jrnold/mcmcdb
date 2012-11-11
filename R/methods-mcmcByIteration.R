##' Iterate over all Mcmc iterations
##'
##' @param data \code{list} Data to combine with parameters on
##' each iteration.
##' @param .fun \code{function} Function to apply to each iteration.
##' This should take one argument,  a \code{list} that includes the
##' parameters in array form and the values from \code{data}.
##' @param ... Options passed to \code{.fun} and \code{\link{dlply}}.
##'
##' @name mcmcByIteration-methods
##' @rdname mcmcByIteration-methods
##' @aliases mcmcByIteration,McmcLong-method
##' @aliases mcmcByIteration
##' @docType methods
##' @keywords methods
##' @export
setGeneric("mcmcByIteration",
           function(object, ...) {
               standardGeneric("mcmcByIteration")
           })

mcmc_by_iteration_mcmc_long <- function(object, .fun=identity, data=list(),  ...) {
    do_iteration <- function(x, skeleton, indices, innerfun, data, ...) {
        values <- structure(x$val, names=as.character(x$parname))
        innerfun(c(mcmc_relist(skeleton, indices, values), data))
    }
    skeleton <- mcmcSkeleton(object)
    indices <- mcmcIndices(object)
    ret <- dlply(object@samples, c("chain_id", "iter"),
                 .fun=do_iteration,
                 skeleton=skeleton,
                 indices=indices,
                 data=data,
                 innerfun=.fun, ...)
    strip_plyr_attr(ret)
}

setMethod("mcmcByIteration", "McmcLong", mcmc_by_iteration_mcmc_long)

