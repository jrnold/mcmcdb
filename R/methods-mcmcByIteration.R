##' Iterate over all Mcmc iterations
##'
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
##' @aliases mcmcByIteration,McmcLong-method
##' @aliases mcmcByIteration
##' @docType methods
##' @keywords methods
##' @export
setGeneric("mcmcByIteration",
           function(object, ...) {
               standardGeneric("mcmcByIteration")
           })

mcmc_by_iteration_mcmc_long <- function(object, data=list(), FUN=identity) {
    do_iteration <- function(x, parameters, innerfun, data, ...) {
        values <- structure(x$val, names=as.character(x$parname))
        innerfun(c(mcmcUnflatten(parameters, values), data))
    }
    ret <- dlply(object@samples, c("chain_id", "iter"),
                 .fun=do_iteration,
                 parameters=object@parameters,
                 data=data,
                 innerfun=FUN)
    strip_plyr_attr(ret)
}

setMethod("mcmcByIteration", "McmcLong", mcmc_by_iteration_mcmc_long)

