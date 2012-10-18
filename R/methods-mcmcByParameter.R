##' Apply function to the MCMC samples of each parameter
##'
##' @param object MCMC object
##' @param .fun Function to apply to the vector of parameter values
##' from all iterations.
##' @param .extract \code{list} passed to \code{[} to subset the
##' dataset first.
##' @return \code{list} of length equal to the number of parameter.
##' Each element contains the results of \code{.fun} for that
##' parameter.
##'
##' @rdname mcmcByParameter-method
##' @name mcmcByParameter-method
##' @docType methods
##' @keywords methods
##' @aliases mcmcByParameter,McmcLong-method
##' @export
setGeneric("mcmcByParameter",
           function(object, ...) {
               standardGeneric("mcmcByParameter")
           })

mcmc_by_parameter <- function(object, .fun=identity, .extract=list(), ...) {
    f <- function(x) .fun(x$val)
    if (length(subset)) {
        subobj <- do.call(`[`, c(list(x=object), as.list(.extract)))
        ret <- dlply(subobj, "parname", .fun=f, ...)
    } else {
        ret <- dlply(object@samples, "parname", .fun=f, ...)
    }
    strip_plyr_attr(ret)
}

setMethod("mcmcByParameter", "McmcLong", mcmc_by_parameter)


