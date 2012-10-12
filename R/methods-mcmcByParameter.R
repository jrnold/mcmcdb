##' Apply function to the MCMC samples of each parameter
##'
##' @param object MCMC object
##' @param .fun Function to apply to the vector of parameter values
##' from all iterations.
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

mcmc_by_parameter <- function(object, .fun=identity, ...) {
    f <- function(x) .fun(x$val)
    strip_plyr_attr(dlply(object@samples, "parname", .fun=f, ...))
}

setMethod("mcmcByParameter", "McmcLong", mcmc_by_parameter)


