##' Methods for function \code{c} for MCMC objects
##'
##' These methods concatenate multiple objects of classes
##' \code{McmcList2} and \code{McmcLong} into an object of the same
##' class. This is generally used to combine results from several
##' chains.
##'
##' @name c-methods
##' @rdname c-methods
##' @param x Object to be concatenated
##' @param ... Other objects to be concatenated
##'
##' @docType methods
##' @keywords methods
##' @aliases c-methods
##' @aliases c,McmcList2-method
##' @aliases c,McmcLong-method
##' @exportMethod c
NULL

c_mcmc_list <- function(x, ...) {
    new("McmcList2",
        .Primitive("c")(x@.Data, ...),
        parameters = x@parameters)
}

setMethod("c", c(x="McmcList2"), c_mcmc_list)

c_mcmc_long <- function(x, ...) {
    binder <- function(objects, name) {
        do.call(base::rbind.data.frame,
                lapply(objects, slot, name=name))
    }
    objects <- .Primitive("c")(list(x), list(...))
    new("McmcLong",
        samples=binder(objects, "samples"),
        chains=binder(objects, "chains"),
        par_chains=binder(objects, "par_chains"),
        chain_iters=binder(objects, "chain_iters"),
        metadata=do.call(c, lapply(objects, slot, "metadata")))
}

setMethod("c", c(x="McmcLong"), c_mcmc_long)

