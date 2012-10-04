##' @exportClass McmcList2
##' @export McmcList2
NULL

##' Improved mcmc.list class
##'
##' This class extends the standard \code{\link[coda]{mcmc.list}}
##' class with metadata about the shape of the parameters that can be
##' used to convert them back to their original shapes to make
##' postprocessing easier.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{indices}}{List of matrices which map each flat parameter name to its parameter array.}
##' \item{\code{template}}{List of parameter arrays in the same form as they were in the estimation.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{mcmc.list}}{directly}
##' }
##'
##' mcmc class with parameter metadata
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{parameters}}{\code{McmcParameterMeta} object}
##' }
##'
##' @rdname McmcList2-class
##' @aliases McmcList2-class
##' @export
setClass("McmcList2", contains="mcmc.list",
         representation(parameters="McmcParameterMeta"))

setGeneric("McmcList2", function(data, ...) standardGeneric("McmcList2"))

mcmc2_default <- function(data, ...,
                          parameter_names=colnames(data[[1]]),
                          fun=parse_parameter_names_default) {
    ## Put this before parparsed to change data before eval
    new("McmcList2", mcmc.list(data),
        parameters=McmcParameterMeta(fun(parameter_names)))
}

setMethod("McmcList2", "mcmc.list", mcmc2_default)

setMethod("McmcList2", "matrix",
          function(data, ...) callGeneric(mcmc.list(data), ...))

mcmc_by_iteration_mcmc_list2 <- function(object, data=list(), FUN=identity, ...) {
    do_iteration <- function(x, metadata, innerfun, data, ...) {
        innerfun(c(mcmcUnflatten(metadata, x), data))
    }
    do_chain <- function(x, indices, template, innerfun, data) {
        alply(x, 1, .fun=do_iteration,
              metadata=metadata,
              data=data, innerfun=innerfun)
    }
    ## element names = be chain.iteration
    n_chains <- length(object)
    n_iter <- sapply(object, nrow)
    listnames <-
        unlist(mapply(function(x, y) paste(x, seq_len(y), sep="."),
                      seq_len(n_chains), n_iter, SIMPLIFY=FALSE))
    ret <- do.call(c, llply(object, do_chain,
                            metadata=object@parameters,
                            innerfun=FUN, data=data))
    names(ret) <- listnames
    ret
}


##' Iterate through mcmc iterations
##'
##' @param object \code{McmcList2} object.
##' @param data \code{function} Data to combine with parameters on
##' each iteration.
##' @param FUN \code{function} Function to apply to each iteration.
##' @param ... Pass to \code{FUN}.
##'
setMethod("mcmcByIteration", "McmcList2", mcmc_by_iteration_mcmc_list2)


