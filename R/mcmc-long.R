##' @exportClass McmcLong
NULL

##' New Mcmc Samples class (long-format)
##'
##' Mcmc samples stored as a table with columns: "parameter", "chain",
##' "iteration", "value". Right now the backend is a
##' \code{data.frame}, so it is pretty damn slow, but this can be
##' changed in the future. However, it seems the most natural for
##' storage in a database, with the ability to aggregate rows,
##' columns, etc. I'll probably write a version that uses a SQLite
##' backend.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{.Data}}{\code{data.frame} with columns "paramter", "chain", "iteration", "value"}
##' \item{\code{parameters}}{\code{McmcParaterMeta} object with the array sizes of the paramters in the sample.}
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{data.frame}}{directly}
##' }
##'
##' @rdname McmcWide-class
##' @aliases McmcWide-class
setClass("McmcLong", contains="data.frame",
         representation(parameters="McmcParameterMeta"))

validate_mcmc_long <- function(object) {
    valid_colnames <- c("parameter", "chain", "iteration", "value")
    msg <- c()
    if (!all(colnames(object) == valid_colnames)) {
        msg <- c(msg, sprintf("colnames must equal: %s",
                              paste(sQuote(valid_colnames)), collapse=","))
    } else {
        ## Maybe consider loosening this
        ## Allow for parameters to exist in data but not in metadata?
        parameters <- as.character(unique(object[["parameter"]]))
        if (!setequal(names(object@parameters@parameters), parameters)) {
            msg <- c(msg, sprintf("parameters in object@parameters do not match data"))
        }
        ## Chain values
        chains <- unique(object$chain)
        n_chain <- length(chains)
        if (!setequal(chains, seq(1, n_chain))) {
            msg <- c(msg, "Chains must be numbered 1:n")
        }
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}

setValidity("McmcLong", validate_mcmc_long)

#####

mcmc_by_iteration_mcmc_long <- function(object, data=list(), FUN=identity) {
    do_iteration <- function(x, metadata, innerfun, data, ...) {
        values <- structure(x$value, names=as.character(x$parameter))
        innerfun(c(mcmcUnflatten(metadata, value), data))
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
