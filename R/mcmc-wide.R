##' @exportClass McmcWide
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
##' @rdname McmcWide-class
##' @aliases McmcWide-class
setClass("McmcWide", contains="data.frame",
         representation(parameters="McmcParameterMeta"))

validate_mcmc_wide <- function(object) {
    if (!all(colnames(object)[1:2] == c("chain", "iteration"))) {
        return(sprintf("Columns 1:2 must equal: 'chain', 'iteration'"))
    }
    ## Validity of parameters
    parameters <- colnames(object)[3:ncol(object)]
    if (!setequal(names(object@parameters@parameters), parameters)) {
        return(sprintf("object@parameters and data do not match"))
    }
    ## validity of chain
    chains <- unique(object$chain)
    n_chain <- length(chains)
    if (!all(sort(chains) == seq_len(n_chain))) {
        return("Chains must be numbered 1:n")
    }
    TRUE
}

setValidity("McmcWide", validate_mcmc_wide)

