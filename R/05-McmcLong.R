.MCMC_LONG_COLUMNS <-
    c(parameter="factor",
      chain="integer",
      iteration="integer",
      value="numeric")

##' MCMC Samples in long-format
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
##' @rdname McmcLong-class
##' @aliases McmcLong-class
##' @docType class
##' @keywords classes
##' @export
setClass("McmcLong", contains="data.frame",
         representation(parameters="McmcParameterMeta"))

validate_mcmc_long <- function(object) {
    if (!all(colnames(object) == names(.MCMC_LONG_COLUMNS))) {
        return(sprintf("colnames must equal: %s",
                       paste(sQuote(names(.MCMC_LONG_COLUMNS)),
                             collapse=",")))
    }
    if (!all(sapply(object, class) == .MCMC_LONG_COLUMNS)) {
        return(sprintf("Classes of columns must equal %s",
                       deparse(unname(.MCMC_LONG_COLUMNS))))
    }
    ## Maybe consider loosening this
    ## Allow for parameters to exist in data but not in metadata?
    parameters <- as.character(unique(object[["parameter"]]))
    if (!setequal(names(object@parameters@parameters), parameters)) {
        return(sprintf("parameters in object@parameters do not match data"))
    }
    ## Chain values
    chains <- unique(object$chain)
    n_chain <- length(chains)
    if (!setequal(chains, seq(1, n_chain))) {
        return("Chains must be numbered 1:n")
    }
    TRUE
}

setValidity("McmcLong", validate_mcmc_long)

## Creation Methods
##' Create \code{McmcLong} objects
##'
##' @param data Object with MCMC samples
##' @param parameter_name \code{character} vector of parameter names which will
##' be parsed by \code{fun}
##' @param fun \code{function} used to parse \code{parameter_name}.
##' See \code{\link{parse_parameter_names_default}} for what this
##' function has to return.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(data="data.frame")}}{\code{data} should have columns
##' \code{c("parameter", "chain", "iteration", "value")}.}
##' }
##'
##' @docType methods
##' @keywords methods
##' @export
setGeneric("McmcLong",
           function(data, ...) {
               standardGeneric("McmcLong")
           })

mcmc_long_default <-
    function(data, 
             parameter_names=NULL,
             fun=parse_parameter_names_default)
{
    ## Put this before parparsed to change data before eval
    if (is.null(parameter_names)) {
        parameter_names <- unique(as.character(data$parameter))
    }

    for (i in seq_along(.MCMC_LONG_COLUMNS)) {
        variable <- names(.MCMC_LONG_COLUMNS)[i]
        class <- unname(.MCMC_LONG_COLUMNS)[i]
        data[[variable]] <- as(data[[variable]], class)
    }
    new("McmcLong", data[ , names(.MCMC_LONG_COLUMNS)],
        parameters=McmcParameterMeta(fun(parameter_names)))
}

setMethod("McmcLong", "data.frame", mcmc_long_default)

mcmc_long_mcmc_list2 <- function(data) {
    as(data, "McmcLong")
}

setMethod("McmcLong", "McmcList2", mcmc_long_mcmc_list2)


## Coercion

## McmcLong -> McmcList2
setAs("McmcLong", "McmcList2",
      function(from, to) {
          ret <- dlply(from, "chain",
                       function(x) mcmc(acast(x, iteration ~ parameter,
                                              value.var="value")))
          strip_plyr_attr(ret)
      })

## McmcList2 -> McmcLong
setAs("McmcList2", "McmcLong",
      function(from, to) {
          new("McmcLong", melt(from), parameters=from@parameters)
      })
