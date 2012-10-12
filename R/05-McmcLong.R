## Names and clases of columns in \code{McmcLong} class
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
##' While these objects enforce multiple chains, it doesn not require
##' that the chains be numbered \code{1:n}. This means it is harder to
##' iterate over chains, but easier to create distinct chain objects
##' to combine later.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{samples}}{\code{data.frame} with columns "paramter", "chain", "iteration", "value"}
##' \item{\code{parameters}}{\code{McmcParaterMeta} object with the array sizes of the paramters in the sample.}
##' \item{\code{chainids}}{\code{data.frame} with columns "chainid" and other data for each chain.}
##' \item{\code{par_chainids}}{\code{data.frame} with columns "parname", "chainid" and other data
##' for each parameter for each chain, e.g. step size multipliers for NUTS.}
##' \item{\code{chain_iters}{\code{data.frame} with columns "chainid", "iter" and other data for
##' each iteration of each chain (which are not parameters), e.g. treedepth, stepsize in NUTS.
##' \item{\code{metadata}{\code{list} with general data about the samples.
##' }
##'
##' @section Extends:
##'
##' \describe{
##' \item{\code{data.frame}}{directly}
##' }
##'
##' @name McmcLong-class
##' @rdname McmcLong-class
##' @aliases McmcLong-class
##' @docType class
##' @keywords classes
##' @export
setClass("McmcLong", 
         representation(samples="data.frame",
                        parameters="McmcParameterMeta",
                        chainids="data.frame", # chainid
                        par_chainids="data.frame", # parname, chainid
                        chain_iters="data.frame", # chainid, iter
                        metadata="list") 
                        

validate_mcmc_long <- function(object) {
    check_df <- validate_data_frame(object@samples, .MCMC_LONG_COLUMNS)
    if (is.character(check_df)) {
        return(check_df)
    }
    ## Maybe consider loosening this
    ## Allow for parameters to exist in data but not in parameters
    parameters <- as.character(unique(object@samples[["parameter"]]))
    if (!setequal(names(object@parameters@parameters), parameters)) {
        return(sprintf("parameters in object@parameters do not match data"))
    }
    ## ## Chain values
    ## chains <- unique(object@samples$chain)
    ## n_chain <- length(chains)
    ## if (!setequal(chains, seq(1, n_chain))) {
    ##     return("Chains must be numbered 1:n")
    ## }
    TRUE
}

setValidity("McmcLong", validate_mcmc_long)

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
##' @rdname McmcLong-methods
##' @name McmcLong-methods
##' @docType methods
##' @keywords methods
##' @aliases McmcLong
##' @aliases McmcLong,data.frame-method
##' @aliases McmcLong,McmcList2-method
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
    new("McmcLong",
        samples=data[ , names(.MCMC_LONG_COLUMNS)],
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
          to <- dlply(from@samples, "chain",
                       function(x) mcmc(acast(x, iteration ~ parameter,
                                              value.var="value")))
          new("McmcList2", mcmc.list(to),
              parameters=from@parameters)
      })

## McmcList2 -> McmcLong
setAs("McmcList2", "McmcLong",
      function(from, to) {
          new("McmcLong", samples=melt(from), parameters=from@parameters)
      })

## McmcLong -> data.frame
setAs("McmcLong", "data.frame",
      function(from, to) from@samples)

