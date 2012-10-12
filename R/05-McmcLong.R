## Utility classes
setClassUnion("DataFrameOrNull", c("data.frame", "NULL"))

# -----------------------
##' @export
setClass("McmcChains", "data.frame")

setValidity("McmcChains",
            function(object) {
                columns <- c(chainid="integer",
                             niter="integer",
                             thin="integer",
                             start="integer",
                             end="integer")
                validate_data_frame(object, columns, exclusive=FALSE)
            })

# -----------------------
##' @export
setClass("McmcParChains", "data.frame")
setValidity("McmcParChains",
            function(object) {
                columns <- c(parname="factor",
                             chainid="integer")
                validate_data_frame(object, columns, exclusive=FALSE)
            })


setClassUnion("McmcParChainsOrNull", c("McmcParChains", "NULL"))

# -----------------------
##' @export
setClass("McmcChainIters", "data.frame")
setValidity("McmcChainIters",
            function(object) {
                columns <- c(chainid="integer",
                             iter="integer")
                validate_data_frame(object, columns, exclusive=FALSE)
            })

setClassUnion("McmcChainItersOrNull", c("McmcChainIters", "NULL"))

# -----------------------

## Names and clases of columns in \code{McmcLong} class
.MCMC_LONG_COLUMNS <-
    c(parname="factor",
      chainid="integer",
      iter="integer",
      val="numeric")

##' MCMC Samples in long-format
##'
##' Mcmc samples stored as a table with columns: "parname", "chainid",
##' "iter", "val". Right now the backend is a
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
##' The column names are slightly non-intuitive, but were chosen to
##' avoid clashes with
##' \href{http://www.postgresql.org/docs/9.2/static/sql-keywords-appendix.html}{SQL
##' reserved keywords}, in order to facilitate better integration with
##' SQL db backends.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{samples}}{\code{data.frame} with columns "parname", "chainid", "iter", "val"}
##' \item{\code{parameters}}{\code{McmcParaterMeta} object with the array sizes of the paramters in the sample.}
##' \item{\code{chains}}{\code{data.frame} with columns "chainid" and other data for each chain.}
##' \item{\code{par_chains}}{\code{data.frame} with columns "parname", "chainid" and other data
##' for each parameter for each chain, e.g. step size multipliers for NUTS.}
##' \item{\code{chain_iters}}{\code{data.frame} with columns "chainid", "iter" and other data for
##' each iteration of each chain (which are not parameters), e.g. treedepth, stepsize in NUTS.}
##' \item{\code{metadata}}{\code{list} with general data about the samples.}
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
                        parameters="McmcParameters",
                        chains="McmcChains", # chainid
                        par_chains="McmcParChainsOrNull", # parname, chainid
                        chain_iters="McmcChainItersOrNull", # chainid, iter
                        metadata="list"))

validate_mcmc_long <- function(object) {
    check_df <- validate_data_frame(object@samples, .MCMC_LONG_COLUMNS)
    if (is.character(check_df)) {
        return(check_df)
    }
    parameters <- as.character(unique(object@samples[["parname"]]))
    if (!setequal(names(object@parameters@parameters), parameters)) {
        return(sprintf("parnames in object@parameters do not match samples"))
    }
    ## All chainids in object@samples need to be in object@chains
    uniq_chainids <- unique(object@samples$chainid)
    bad_chainids <- uniq_chainids[! uniq_chainids %in% object@chains$chainid]
    if (length(bad_chainids)) {
        return(sprintf("Invalid values of object@samples$chainid: %s",
                       paste(sQuote(bad_chainids), collapse=", ")))
    }
    ## TODO: test iterations are valid?
    TRUE
}

setValidity("McmcLong", validate_mcmc_long)

##' Create \code{McmcLong} objects
##'
##' @param data Object with MCMC samples
##' @param parnames \code{character} vector of parameter names which will
##' be parsed by \code{fun}
##' @param fun \code{function} used to parse \code{parnames}.
##' See \code{\link{mcmc_parse_parname_default}} for what this
##' function has to return.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(data="data.frame")}}{\code{data} should have columns
##' \code{c("parname", "chainid", "iter", "val")}.}
##' }
##'
##' @rdname McmcLong-methods
##' @name McmcLong-methods
##' @docType methods
##' @keywords methods
##' @aliases McmcLong
##' @aliases McmcLong,data.frame-method
##' @export
setGeneric("McmcLong",
           function(data, ...) {
               standardGeneric("McmcLong")
           })

mcmc_long_default <-
    function(data,
             parnames=NULL,
             fun=mcmc_parse_parname_default,
             chains=NULL,
             par_chains=NULL,
             chain_iters=NULL,
             metadata=list())
{
    ## Put this before parparsed to change data before eval
    if (is.null(parnames)) {
        parnames <- unique(as.character(data$parname))
    }
    ## Coerce columns in samples to the correct type
    for (i in seq_along(.MCMC_LONG_COLUMNS)) {
        variable <- names(.MCMC_LONG_COLUMNS)[i]
        class <- unname(.MCMC_LONG_COLUMNS)[i]
        data[[variable]] <- as(data[[variable]], class)
    }
    ## Create chains table if none given
    if (is.null(chains)) {
        chains <- ddply(data, "chainid",
                        function(x) {
                            data.frame(niter=nrow(x),
                                       thin=1L,
                                       start=1L,
                                       end=nrow(x))
                        })
    }
    ## Put these in classes to check for presence of columns
    if (!is.null(par_chains)) {
        par_chains <- new("McmcParChains", par_chains)
    }
    if (!is.null(chain_iters)) {
        chain_iters <- new("McmcChainIters", chain_iters)
    }

    new("McmcLong",
        samples=data[ , names(.MCMC_LONG_COLUMNS)],
        parameters=McmcParameters(fun(parnames)),
        chains=new("McmcChains", chains),
        chain_iters=chain_iters,
        par_chains=par_chains,
        metadata=as.list(metadata))
}

setMethod("McmcLong", "data.frame", mcmc_long_default)

setMethod("McmcLong", "mcmc.list",
          function(data, ...) {
              callGeneric(melt(data), ...)
          })

setMethod("McmcLong", "mcmc",
          function(data, ...) {
              callGeneric(melt(mcmc.list(data)), ...)
          })

## Coercion

## McmcLong -> McmcList2
setAs("McmcLong", "mcmc.list",
      function(from, to) {
          to <- dlply(from@samples, "chainid",
                       function(x) mcmc(acast(x, iter ~ parname,
                                              value.var="val")))
          mcmc.list(to)
      })

## McmcList2 -> McmcLong
setAs("mcmc.list", "McmcLong",
      function(from, to) {
          McmcLong(from)
      })

## McmcLong -> data.frame
setAs("McmcLong", "data.frame",
      function(from, to) from@samples)

