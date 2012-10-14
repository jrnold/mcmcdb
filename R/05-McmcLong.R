## Names and clases of columns in \code{McmcLong} class
##' @exportClass McmcSamples
NULL
subclass_data_frame_plus("McmcSamples",
                         columns=c(parname="factor",
                         chain_id="integer",
                         iter="integer",
                         val="numeric"),
                         keys=c("parname", "chain_id", "iter"))

# -----------------------
##' @exportClass McmcChains
NULL
subclass_data_frame_plus("McmcChains",
                         columns = c(chain_id="integer",
                         niter="integer",
                         thin="integer",
                         start="integer",
                         end="integer"),
                         keys=c("chain_id"))


##' @exportClass McmcParChains
NULL
subclass_data_frame_plus("McmcParChains",
                         columns = c(parname="factor",
                         chain_id="integer"),
                         keys=c("parname", "chain_id"))

##' @exportClass McmcParChainsOrNull
NULL
setClassUnion("McmcParChainsOrNull", c("McmcParChains", "NULL"))

# -----------------------
##' @exportClass McmcChainIters
NULL
subclass_data_frame_plus("McmcChainIters",
                         columns = c(chain_id="integer",
                         iter="integer"),
                         keys=c("chain_id", "iter"))

##' @exportClass McmcChainItersOrNull
NULL
setClassUnion("McmcChainItersOrNull", c("McmcChainIters", "NULL"))

# -----------------------

##' MCMC Samples in long-format
##'
##' Mcmc samples stored as a table with columns: "parname", "chain_id",
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
##' \item{\code{samples}}{\code{data.frame} with columns "parname", "chain_id", "iter", "val"}
##' \item{\code{parameters}}{\code{McmcParaterMeta} object with the array sizes of the paramters in the sample.}
##' \item{\code{chains}}{\code{data.frame} with columns "chain_id", "niter",
##' "start", "end", and "thin" and other data for each chain.}
##' \item{\code{par_chains}}{\code{data.frame} with columns "parname", "chain_id" and other data
##' for each parameter for each chain, e.g. step size multipliers for NUTS.}
##' \item{\code{chain_iters}}{\code{data.frame} with columns "chain_id", "iter" and other data for
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
         representation(samples="McmcSamples",
                        parameters="McmcParameters",
                        chains="McmcChains", # chain_id
                        par_chains="McmcParChainsOrNull", # parname, chain_id
                        chain_iters="McmcChainItersOrNull", # chain_id, iter
                        metadata="list"))

validate_mcmc_long <- function(object) {
    ## Parameters
    parameters <- as.character(unique(object@samples[["parname"]]))
    if (!setequal(names(object@parameters@parameters), parameters)) {
        return(sprintf("parnames in object@parameters do not match samples"))
    }
    ## All chain_ids in object@samples need to be in object@chains
    uniq_chainids <- unique(object@samples$chain_id)
    bad_chainids <- uniq_chainids[! uniq_chainids %in% object@chains$chain_id]
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
##' \code{c("parname", "chain_id", "iter", "val")}.}
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
    ## Samples
    data <- new("McmcSamples", data)
    ## Create chains table if none given
    if (is.null(chains)) {
        chains <- ddply(data, "chain_id",
                        function(x) {
                            maxiter <- max(x$iter)
                            data.frame(niter=maxiter,
                                       thin=1L,
                                       start=1L,
                                       end=maxiter)
                        })
    }
    chains <- new("McmcChains", chains)
    if (!is.null(par_chains)) {
        par_chains <- new("McmcParChains", par_chains)
    }
    if (!is.null(chain_iters)) {
        chain_iters <- new("McmcChainIters", chain_iters)
    }
    new("McmcLong",
        samples=data,
        parameters=McmcParameters(fun(parnames)),
        chains=chains,
        chain_iters=chain_iters,
        par_chains=par_chains,
        metadata=as.list(metadata))
}

setMethod("McmcLong", "data.frame", mcmc_long_default)

setMethod("McmcLong", "mcmc.list",
          function(data, ...) {
              chains <- ldply(data, function(x) attr(x, "mcpar"))
              names(chains) <- c("start", "end", "thin")
              chains$chain_id <- seq_len(nrow(chains))
              chains <- transform(chains, niter = (end - start + 1) / thin)
              chains <- chains[ , c("chain_id", "niter", "start", "end", "thin")]
              for (i in c("niter", "start", "end", "thin")) {
                  chains[[i]] <- as.integer(chains[[i]])
              }
              callGeneric(melt(data), chains=chains, ...)
          })

setMethod("McmcLong", "mcmc",
          function(data, ...) {
              callGeneric(melt(mcmc.list(data)), ...)
          })

## Coercion

## McmcLong -> McmcList2
setAs("McmcLong", "mcmc.list",
      function(from, to) {
          mcpars <- dlply(from@chains, "chain_id",
                          function(x) as.integer(x[1 , c("start", "end", "thin")]))
          to <- dlply(from@samples, "chain_id",
                       function(x) acast(x, iter ~ parname,
                                         value.var="val"))
          to <- mapply(function(x, y) mcmc(x, start=y[1], end=y[2], thin=y[3]),
                       to, mcpars)
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

