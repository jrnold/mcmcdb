#' @include parameter-parsers.R
NULL 

#' Internal classes
#'
#' Various classes used internally in \pkg{mcmc4}.
#' These are all subclasses of \code{DataFramePlus}
#' with specific constraints.
#'
#' @name internal-classes
#' @rdname internal-classes
#' @aliases McmcSamples-class
#' @aliases McmcChains-class
#' @aliases McmcParChains-class
#' @aliases McmcParChainsOrNull-class
#' @aliases McmcChainIters-class
#' @aliases McmcChainItersOrNull-class
#' @keywords classes internal
#' @docType class
NULL

#' @exportClass McmcSamples
NULL
McmcSamples <- 
  constrained_data_frame("McmcSamples",
                         columns=c(parname="factor",
                           chain_id="integer",
                           iter="integer",
                           val="numeric"))

# -----------------------
#' @exportClass McmcChains
NULL
McmcChains <- 
  constrained_data_frame("McmcChains",
                         columns = c(chain_id="integer",
                           niter="integer",
                           thin="integer",
                           start="integer",
                           end="integer"))

# -----------------------
#' @exportClass McmcChainIters
NULL
McmcChainIters <- 
  constrained_data_frame("McmcChainIters",
                         columns = c(chain_id="integer",
                           iter="integer"))

# ----------------------

#' @exportClass McmcParChains
NULL
McmcParChains <- 
  constrained_data_frame("McmcParChains",
                         columns = c(parname="factor",
                           chain_id="integer"))

#' @exportClass McmcParChainsOrNull
NULL
setClassUnion("McmcParChainsOrNull", c("McmcParChains", "NULL"))


# -----------------------

#' MCMC Samples in long-format
#'
#' Mcmc samples stored as a table with columns: "parname", "chain_id",
#' "iter", "val". Right now the backend is a
#' \code{data.frame}, so it is pretty damn slow, but this can be
#' changed in the future. However, it seems the most natural for
#' storage in a database, with the ability to aggregate rows,
#' columns, etc. I'll probably write a version that uses a SQLite
#' backend.
#'
#' While these objects enforce multiple chains, it doesn not require
#' that the chains be numbered \code{1:n}. This means it is harder to
#' iterate over chains, but easier to create distinct chain objects
#' to combine later.
#'
#' The column names are slightly non-intuitive, but were chosen to
#' avoid clashes with
#' \href{http://www.postgresql.org/docs/9.2/static/sql-keywords-appendix.html}{SQL
#' reserved keywords}, in order to facilitate better integration with
#' SQL db backends.
#'
#' @section Slots:
#'
#' \describe{
#' \item{\code{samples}}{\code{McmcSamples}.}
#' \item{\code{parameters}}{\code{McmcParameters}.}
#' \item{\code{chains}}{\code{McmcChains}.}
#' \item{\code{par_chains}}{\code{McmcParChains}.}
#' \item{\code{chain_iters}}{\code{McmcChainIters}.}
#' \item{\code{metadata}}{\code{list} with general data about the samples.}
#' \item{\code{version}}{\code{character} version of \pkg{mcmcdb} with which the object was created}
#' }
#'
#' @name McmcLong-class
#' @rdname McmcLong-class
#' @aliases McmcLong-class
#' @docType class
#' @keywords classes
#' @export
setClass("McmcLong",
         representation(samples="McmcSamples",
                        parameters="McmcParameters",
                        chains="McmcChains", # chain_id
                        chain_iters="McmcChainIters", # chain_id, iter
                        par_chains="McmcParChainsOrNull", # parname, chain_id
                        metadata="list",
                        version="character"))

## #' Create \code{McmcLong} objects
## #'
## #' @param data Object with MCMC samples
## #' @param parnames \code{character} vector of parameter names which will
## #' be parsed by \code{fun}
## #' @param fun \code{function} used to parse \code{parnames}.
## #' See \code{\link{mcmc_parse_parname_default}} for what this
## #' function has to return.
## #'
## #' @section Methods:
## #' \describe{
## #' \item{\code{signature(data="data.frame")}}{\code{data} should have columns
## #' \code{c("parname", "chain_id", "iter", "val")}.}
## #' }
## #'
## #' @rdname McmcLong-methods
## #' @name McmcLong-methods
## #' @docType methods
## #' @keywords methods
## #' @aliases McmcLong
## #' @aliases McmcLong,data.frame-method
## #' @aliases McmcLong,mcmc-method
## #' @aliases McmcLong,mcmc.list-method
## #' @export
## setGeneric("McmcLong",
##            function(data, ...) {
##                standardGeneric("McmcLong")
##            })

## mcmc_long_default <-
##     function(data,
##              parnames=NULL,
##              fun=mcmc_parse_parname_default,
##              chains=NULL,
##              par_chains=NULL,
##              chain_iters=NULL,
##              metadata=list())
## {
##     ## Put this before parparsed to change data before eval
##     if (is.null(parnames)) {
##         parnames <- fun(unique(as.character(data$parname)))
##     }
##     pararrays <- parnames_to_pararrays(parnames)
##     ## Samples
##     data <- new("McmcSamples", data)
##     ## Create chains table if none given
##     if (is.null(chains)) {
##         chains <- ddply(data, "chain_id",
##                         function(x) {
##                             maxiter <- max(x$iter)
##                             data.frame(niter=maxiter,
##                                        thin=1L,
##                                        start=1L,
##                                        end=maxiter)
##                         })
##     }
##     chains <- new("McmcChains", chains)
##     if (!is.null(par_chains)) {
##         par_chains <- new("McmcParChains", par_chains)
##     }
##     if (!is.null(chain_iters)) {
##         chain_iters <- new("McmcChainIters", chain_iters)
##     }
##     new("McmcLong",
##         samples=data,
##         parnames=parnames,
##         pararrays=pararrays,
##         chains=chains,
##         chain_iters=chain_iters,
##         par_chains=par_chains,
##         metadata=as.list(metadata))
## }

## setMethod("McmcLong", "data.frame", mcmc_long_default)

## setMethod("McmcLong", "mcmc.list",
##           function(data, ...) {
##               chains <- ldply(data, function(x) attr(x, "mcpar"))
##               names(chains) <- c("start", "end", "thin")
##               chains$chain_id <- seq_len(nrow(chains))
##               chains <- transform(chains, niter = (end - start + 1) / thin)
##               chains <- chains[ , c("chain_id", "niter", "start", "end", "thin")]
##               for (i in c("niter", "start", "end", "thin")) {
##                   chains[[i]] <- as.integer(chains[[i]])
##               }
##               callGeneric(melt(data), chains=chains, ...)
##           })

## setMethod("McmcLong", "mcmc",
##           function(data, ...) {
##               callGeneric(melt(mcmc.list(data)), ...)
##           })

## ## Coercion

## ## McmcLong -> McmcList2
## setAs("McmcLong", "mcmc.list",
##       function(from, to) {
##           mcpars <- dlply(from@chains, "chain_id",
##                           function(x) as.integer(x[1 , c("start", "end", "thin")]))
##           to <- dlply(from@samples, "chain_id",
##                        function(x) acast(x, iter ~ parname,
##                                          value.var="val"))
##           to <- mapply(function(x, y) mcmc(x, start=y[1], end=y[2], thin=y[3]),
##                        to, mcpars)
##           mcmc.list(to)
##       })

## ## McmcList2 -> McmcLong
## setAs("mcmc.list", "McmcLong",
##       function(from, to) {
##           McmcLong(from)
##       })

## ## McmcLong -> data.frame
## setAs("McmcLong", "data.frame",
##       function(from, to) from@samples)

