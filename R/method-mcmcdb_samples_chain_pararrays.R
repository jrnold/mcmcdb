#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_pararrays.R
#' @exportMethod mcmcdb_samples_chain_pararrays
NULL

#' @rdname mcmcdb_samples_chain_pararrays-methods
#' @docType methods
#' @title  Extract MCMC Samples (Chains, Paramter arrays)
#'
#' @description Extract MCMC samples as a list of chains,
#' Each chain is a named list of parameter arrays.
#' 
#' @param object An object containing the MCMC samples.
#' @param pararrays \code{character}. Parameter arrays to include. 
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element represents a chain.
#' Each element of the chain is a named \code{list} of arrays,
#' each array representing a parameter array and including the iterations
#' of that parameter.
#' 
#' @examples
#' data(line_samples)
#' line_samples_chain_pars <- mcmcdb_samples_chain_pararrays(line_samples)
#' lapply(line_samples_chain_pars[[1]], dim)
setGeneric("mcmcdb_samples_chain_pararrays",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_chain_pararrays")
           })

#' @rdname mcmcdb_samples_chain_pararrays-methods
#' @aliases mcmcdb_samples_chain_pararrays,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_chain_pararrays", "Mcmcdb",
          function(object, pararrays=NULL, iter=NULL,
                   chain_id=NULL, ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object, drop=TRUE)
            }
            names(chain_id) <- chain_id
            .fun <- function(i) {
              mcmcdb_samples_pararrays(object, chain_id = i,
                                       pararrays = pararrays)
            }
            llply(chain_id, .fun=.fun, ...)
          })
