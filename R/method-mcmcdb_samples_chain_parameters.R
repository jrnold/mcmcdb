#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_parameters.R
#' @exportMethod mcmcdb_samples_chain_parameters
NULL

#' @rdname mcmcdb_samples_chain_parameters-methods
#' @docType methods
#' @title  Extract MCMC Samples (Chains, Paramter arrays)
#'
#' @description Extract MCMC samples as a list of chains,
#' Each chain is a named list of parameter arrays.
#' 
#' @param object An object containing the MCMC samples.
#' @param parameters \code{character}. Parameter arrays to include. 
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function} Function to apply to each chain's list of parameter arrays.
#' \code{function(x)} where \code{x} is a named \code{list} of \code{array} objects.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. If \code{FUN = NULL}, then each element represents a chain.
#' Each element of the chain is a named \code{list} of arrays,
#' each array representing a parameter array and including the iterations
#' of that parameter. If \code{FUN != NULL}, then each element is the result of
#' \code{FUN}.
#' 
#' @examples
#' data(line_samples)
#' line_samples_chain_pars <- mcmcdb_samples_chain_parameters(line_samples)
#' lapply(line_samples_chain_pars[[1]], dim)
setGeneric("mcmcdb_samples_chain_parameters",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_chain_parameters")
           })

#' @rdname mcmcdb_samples_chain_parameters-methods
#' @aliases mcmcdb_samples_chain_parameters,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_chain_parameters", "Mcmcdb",
          function(object, parameters = NULL, iter = NULL,
                   chain_id = NULL, FUN = identity, return_type = "l", ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object, drop=TRUE)
            }
            names(chain_id) <- chain_id
            .fun <- function(i) {
              FUN(mcmcdb_samples_parameters(object, chain_id = i,
                                           parameters = parameters))
            }
            plyr_fun("l", return_type)(chain_id, .fun=.fun, ...)
          })
