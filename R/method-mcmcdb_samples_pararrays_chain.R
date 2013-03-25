#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_parameters.R
#' @include method-mcmcdb_samples_pararrays.R
#' @exportMethod mcmcdb_samples_pararrays_chain
NULL

#' @rdname mcmcdb_samples_pararrays_chain-methods
#' @docType methods
#' @title Extract MCMC samples (Parameter arrays, chains)
#'
#' @description Return MCMC samples as a list of parameter arrays
#' Each parameter array is a list of arrays, containing the iterations in each chain.
#' 
#' @param object An object containing the MCMC samples.
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element of the list is a \code{list} 
#' of \code{array} objects, representing the samples of each parameter array in each chain.
#'
#' @examples
#' data(line_samples)
#' line_samples_pc <- mcmcdb_samples_pararrays_chain(line_samples)
#' str(line_samples_pc)
setGeneric("mcmcdb_samples_pararrays_chain",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_pararrays_chain")
           })

#' @rdname mcmcdb_samples_pararrays_chain-methods
#' @aliases mcmcdb_samples_pararrays_chain,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_pararrays_chain", "Mcmcdb",
          function(object, pararrays=NULL,
                   iter=NULL, chain_id=NULL, ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object)
            }
            if (is.null(pararrays)) {
              pararrays <- names(mcmcdb_parameters(object))
            }
            names(pararrays) <- pararrays
            .fun <- function(par) {
              .fun2 <- function(i) {
                mcmcdb_samples_pararrays(object, pararrays = par,
                                         chain_id = i)
              }
              llply(chain_id, .fun = .fun2)
            }
            llply(pararrays, .fun = .fun, ...)
          })
