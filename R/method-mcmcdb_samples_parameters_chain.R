#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_parameters.R
#' @include method-mcmcdb_samples_parameters.R
#' @exportMethod mcmcdb_samples_parameters_chain
NULL

#' @rdname mcmcdb_samples_parameters_chain-methods
#' @docType methods
#' @title Extract MCMC samples (Parameter arrays, chains)
#'
#' @description Return MCMC samples as a list of parameter arrays
#' Each parameter array is a list of arrays, containing the iterations in each chain.
#' 
#' @param object An object containing the MCMC samples.
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function}. Function to apply to each list of chains. The function should
#' take the form \code{function(x)}, where \code{x} is a \code{list} of \code{array} objects.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. If \code{FUN = NULL}, each element of the list is a \code{list} 
#' of \code{array} objects, representing the sample values for each parameter array in each chain.
#' If \code{FUN != NULL}, then each element contains the results of \code{FUN} for that parameter.
#'
#' @examples
#' data(line_samples)
#' line_samples_pc <- mcmcdb_samples_parameters_chain(line_samples)
#' str(line_samples_pc)
setGeneric("mcmcdb_samples_parameters_chain",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_parameters_chain")
           })

#' @rdname mcmcdb_samples_parameters_chain-methods
#' @aliases mcmcdb_samples_parameters_chain,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_parameters_chain", "Mcmcdb",
          function(object, parameters=NULL, iter=NULL, chain_id=NULL,
                   FUN = identity, return_type = "l", ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object)
            }
            names(chain_id) <- chain_id
            if (is.null(parameters)) {
              parameters <- names(mcmcdb_parameters(object))
            }
            names(parameters) <- parameters
            .fun1 <- function(par) {
              .fun2 <- function(i) {
                mcmcdb_samples_parameters(object, parameters = par,
                                         chain_id = i)[[1]]
              }
              FUN(llply(chain_id, .fun = .fun2))
            }
            plyr_fun("l", return_type)(parameters, .fun = .fun1, ...)
          })
