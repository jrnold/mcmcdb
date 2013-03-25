#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_flatpars.R
#' @exportMethod mcmcdb_samples_chain_flatpars
NULL

#' @rdname mcmcdb_samples_chain_flatpars-methods
#' @docType methods
#' @title Extract MCMC samples (Chains, Flat parameters)
#'
#' @description Return MCMC samples as a list of chains.
#' Each chain is a named list of arrays with each array
#' containing all iterations in that chain for that parameter array.
#' 
#' @param object An object containing the MCMC samples.
#' @param flatpars \code{character}. Flat parameters to include. If \code{NULL}, all flat parameters.
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element is a chain.
#' Each chain is a named \code{list} of \code{matrix} objects.
#' Each matrix contains the iterations of the flat parameters.
#'
#' @examples
#' data(line_samples)
#' line_samples_chain_pars <- mcmcdb_samples_chain_flatpars(line_samples)
#' length(line_samples_chain_pars)
#' summary(line_samples_chain_pars[[1]])
setGeneric("mcmcdb_samples_chain_flatpars",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_chain_flatpars")
           })


#' @rdname mcmcdb_samples_chain_flatpars-methods
#' @aliases mcmcdb_samples_chain_flatpars,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_chain_flatpars", "Mcmcdb",
          function(object, flatpars=NULL, pararrays=NULL,
                   iter=NULL, chain_id=NULL, ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object, drop=TRUE)
            }
            names(chain_id) <- chain_id
            .fun <- function(i) {
              mcmcdb_samples_flatpars(object, chain_id = i,
                                      flatpars = flatpars, 
                                      pararrays = pararrays)
            }
            llply(chain_id, .fun=.fun, ...)
          })
