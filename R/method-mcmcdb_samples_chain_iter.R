#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_iter.R
#' @exportMethod mcmcdb_samples_chain_iter
NULL

#' @rdname mcmcdb_samples_chain_iter-methods
#' @docType methods
#' @title Extract MCMC samples (Chains, Iterations)
#'
#' @description Return MCMC samples as a list of chains.
#' Each chain is a list of the iterations in that chain.
#' Each iteration is a list of parameter arrays.
#' 
#' @param object An object containing the MCMC samples.
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element represents a chain.
#' Each element of the chain is a \code{list} of iterations.
#' Each iteration is a list of \code{array} objects representing parameter arrays.
#'
#' @examples
#' data(line_samples)
#' line_samples_ci <- mcmcdb_samples_chain_iter(line_samples)
#' length(line_samples_ci)
#' # 1st iteration from the 1st chain
#' line_samples_ci[[1]][[1]]
setGeneric("mcmcdb_samples_chain_iter",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_chain_iter")
           })


#' @rdname mcmcdb_samples_chain_iter-methods
#' @aliases mcmcdb_samples_chain_iter,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("mcmcdb_samples_chain_iter", "Mcmcdb",
          function(object, parameters=NULL, iter=NULL, FUN = identity,
                   chain_id=NULL, return_type = "l", ...) {
            if (is.null(chain_id)) {
              chain_id <- mcmcdb_chains(object, drop=TRUE)
            }
            names(chain_id) <- chain_id
            .fun <- function(i) {
              FUN(mcmcdb_samples_iter(object, chain_id = i,
                                      parameters = parameters))
            }
            plyr_fun("l", return_type)(chain_id, .fun = .fun, ...)
          })
