#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_iter
NULL

#' @rdname mcmcdb_samples_iter-methods
#' @docType methods
#' @title Extract MCMC samples (Iterations)
#'
#' @description Return MCMC samples as a list of iterations,
#' in which each iteration is a list of parameter arrays.
#' 
#' @param object An object containing the MCMC samples.
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param ... Options passed to internal functions.
#'
#' @return \code{list}. Each element of the list is a \code{list} 
#' of \code{array} objects representing the parameter arrays.
#'
#' @examples
#' data(line_samples)
#' line_samples_iter <- mcmcdb_samples_iter(line_samples)
#' length(line_samples_iter)
#' line_samples_iter[[1]]
setGeneric("mcmcdb_samples_iter",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_iter")
           })

#' @rdname mcmcdb_samples_iter-methods
#' @aliases mcmcdb_samples_iter,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_iter", "McmcdbWide",
          function(object, pararrays=NULL, iter=NULL,
                   chain_id=NULL, ...) {
            x <- mcmcdb_wide_subset(object,
                                    pararrays = pararrays,
                                    iter = iter,
                                    chain_id = chain_id)
            if (is.null(pararrays)) {
              parameters <- object@parameters
            } else {
              parameters <- object@parameters[pararrays]
            }
            alply(x, 1, mcmcdb_unflatten, parameters = parameters, ...)
          })


## pararrays <- names(mcmcdb_parameters(line_samples))
## chains <- mcmcdb_chains(line_samples)
## by_chain <- function(par) llply(chains, function(i) mcmcdb_samples_pararrays(line_samples, pararrays = par, chain_id = i, drop=TRUE))
## str(llply(pararrays, by_chain))

