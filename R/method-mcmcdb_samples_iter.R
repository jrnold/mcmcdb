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
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function}. Function to apply to each iteration. \code{function(x)}, where
#' \code{x} is a named \code{list} of the parameter arrays for a single iteration.
#' @param return_type \code{character} Return type of the plyr function used internally.
#' @param ... Options passed to internal functions.
#'
#' @return Return type specified in \code{return_type}.
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

mcmcdb_samples_iter.McmcdbWide <-
  function(object, parameters = NULL, iter = NULL,
           chain_id = NULL, FUN = identity, return_type = "l", ...) {
    x <- mcmcdb_wide_subset(object,
                            parameters = parameters,
                            iter = iter,
                            chain_id = chain_id)
    if (is.null(parameters)) {
      parameters <- object@parameters
    } else {
      parameters <- object@parameters[parameters]
    }
    .fun <- function(x, ...) {
      FUN(mcmcdb_unflatten(x, parameters=parameters), ...)
    }
    x <- plyr_fun("a", return_type)(x, .margins = 1, .fun = .fun, ...)
    for (i in c("split_type", "split_labels")) {
      attr(x, i) <- NULL
    }
    x
  }

#' @rdname mcmcdb_samples_iter-methods
#' @aliases mcmcdb_samples_iter,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_iter", "McmcdbWide",
          mcmcdb_samples_iter.McmcdbWide)
