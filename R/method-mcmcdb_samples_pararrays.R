#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_pararrays
NULL

#' @rdname mcmcdb_samples_pararrays-methods
#' @docType methods
#' @title Extract MCMC samples (Parameter arrays)
#'
#' @description Extract MCMC samples from an object as a 
#' a list of arrays.
#'
#' @param object An object containing the MCMC samples.
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function}. Function to apply to each paramter array. \code{function(x)} where
#' \code{x} is the 
#' @param ... Options passed to internal functions.
#' 
#' @return \code{list} of \code{array} objects. The arrays represent all
#' iterations of each parameter array.
#' 
#' @examples
#' data(line_samples)
#' line_arrays <- mcmcdb_samples_pararrays(line_samples)
#' summary(line_arrays)
#' lapply(line_arrays, dim)
setGeneric("mcmcdb_samples_pararrays",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_pararrays")
           })

#' @rdname mcmcdb_samples_pararrays-methods
#' @aliases mcmcdb_samples_pararrays,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_pararrays", "McmcdbWide",
          function(object, pararrays=NULL, iter=NULL,
                   chain_id=NULL, FUN = identity, return_type = "l",  ...) {
            if (is.null(pararrays)) {
              parameters <- object@parameters
            } else {
              parameters <- object@parameters[pararrays]
            }
            x <- mcmcdb_unflatten(mcmcdb_wide_subset(object,
                                                     pararrays = pararrays,
                                                     iter = iter,
                                                     chain_id = chain_id),
                                  parameters = parameters)
            if (!(identical(FUN, identity) && identical(return_type, "l"))) {
              x <- plyr_fun("l", return_type)(x, FUN, ...)
              for (i in c("split_type", "split_labels")) {
                attr(x, i) <- NULL
              }
            }
            x
          })
