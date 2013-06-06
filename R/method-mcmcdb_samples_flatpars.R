#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_samples_flatpars
NULL

#' @rdname mcmcdb_samples_flatpars-methods
#' @docType methods
#' @title  Extract MCMC Samples (Flat parameter form)
#'
#' @description Extract MCMC samples from an object as a 
#' a matrix of flat parameters.
#' 
#' @param object An object containing the MCMC samples.
#' @param flatpars \code{character}. Flat parameters to include. If \code{NULL}, all flat parameters.
#' @param parameters \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' The union of flat parameters in \code{parameters} and \code{flatpars} is included.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param FUN \code{function}. Function to apply to each 
#' @param ... Options passed to internal functions.
#' 
#' @return If \code{FUN = NULL}, then a \code{matrix} with columns equal to flat parameters,
#' and rows equal to iterations. If \code{FUN != NULL}, then a \code{list}, with the results
#' of \code{FUN} applied to each flat parameter.
#' 
#' @examples
#' data(line_samples)
#' line_wide <- mcmcdb_samples_flatpars(line_samples)
#' dim(line_wide)
#' head(line_wide)
#' summary(line_wide)
setGeneric("mcmcdb_samples_flatpars",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_flatpars")
           })

mcmcdb_samples_flatpars.McmcdbWide <-
  function(object, FUN=identity,
           flatpars=NULL, parameters=NULL, iter=NULL,
           chain_id=NULL, return_type = "a", ...) {
    x <- mcmcdb_wide_subset(object,
                            flatpars=flatpars, parameters=parameters,
                            iter=iter, chain_id=chain_id)
    if (identical(FUN, identity) && identical(return_type, "a")) {
      # Notable Special case 
      x
    } else {
      plyr_fun("a", return_type)(x, 2, FUN, ...)
    }
  }

#' @rdname mcmcdb_samples_flatpars-methods
#' @aliases mcmcdb_samples_flatpars,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_flatpars", "McmcdbWide",
          mcmcdb_samples_flatpars.McmcdbWide)
