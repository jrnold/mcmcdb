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
#' @param pararrays \code{character}. Parameter arrays to include. If \code{NULL}, all parameter arrays.
#' The union of flat parameters in \code{pararrays} and \code{flatpars} is included.
#' @param chain_id \code{integer}. Chains to include. If \code{NULL}, all chains.
#' @param iter \code{integer}. Iterations to include. If \code{NULL}, all iterations.
#' @param drop \code{logical}. If \code{TRUE}, and only a single flat parameter is returned,
#' the result is coerced to a \code{numeric} vector.
#' @param ... Options passed to internal functions.
#' 
#' @return \code{matrix}. Columns are flat parameters.
#' Rows are iterations, from all chains.
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

#' @rdname mcmcdb_samples_flatpars-methods
#' @aliases mcmcdb_samples_flatpars,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_flatpars", "McmcdbWide",
          function(object, flatpars=NULL, pararrays=NULL, iter=NULL,
                   chain_id=NULL, drop=FALSE, .fun=NULL, ...) {
            x <- mcmcdb_wide_subset(object,
                                    flatpars=flatpars, pararrays=pararrays,
                                    iter=iter, chain_id=chain_id, drop=drop)
            if (!is.null(.fun)) {
              alply(x, 2, .fun, ...)
            } else {
              x
            }
          })
