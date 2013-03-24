#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_pararray
NULL

#' @rdname mcmcdb_samples_pararrays-methods
#' @title Mcmcdb Object samples (by Pararray)
#'
#' @description Return samples from a Mcmcdb
#' object in a list, one element for each parameter
#' array.
#' 
#' @return \code{list} with length equal to the number of
#' parameter arrays. Each element of the list
#' is an \code{array}.
#' @examples
#' data(line_samples)
#' summary(mcmcdb_samples_pararray(line_samples))
setGeneric("mcmcdb_samples_pararray",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_pararray")
           })

#' @rdname mcmcdb_samples_pararray-methods
#' @aliases mcmcdb_samples_pararray,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_samples_pararray", "McmcdbWide",
          function(object, flatpars=NULL, pararrays=NULL, iter=NULL,
                   chain_id=NULL, ...) {
            mcmcdb_unflatten(mcmcdb_wide_subset(object,
                                                flatpars = flatpars,
                                                pararrays = pararrays,
                                                iter = iter,
                                                chain_id = chain_id),
                             parameters = object@parameters)
          })
