#' @include package.R
#' @include class-McmcdbWide.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod mcmcdb_samples_iter
NULL

#' @rdname mcmcdb_samples_iter-methods
#' @title Mcmcdb Object samples (by Iteration)
#'
#' @description Return samples from a Mcmcdb
#' object in a list, one element for each iteration.
#' Each element is a list of arrays, one for each
#' parameter in its original dimensions.
#' 
#' @return \code{list} with length equal to the number of
#' elements. Each element of the list
#' is a \code{list} of \code{array} objects.
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
