#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_drop_iters
NULL

#' @rdname mcmcdb_drop_iters-methods
#' @docType methods
#' @title Methods for function \code{mcmcdb_drop_iters}
#'
#' @description Drop specified iterations from a \code{Mcmcdb} object.
#'
#' @param object An object containing MCMC samples
#' @param iter Iteration identifiers
#' @return An object of the same class as \code{object}.
#' @family McmcdbWide methods
setGeneric("mcmcdb_drop_iters",
           function(object, iter, ...) {
             standardGeneric("mcmcdb_drop_iters")
           })

mcmcdb_drop_iters.McmcdbWide <- function(object, iter, chain_id=NULL) {
  iters <- ! mcmcdb_wide_select_iters(object, iter=iter, chain_id=chain_id)
  object@samples <- object@samples[iters, ]
  object@iters <- object@iters[iters, ]
  validObject(object)
  object
}

#' @rdname mcmcdb_drop_iters-methods
#' @aliases mcmcdb_drop_iters,McmcdbWide,integer-method
setMethod("mcmcdb_drop_iters",
          c(object = "McmcdbWide", iter = "integer"),
          mcmcdb_drop_iters.McmcdbWide)

#' @rdname mcmcdb_drop_iters-methods
#' @aliases mcmcdb_drop_iters,McmcdbWide,numeric-method
setMethod("mcmcdb_drop_iters",
          c(object = "McmcdbWide", iter = "numeric"),
          function(object, iter) {
            callGeneric(object, as.integer(iter))
          })
