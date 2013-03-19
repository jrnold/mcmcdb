#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_init
NULL

#' @name mcmcdb_init-methods
#' @rdname mcmcdb_init-methods
#' @aliases mcmcdb_init-methods
#' @aliases mcmcdb_init
#' @title Get parameter initial values from a Mcmcdb object
#'
#' @description Get the parameter initial values used in model estimation,
#' if it was saved in the object.
#' @param object An object for which the method is defined.
#' @param flatten \code{logical} Whether to return the parameter values
#' in their flattened or original array shapes.
#' @return If \code{flattened=TRUE}, then a named \code{numeric} vector. If
#' \code{flattened=FALSE}, then a list of numeric \code{array} objects.
setGeneric("mcmcdb_init",
           function(object, ...) {
             standardGeneric("mcmcdb_init")
           })


#' @rdname mcmcdb_init-methods
#' @aliases mcmcdb_init,McmcdbWide-method
setMethod("mcmcdb_init", "McmcdbWide",
          function(object, flatten=TRUE) {
            if (flatten) {
              object@parinit
            } else {
              mcmcdb_unflatten(object@parinit, mcmcdb_parameters(object))
            }
          })

