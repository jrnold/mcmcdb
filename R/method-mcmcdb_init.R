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
#' @return Named \code{numeric} vector. Names are the flat parameter names,
#' values are the parmeter initial values.
setGeneric("mcmcdb_init",
           function(object, ...) {
             standardGeneric("mcmcdb_init")
           })


#' @rdname mcmcdb_init-methods
#' @aliases mcmcdb_init,McmcdbWide-method
setMethod("mcmcdb_init", "McmcdbWide",
          function(object) object@parinit)

