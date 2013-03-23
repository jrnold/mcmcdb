#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-McmcdbMem.R
#' @exportMethod mcmcdb_pardims
NULL

#' Get parameter array dimensions
#'
#' @param object Object
#' @rdname mcmcdb_pardims-methods
#' @name mcmcdb_pardims-methods
#' @return Named \code{list} of \code{integer} vectors. Names are the parameter
#' array names; element values are the associated dimensions of the arrays.
#'
#' @aliases mcmcdb_pardims
#' @aliases mcmcdb_pardims-methods
setGeneric("mcmcdb_pardims",
           function(object, ...) standardGeneric("mcmcdb_pardims"))

dim.McmcdbParameters <- function(object) {
  ret <- llply(seq_along(object), function(i) dim(object[[i]]))
  names(ret) <- names(object)
  ret
}

#' @rdname mcmcdb_pardims-methods
#' @aliases mcmcdb_pardims,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_pardims", "McmcdbParameters", dim.McmcdbParameters)

#' @rdname mcmcdb_pardims-methods
#' @aliases mcmcdb_pardims,McmcdbMem-method
#' @family McmcdbMem methods
setMethod("mcmcdb_pardims", "McmcdbMem",
          function(object) callGeneric(object@parameters))
