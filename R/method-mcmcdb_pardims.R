#' @include pacakge.R
#' @include McmcParameters.R
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
#' @export
setGeneric("mcmcdb_pardims",
           function(object, ...) standardGeneric("mcmcdb_pardims"))

mcmcdb_pardims.McmcdbParameters <- function(object) {
  llply(object@pararrays, slot, name = "dim")
}

#' @rdname mcmcdb_pardims-methods
#' @aliases mcmcdb_pardims,McmcdbParameters-method
setMethod("mcmcdb_pardims", "McmcdbParameters",
          mcmcdb_pardims.McmcdbParameters)
