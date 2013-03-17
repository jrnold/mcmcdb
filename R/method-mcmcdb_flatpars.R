#' @include package.R
#' @include class-McmcdbParameters.R
NULL

#' Get flat parameter names
#'
#' @param object Object
#' @return Named \code{character} vector. Names are the flat parameter names;
#' values are the associated parameter arrays.
#'
#' @name mcmcdb_flatpars-methods
#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars
#' @aliases mcmcdb_flatpars-method
#' @export
setGeneric("mcmcdb_flatpars",
           function(object, ...) standardGeneric("mcmcdb_flatpars"))

mcmcdb_flatpars.McmcdbParameters <- function(object) {
  unlist(llply(object@flatpars, slot, name = "pararray"))
}

#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_flatpars", "McmcdbParameters",
          mcmcdb_flatpars.McmcdbParameters)

#' @rdname mcmcdb_flatpars-methods
#' @aliases mcmcdb_flatpars,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_flatpars", "McmcdbWide",
          function(object) callGeneric(object@parameters))
