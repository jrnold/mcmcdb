#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-McmcdbWide.R
NULL

#' @export
setGeneric("mcmcdb_parameters",
          function(object, ...) {
            standardGeneric("mcmcdb_parameters")
          })

#' Get parameters data from Mcmcdb object
#'
#' @param object object
#' @return \linkS4class{McmcdbParameters}
#' @family get-methods
#' @seealso \code{\linkS4class{McmcdbWide}}
#' @aliases mcmcdb_parameters,McmcdbWide-method
setMethod("mcmcdb_parameters", "McmcdbWide",
            function(object) {
              object@parameters
            })