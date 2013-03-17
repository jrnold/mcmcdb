#' @include class-McmcdbWide.R
NULL

#' @export
setGeneric("mcmcdb_metadata",
           function(object, ...) {
             standardGeneric("mcmcdb_metadata")
           })

#' Get metadata from an Mcmcdb object
#'
#' @param object object
#' @return \code{list}
#' @aliases mcmcdb_metadata,McmcdbWide-method
#' @seealso \code{\linkS4class{McmcdbWide}}
#' @family McmcdbWide methods
setMethod("mcmcdb_metadata", "McmcdbWide",
          function(object) object@metadata)

