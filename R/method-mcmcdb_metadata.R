#' @include class-McmcdbMem.R
#' @exportMethod mcmcdb_metadata
NULL

setGeneric("mcmcdb_metadata",
           function(object, ...) {
             standardGeneric("mcmcdb_metadata")
           })

#' Get metadata from an Mcmcdb object
#'
#' @param object object
#' @return \code{list}
#' @aliases mcmcdb_metadata,McmcdbMem-method
#' @seealso \code{\linkS4class{McmcdbMem}}
#' @family McmcdbMem methods
setMethod("mcmcdb_metadata", "McmcdbMem",
          function(object) object@metadata)

