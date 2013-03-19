#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_parameters
NULL

#' @name mcmcdb_parameters-methods
#' @rdname mcmcdb_parameters-methods
#' @aliases mcmcdb_parameters
#' @aliases mcmcdb_parameters-methods
#' @title Get paramters of Mcmcdb object
#'
#' @param object object
#' @return An object of class \code{\linkS4class{McmcdbParameters}}.
NULL
setGeneric("mcmcdb_parameters",
           function(object, ...) {
             standardGeneric("mcmcdb_parameters")
           })

setMethod("mcmcdb_parameters", "McmcdbWide",
          function(object) object@parameters)
