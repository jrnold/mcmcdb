#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_data
NULL

#' @name mcmcdb_data-methods
#' @rdname mcmcdb_data-methods
#' @aliases mcmcdb_data-methods
#' @aliases mcmcdb_data
#' @title Get model data from a Mcmcdb object
#'
#' @description Get the data used in model estimation,
#' if it was saved in the object.
#' @param object An object for which the method is defined.
#' @return \code{list} in which each element is some data object.
setGeneric("mcmcdb_data",
           function(object, ...) {
             standardGeneric("mcmcdb_data")
           })


#' @rdname mcmcdb_data-methods
#' @aliases mcmcdb_data,McmcdbWide-method
setMethod("mcmcdb_data", "McmcdbWide",
          function(object) object@model_data)

