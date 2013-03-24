#' @include package.R
#' @include class-misc.R
#' @exportClass McmcdbParameters
#' @export McmcdbParameters
#' @exportMethod [
NULL

setClass("ParnameArray", "array")

ParnameArray <- function(x) {
  x <- as.array(x)
  d <- dim(x)
  new("ParnameArray", array(as.character(x), d))
}

setValidity("ParnameArray",
            function(object) {
              if (!is.character(object)) {
                return("object is not a character vector.")
              }
              if (any(sapply(object, is.na))) {
                return("No array elements can be missing")
              }
              TRUE
            })

#' @name McmcdbParameters-class
#' @rdname McmcdbParameters-class
#' @docType class
#' @aliases McmcdbParameters-class
#' @title McmcdbParameters Class
#'
#' @description Metadata about MCMC parameters, providing information about
#' their dimension and the names of the associated flattened parameters.
#' This class and its methods are used for converting between parameter
#' arrays and flattened arrays.
#'
#' Objects of this class are usually created by the  \code{\link{McmcdbParameters}} function.
#'
#" @section Extends:
#' \describe{
#'   \item{\code{ListOfCharArrays}}{directly. A \code{list} in which all
#'   elements must be arrays containing \code{character} data.}
#'   \item{\code{namedList}}{By \code{ListOfCharArrays}}
#' }
#' 
#' @keywords internal
#' @seealso \code{\link{McmcdbParameters}}
#' @family McmcdbParameters methods
#' @examples
#' showClass("McmcdbParameters")
NULL

hlist_class("McmcdbParameters", "ParnameArray",
            unique_names = TRUE, empty_names = FALSE)

show.McmcdbParameters <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcdbParameters")))
  cat("Parameters:\n")
  for (i in seq_along(object)) {
    cat(sprintf("$ %s [%s]\n",
                names(object)[i], paste(dim(object[[i]]), collapse=",")))
  }
}

setMethod("show", "McmcdbParameters", show.McmcdbParameters)
