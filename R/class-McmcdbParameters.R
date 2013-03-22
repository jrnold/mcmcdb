#' @include package.R
#' @include class-misc.R
#' @exportClass McmcdbParameters
#' @export McmcdbParameters
#' @exportMethod [
NULL

CharacterArray <- setClass("CharacterArray", "array")

setValidity("CharacterArray",
            function(object) {
              if (!is.character(object)) {
                return("object is not a character vector.")
              }
              TRUE
            })

hlist_class("ListOfCharArrays", "CharacterArray")

ListOfCharArrays <- function(x) {
  new("ListOfCharArrays", llply(x, CharacterArray))
}

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
setClass("McmcdbParameters", "ListOfCharArrays")

setValidity("McmcdbParameters",
            function(object) {
              if (length(object)) {
                if (any(sapply(object@names, is.na)) ||
                    any(sapply(object@names, `==`, ""))) {
                  return("No names can be missing")
                }
                if (any(unlist(lapply(object, is.na)))) {
                  return("No array elements can be missing")
                }
              }
              TRUE
            })

show.McmcdbParameters <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcdbParameters")))
  cat("Parameters:\n")
  for (i in seq_along(object)) {
    cat(sprintf("$ %s [%s]\n",
                names(object)[i], paste(dim(object[[i]]), collapse=",")))
  }
}

setMethod("show", "McmcdbParameters", show.McmcdbParameters)

#' @rdname McmcdbParameters-class
#' @aliases [,McmcdbParameters,missing,ANY-method
setMethod("[", c(x="McmcdbParameters", i="missing"),
          function(x, i, j, ...) x)

#' @rdname McmcdbParameters-class
#' @aliases [,McmcdbParameters,ANY,ANY-method
setMethod("[", c(x="McmcdbParameters", i="ANY"),
          function(x, i, j, ...) new("McmcdbParameters", as(x, "ListOfCharArrays")[i]))
