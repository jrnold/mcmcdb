#' @include package.R
#' @include class-McmcdbPararray.R
#' @include class-McmcdbFlatpar.R
#' @include class-misc.R
#' @exportClass McmcdbParameters
#' @export McmcdbParameters
#' @exportMethod dim
#' @exportMethod dimnames
NULL

CharacterArray <- setClass("CharacterArray", "array")

setValidity("CharacterArray",
            function(object) {
              if (!is.character(object)) {
                return("object is not a character vector.")
              }
              TRUE
            })

subclass_homog_list("ListOfCharArrays", "CharacterArray")

ListOfCharArrays <- function(x) {
  new("ListOfCharArrays", llply(x, CharacterArray))
}

#' @name McmcdbParameters-class
#' @rdname McmcdbParameters-class
#' @docType class
#' @aliases McmcdbParameters
#' @aliases McmcdbParameters-class
#' @aliases dim,McmcdbParameters-method
#' @aliases dimnames,McmcdbParameters-method
#' @title McmcdbParameters Class
#'
#' @description Metadata about MCMC parameters, providing information about
#' their dimension and the names of the associated flattened parameters.
#' This class and its methods are used for converting between parameter
#' arrays and flattened arrays.
#'
#' Objects of this class are usually created by \code{\link{mcmc_parse_parnames}}.
#'
#" @section Extends:
#' \describe{
#'   \item{\code{ListOfCharArrays}}{directly. A \code{list} in which all
#'   elements must be arrays containing \code{character} data.}
#'   \item{\code{namedList}}{By \code{ListOfCharArrays}}
#' }
#' 
#' @section Methods:
#' \describe{
#'  \item{dim}{\code{signature(x = "McmcdbParameters")}:
#'  Returns a named \code{integer} vector with the number of flat parameters and parameter arrays.}    
#'  \item{dimnames}{\code{signature(x = "McmcdbParameters")}:
#'  Returns a \code{list} with names of the flat parameters and parameter arrays.}
#' }
#' 
#' @keywords internal
#' @seealso \code{\link{mcmc_parse_parnames}}
#' @family McmcdbParameters methods
#' @examples
#' showClass("McmcdbParameters")
NULL
setClass("McmcdbParameters", "ListOfCharArrays")

#' @rdname McmcdbParameters-class
McmcdbParameters <- function(x) {
  new("McmcdbParameters", ListOfCharArrays(x))
}

setValidity("McmcdbParameters",
            function(object) {
              if (length(object)) {
                if (any(sapply(object@names, is.na)) ||
                    any(sapply(object@names, `==`, ""))) {
                  return("No object names can be missing")
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

setMethod("dim", "McmcdbParameters",
          function(x) {
            ret <- llply(seq_along(x), function(i) dim(x[[i]]))
            names(ret) <- names(x)
            ret
          })

setMethod("dimnames", "McmcdbParameters",
          function(x) {
            ret <- llply(seq_along(x), function(i) as.character(x[[i]]))
            names(ret) <- names(x)
            ret
          })


