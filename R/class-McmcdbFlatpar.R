#' @include package.R
#' @include utilities.R
#' @exportClass McmcdbFlatpar
#' @exportClass McmcdbFlatparList
#' @export McmcdbFlatpar
#' @export McmcdbFlatparList
NULL

#' McmcdbFlatpar class
#'
#' Class representing attributes of a flattened MCMC parameter: the
#' name of its parameter array and its index in that array. 
#'
#' @section Slots:
#' \describe{
#' \item{\code{pararray}}{\code{"character"}. Name of parameter array}
#' \item{\code{index}}{\code{"index"}. Index of parameter}
#' }
#' @docType class
#' @name McmcdbFlatpar-class
#' @aliases McmcdbFlatpar
#' @aliases McmcdbFlatpar-class
#' @seealso \linkS4class{McmcdbFlatparList} for a list of \code{McmcdbFlatpar} objects.
#' @examples
#' # beta[1,1]
#' showClass("McmcdbFlatpar")
#' McmcdbFlatpar(pararray="beta", index=c(1L, 1L))

McmcdbFlatpar <-
  setClass("McmcdbFlatpar",
           representation(pararray = "character",
                          index = "integer"))

setMethod("show", "McmcdbFlatpar",
          function(object) {
            cat(sprintf("An object of class %s: %s[%s]\n", dQuote("McmcdbFlatpar"),
                    object@pararray,
                        paste(object@index, collapse=",")))
          })

setMethod("initialize", "McmcdbFlatpar",
          function(.Object, pararray, index) {
            .Object@pararray <- as.character(pararray)
            .Object@index <- as.integer(index)
            validObject(.Object)
            .Object
          })

#' McmcdbFlatparList class
#'
#' A list of \linkS4class{McmcdbFlatpar} objects.
#'
#' @section Extends:
#' \describe{
#' \item{\code{namedList}}{directly}
#' }
#' @seealso \linkS4class{McmcdbFlatpar}
#' @name McmcdbFlatparList-class
#' @aliases McmcdbFlatparList-class
#' @aliases McmcdbFlatparList
#' @docType class
#' @keywords internal
#' @examples
#' showClass("McmcdbFlatparList")
#' flatpars <-
#'   structure(mapply(function(x, y) {
#'    McmcdbFlatpar(pararray=x, index=y)
#'   }, "beta", 1:2, SIMPLIFY=FALSE),
#'            .Names = paste0("beta[", 1:2, "]"))
#' McmcdbFlatparList(flatpars)                         
McmcdbFlatparList <-
  subclass_homog_list("McmcdbFlatparList", "McmcdbFlatpar")
