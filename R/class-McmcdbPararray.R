#' @include package.R
#' @exportClass McmcdbPararray
#' @exportClass McmcdbPararrayList
#' @export McmcdbPararray
#' @export McmcdbPararrayList
NULL


#' McmcdbPararray Class
#'
#' An S4 class containing metadata for an MCMC parameter array:
#' its dimensions, and the names of the flattened parameters within it.
#'
#' @section Slots:
#' \describe{
#' \item{\code{flatpars}}{\code{"character"}. Name of flattened parameters in the parameter array}
#' \item{\code{dim}}{\code{"integer"}. Dimension of the array}
#' }
#' @docType class
#' @name McmcdbPararray-class
#' @aliases McmcdbPararray-class
#' @aliases McmcdbPararray
#' @keywords internal
#' @examples
#' showClass("McmcdbPararray")
#' McmcdbPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L)
McmcdbPararray <-
  setClass("McmcdbPararray",
           representation(dim = "integer", flatpars = "character"))

mcmc_pararray_validity <- function(object) {
  if (length(object@flatpars) != prod(object@dim)) {
    return("length(object@flatpars) != prod(object@dims)")
  }
  TRUE
}

setValidity("McmcdbPararray", mcmc_pararray_validity)

setMethod("show", "McmcdbPararray",
          function(object) {
            cat(sprintf("An object of class %s\nDim (%s): %s",
                        dQuote("McmcdbPararray"),
                        paste(object@dim, collapse=","),
                        ifelse(length(object@flatpars) == 1,
                               object@flatpars[1],
                               paste(object@flatpars[1], "..."))), "\n")
          })

setMethod("initialize", "McmcdbPararray",
          function(.Object, dim, flatpars) {
            .Object@dim <- as.integer(dim)
            .Object@flatpars <- as.character(flatpars)
            validObject(.Object)
            .Object
          })

#' McmcdbPararrayList Class
#'
#' A list of \linkS4class{McmcdbPararray} objects.
#'
#' @section Extends:
#' \describe{
#' \item{\code{namedList}}{directly.}
#' }
#' @docType class
#' @name McmcdbPararrayList-class
#' @aliases McmcdbPararrayList-class
#' @aliases McmcdbPararrayList
#' @keywords internal
#' @seealso \code{\link{McmcdbPararray}}
#' @examples
#' showClass("McmcdbPararrayList")
#' mcmcpararrays <-
#'  list(beta = McmcdbPararray(flatpars=paste0("beta[", 1:2L, "]"), dim=2L),
#'       alpha = McmcdbPararray(flatpars="alpha", dim=1L))
#' McmcdbPararrayList(mcmcpararrays)
McmcdbPararrayList <-
  subclass_homog_list("McmcdbPararrayList", "McmcdbPararray")
