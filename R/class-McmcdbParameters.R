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

ListOfCharArrays <- subclass_homog_list("ListOfCharArrays", "CharacterArray")

#' McmcdbParameters Class
#'
#' Metadata about MCMC parameters, including the names
#' of flattened parameters, mapping between flattened parameter
#' names and parameter array names, and the dimenions of
#' parameter arrays.
#'
#' Objects of this class are usually created by \code{\link{mcmc_parse_parnames}}.
#'
#' @section Slots:
#' \describe{
#' \item{\code{flatpars}}{An object of \code{\link{McmcdbFlatparList}}.
#' Names and information on the flattened parameters.}
#' \item{\code{pararrays}}{An object of \code{\link{McmcdbPararrayList}}.
#' Names and information on the parameter arrays.}
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
#' @docType class
#' @name McmcdbParameters-class
#' @aliases McmcdbParameters
#' @aliases McmcdbParameters-class
#' @aliases dim,McmcdbParameters-method
#' @aliases dimnames,McmcdbParameters-method
#' @keywords internal
#' @seealso \code{\link{mcmc_parse_parnames}}
#' @family McmcdbParameters methods
#' @examples
#' showClass("McmcdbParameters")
McmcdbParameters <-
  setClass("McmcdbParameters", 
           representation(pararrays = "ListOfCharArrays"))
                          
                          
show.McmcdbParameters <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcdbParameters")))
  cat("Parameters:\n")
  for (i in seq_along(object@pararrays)) {
    parname <- names(object@pararrays)[i]
    pardim <- dim(object@pararrays[[i]])
    cat(sprintf("$ %s [%s]\n",
                parname, paste(pardim, collapse=",")))
  }
}

setMethod("show", "McmcdbParameters", show.McmcdbParameters)

setMethod("dimnames", "McmcdbParameters",
          function(x) {
            list(flatpars = names(x@flatpars),
                 pararrays = names(x@pararrays))
          })

setMethod("dim", "McmcdbParameters",
          function(x) {
            c(flatpars = length(x@flatpars),
              pararrays = length(x@pararrays))
          })

