#' Internal classes
#'
#' Various classes used internally in this package.
#' These are all subclasses of \linkS4class{DataFrameConstr},
#' i.e. \code{data.frame} objects with required
#' columns.
#'
#' @name internal-classes
#' @rdname internal-classes
#' @aliases McmcSamples-class
#' @aliases McmcSamples
#' @aliases McmcChains-class
#' @aliases McmcChains
#' @aliases McmcIters-class
#' @aliases McmcIters
#' @aliases McmcFlatparChains-class
#' @aliases McmcFlatparChains
#' @aliases McmcFlatparChainsOrNull-class
#' @keywords classes
#' @docType class
#' @examples
#' showClass("McmcSamples")
#' showClass("McmcChains")
#' showClass("McmcIters")
#' showClass("McmcFlatparChains")
NULL

#' @exportClass McmcSamples
#' @export McmcSamples
NULL
McmcSamples <- 
  constrained_data_frame("McmcSamples",
                         columns=c(flatpar="factor",
                           chain_id="integer",
                           iter="integer",
                           val="numeric"))

# -----------------------

#' @exportClass McmcChains
#' @export McmcChains
NULL
McmcChains <- 
  constrained_data_frame("McmcChains",
                         columns = c(chain_id="integer"))

# -----------------------
#' @exportClass McmcIters
#' @export McmcIters
NULL
McmcIters <- 
  constrained_data_frame("McmcIters",
                         columns = c(chain_id="integer",
                           iter="integer"))

# ----------------------

#' @exportClass McmcFlatparChains
#' @export McmcFlatparChains
NULL
McmcFlatparChains <- 
  constrained_data_frame("McmcFlatparChains",
                         columns = c(flatpar="factor",
                           chain_id="integer"))

#' @exportClass McmcFlatparChainsOrNull
NULL
setClassUnion("McmcFlatparChainsOrNull", c("McmcFlatparChains", "NULL"))

## subclasses of matrix that restrict to a given type

NumericMatrix <- setClass("NumericMatrix", "matrix")

setValidity("NumericMatrix",
            function(object) {
              if (!is.numeric(object)) {
                return("Object is not numeric.")
              }
              TRUE
            })

IntegerMatrix <- setClass("IntegerMatrix", "matrix")

setValidity("IntegerMatrix",
            function(object) {
              if (!is.integer(object)) {
                return("Object is not integer.")
              }
              TRUE
            })

## Missing Coercions
setAs("character", "factor", function(from, to) as.factor(from))

