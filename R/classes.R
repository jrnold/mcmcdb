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
#' @aliases McmcParChains-class
#' @aliases McmcParChains
#' @aliases McmcParChainsOrNull-class
#' @keywords classes
#' @docType class
#' @examples
#' showClass("McmcSamples")
#' showClass("McmcChains")
#' showClass("McmcIters")
#' showClass("McmcParChains")
NULL

#' @exportClass McmcSamples
#' @export McmcSamples
NULL
McmcSamples <- 
  constrained_data_frame("McmcSamples",
                         columns=c(parname="factor",
                           chain_id="integer",
                           iter="integer",
                           val="numeric"))

# -----------------------

#' @exportClass McmcChains
#' @export McmcChains
NULL
McmcChains <- 
  constrained_data_frame("McmcChains",
                         columns = c(chain_id="integer"),
                         constraints = list(constraint_check_niter))

# -----------------------
#' @exportClass McmcIters
#' @export McmcIters
NULL
McmcIters <- 
  constrained_data_frame("McmcIters",
                         columns = c(chain_id="integer",
                           iter="integer"))

# ----------------------

#' @exportClass McmcParChains
#' @export McmcParChains
NULL
McmcParChains <- 
  constrained_data_frame("McmcParChains",
                         columns = c(parname="factor",
                           chain_id="integer"))

#' @exportClass McmcParChainsOrNull
NULL
setClassUnion("McmcParChainsOrNull", c("McmcParChains", "NULL"))

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

