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
#' @aliases McmcdbFlatparChains-class
#' @aliases McmcdbFlatparChains
#' @aliases McmcdbFlatparChainsOrNull-class
#' @keywords classes
#' @docType class
#' @examples
#' showClass("McmcSamples")
#' showClass("McmcChains")
#' showClass("McmcIters")
#' showClass("McmcdbFlatparChains")
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

#' @exportClass McmcdbFlatparChains
#' @export McmcdbFlatparChains
NULL
McmcdbFlatparChains <- 
  constrained_data_frame("McmcdbFlatparChains",
                         columns = c(flatpar="factor",
                           chain_id="integer"))

#' @exportClass McmcdbFlatparChainsOrNull
NULL
setClassUnion("McmcdbFlatparChainsOrNull", c("McmcdbFlatparChains", "NULL"))

