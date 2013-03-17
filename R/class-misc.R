#' Miscellaneous classes
#'
#' Various classes used internally in this package.
#' These are all subclasses of \linkS4class{DataFrameConstr},
#' i.e. \code{data.frame} objects with required
#' columns.
#'
#' @name misc-classes
#' @rdname misc-classes
#' @aliases McmcdbSamples-class
#' @aliases McmcdbSamples
#' @aliases McmcdbChains-class
#' @aliases McmcdbChains
#' @aliases McmcdbIters-class
#' @aliases McmcdbIters
#' @aliases McmcdbFlatparChains-class
#' @aliases McmcdbFlatparChains
#' @aliases McmcdbFlatparChainsOrNull-class
#' @keywords classes
#' @docType class
#' @examples
#' showClass("McmcdbSamples")
#' showClass("McmcdbChains")
#' showClass("McmcdbIters")
#' showClass("McmcdbFlatparChains")
NULL

#' @exportClass McmcdbSamples
#' @export McmcdbSamples
NULL
McmcdbSamples <- 
  constrained_data_frame("McmcdbSamples",
                         columns=c(flatpar="factor",
                           chain_id="integer",
                           iter="integer",
                           val="numeric"))

# -----------------------

#' @exportClass McmcdbChains
#' @export McmcdbChains
NULL
McmcdbChains <- 
  constrained_data_frame("McmcdbChains",
                         columns = c(chain_id="integer"))

# -----------------------
#' @exportClass McmcdbIters
#' @export McmcdbIters
NULL
McmcdbIters <- 
  constrained_data_frame("McmcdbIters",
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

