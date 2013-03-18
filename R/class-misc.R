#' @include package.R
#' @exportClass McmcdbSamples
#' @exportClass McmcdbChains
#' @exportClass McmcdbIters
#' @exportClass McmcdbFlatparChains
#' @exportClass McmcdbFlatparChainsOrNull
#' @exportClass McmcdbFlatpars
#' @export McmcdbSamples
#' @export McmcdbChains
#' @export McmcdbIters
#' @export McmcdbFlatparChains
#' @export McmcdbFlatpars
NULL

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

McmcdbSamples <- 
  constrained_data_frame("McmcdbSamples",
                         columns=c(flatpar="factor",
                           chain_id="integer",
                           iter="integer",
                           val="numeric"))

McmcdbChains <- 
  constrained_data_frame("McmcdbChains",
                         columns = c(chain_id="integer"))


McmcdbIters <- 
  constrained_data_frame("McmcdbIters",
                         columns = c(chain_id="integer",
                           iter="integer"))

McmcdbFlatparChains <- 
  constrained_data_frame("McmcdbFlatparChains",
                         columns = c(flatpar="factor",
                           chain_id="integer"))

setClassUnion("McmcdbFlatparChainsOrNull", c("McmcdbFlatparChains", "NULL"))

McmcdbFlatpars <-
  constrained_data_frame("McmcdbFlatpars",
                         columns
                         = c(idx = "character",
                           flatpar = "character",
                           pararray = "character"),
                         constraints =
                         list(function(x) {
                           if (nrow(x)) {
                             all(str_matchl(x$idx, "^\\d+(,\\d)*$"))
                           }
                         }))
                         
