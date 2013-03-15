#' @include classes.R
#' @include utilities.R
#' @include package.R
NULL

#' MCMC Samples in long-format
#'
#' Mcmc samples stored as a matrix with (number of chains x number of
#' iterations) rows and (number of flat parameters) columns.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{samples}}{\code{matrix} containing the sample parameter values. This matrix has (number of chains * iterations) rows, and )number of flat parameters) columns.}
#' \item{\code{parameters}}{\linkS4class{McmcParameters}.}
#' \item{\code{chains}}{\linkS4class{McmcChains}.}
#' \item{\code{iters}}{\linkS4class{McmcIters}.}
#' \item{\code{par_chains}}{\linkS4class{McmcParChains}.}
#' \item{\code{metadata}}{\code{list} with general data about the samples.}
#' \item{\code{version}}{\code{character} version of \pkg{mcmcdb} with which the object was created}
#' }
#' 
#' @name McmcWide-class
#' @rdname McmcWide-class
#' @aliases McmcWide-class
#' @docType class
#' @export
#' @examples
#' showClass("McmcdbWide")
setClass("McmcdbWide",
         representation(samples="matrix",
                        parameters="McmcParameters",
                        chains="McmcChains", # chain_id
                        iters="McmcIters", # chain_id, iter
                        par_chains="McmcParChainsOrNull", # parname, chain_id
                        metadata="list",
                        version="character"),
         prototype(samples = matrix(),
                   parameters = McmcParameters(),
                   chains = McmcChains(),
                   iters = McmcIters(),
                   metadata = list(),
                   version = VERSION))

validate_mcmc_wide <- function(object) {
  nsamples <- nrow(object@samples)
  if (nsamples != nrow(object@iters)) {
    return("nrow(object@iters) != nrow(object@samples))")
  }
  if (!all(colnames(object@samples) %in% names(object@parameters@flatpars))) {
    return("Columns in object@samples are missing from object@parameters")
  }
  ## Additional tests
  TRUE
}

setValidity("McmcdbWide", validate_mcmc_wide)
