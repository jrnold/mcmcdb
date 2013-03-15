#' MCMC Samples in long-format
#'
#' Mcmc samples stored as a matrix with (number of chains x number of
#' iterations) rows and (number of flat parameters) columns.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{samples}}{\code{matrix}.}
#' \item{\code{parameters}}{\code{McmcParameters}.}
#' \item{\code{chains}}{\code{McmcChains}.}
#' \item{\code{par_chains}}{\code{McmcParChains}.}
#' \item{\code{chain_iters}}{\code{McmcChainIters}.}
#' \item{\code{metadata}}{\code{list} with general data about the samples.}
#' \item{\code{version}}{\code{character} version of \pkg{mcmcdb} with which the object was created}
#' }
#' 
#' @name McmcWide-class
#' @rdname McmcWide-class
#' @aliases McmcWide-class
#' @docType class
#' @export
setClass("McmcWide",
         representation(samples="matrix",
                        parameters="McmcParameters",
                        chains="McmcChains", # chain_id
                        chain_iters="McmcChainIters", # chain_id, iter
                        par_chains="McmcParChainsOrNull", # parname, chain_id
                        metadata="list",
                        version="character"))

validate_mcmc_wide <- function(object) {
  nsamples <- nrow(object@samples)
  if (nsamples != length(object@iterations)) {
    return("length(object@iterations) != nrow(object@samples))")
  }
  if (nsamples != length(object@chains)) {
    return("length(object@chains) != nrow(object@samples))")
  }
  ## Additional tests
  TRUE
}

setValidity("McmcWide", validate_mcmc_wide)
