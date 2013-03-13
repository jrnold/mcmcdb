##' MCMC Samples in long-format
##'
##' Mcmc samples stored as a matrix with (number of chains x number of
##' iterations) rows and (number of flat parameters) columns.
##' 
##' @name McmcWide-class
##' @rdname McmcWide-class
##' @aliases McmcWide-class
##' @docType class
##' @export
setClass("McmcWide",
         representation(samples="matrix",
                        iterations="integer",
                        chains="integer",
                        parnames="McmcParnames",
                        pararrays="McmcPararrays",
                        chains="McmcChains", # chain_id
                        par_chains="McmcParChainsOrNull", # parname, chain_id
                        chain_iters="McmcChainItersOrNull", # chain_id, iter
                        metadata="list"))

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
