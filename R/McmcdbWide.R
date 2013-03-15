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
#' @name McmcdbWide-class
#' @rdname McmcdbWide-class
#' @aliases McmcdbWide-class
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

validate_McmcdbWide <- function(object) {
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

setValidity("McmcdbWide", validate_McmcdbWide)

show_McmcdbWide <- function(object) {
  cat(sprintf("Object of class %s\n", dQuote("McmcdbWide")))
  nsamples <- nrow(object@samples)
  nchains <- nrow(object@chains)
  cat(sprintf("%d total observations from %d chains\n",
              nsamples, nchains))
  cat(sprintf("Parameter arrays:\n"))
  for (i in names(object@parameters@pararrays)) {
    cat("$ %s (%s)\n", i, 
        paste(object@parameters@pararrays[[i]]@dim, collapse=","))
  }
}

setMethod("show", "McmcdbWide", show_McmcdbWide)

#' McmcdbWide
#'
#' Function to create \linkS4code{McmcdbWide} objects.
#'
#' @param x Numeric \code{matrix} with parameter sample values. Columns
#' be named with the parameter names.
#' @param parameters \linkS4class{McmcParameters} object or a \code{function} that returns
#' a \linkS4class{McmcParameters} given \code{colnames(x)}.
#' @param chains \linkS4class{McmcChain} object or \code{NULL}.
#' @param iters \linkS4class{McmcIters} object or \code{NULL}.
#' @param parchains \linkS4class{McmcParChains} object or \code{NULL}.
#' @return An object of class \linkS4class{McmcdbWide}.
#' @seealso \linkS4class{McmcdbWide}
#' @export 
function <- McmcdbWide(x, parameters, chains=NULL, iters=NULL, parchains=NULL) {
  if (is.null(chains)) {
    chains <- McmcChains(data.frame(chain_id = 1L,
                                    start = 1L,
                                    end = nrow(x),
                                    thin = 1L))
  } else if (!is(chains, "McmcChains")) {
    chains <- McmcChains(chains)
  }
  
  if (is.null(iters)) {
    iters <- McmcIters(data.frame(chain_id = 1L,
                                  iter = seq_len(nrow(x))))
  } else if (!is(iters, "McmcIters")) {
    iters <- McmcIters(iters)
  }
  
  if (is(parameters, "function")) {
    parameters <- mcmc_parse_parnames(colnames(x), parameters)
  }

  if (!is.null(parchains) & !is(parchains, "McmcParChains")) {
    parchains <- McmcParchains(parchains)
  }

  new("McmcdbWide",
      samples = as.matrix(x),
      parameters = parameters, chains = chains,
      iters = iters, parchains = parchains)
}
