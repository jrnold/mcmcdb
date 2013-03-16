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
#' \item{\code{parchains}}{\linkS4class{McmcFlatparChains}.}
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
#' 
#' # Example included in the package
#' data("line_mcmcdbwide")
#' print(line_mcmcdbwide)
#' 
#' # access data
#' mcmcdb_chains(line_mcmcdbwide)
#' mcmcdb_chains(line_mcmcdbwide, drop=TRUE)
#' mcmcdb_parameters(line_mcmcdbwide)
#' mcmcdb_iters(line_mcmcdbwide)
#' mcmcdb_parchains(line_mcmcdbwide)
#' mcmcdb_metadata(line_mcmcdbwide)
setClass("McmcdbWide",
         representation(samples="matrix",
                        parameters="McmcParameters",
                        chains="McmcChains", # chain_id
                        iters="McmcIters", # chain_id, iter
                        parchains="McmcFlatparChainsOrNull", # parname, chain_id
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
  cat(sprintf("An object of class %s\n", dQuote("McmcdbWide")))
  nsamples <- nrow(object@samples)
  nchains <- nrow(object@chains)
  cat(sprintf("%d total observations from %d chains\n",
              nsamples, nchains))
  cat(sprintf("Parameter arrays:\n"))
  for (i in names(object@parameters@pararrays)) {
    cat(sprintf("$ %s [%s]\n", i, 
                paste(object@parameters@pararrays[[i]]@dim, collapse=",")))
  }
}

setMethod("show", "McmcdbWide", show_McmcdbWide)


#' Create McmcdbWide objects
#'
#' Methods to create \linkS4class{McmcdbWide} objects.
#'
#' @param x Numeric \code{matrix} with parameter sample values. Columns
#' be named with the parameter names.
#' @param parameters \linkS4class{McmcParameters} object or a \code{function} that returns
#' a \linkS4class{McmcParameters} given \code{colnames(x)}.
#' @param chains \linkS4class{McmcChains} object or \code{NULL}.
#' @param iters \linkS4class{McmcIters} object or \code{NULL}.
#' @param parchains \linkS4class{McmcFlatparChains} object or \code{NULL}.
#' @param metadata \code{list} with additional data about the MCMC samples.
#' @return An object of class \linkS4class{McmcdbWide}.
#' @seealso \linkS4class{McmcdbWide} for a definition of the class
#' @name McmcdbWide-method
#' @rdname McmcdbWide-method
#' @aliases McmcdbWide-method
#' @export
setGeneric("McmcdbWide",
           function(x, ...) {
             standardGeneric("McmcdbWide")
           })

McmcdbWide_matrix <- function(x, parameters=mcmc_parparser_guess,
                              chains=NULL, iters=NULL, parchains=NULL,
                              metadata = list()) {
  if (is.null(iters)) {
    iters <- McmcIters(data.frame(chain_id = 1L,
                                  iter = seq_len(nrow(x))))
  } else if (!is(iters, "McmcIters")) {
    iters <- McmcIters(iters)
  }

  if (is.null(chains)) {
    chains <- McmcChains(data.frame(chain_id = 1L,
                                    start = 1L,
                                    end = nrow(x),
                                    thin = 1L))
  } else if (!is(chains, "McmcChains")) {
    chains <- McmcChains(chains)
  }
  
  if (is(parameters, "function")) {
    parameters <- mcmc_parse_parnames(colnames(x), parameters)
  }

  if (!is.null(parchains) & !is(parchains, "McmcFlatparChains")) {
    parchains <- McmcFlatparChains(parchains)
  }

  new("McmcdbWide",
      samples = as.matrix(x),
      parameters = parameters, chains = chains,
      iters = iters, parchains = parchains,
      metadata = metadata)
}


#' @rdname McmcdbWide-method
#' @aliases McmcdbWide,matrix-method
setMethod("McmcdbWide", "matrix", McmcdbWide_matrix)

McmcdbWide_mcmc <- function(x, parameters = mcmc_parparser_guess, ...) {
  mcpar <- attr(x, "mcpar")
  chains <- data.frame(chain_id = 1L,
                       n_iter = nrow(x),
                       iter_start = mcpar[1],
                       iter_end = mcpar[2],
                       iter_thin = mcpar[3])
  iters <- data.frame(chain_id = 1L,
                      iter = seq_len(nrow(x)))
  McmcdbWide(do.call(rbind, x), parameters = parameters,
             chains = chains, iters = iters)
                       
}


#' @rdname McmcdbWide-method
#' @aliases McmcdbWide,mcmc-method
setMethod("McmcdbWide", "mcmc", McmcdbWide_mcmc)

McmcdbWide_mcmc_list <- function(x, parameters = mcmc_parparser_guess, ...) {
  chains <- ldply(seq_along(x), 
                  function(i) {
                    mcpar <- attr(x[[i]], "mcpar")
                    data.frame(chain_id = i,
                               n_iter = nrow(x[[i]]),
                               iter_start = mcpar[1],
                               iter_end = mcpar[2],
                               iter_thin = mcpar[3])
                  })
  iters <- ddply(chains, "chain_id",
                 function(x) data.frame(iter = seq_len(x[["n_iter"]])))
  McmcdbWide(as.matrix(do.call(rbind, x)), parameters = parameters,
             chains = chains, iters = iters)
}

#' @rdname McmcdbWide-method
#' @aliases McmcdbWide,mcmc.list-method
setMethod("McmcdbWide", "mcmc.list", McmcdbWide_mcmc_list)
