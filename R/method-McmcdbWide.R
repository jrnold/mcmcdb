#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod McmcdbWide
NULL

#' Create McmcdbWide objects
#'
#' Methods to create \linkS4class{McmcdbWide} objects.
#'
#' @param x Numeric \code{matrix} with parameter sample values. Columns
#' be named with the parameter names.
#' @param parameters \linkS4class{McmcdbParameters} object or a \code{function} that returns
#' a \linkS4class{McmcdbParameters} given \code{colnames(x)}.
#' @param chains \linkS4class{McmcdbChains} object or \code{NULL}.
#' @param iters \linkS4class{McmcdbIters} object or \code{NULL}.
#' @param flatpar_chains \linkS4class{McmcdbFlatparChains} object or \code{NULL}.
#' @param metadata \code{list} with additional data about the MCMC samples.
#' @return An object of class \linkS4class{McmcdbWide}.
#' @seealso \linkS4class{McmcdbWide} for a definition of the class
#' @name McmcdbWide-method
#' @rdname McmcdbWide-method
#' @aliases McmcdbWide-method
NULL
setGeneric("McmcdbWide",
           function(x, ...) {
             standardGeneric("McmcdbWide")
           })

McmcdbWide.matrix <- function(x, parameters=mcmc_parparser_guess,
                              chains=NULL, iters=NULL, flatpar_chains=NULL,
                              metadata = list()) {
  if (is.null(iters)) {
    iters <- McmcdbIters(data.frame(chain_id = 1L,
                                  iter = seq_len(nrow(x))))
  } else if (!is(iters, "McmcdbIters")) {
    iters <- McmcdbIters(iters)
  }

  if (is.null(chains)) {
    chains <- McmcdbChains(data.frame(chain_id = 1L,
                                    start = 1L,
                                    end = nrow(x),
                                    thin = 1L))
  } else if (!is(chains, "McmcdbChains")) {
    chains <- McmcdbChains(chains)
  }
  
  if (is(parameters, "function")) {
    parameters <- mcmc_parse_parnames(colnames(x), parameters)
  }

  if (!is.null(flatpar_chains) & !is(flatpar_chains, "McmcdbFlatparChains")) {
    flatpar_chains <- McmcdbFlatparChains(flatpar_chains)
  }

  new("McmcdbWide",
      samples = as.matrix(x),
      parameters = parameters, chains = chains,
      iters = iters, flatpar_chains = flatpar_chains,
      metadata = metadata)
}


#' @rdname McmcdbWide-method
#' @aliases McmcdbWide,matrix-method
setMethod("McmcdbWide", "matrix", McmcdbWide.matrix)

McmcdbWide.mcmc <- function(x, parameters = mcmc_parparser_guess, ...) {
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
setMethod("McmcdbWide", "mcmc", McmcdbWide.mcmc)

McmcdbWide.mcmc.list <- function(x, parameters = mcmc_parparser_guess, ...) {
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
setMethod("McmcdbWide", "mcmc.list", McmcdbWide.mcmc.list)
