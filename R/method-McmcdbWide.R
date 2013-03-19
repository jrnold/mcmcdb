#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod McmdbWide
NULL

#' @name McmcdbWide-methods
#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide-methods
#' @aliases McmcdbWide
#' @title Create \code{McmcdbWide} objects
#' @description
#' Create \code{\linkS4class{McmcdbWide}} objects.
#'
#' @param x object
#' @return An object of class \code{\linkS4class{McmcdbWide}} objects.
NULL

setGeneric("McmcdbWide",
           function(x, ...) standardGeneric("McmcdbWide"))

McmcdbWide.matrix <- function(x, chains = NULL, iters = NULL,
                              parameters = mcmc_parparser_guess,
                              flatpar_chains = NULL,
                              metadata = list(),
                              parinit = NULL, model_data = NULL) {
  # Attempt to fill in chains or iters if missing
  if (is.null(chains) & is.null(iters)) {
    chains <- McmcdbChains(data.frame(chain_id = 1L))
    iters <- McmcdbIters(data.frame(chain_id = 1L, iter = seq_len(nrow(x))))
  } else if (is.null(chains) & ! is.null(iters)) {
    chains <- McmcdbChains(data.frame(chain_id = unique(iters[["chain_id"]])))
  } else if (!is.null(chains) & is.null(iters)) {
    stop("If chains != NULL, then iters cannot be NULL")
  }
  # if parameters is a function, run it on colnames
  if (is(parameters, "character")) {
    parameters <- match.fun(parameters)
  }
  if (is(parameters, "function")) {
    parameters <- McmcdbParameters(colnames(x), parameters)
  }

  # needed, because list does not extend
  if (!is.null(model_data)) {
    model_data <- as(model_data, "namedList")
  }
  
  new("McmcdbWide", samples = x, chains = chains, iters = iters,
      parameters = parameters,
      flatpar_chains = flatpar_chains, metadata = metadata,
      parinit = parinit, model_data = model_data)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdWide,matrix-method
setMethod("McmcdbWide", "matrix", McmcdbWide.matrix)

#' @rdname McmcdbWide-methods
#' @aliases McmcdWide,data.frame-method
setMethod("McmcdbWide", "data.frame",
          function(x, ...) {
            callGeneric(as(x, "matrix"), ...)
          })

McmcdbWide.mcmc <- function(x, parameters = mcmc_parparser_guess) {
  mcpar <- attr(x, "mcpar")
  chains <- McmcdbChains(data.frame(chain_id = 1L,
                                    n_iter = nrow(x),
                                    iter_start = mcpar[1],
                                    iter_end = mcpar[2],
                                    iter_thin = mcpar[3]))
  iters <- McmcdbIters(data.frame(chain_id = 1L,
                                  iter = seq_len(nrow(x))))
  McmcdbWide(as(x, "matrix"),
             parameters = parameters,
             chains = chains, iters = iters)
}


#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc-method
setMethod("McmcdbWide", "mcmc", McmcdbWide.mcmc)

McmcdbWide.mcmc.list <- function(x, parameters = mcmc_parparser_guess) {
  chains <-
    McmcdbChains(ldply(seq_along(x), 
                      function(i) {
                        mcpar <- attr(x[[i]], "mcpar")
                        data.frame(chain_id = i,
                                   n_iter = nrow(x[[i]]),
                                   iter_start = mcpar[1],
                                   iter_end = mcpar[2],
                                   iter_thin = mcpar[3])
                      }))
  iters <-
    McmcdbIters(ddply(chains, "chain_id",
                      function(x) data.frame(iter = seq_len(x[["n_iter"]]))))
  McmcdbWide(do.call(rbind, x),
             parameters = parameters,
             chains = chains, iters = iters)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc.list-method
setMethod("McmcdbWide", "mcmc.list", McmcdbWide.mcmc.list)
