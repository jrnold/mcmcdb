#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod McmcdbWide
NULL

#' @name McmcdbWide-methods
#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide-methods
#' @aliases McmcdbWide
#' @title Create \code{McmcdbWide} objects
#' @description
#' Create \code{\linkS4class{McmcdbWide}} objects.
#'
#' @param x An object for which a method is available.
#' @return An object of class \code{\linkS4class{McmcdbWide}} objects.
#' @examples
#' \dontrun{
#' # Convert stanfit object
#' library(rstan)
#' scode <- "
#'      parameters {
#'        real y[2]; 
#'      } 
#'      model {
#'        y[1] ~ normal(0, 1);
#'        y[2] ~ double_exponential(0, 2);
#'      } 
#'      "
#' fit1 <- stan(model_code = scode, iter = 10, verbose = FALSE)
#' fit2 <- McmcdbWide(fit1)
#' }
setGeneric("McmcdbWide",
           function(x, ...) standardGeneric("McmcdbWide"))

McmcdbWide.matrix <- function(x, chains = NULL, iters = NULL,
                              parameters = mcmc_parparser_guess,
                              flatpar_chains = NULL,
                              metadata = list(),
                              model_data = list()) {
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
  if (is.null(flatpar_chains)) {
    flatpar_chains <- expand.grid(flatpar = colnames(x),
                                  chain_id = chains[["chain_id"]])
    flatpar_chains[["init"]] <- NA_real_
    flatpar_chains <- McmcdbFlatparChains(flatpar_chains)
  }
  new("McmcdbWide", samples = x, chains = chains, iters = iters,
      parameters = parameters,
      flatpar_chains = flatpar_chains, metadata = metadata,
      model_data = as(model_data, "namedList"))
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,matrix-method
setMethod("McmcdbWide", "matrix", McmcdbWide.matrix)

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,data.frame-method
setMethod("McmcdbWide", "data.frame",
          function(x, ...) {
            callGeneric(as(x, "matrix"), ...)
          })

McmcdbWide.mcmc <- function(x, parameters = mcmc_parparser_guess) {
  mcpar <- attr(x, "mcpar")
  chains <-
    McmcdbChains(chain_id = 1L,
                 n_iter = nrow(x),
                 iter_start = mcpar[1],
                 iter_end = mcpar[2],
                 iter_thin = mcpar[3])
  iters <- McmcdbIters(chain_id = 1L,
                       iter = seq_len(nrow(x)))
  McmcdbWide(as(x, "matrix"),
             parameters = parameters,
             chains = chains,
             iters = iters)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc-method
setMethod("McmcdbWide", "mcmc", McmcdbWide.mcmc)

McmcdbWide.mcmc.list <- function(x, parameters = mcmc_parparser_guess) {
  chains <-
    new("McmcdbChains", 
        ldply(seq_along(x), 
              function(i) {
                mcpar <- attr(x[[i]], "mcpar")
                data.frame(chain_id = i,
                           n_iter = nrow(x[[i]]),
                           iter_start = mcpar[1],
                           iter_end = mcpar[2],
                           iter_thin = mcpar[3])
              }))
  iters <-
    new("McmcdbIters", 
        ddply(chains, "chain_id",
              function(x) data.frame(iter = seq_len(x[["n_iter"]]))))
  
  McmcdbWide(do.call(rbind, x),
             parameters = parameters,
             chains = chains,
             iters = iters)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc.list-method
setMethod("McmcdbWide", "mcmc.list", McmcdbWide.mcmc.list)

McmcdbWide.stanfit <- function(x) {
  samples <-
    do.call(rbind, llply(x@sim[["samples"]],
                         function(y) do.call(cbind, y)))

  chains <-
    ldply(x@sim[["samples"]],
          function(DF) {
            y <- as.data.frame(Filter(Negate(is.null), attr(DF, "args")))
            y[["adaptation_info"]] <- attr(DF, "adaptation_info")
            y
          })

  iters <- 
    mdply(chains[ , c("chain_id", "iter_save", "warmup")],
          function(chain_id, iter_save, warmup) {
            data.frame(chain_id = chain_id,
                       iter = seq_len(iter_save),
                       warmup = (seq_len(iter_save) <= warmup))
          })

  sampler_params <- 
    ldply(x@sim[["samples"]],
          function(x) {
            as.data.frame(attr(x, "sampler_params"))
          })
  iters <- cbind(iters, sampler_params)

  flatpar_chains <-
    expand.grid(flatpar = as.character(colnames(samples)),
                chain_id = chains[["chain_id"]])
  flatpar_chains[["init"]] <- NA_real_

  metadata <- list()
  metadata[["model_name"]] <- x@model_name
  metadata[["date"]] <- x@date
  metadata[["stanmodel"]] <- x@stanmodel
  
  McmcdbWide(samples,
             parameters = mcmc_parparser_stan,
             flatpar_chains = McmcdbFlatparChains(flatpar_chains),
             chains = McmcdbChains(chains),
             iters = McmcdbIters(iters),
             metadata = metadata)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,stanfit-method
setMethod("McmcdbWide", "stanfit", McmcdbWide.stanfit)

