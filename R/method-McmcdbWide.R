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
#' @param object An object for which a method is available.
#' @return An object of class \code{\linkS4class{McmcdbWide}} objects.
setGeneric("McmcdbWide",
           function(object, ...) standardGeneric("McmcdbWide"))

McmcdbWide.McmcdbWide <- function(object, ...) {
  new("McmcdbWide", object, ...)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,McmcdbWide-method
setMethod("McmcdbWide", "McmcdbWide", McmcdbWide.McmcdbWide)

McmcdbWide.matrix <- function(object, chains = NULL, iters = NULL,
                              parameters = mcmc_parparser_guess,
                              flatpar_chains = NULL,
                              metadata = list(),
                              model_data = list()) {

  isany <- function(object, class) {
    any(sapply(class, function(class2) is(object, class2)))
  }
  valid_parameter_classes <- c("character", "function", "McmcdbParameters")
  if (! isany(parameters, valid_parameter_classes)) { 
    stop(sprintf("%s must be an object of class: %s",
                 sQuote("parameters"), paste(dQuote(valid_parameter_classes)), ","))
  }
  
  # Attempt to fill in chains or iters if missing
  if (is.null(chains) & is.null(iters)) {
    chains <- McmcdbChains(data.frame(chain_id = 1L))
    iters <- McmcdbIters(data.frame(chain_id = 1L, iter = seq_len(nrow(object))))
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
    parameters <- McmcdbParameters(colnames(object), parameters)
  }
  if (is.null(flatpar_chains)) {
    flatpar_chains <- expand.grid(flatpar = colnames(object),
                                  chain_id = chains[["chain_id"]])
    flatpar_chains[["init"]] <- NA_real_
    flatpar_chains <- McmcdbFlatparChains(flatpar_chains)
  }
  new("McmcdbWide", samples = object, chains = chains, iters = iters,
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
          function(object, ...) {
            callGeneric(as(object, "matrix"), ...)
          })

McmcdbWide.mcmc <- function(object, parameters = mcmc_parparser_guess) {
  mcpar <- attr(object, "mcpar")
  chains <-
    McmcdbChains(chain_id = 1L,
                 n_iter = nrow(object),
                 iter_start = mcpar[1],
                 iter_end = mcpar[2],
                 iter_thin = mcpar[3])
  iters <- McmcdbIters(chain_id = 1L,
                       iter = seq_len(nrow(object)))
  McmcdbWide(as(object, "matrix"),
             parameters = parameters,
             chains = chains,
             iters = iters)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc-method
setMethod("McmcdbWide", "mcmc", McmcdbWide.mcmc)

McmcdbWide.mcmc.list <- function(object, parameters = mcmc_parparser_guess) {
  chains <-
    new("McmcdbChains", 
        ldply(seq_along(object), 
              function(i) {
                mcpar <- attr(object[[i]], "mcpar")
                data.frame(chain_id = i,
                           n_iter = nrow(object[[i]]),
                           iter_start = mcpar[1],
                           iter_end = mcpar[2],
                           iter_thin = mcpar[3])
              }))
  iters <-
    new("McmcdbIters", 
        ddply(chains, "chain_id",
              function(object) data.frame(iter = seq_len(object[["n_iter"]]))))
  
  McmcdbWide(do.call(rbind, object),
             parameters = parameters,
             chains = chains,
             iters = iters)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcmc.list-method
setMethod("McmcdbWide", "mcmc.list", McmcdbWide.mcmc.list)

McmcdbWide.mcarray <- function(object, parname = "Par") {
  d <- dim(object)
  nchains <- dim(object)["chain"]
  niter <- dim(object)["iteration"]
  chains <- McmcdbChains(chain_id = seq_len(nchains))
  iters <-
    McmcdbIters(chain_id = rep(seq_len(nchains), each=niter),
                iter = rep(seq_len(niter), nchains))
  McmcdbWide(mcmcdb_flatten(object, parname, FUN = mcmc_parnames_bugs),
             chains = chains, iters = iters,
             parameters = mcmc_parparser_bugs)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,mcarray-method
setMethod("McmcdbWide", "mcarray", McmcdbWide.mcarray)

McmcdbWide.McarrayList <- function(object) {
  d <- dim(object[[1]])
  nchains <- dim(object[[1]])["chain"]
  niter <- dim(object[[1]])["iteration"]
  chains <- McmcdbChains(chain_id = seq_len(nchains))
  iters <-
    McmcdbIters(chain_id = rep(seq_len(nchains), each=niter),
                iter = rep(seq_len(niter), nchains))
  samples <- mcmcdb_flatten(object, FUN = mcmc_parnames_bugs)
  McmcdbWide(samples, 
             chains = chains, iters = iters,
             parameters = mcmc_parparser_bugs)
}

#' @rdname McmcdbWide-methods
#' @aliases McmcdbWide,McarrayList-method
setMethod("McmcdbWide", "McarrayList", McmcdbWide.McarrayList)

