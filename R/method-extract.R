#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_unflatten.R
#' @include method-mcmcdb_flatpars.R
#' @exportMethod [
#' @exportMethod [[
#' @exportMethod $
NULL

#' @rdname extract-methods
#' @name extract-methods
#' @docType methods
#' @title Extract
#'
#' @description The methods \code{[}, \code{[[} and \code{$} are
#' defined for \code{\linkS4class{McmcdbWide}} objects
#' to be able to select sample values by parameter (both
#' flat parameters and parameter arrays), chain, and iteration.
#'
#' @param i \code{character} Parameter name. For \code{[}, the flattened parameter name. For \code{[[}
#' and \code{$}, the parameter array name.
#' @param j \code{integer}. Chain number.
#' @param k \code{integer}. Iteration number.
#' @param drop \code{logical}. Determines the object returned. In general, if
#' @return If  \code{drop=TRUE}, then a vector or array is returned, otherwise a \code{data.frame} is returned.
NULL

`[.McmcdbWide` <- function(x, i, j, k, ..., drop=TRUE) {
  ## flatpar indices
  if (missing(i)) {
    i <- NULL
  }
  if (missing(j)) {
    j <- NULL
  }
  if (missing(k)) {
    k <- NULL
  }
  samples <- subset_mcmcdb_wide(x, flatpars = i, chain_id = j, iter = k)
  if (drop) {
    samples
  } else {
    chain_iters <- x@iters[select_iters(x, chain_id = j, iter = k), ]
    samples <-cbind(chain_iters, samples)
    melt(mcmcdb_samples, id.vars = c("chain_id", "iter"))
  }
}

#' @rdname extract-methods
#' @aliases [,McmcdbWide,ANY,ANY-method
#' @family McmcdbWide methods
setMethod("[", c(x = "McmcdbWide", i="ANY", j="ANY"), `[.McmcdbWide`)

###########################################################################

`[[.McmcdbWide` <- function(x, i, j, k, drop=TRUE) {
  # parameter array name
  if (length(i) > 1) {
    warning("length(i) > 1. Only first element will be used.")
    i <- i[1]
  }
  # iterations
  if (missing(j)) {
    j <- NULL
  } 
  if (missing(k)) {
    k <- NULL
  }
  samples <- subset_mcmcdb_wide(x, pararrays = i, chain_id = j, iter = k)
  if (drop == TRUE) {
    mcmcdb_unflatten(samples, x@parameters[i])[[1]]
  } else {
    chain_iters <- x@iters[select_iters(x, chain_id = j, iter = k), ]
    samples <-cbind(chain_iters, samples)
    melt(mcmcdb_samples, id.vars = c("chain_id", "iter"))
  }
}

#' @rdname extract-methods
#' @aliases [[,McmcdbWide-method
setMethod("[[", c(x = "McmcdbWide"), `[[.McmcdbWide`)

##########################################################################

`$.McmcdbWide` <- function(x, name) {
  mcmcdb_unflatten(x, name)[[1]]
}

#' @rdname extract-methods
#' @aliases $,McmcdbWide-method
setMethod("$", c(x="McmcdbWide"), `$.McmcdbWide`)
