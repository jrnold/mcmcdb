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
    ii <- TRUE
  } else {
    if (is.numeric(i)) {
      i <- names(mcmc_flatpars(x))[i]
    } 
    ii <- (colnames(x@samples) %in% as.character(i))
  }
  ## iterations
  if (missing(j)) {
    jj <- TRUE
  } else {
    jj <- (x@iters[["chain_id"]] %in% j)
  }

  if (missing(k)) {
    kk <- TRUE
  } else {
    kk <- (x@iters[["iter"]] %in% k)
  }
  iters <- jj & kk
  ## select 
  xsub <- melt(x@samples[iters, ii, drop=FALSE],
               varnames = c("Var1", "flatpar"),
               value.name = "val")
  if (drop) {
    xsub[["val"]]
  } else {
    chain_iters <- as(x@iters, "data.frame")[iters, , drop=FALSE]
    ## Recycles over parameters
    xsub[["iter"]] <- chain_iters[["iter"]]
    xsub[["chain_id"]] <- chain_iters[["chain_id"]]
    McmcdbSamples(xsub[ , c("flatpar", "chain_id", "iter", "val")])
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
    jj <- TRUE
  } else {
    jj <- x@iters[["chain_id"]] %in% j
  }
  if (missing(k)) {
    kk <- TRUE
  } else {
    kk <- x@iters[["iter"]] %in% k
  }
  if (drop == TRUE) {
    mcmcdb_unflatten(x@samples[jj & kk, ], x@parameters[i])[[1]]
  } else {
    ii <- names(mcmcdb_flatpars(x@parameters[i]))
    xsub <- melt(x@samples[jj & kk, ii, drop=FALSE],
                 varnames = c("Var1", "flatpar"),
                 value.name = "val")
    chain_iters <- as(x@iters, "data.frame")[jj & kk, , drop=FALSE]
    ## Recycles over parameters
    xsub[["iter"]] <- chain_iters[["iter"]]
    xsub[["chain_id"]] <- chain_iters[["chain_id"]]
    McmcdbSamples(xsub[ , c("flatpar", "chain_id", "iter", "val")])
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
