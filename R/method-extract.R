#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_unflatten.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod [
#' @exportMethod [[
#' @exportMethod $
NULL

#' @rdname extract-methods
#' @name extract-methods
#' @docType methods
#' @title Extract methods
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
#' @return If \code{drop=TRUE}, a matrix is returned, otherwise a \code{data.frame} is returned.
NULL

`[.McmcdbWide` <- function(x, i, j, k, drop=TRUE) {
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
  if (drop == TRUE) {
    mcmcdb_samples_flatpars(x, flatpars = i, chain_id = j, iter = k)
  } else {
    mcmcdb_samples_long(x, flatpars = i, chain_id = j, iter = k)
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
  if (drop == TRUE) {
    mcmcdb_samples_pararrays(x, pararrays = i, chain_id = j, iter = k)[[i]]
  } else {
    mcmcdb_samples_long(x, pararrays = i, chain_id = j, iter = k)
  }
}

#' @rdname extract-methods
#' @aliases [[,McmcdbWide-method
setMethod("[[", c(x = "McmcdbWide"), `[[.McmcdbWide`)

##########################################################################

`$.McmcdbWide` <- function(x, name) {
  x[[name]]
}

#' @rdname extract-methods
#' @aliases $,McmcdbWide-method
setMethod("$", c(x="McmcdbWide"), `$.McmcdbWide`)
