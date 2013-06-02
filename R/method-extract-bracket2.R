#' @include package.R
#' @include class-Mcmcdb.R
#' @include method-mcmcdb_samples_long.R
#' @include method-mcmcdb_samples_parameters.R
#' @include mcmcdb_wide_misc.R
#' @exportMethod [[
NULL


#' @name [[-methods
#' @rdname extract-bracket2-methods
#' @docType methods
#' @title Methods for function \code{[[}
#'
#' @param x An object.
#' @param i \code{character} Paramter array names.
#' @param j \code{integer}. Chain ids.
#' @param k \code{integer}. Iteration numbers.
#' @param drop \code{logical}. Determines the type of object returned.
#'
#' @seealso See \code{\linkS4class{McmcdbWide}} for examples.
#' @return If \code{drop=TRUE}, a matrix is returned, otherwise a \code{data.frame} is returned.
NULL

`[[.Mcmcdb` <- function(x, i, j, k, drop=TRUE) {
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
    mcmcdb_samples_parameters(x, parameters = i, chain_id = j, iter = k)[[i]]
  } else {
    mcmcdb_samples_long(x, parameters = i, chain_id = j, iter = k)
  }
}

#' @rdname extract-bracket2-methods
#' @aliases [[,Mcmcdb-method
#' @family McmcdbWide methods
setMethod("[[", c(x = "Mcmcdb"), `[[.Mcmcdb`)
