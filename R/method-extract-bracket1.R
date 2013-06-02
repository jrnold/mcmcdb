#' @include package.R
#' @include class-McmcdbWide.R
#' @include method-mcmcdb_samples_long.R
#' @include method-mcmcdb_samples_parameters.R
#' @exportMethod [
NULL

#' @name [-methods
#' @rdname extract-bracket1-methods
#' @docType methods
#' @title Methods for function \code{[}
#'
#' @param x An object.
#' @param i \code{character} Flat parameter names.
#' @param j \code{integer}. Chain ids.
#' @param k \code{integer}. Iteration numbers.
#' @param drop \code{logical}. Determines the type of object returned.
#'
#' @seealso See \code{\linkS4class{McmcdbWide}} for examples.
#' @return If \code{drop=TRUE}, a matrix is returned, otherwise a \code{data.frame} is returned.
NULL

# Define this for Mcmcdb virtual class since it only depends on
# mcmcdb_samples_long and mcmcdb_samples_flatpars
`[.Mcmcdb` <- function(x, i, j, k, drop=TRUE) {
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

#' @rdname extract-bracket1-methods
#' @aliases [,Mcmcdb,ANY,ANY-method
#' @family McmcdbWide methods
setMethod("[", c(x = "Mcmcdb", i="ANY", j="ANY"), `[.Mcmcdb`)
