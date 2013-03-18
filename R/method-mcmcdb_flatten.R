#' @include package.R
#' @exportMethod mcmcdb_flatten
NULL

#' @name mcmcdb_flatten-methods
#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten-methods
#' @title Flatten Array of MCMC samples to a vector
#' @description Flatten an array of MCMC samples
#' to a numeric vector. As with R, the samples
#' are flattened in column-major order.
#" 
#' @param x object
#' @param parname \code{character} Parameter array name.
#' @param FUN \code{function} Function with arguments
#' \code{name} and \code{dim} and returns a vector of names
#' to use.
setGeneric("mcmcdb_flatten",
           function(x, ...) standardGeneric("mcmcdb_flatten"))


# setMethod("mcmcdb_flatten", "array")
mcmcdb_flatten.array <- function(x, parname="x", FUN=mcmc_parnames_stan) {
  xparnames <- FUN(dim(x))
  dim(x) <- NULL
  names(x) <- xparnames
  x
}

# setMethod("mcmcdb_flatten", "array")
mcmcdb_flatten.array <- function(x, parname="x", FUN=mcmc_parnames_stan) {
  xparnames <- FUN(dim(x))
  dim(x) <- NULL
  names(x) <- xparnames
  x
}

foo <- matrix(1:10, nrow=5)
foo
dim(foo) <- NULL
foo

foo <- array(1:24, c(2, 3, 4))
foo
dim(foo) <- NULL


