#' @include package.R
#' @include mcmc_parnames.R
#' @exportMethod mcmcdb_flatten
NULL

#' @name mcmcdb_flatten-methods
#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten-methods
#' @title Flatten Array of MCMC samples to a vector
#' @description Flatten an array of MCMC samples
#' to a numeric vector. As with R, the samples
#' are flattened in column-major order.
#'
#' Currently these methods only handle objects representing
#' a single iteration.
#" 
#' @param x object
#' @param parname \code{character} Parameter array name.
#' @param FUN \code{function} Function with arguments
#' \code{name} and \code{dim} and returns a vector of names
#' to use.
#' @return Named \code{numeric} vector
setGeneric("mcmcdb_flatten",
           function(x, ...) standardGeneric("mcmcdb_flatten"))


# setMethod("mcmcdb_flatten", "array")
mcmcdb_flatten.array <- function(x, parname="Par", FUN=mcmc_parnames_stan) {
  xparnames <- FUN(parname, dim(x))
  dim(x) <- NULL
  names(x) <- xparnames
  x
}

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,array-method
setMethod("mcmcdb_flatten", "array", mcmcdb_flatten.array)

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,numeric-method
setMethod("mcmcdb_flatten", "numeric",
          function(x, ...) {
            mcmcdb_flatten(as(x, "array"), ...)
          })

mcmcdb_flatten.list <- function(x, FUN = mcmc_parnames_stan, ...) {
  if(is.null(names(x))) {
    params <- paste0("Par", 1:length(x))
  } else {
    params <- names(x)
  }
  
  ilist <- seq_len(length(x))
  flatten_el <- function(i) {
    parname <- params[i]
    mcmcdb_flatten(x[[i]], parname = parname, FUN = FUN)
  }
  ret <- llply(ilist, flatten_el, ...)
  do.call(c, unname(ret))
}

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,list-method
setMethod("mcmcdb_flatten", "list", mcmcdb_flatten.list)

mcmcdb_flatten.mcarray <- function(x, parname="Par", FUN=mcmc_parnames_bugs) {
  x <- as(x, "array")
  ndim <- length(dim(x))
  array_dim <- dim(x)[1:(ndim - 2)]
  chain_iter_dim <- dim(x)[(ndim - 1):ndim]
  dim(x) <- c(prod(array_dim), prod(chain_iter_dim))
  rownames(x) <- FUN(parname, array_dim)
  t(x)
}

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,mcarray-method
setMethod("mcmcdb_flatten", "mcarray", mcmcdb_flatten.mcarray)

mcmcdb_flatten.McarrayList <- function(x, FUN=mcmc_parnames_bugs, ...) {
  parnames <- names(x)
  flatten_el <- function(i) {
    mcmcdb_flatten(x[[i]], parname = parnames[i], FUN = FUN)
  }
  do.call(cbind, unname(llply(seq_len(length(x)), flatten_el, ...)))
}

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,McarrayList-method
setMethod("mcmcdb_flatten", "McarrayList", mcmcdb_flatten.McarrayList)
