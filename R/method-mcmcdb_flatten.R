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
NULL
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
            callGeneric(as(x, "array"), ...)
          })

#' @rdname mcmcdb_flatten-methods
#' @aliases mcmcdb_flatten,numeric-method
setMethod("mcmcdb_flatten", "list",
          function(x, FUN=mcmc_parnames_stan, ...) {
            if(is.null(names(x))) {
              parameternames <- paste0("Par", 1:length(x))
            } else {
              parameternames <- names(x)
            }

            flatten_el <- function(i) {
              parname <- parameternames[i]
              mcmcdb_flatten(x[[i]], parname=parname, FUN=FUN)
            }
            do.call(c, unname(llply(seq_len(length(x)), flatten_el, ...)))
          })


