#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_unflatten
NULL

#' @name mcmcdb_unflatten-method
#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten
#' 
#' @title Unflatten MCMC parameters
#'
#' @description Convert parameter values from their flattened form to their original
#' array shapes.
#'
#' @param x Flattened parameter values
#' @param parameters Object mapping flattened parameters to parameter arrays
#' @param pararrays \code{character} or \code{NULL}. Names of parameter arrays to use.
#' If \code{NULL}, then it will use all parameter arrays that are in \code{parameters}
#' and \code{x}. Any flat parameters implied by the parameter arrays in \code{paramters}
#' which do not appear in the columns of \code{x} will be set to 0 in the returned arrays.
#' 
#' @return Named \code{list} of parameter arrays
setGeneric("mcmcdb_unflatten",
           function(x, parameters, ...) {
           standardGeneric("mcmcdb_unflatten")
         })

mcmcdb_unflatten.numeric.McmcdbParameters <- function(x, parameters) {
  # bound: x
  unflatten_one_par <- function(param) {
    d <- dim(param)
    array(x[as.character(param)], d)
  }
  ret <- llply(as(parameters, "list"), unflatten_one_par)
  names(ret) <- names(parameters)
  ret
}

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,McmcdbParameters-method
#' @family McmdbParameters methosd
setMethod("mcmcdb_unflatten",
          c(x = "numeric", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.numeric.McmcdbParameters)

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,function-method
#' @family McmdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "numeric", parameters = "function"),
          function(x, parameters) callGeneric(x, McmcdbParameters(names(x), parameters)))

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,missing-method
#' @family McmdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "numeric", parameters = "missing"),
          function(x, parameters) callGeneric(x, McmcdbParameters(names(x))))

mcmcdb_unflatten.matrix.McmcdbParameters <- function(x, parameters) {
  # bound: x
  unflatten_one_par <- function(param) {
    d <- dim(param)
    n <- nrow(x)
    # put iterations into columns to reshape
    xpar <- t(x[ , as.character(param)])
    dim(xpar) <- c(d, n)
    xpar
  }
  ret <- llply(as(parameters, "list"), unflatten_one_par)
  names(ret) <- names(parameters)
  ret
}

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,McmcdbParameters-method
#' @family McmdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.matrix.McmcdbParameters)

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,function-method
#' @family McmdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "function"),
          function(x, parameters) callGeneric(x, McmcdbParameters(colnames(x), parameters)))

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,missing-method
#' @family McmdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "missing"),
          function(x, parameters) callGeneric(x, McmcdbParameters(colnames(x))))

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,McmcdbWide,missing-method
#' @family McmdbWide methods
setMethod("mcmcdb_unflatten", c(x = "McmcdbWide", parameters = "missing"),
          function(x, parameters) callGeneric(x@samples, x@parameters))

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,McmcdbWide,character-method
#' @family McmdbWide methods
setMethod("mcmcdb_unflatten", c(x = "McmcdbWide", parameters = "character"),
          function(x, parameters) callGeneric(x@samples, x@parameters[parameters]))


