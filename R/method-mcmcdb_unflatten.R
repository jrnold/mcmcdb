#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-McmcdbWide.R
#' @exportMethod mcmcdb_unflatten
NULL

#' @name mcmcdb_unflatten-method
#' @rdname mcmcdb_unflatten-method
#' @keywords methods
#' @aliases mcmcdb_unflatten
#' 
#' @title Unflatten MCMC parameters
#'
#' @description Convert parameter values from their flattened form to their original
#' array shapes.
#'
#' @param x Flattened parameter values
#' @param parameters Object mapping flattened parameters to parameter arrays
#' 
#' @return All methods return a named \code{list} of parameter arrays.
#'
#' @section Methods:
#' 
#' \describe{
#' \item{\code{signature(x = "matrix", parameters = "function")}}{
#' Each row if the matrix represents a sample interation. The returned array for
#' a parameter array with dimension \code{d} is \code{c(d, n)}, where
#' \code{n} is the number of iterations.  The function
#' \code{parameters} should return an object of class
#' \code{McmcdbFlatpars}.
#' }
#' 
#' \item{\code{signature(x = "matrix", parameters = "McmcdbParameters")}}{
#' See method for \code{signature(x = "matrix", parameters = "McmcdbParameters")}.
#' }
#' 
#' \item{\code{signature(x = "matrix", parameters = "missing")}}{
#'     If \code{parameters} is missing, then the default for \code{\link{McmcdbParameters}} is used.
#' }
#' 
#' \item{\code{signature(x = "McmcdbWide", parameters = "character")}}{
#'   The character vector \code{parameters} specifies a subset of parameter arrays to return.}
#' 
#' \item{\code{signature(x = "McmcdbWide", parameters = "missing")}}{
#'   Uses the result of \code{mcmcdb_parameters(x)} as the parameter object.
#' }
#' 
#' \item{\code{signature(x = "numeric", parameters = "function")}}{
#'   The numeric vector represents a single iteration.
#' }
#' 
#' \item{\code{signature(x = "numeric", parameters = "McmcdbParameters")}}{
#'  See method for \code{signature(x = "numeric", parameters = "function")}.
#' }
#' 
#' \item{\code{signature(x = "numeric", parameters = "missing")}}{
#'  See method for \code{signature(x = "numeric", parameters = "function")}.
#' }
#' }
#' 
#' @examples
#' parnames <- c("alpha", "beta.1.1", "beta.1.2", "beta.2.1", "beta.2.2")
#' parameters <- McmcdbParameters(parnames)
#' samples <- rnorm(length(parnames))
#' names(samples) <- parnames
#' samples
#' mcmcdb_unflatten(samples)
setGeneric("mcmcdb_unflatten",
           function(x, parameters, ...) {
           standardGeneric("mcmcdb_unflatten")
         })

mcmcdb_unflatten.numeric.McmcdbParameters <- function(x, parameters, ...) {
  # bound: x
  unflatten_one_par <- function(param) {
    y <- x[as.character(param)]
    d <- dim(param)
    if (!is.null(d)) {
      y <- array(y, d)
    }
    y
  }
  ret <- llply(as(parameters, "list"), unflatten_one_par, ...)
  names(ret) <- names(parameters)
  ret
}

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,McmcdbParameters-method
#' @family McmcdbParameters methosd
setMethod("mcmcdb_unflatten",
          c(x = "numeric", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.numeric.McmcdbParameters)

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,function-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "numeric", parameters = "function"),
          function(x, parameters, ...)
          callGeneric(x, McmcdbParameters(names(x), parameters), ...))

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,missing-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "numeric", parameters = "missing"),
          function(x, parameters, ...)
          callGeneric(x, McmcdbParameters(names(x)), ...))

mcmcdb_unflatten.matrix.McmcdbParameters <- function(x, parameters, ...) {
  # bound: x
  unflatten_one_par <- function(param) {
    d <- dim(param)
    n <- nrow(x)
    # put iterations into columns to reshape
    xpar <- t(x[ , as.character(param)])
    if (!is.null(d)) {
      dim(xpar) <- c(d, n)
    }
    xpar
  }
  ret <- llply(as(parameters, "list"), unflatten_one_par, ...)
  names(ret) <- names(parameters)
  ret
}

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,McmcdbParameters-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.matrix.McmcdbParameters)

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,function-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "function"),
          function(x, parameters, ...) {
            callGeneric(x, McmcdbParameters(colnames(x), parameters), ...)
          })

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,missing-method
#' @family McmcdbParameters methods
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "missing"),
          function(x, parameters, ...) {
            callGeneric(x, McmcdbParameters(colnames(x)), ...)
          })

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,McmcdbWide,missing-method
#' @family McmcdbWide methods
setMethod("mcmcdb_unflatten", c(x = "McmcdbWide", parameters = "missing"),
          function(x, parameters, .iter=NULL, .chain_id=NULL, ...) {
            callGeneric(x, names(x@parameters), .iter=.iter, .chain_id=.chain_id, ...)
          })

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,McmcdbWide,character-method
#' @family McmcdbWide methods
setMethod("mcmcdb_unflatten", c(x = "McmcdbWide", parameters = "character"),
          function(x, parameters, .iter=NULL, .chain_id=NULL, ...) {
            callGeneric(mcmcdb_wide_subset(x, parameters = parameters,
                                           chain_id = .chain_id,
                                           iter = .iter), 
                        x@parameters[parameters], ...)
          })


