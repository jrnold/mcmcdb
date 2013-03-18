#' @include package.R
#' @include class-McmcdbParameters.R
#' @include class-McmcdbWide.R
NULL

#' Unflatten MCMC parameters
#'
#' @param x Flattened parameter values
#' @param parameters Object mapping flattened parameters to parameter arrays
#' @param pararrays \code{character} or \code{NULL}. Names of parameter arrays to use.
#' If \code{NULL}, then it will use all parameter arrays that are in \code{parameters}
#' and \code{x}. Any flat parameters implied by the parameter arrays in \code{paramters}
#' which do not appear in the columns of \code{x} will be set to 0 in the returned arrays.
#' @return Named \code{list} of parameter arrays
#'
#' @name mcmcdb_unflatten-method
#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten
#' @export
setGeneric("mcmcdb_unflatten",
           function(x, parameters, ...) {
           standardGeneric("mcmcdb_unflatten")
         })


mcmcdb_unflatten.numeric.McmcdbParameters <- function(x, parameters, pararrays=NULL) {
  if (is.null(pararrays)) {
    flatpars <- mcmcdb_flatpars(parameters)
    if (any(! colnames(x)  %in% names(flatpars))) {
      warning("Some columns of x not it parameters")
    }
    pararrays <- unname(unique(flatpars[intersect(names(flatpars), colnames(x))]))
  }
  llply(parameters@pararrays[pararrays],
        function(pa, x) {
          tmpl <- array(0, pa@dim)
          flatpars <- pa@flatpars
          indices <- t(sapply(flatpars, function(i) parameters@flatpars[[i]]@index))
          tmpl[indices] <- x[flatpars]
          tmpl
        }, x = x)
}


#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,numeric,McmcdbParameters-method
setMethod("mcmcdb_unflatten", c(x = "numeric", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.numeric.McmcdbParameters)

## TODO: how to treat parameters without rows in x
mcmcdb_unflatten.matrix.McmcdbParameters <- function(x, parameters, pararrays=NULL) {
  if (is.null(pararrays)) {
    flatpars <- mcmcdb_flatpars(parameters)
    if (any(! colnames(x)  %in% names(flatpars))) {
      warning("Some columns of x not it parameters")
    }
    pararrays <- unname(unique(flatpars[intersect(names(flatpars), colnames(x))]))
  }
  llply(parameters@pararrays,
        function(pa, x, flatpars) {
          n <- nrow(x)
          tmpl <- array(0, c(pa@dim, n))
          ## TODO. Expand indices
          indices <- laply(pa@flatpars, function(i) flatpars[[i]]@index, .drop = FALSE)
          for (i in seq_len(n)) {
            tmpl[cbind(indices, i)] <- x[i, pa@flatpars, drop=FALSE]
          }
          tmpl
        }, x = x, flatpars = parameters@flatpars)
}

#' @rdname mcmcdb_unflatten-method
#' @aliases mcmcdb_unflatten,matrix,McmcdbParameters-method
setMethod("mcmcdb_unflatten", c(x = "matrix", parameters = "McmcdbParameters"),
          mcmcdb_unflatten.matrix.McmcdbParameters)

