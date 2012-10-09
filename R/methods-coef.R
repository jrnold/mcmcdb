##' Methods for function \code{mean}
##'
##' This method is meant to be equivalent to the \code{coef} method
##' for \code{mle} objects in package \pkg{stats4}, in the sense that
##' it returns a vector point estimate for each parameter. By default,
##' the point estimate returned is the mean.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(object="mcmc")}}{Method for class \code{mcmc}}
##' \item{\code{signature(object="mcmc.list")}}{Method for class \code{mcmc.list}}
##' }
##'
##' @param FUN Function used to calculate the point estimate.
##'
##' @aliases coef,mcmc-method coef,mcmc.list-method.
##' @rdname coef-methods
##' @name coef-methods
NULL

setMethod("coef", "mcmc",
          function(object, FUN="mean", ...) {
              apply(object, 2, FUN)
          })

setMethod("coef", "mcmc.list",
          function(object, FUN="mean", ...) {
              mcmc_iter_column(object, FUN=FUN, ...)
          })

