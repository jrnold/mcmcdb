##' Methods for function \code{vcov}
##'
##' Calculate the variance-covariance matrix for MCMC samples.
##'
##' @param object The MCMC sample object.
##' @param ... Options passed to \code{\link{cov}}.
##'
##' @section Methods:
##' \describe{
##' \item{\code{signature(x="mcmc")}}{Method for class \code{mcmc}.}
##' \item{\code{signature(x="mcmc.list")}}{Method for class \code{mcmc.list}.}
##' }
##'
##' @name vcov-methods
##' @rdname vcov-methods
##' @aliases vcov,mcmc.list-method vcov,mcmc-method
##' @seealso \code{\link[stats4:vcov-methods]{vcov}}
NULL

##' @export
setMethod("vcov", "mcmc.list",
          function(object, ...) {
              cov(as(object, "matrix"), ...)
          })

##' @export
setMethod("vcov", "mcmc",
          function(object, ...) cov(object, ...))


