##' Quantile methods for MCMC samples
##'
##' Calculate quantile of paramters in MCMC sample objects.
##'
##' @section Methods:
##'
##' \describe{
##' \item{\code{signature(x="mcmc")}}{Method for class \code{mcmc}.}
##' \item{\code{signature(x="mcmc.list")}}{Method for class \code{mcmc.list}.}
##' }
##'
##' @param x MCMC sample object.
##' @param ... Arguments passed to \code{\link[stats]{quantile}}.
##'
##' @rdname quantile-methods
##' @name quantile-methods
##' @aliases quantile,mcmc-method quantile,mcmc.list-method
##' @export
setGeneric("quantile", stats::quantile)

setMethod("quantile", "mcmc",
          function(x, ...) {
              apply(x, 2, quantile, ...)
          })

setMethod("quantile", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, quantile, ...)
          })
