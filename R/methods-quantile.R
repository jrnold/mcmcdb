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
##' @aliases quantile,mcmc-method
##' @aliases quantile,mcmc.list-method
##' @aliases quantile,McmcLong-method
##' @docType methods
##' @keywords methods
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

setMethod("quantile", "McmcLong",
          function(x, ...) {
              ddply(x, "parameter", summarise, quantile=quantile(x, ...))
          })
