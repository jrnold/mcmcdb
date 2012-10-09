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
          function(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), ...) {
              apply(x, 2, quantile, probs=probs, ...)
          })

setMethod("quantile", "mcmc.list",
          function(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), ...) {
              mcmc_iter_column(x, quantile, probs=probs,  ...)
          })

setMethod("quantile", "McmcLong",
          function(x, probs=c(0.025, 0.25, 0.5, 0.75, 0.975), na.rm=FALSE, names=TRUE, type=7, ...) {
              ddply(x, "parameter", summarise,
                    quantile=quantile(x, probs=probs, na.rm=na.rm, names=names, type=type),
                    ...)
          })

