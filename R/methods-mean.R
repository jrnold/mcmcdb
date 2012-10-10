##' Methods for MCMC samples
##'
##' Calculate the mean for each paramter in MCMC samples.
##'
##' @usage
##' \S4method{mean}{mcmc}(x, ...)
##' \S4method{mean}{mcmc.list}(x, ...)
##' \S4method{mean}{McmcLong}(x, ...)
##'
##' @name mean-method
##' @rdname mean-method
##' @aliases mean,mcmc-method
##' @aliases mean,mcmc.list-method
##' @aliases mean,McmcLong-method
##' @seealso \code{\link[base]{mean}}
##' @docType methods
##' @keywords methods
##' @export
setGeneric("mean", base::mean)

setMethod("mean", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, mean, ...)
          })

setMethod("mean", "mcmc",
          function(x, ...) {
              apply(x, 2, mean, ...)
          })

setMethod("mean", "McmcLong",
          function(x, ...) {
              ddply(x@samples, "parameter", summarise, mean=mean(value, ...))
          })


