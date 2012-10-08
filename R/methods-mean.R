##' Methods for MCMC samples
##'
##' Calculate the mean for each paramter in MCMC samples.
##'
##' @usage
##' \S4method{mean}{mcmc}(x, ...)
##' \S4method{mean}{mcmc.list}(x, ...)
##'
##' @name mean-method
##' @rdname mean-method
##' @aliases mean,mcmc-method mean,mcmc.list-method
##' @seealso \code{\link[base]{mean}}
NULL

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
