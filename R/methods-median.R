##' Methods for function \code{methods} for MCMC samples
##'
##' Calculate the median for each parameter of an MCMC sample object.
##'
##' @section Methods:
##'
##' \describe{
##' \item{\code{signature(x="mcmc")}}{Method for class \code{mcmc}.}
##' \item{\code{signature(x="median")}}{Method for class \code{mcmc.list}.}
##' }
##'
##' @rdname median-methods
##' @name median-methods
##' @aliases median,mcmc-method
##' @aliases median,mcmc.list-method
##' @aliases median,McmcLong-method
##' @seealso \code{\link{median}}
##' @docType methods
##' @keywords methods
##' @export
setGeneric("median", function(x, ...) stats::median(x, ...))

##' @aliases median,mcmc-method
setMethod("median", "mcmc",
          function(x, na.rm=FALSE) {
              apply(x, 2, stats::median, na.rm=na.rm)
          })


setMethod("median", "mcmc.list",
          function(x, na.rm=FALSE) {
              mcmc_iter_column(x, stats::median, na.rm=na.rm)
          })

## TODO: maybe another median method?
setMethod("median", "McmcLong",
          function(x, na.rm=FALSE, ...) {
              f <- function(x) {
                  data.frame(median=stats::median(x$value, na.rm=na.rm))
              }
              ddply(x, "parameter", f, ...)
          })

