
##' @aliases mean,mcmc-method

##' @export
setMethod("mean", "mcmc.list",
          function(x, ...) {
              mcmc_iter_column(x, mean, ...)
          })

##' @export
setMethod("mean", "mcmc",
          function(x, ...) {
              apply(x, 2, mean, ...)
          })
