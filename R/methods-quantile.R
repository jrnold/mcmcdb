## ##' @aliases quantile,mcmc-method
## setMethod("quantile", "mcmc",
##           function(x, ...) {
##               apply(x, 2, quantile, ...)
##           })

## setMethod("quantile", "mcmc.list",
##           function(x, ...) {
##               mcmc_iter_column(x, quantile, ...)
##           })
