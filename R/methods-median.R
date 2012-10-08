## ##' @aliases median,mcmc-method
## setMethod("median", "mcmc",
##           function(x, na.rm=FALSE) {
##               apply(x, 2, median, na.rm=na.rm)
##           })

## setMethod("median", "mcmc.list",
##           function(x, na.rm=FALSE) {
##               mcmc_iter_column(x, median, na.rm=na.rm)
##           })
