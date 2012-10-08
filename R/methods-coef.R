## ##' mcmc basic generic functions
## ##'
## ##' This method is meant to be equivalent to the "coef" method for
## ##' "mle" objects, in the sense that it returns a vector point
## ##' estimate for each parameter. By default, the point estimate
## ##' returned is the mean.
## ##'
## ##' @param FUN Function used to calculate the point estimate.
## ##'
## ##' @aliases vcov,mcmc-method
## setMethod("coef", "mcmc",
##           function(object, FUN="mean", ...) {
##               apply(object, 2, FUN)
##           })
## setMethod("coef", "mcmc.list",
##           function(object, FUN="mean", ...) {
##               mcmc_iter_column(object, FUN=FUN, ...)
##           })
