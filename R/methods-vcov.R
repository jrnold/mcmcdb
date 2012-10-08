
## setMethod("vcov", "mcmc.list",
##           function(object, ...) {
##               cov(as(object, "matrix"), ...)
##           })


## ##' @aliases vcov,mcmc-method
## setMethod("vcov", "mcmc",
##           function(object, ...) cov(object, ...))


