
# ---------------------------------

setGeneric("mcmcDoParameter",
           function(object, parameter, ...) {
               standardGeneric("mcmcDoParameter")
           })

mcmc_do_parameter <- function(object, parameter, .fun=identity, ...) {
    .fun(x[x$parameter == parameter, ]$value)
}

setMethod("mcmcDoParameter",
          signature=c(object="McmcLong", parameter="character"),
          mcmc_do_parameter)

# --------------------------------

setGeneric("mcmcByParameter",
           function(object, ...) {
               standardGeneric("mcmcByParameter")
           })

mcmc_by_parameter <- function(object, .fun=identity, ...) {
    f <- function(object) .fun(x$value)
    ddply(object, "parameter", .fun=f, ...)
}

setMethod("mcmcByParameter", "McmcLong", mcmc_by_parameter)


