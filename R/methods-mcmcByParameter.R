setGeneric("mcmcGetParnames",
           function(object, ...) {
               standardGeneric("mcmcGetParnames")
           })

setMethod("mcmcGetParnames", "McmcLong",
          function(object) unique(object@samples$parnames))

setMethod("mcmcGetParnames", "mcmc.list",
          function(object) colnames(object[[1]]))

# ----------------------------------

setGeneric("mcmcGetChains",
           function(object, ...) {
               standardGeneric("mcmcGetChains")
           })

setMethod("mcmcGetChains", "McmcLong",
          function(object) unique(object@samples$chains))

setMethod("mcmcGetChains", "mcmc.list",
          function(object) length(object))

# ---------------------------------

setGeneric("mcmcGetIters",
           function(object, ...) {
               standardGeneric("mcmcGetChains")
           })

setMethod("mcmcGetIters", "McmcLong",
          function(object) unique(object@samples$iter))

setMethod("mcmcGetIters", "mcmc.list",
          function(object) seq_len(nrow(object[[1]])))

# ---------------------------------

setGeneric("mcmcDoParameter",
           function(object, parameter, ...) {
               standardGeneric("mcmcDoParameter")
           })

mcmc_do_parameter <- function(x, parameter, .fun=identity, ...) {
    .fun(x[x$parameter == parameter, ]$value)
}

setMethod("mcmcDoParameter", "McmcLong", mcmc_do_parameter)

# --------------------------------

setGeneric("mcmcByParameter",
           function(object, ...) {
               standardGeneric("mcmcByParameter")
           })

mcmc_by_parameter <- function(x, .fun=identity, ...) {
    f <- function(x) .fun(x$value)
    ddply(x, "parameter", .fun=f, ...)
}

setMethod("mcmcByParameter", "McmcLong", mcmc_by_parameter)





