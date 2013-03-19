#' @include package.R
#' @include class-McmcdbWide.R
NULL

#' Apply function to each iteration of MCMC samples
#'
#' Split the MCMC samples into iterations, convert
#' the parameters into a named list of arrays, and apply
#' a function to each iteration.
#'
NULL

setGeneric("mcmcdb_iply", function(x, ...) standardGeneric("mcmcdb_iply"))

setMethod("mcmcdb_iply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            alply(x@samples, 1,
                  function(iter) .FUN(mcmcdb_unflatten(iter, parameters=x@parameters)),
                  ...)
          })

setGeneric("mcmcdb_fply", function(x, ...) standardGeneric("mcmcdb_fply"))

setMethod("mcmcdb_fply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            ret <- alply(x@samples, 2, .fun = .FUN, ...)
            names(ret) <- names(mcmcdb_flatpars(x))
            ret
          })

setGeneric("mcmcdb_aply", function(x, ...) standardGeneric("mcmcdb_aply"))

setMethod("mcmcdb_aply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            ret <- llply(mcmcdb_unflatten(x), .fun=.FUN, ...)
            names(ret) <- names(mcmcdb_parameters(x))
            ret
          })

