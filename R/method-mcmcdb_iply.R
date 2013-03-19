#' @include package.R
#' @include class-McmcdbWide.R
#' @export mcmcdb_aply
#' @export mcmcdb_iply
#' @export mcmcdb_fply
NULL

#' @name mcmcdb_iply-methods
#' @rdname mcmcdb_iply-methods
#' @docType methods
#' @title Apply function to each iteration of MCMC samples
#'
#' @description Split the MCMC samples into iterations, convert
#' the parameters into a named list of arrays, and apply
#' a function to each iteration.
#'
#' @param x Object
#' @param .fun function to apply to each iteration. The first argument of
#' \code{.fun} will be the list of parameter arrays for that iteration.
#' @param ... arguments passed to \code{\link{alply}} which is used internally.
#' @return \code{list}
setGeneric("mcmcdb_iply", function(x, ...) standardGeneric("mcmcdb_iply"))

#' @rdname mcmcdb_iply-methods
#' @aliases mcmcdb_iply,McmcdbWide-method
#' @family McmdbWide methods
setMethod("mcmcdb_iply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            alply(x@samples, 1,
                  function(iter) .FUN(mcmcdb_unflatten(iter, parameters=x@parameters)),
                  ...)
          })

#' @name mcmcdb_fply-methods
#' @rdname mcmcdb_fply-methods
#' @docType methods
#' @title Apply function to each flat parameter in MCMC samples
#'
#' @description Split the MCMC samples into arrays of the original dimension x the
#' total number of iterations and apply a function.
#'
#' @param x Object
#' @param .fun function to apply to each iteration. The first argument of
#' \code{.fun} will be an \code{array}; If the array has \code{m} dimensions,
#' the first \code{m-1} dimensions are the dimensions of parameter array,
#' and the last dimension is the iterations for all chains.
#' @param ... arguments passed to \code{\link{alply}} which is used internally.
#' @return \code{list}
setGeneric("mcmcdb_fply", function(x, ...) standardGeneric("mcmcdb_fply"))

#' @rdname mcmcdb_fply-methods
#' @aliases mcmcdb_fply,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_fply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            ret <- alply(x@samples, 2, .fun = .FUN, ...)
            names(ret) <- names(mcmcdb_flatpars(x))
            ret
          })

#' @name mcmcdb_aply-methods
#' @rdname mcmcdb_aply-methods
#' @docType methods
#' @title Apply function to each parameter array in MCMC samples
#'
#' @description Split the MCMC samples into flat paramers, vectors of length equal to the
#' total number of iterations, and apply a function to each.
#'
#' @param x Object
#' @param .fun function to apply to each flat parameter. The first argument of
#' \code{.fun} will be a \code{numeric} vector repres
#' @param ... arguments passed to \code{\link{alply}} which is used internally.
#' @return \code{list}
setGeneric("mcmcdb_aply", function(x, ...) standardGeneric("mcmcdb_aply"))

#' @rdname mcmcdb_aply-methods
#' @aliases mcmcdb_aply,McmcdbWide-method
#' @family McmcdbWide methods
setMethod("mcmcdb_aply", "McmcdbWide",
          function(x, .fun=identity, ...) {
            .FUN <- .fun
            ret <- llply(mcmcdb_unflatten(x), .fun=.FUN, ...)
            names(ret) <- names(mcmcdb_parameters(x))
            ret
          })

