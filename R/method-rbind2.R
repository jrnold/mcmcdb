#' @include package.R
#' @include class-McmcdbWide.R
#' @exportMethod rbind2
NULL

#' @rdname rbind2-methods
#' @name rbind2-methods
#' @title Methods for function \code{rbind2}
#'
#' @param x First object
#' @param y Object to bind to \code{x}.
#' @return An object of the same class as \code{x}.
NULL

rbind2.McmcdbWide.McmcdbWide <- function(x, y) {
  samples <- rbind(x@samples, y@samples)
  parameters <- x@parameters
  chains <- rbind2(x@chains, y@chains)
  iters <- rbind2(x@iters, y@iters)
  flatpar_chains <- rbind2(x@flatpar_chains, y@flatpar_chains)
  metadata <- x@metadata
  version <- x@version
  model_data <- x@model_data
  new("McmcdbWide",
      samples = samples, parameters = parameters,
      chains = chains, iters = iters, flatpar_chains = flatpar_chains,
      metadata = metadata, version = version, model_data = model_data)
}


#' @rdname rbind2-methods
#' @aliases rbind2,McmcdbWide,missing-method
setMethod("rbind2", c(x = "McmcdbWide", y = "missing"),
          function(x, y) x)

#' @rdname rbind2-methods
#' @aliases rbind2,McmcdbWide,McmcdbWide-method
setMethod("rbind2", c(x = "McmcdbWide", y = "McmcdbWide"),
          rbind2.McmcdbWide.McmcdbWide)
