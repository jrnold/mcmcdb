#' @include package.R
#' @include class-McmcdbWide.R
NULL

#' Retrieve Sample data from Mcmcdb Object
#'
#'
setGeneric("mcmcdb_samples_long",
           function(object, ...) {
             standardGeneric("mcmcdb_samples_long")
           })

setMethod("mcmcdb_samples_long", "McmcdbWide",
          function(object) {
            ## TODO: skip this step
            x <- as.data.frame(object@samples)
            x$chain_id <- object@chains[["chain_id"]]
            x$iter <- object@chains[["iter"]]
            reshape(x, id.vars=c("chain_id", "iter"))
          })

subset_mcmcdb_wide <- function(x, pararrays = NULL, flatpars = NULL, chain_id = NULL, iter = NULL) {
  if (is.null(chain_id)) {
    i <- TRUE
  } else {
    i <- 
  }
  ## iterations
  if (missing(j)) {
    jj <- TRUE
  } else {
    jj <- (x@iters[["chain_id"]] %in% j)
  }

  if (missing(k)) {
    kk <- TRUE
  } else {
    kk <- (x@iters[["iter"]] %in% k)
  }
  parrays <- mcmcdb_parameters[
}
