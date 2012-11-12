##' Relist MCMC samples
##'
##' Relist MCMC parameters from their unlisted form into a list of
##' arrays.
##'
##' @param flesh \code{numeric}
##' @param skeleton Named \code{list} of arrays of zeros in the
##' shape of each parameter array.
##' @param indices Named \code{list} of matrices with the indices
##' @return \code{list} of arrays in the same shape as \code{skeleton}
##' filled in with the parameter values.
##' @export
mcmc_relist <- function(flesh, skeleton, indices) {
    results <- skeleton
    for (j in names(results)) {
        pars <- indices[[j]]
        results[[j]][pars] <- flesh[rownames(pars)]
    }
    results
}
