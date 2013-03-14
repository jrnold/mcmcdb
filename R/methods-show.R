show_mcmc_long <- function(object) {
    chains <- nrow(object@chains)
    iter <- sum(object@chains$niter)
    nparnames <- nrow(object@parnames)
    npararrays <- nrow(object@pararrays)
    cat("Object of class McmcLong\n")
    cat(sprintf("%d total iterations from %d chain(s)\n",
                iter, chains))
    cat(sprintf("%d parameters (%d arrays)\n",
                nparnames, npararrays))
}

##' Methods for function \code{show}
##'
##' @aliases show,McmcLong-method
##' @export
setMethod("show", "McmcLong", show_mcmc_long)


