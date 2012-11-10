## Create skeleton for relisting MCMC samples
##
## @param x McmcArrays object
## @return \code{list} of numeric \code{array} objects, one for
## each value of \code{pararray}, and in the dimension
## of that \code{parrray}, but with all entries set to 0.
mcmc_pararrays_to_skeleton <- function(x) {
    result <-
        dlply(x, "pararray",
              function(x) {
                  zeros(dim=eval(parse(text=sprintf("c(%s)",
                                       x$dim_sz))))
              })
    strip_plyr_attr(result)
}

## @param x McmcParnames object
mcmc_parnames_to_indices <- function(x) {
    result <-
        dlply(x, "pararray",
              function(y) {
                  indices <- y$idx
                  dim_n <- str_count(indices[1], fixed(",")) + 1
                  ret <- matrix(as.integer(str_split_fixed(indices, fixed(","),
                                                           dim_n)),
                                nrow(y), dim_n)
                  rownames(ret) <- y$parname
                  ret
              })
    strip_plyr_attr(result)
}

mcmc_relist_0 <- function(skeleton, indices, flesh) {
    results <- skeleton
    for (j in names(results)) {
        pars <- indices[[j]]
        results[[j]][pars] <- flesh[rownames(pars)]
    }
    results
}


##' Relist MCMC samples
##'
##' Convert a \code{numeric} vector with MCMC samples, into a list of
##' arrays with the original dimensions of those parameters.
##'
##' @param param parnames \code{McmcParnames} object.
##' @param param pararrays \code{McmcPararrays} object.
##' @param param flesh Named \code{numeric} vector with names
##' corresponding to the parameters.
##' @seealso \code{\link{relist}}
##' @export
mcmc_relist <- function(parnames, pararrays, flesh) {
    skeleton <- mcmc_pararrays_to_skeleton(pararrays)
    indices <- mcmc_parnames_to_indices(parnames)
    mcmc_relist_0(skeleton, indices, flesh)
}

