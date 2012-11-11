##' @exportMethod mcmcSkeleton
##' @exportMethod mcmcIndices
##' @exportMethod mcmcRelist
NULL

setGeneric("mcmcSkeleton",
           function(x, ...) standardGeneric("mcmcSkeleton"))

setGeneric("mcmcIndices",
           function(x, ...) standardGeneric("mcmcIndices"))

## Create skeleton for relisting MCMC samples
##
## @param x McmcPararrays object
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

setGeneric("mcmcSkeleton", "McmcPararray", mcmc_pararrays_to_skeleton)

setGeneric("mcmcSkeleton", "data.frame",
           function(x) callGeneric(McmcPararray(x)))

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

setGeneric("mcmcSkeleton", "McmcParnames", mcmc_parnames_to_indices)

setGeneric("mcmcIndices", "data.frame",
           function(x) callGeneric(McmcParnames(x)))


mcmc_relist <- function(flesh, skeleton, indices) {
    results <- skeleton
    for (j in names(results)) {
        pars <- indices[[j]]
        results[[j]][pars] <- flesh[rownames(pars)]
    }
    results
}


##' Relist MCMC samples method.
##'
##' Convert a \code{numeric} vector with MCMC samples, into a list of
##' arrays with the original dimensions of those parameters. The
##'
##' @param param parnames \code{McmcParnames} object.
##' @param param pararrays \code{McmcPararrays} object.
##' @param param flesh Named \code{numeric} vector with names
##' corresponding to the parameters.
##' @seealso \code{\link{relist}}
##' @export
NULL

setGeneric("mcmcRelist",
           function(flesh, skeleton, ...) standardGeneric("mcmcRelist"))

mcmc_relist_numeric_mcmcpararrays <-
    function(flesh, skeleton, parnames) {
        mcmc_relist(flesh, mcmcSkeleton(skeleton),
                    mcmcIndices(indices))
    }

setMethod("mcmcRelist", c("numeric", "mcmcPararrays"),
          mcmc_relist_numeric_pararrays)
