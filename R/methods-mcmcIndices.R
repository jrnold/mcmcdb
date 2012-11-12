##' @exportMethod mcmcIndices
NULL

##' Create Indices for MCMC Sample parameters
##'
##' @name mcmcSkeleton
##' @rdname mcmcSkeleton
##' @param x object
##' @return Named \code{list} of matrices. Each element is the indices
##' of the unlisted parameters in the parameter arrays.
##' @docType methods
##' @keywords methods
##' @aliases mcmcIndices-methods
##' @aliases mcmcIndices,data.frame-method
##' @aliases mcmcIndices,McmcParnames-method
##' @aliases mcmcIndices,McmcLong-method
NULL

setGeneric("mcmcIndices",
           function(x, ...) standardGeneric("mcmcIndices"))

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

setMethod("mcmcIndices", "McmcParnames", mcmc_parnames_to_indices)

setMethod("mcmcIndices", "data.frame",
          function(x) callGeneric(McmcParnames(x)))

setMethod("mcmcIndices", "McmcLong",
          function(x) callGeneric(x@parnames))
