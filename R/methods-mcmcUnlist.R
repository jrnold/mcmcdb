##' @exportMethod mcmcUnlist
NULL

##' Methods for function \code{mcmcUnlist}
##'
##' @param x Object
##' @param name \code{character}. Parameter name.
##' @param fun Function used to generate unlisted parameter names.
##'
##' @name mcmcUnlist-method
##' @rdname mcmcUnlist-method
##' @aliases mcmcUnlist-methods
##' @aliases mcmcUnlist,numeric-method
##' @aliases mcmcUnlist,array-method
##' @aliases mcmcUnlist,list-method
NULL

setGeneric("mcmcUnlist", function(x, ...) standardGeneric("mcmcUnlist"))

arr_ind <- function(dim, i=1) {
    ndim <- length(dim)
    if (i == 1) {
        ret <- rep(seq_len(dim[i]),
                   prod(dim[-i]))
    } else if (i > 1 && i < ndim) {
        ret <-
            rep(rep(seq_len(dim[i]),
                    each=prod(dim[1:(i-1)])),
                dim[(i+1):ndim])
    } else {
        ret <-
            rep(seq_len(dim[i]), each=prod(dim[-i]))
    }
    ret
}

flatten <- function(x, na.rm=TRUE) {
    if (na.rm) {
        ok <- !is.na(x)
    }
    ret <- list()
    dimensions <- dim(x)
    ndim <- length(dimensions)
    if (na.rm) {
        ret$v <- as.vector(x)[ok]
        ret$i <- matrix(0, sum(ok), ndim)
    } else {
        ret$v <- as.vector(x)
        ret$i <- matrix(0, length(x), ndim)
    }
    for (i in seq_along(dimensions)) {
        tmp <- arr_ind(dimensions, i)
        if (na.rm) {
            ret$i[ , i] <- tmp[ok]
        } else {
            ret$i[ , i] <- tmp
        }
    }
    ## Trick: which(array(TRUE, dim(x)), arr.ind=TRUE)
    ## Trick: which(!is.na(na), arr.ind=TRUE)
    ret
}

mcmc_unlist_array <- function(x, name, fun=mcmc_create_parnames_stan) {
     y <- flatten(x)
     structure(y$v, names=mcmc_create_parnames_stan(name, y$i))
}

setMethod("mcmcUnlist", "array", mcmc_unlist_array)

setMethod("mcmcUnlist", "numeric", mcmc_unlist_array)

mcmc_unlist_list <- function(x, fun=mcmc_create_parnames_stan) {
    do.call(c, mapply(function(x, name) mcmc_unlist_array(x, name, fun=fun),
                      x, names(x), USE.NAMES=FALSE, SIMPLIFY=FALSE))
}

setMethod("mcmcUnlist", "list", mcmc_unlist_list)
