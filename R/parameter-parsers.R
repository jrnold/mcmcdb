##' Parse MCMC parameter names
##'
##' These functions parse a character vector of parameter names and
##' return a \code{McmcParnames} object.
##'
##' @param x \code{character} vector of character names.
##'
##' @return \code{McmcParnames} data.frame.
##'
##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_default <- function(x) {
    result <- data.frame(parname=as.factor(x),
                         pararray=as.factor(x),
                         idx=rep("1", length(x)),
                         stringsAsFactors=FALSE)
    rownames(result) <- x
    new("McmcParnames", result)
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_stan <- function(x) {
    result <- data.frame(str_split_fixed(x, fixed("."), n=2))
    colnames(result) <- c("pararray", "idx")
    result$parname <- as.factor(x)
    rownames(result) <- x
    result$idx <- str_replace_all(result[ , 2], fixed("."), fixed(","))
    result$idx[result$idx == ""] <- "1"
    new("McmcParnames", result[ , c("parname", "pararray", "idx")])
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_bugs <- function(x) {
    result <-
        data.frame(str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)])
    colnames(result) <- c("pararray", "idx")
    result$idx <- as.character(result$idx)
    result$parname <- as.factor(x)
    rownames(result) <- x
    result$idx[result$idx == ""] <- "1"
    new("McmcParnames", result[ , c("parname", "pararray", "idx")])
}

## @param x McmcParnames objects
parnames_to_pararrays <- function(x) {
    f <- function(x) {
        indices <- x$idx
        dim_n <- str_count(indices[1], fixed(",")) + 1L
        dims <- matrix(as.integer(str_split_fixed(indices, fixed(","), dim_n)),
                       nrow(x), dim_n)
        dim_sz <- paste(apply(dims, 2, max), collapse=",")
        data.frame(dim_n = dim_n, dim_sz = dim_sz,
                   stringsAsFactors=FALSE)
    }
    new("McmcPararrays", strip_plyr_attr(ddply(x, "pararray", f)))
}

checkif_bugs_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z.][A-Za-z.0-9]*(\\[\\d(,\\d)*\\])?$")
}

# All Stan parameters are technically BUGS parameters
checkif_stan_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z][A-Za-z0-9_]*(\\.\\d)*$")
}


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

