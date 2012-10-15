##' Parse MCMC parameter names
##'
##' These functions parse a character vector of parameter names and
##' return a \code{McmcParnames} object.
##'
##' @param x \code{character} vector of character names.
##'
##' @return \code{data.frame} with rownames equal to \code{x} and
##' two columns
##' \describe{
##' \item{\code{idx}}{Comma seperated string of the idx of the flat parameter in the array parameter. E.g. If "beta.1.2" is the first column, second row of "beta", then this should be equal to "1,2".}
##' \item{\code{name}}{Name of the array parameter. E.g. If "beta.1.2" is the first column, second row of "beta", then this should be equal to "beta".}
##' }
##'
##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_default <- function(x) {
    result <- data.frame(parname=x,
                         pararray=x,
                         idx=rep("1", length(x)),
                         stringsAsFactors=FALSE)
    rownames(result) <- x
    result
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_stan <- function(x) {
    result <- data.frame(str_split_fixed(x, fixed("."), n=2),
                          stringsAsFactors=FALSE)
    colnames(result) <- c("pararray", "idx")
    result$parname <- x
    rownames(result) <- x
    result$idx <- str_replace_all(result[ , 2], fixed("."), fixed(","))
    result$idx[result$idx == ""] <- "1"
    result[ , c("parname", "pararray", "idx")]
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_bugs <- function(x) {
    result <-
        data.frame(str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)],
                   stringsAsFactors=FALSE)
    colnames(result) <- c("pararray", "idx")
    result$parname <- x
    rownames(result) <- x
    result$idx[result$idx == ""] <- "1"
    result[ , c("parname", "pararray", "idx")]
}

## @param x McmcParnames objects
parnames_to_pararrays <- function(x) {
    f <- function(x) {
        indices <- x$idx
        dim_n <- str_count(indices[1], fixed(",")) + 1L
        dims <- matrix(as.integer(str_split_fixed(indices, fixed(","), dim_n)))
        dim_sz <- as.character(apply(dims, 2, max), collapse=",")
        data.frame(dim_n = dim_n, dim_sz = dim_sz,
                   stringsAsFactors=FALSE)
    }
    strip_plyr_attr(ddply(x, "pararray", f))
}

checkif_bugs_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z.][A-Za-z.0-9]*(\\[\\d(,\\d)*\\])?$")
}

# All Stan parameters are technically BUGS parameters
checkif_stan_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z][A-Za-z0-9_]*(\\.\\d)*$")
}

## parsed_parameters_to_indices <- function(x) {
##     f <- function(x) {
##         indices <- x$idx
##         n_dim <- str_count(x$idx[1], fixed(",")) + 1
##         dimlist <- c(nrow(x), n_dim)
##         result <- matrix(as.integer(str_split_fixed(indices, fixed(","), n_dim)),
##                         nrow=nrow(x), ncol=n_dim)
##         rownames(result) <- rownames(x)
##         result
##     }
##     strip_plyr_attr(dlply(x, "pararray", f))
## }


## pararrays_from_parnames <- function(x) {
##     split_indices <- function(indices, n_dim) {
##         str_split_fixed(indices, fixed(","), n_dim)
##     }
##     result <-
##         dlply(x, "pararray",
##               function(y) {
##                   n_dim <- str_count(y$idx[1], fixed(",")) + 1
##                   if (n_dim > 1) {
##                       y_dim <- apply(split_indices(y$idx, n_dim),
##                                      2, function(z) max(as.integer(z)))
##                   } else {
##                       y_dim <- max(as.integer(y$idx))
##                   }
##                   zeros(y_dim)
##               })
##     strip_plyr_attr(result)
## }



## ##' Unflatten mcmc sample vector
## ##'
## ##' @rdname mcmcUnflatten
## ##' @export
## setGeneric("mcmcUnflatten",
##            function(metadata, x, ...) standardGeneric("mcmcUnflatten"))

## mcmc_unflatten <- function(metadata, x, ...) {
##     results <- metadata@skeleton
##     for (j in names(results)) {
##         pars <- metadata@indices[[j]]
##         results[[j]][pars] <- x[rownames(pars)]
##     }
##     results
## }

## ##' @rdname mcmcUnflatten
## ##' @aliases mcmcUnflatten,McmcParameters,ANY-method
## setMethod("mcmcUnflatten", c(metadata="McmcParameters", x="ANY"),
##           mcmc_unflatten)

## mcmc_unflatten_matrix <- function(metadata, x, ...) {
##     alply(x, 1, mcmc_unflatten, metadata=metadata, ...)
## }

## ##' @rdname mcmcUnflatten
## ##' @aliases mcmcUnflatten,McmcParameters,matrix-method
## setMethod("mcmcUnflatten", c(metadata="McmcParameters", x="matrix"),
##           mcmc_unflatten_matrix)

