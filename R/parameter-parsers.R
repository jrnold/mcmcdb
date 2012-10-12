##' Parse MCMC parameter names
##'
##' These functions parse a character vector of parameter names and
##' return a \code{data.frame} (described below) that can easily be
##' turned in a \code{McmcParameterMeta} object.
##'
##' @param x \code{character} vector of character names.
##'
##' @return \code{data.frame} with rownames equal to \code{x} and
##' two columns
##' \describe{
##' \item{\code{index}}{Comma seperated string of the index of the flat parameter in the array parameter. E.g. If "beta.1.2" is the first column, second row of "beta", then this should be equal to "1,2".}
##' \item{\code{name}}{Name of the array parameter. E.g. If "beta.1.2" is the first column, second row of "beta", then this should be equal to "beta".}
##' }
##'
##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_default <- function(x) {
    result <- data.frame(pararray=x,
                         index=rep("1", length(x)),
                         stringsAsFactors=FALSE)
    rownames(result) <- x
    result
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_stan <- function(x) {
    result <- data.frame(str_split_fixed(x, fixed("."), n=2),
                          stringsAsFactors=FALSE)
    colnames(result) <- c("pararray", "index")
    rownames(result) <- x
    result$index <- str_replace_all(result[ , 2], fixed("."), fixed(","))
    result$index[result$index == ""] <- "1"
    result
}

##' @rdname mcmc_parse_parname
##' @export
mcmc_parse_parname_bugs <- function(x) {
    result <-
        data.frame(str_match(x, "([^\\[]+)(\\[([0-9,]+)\\])?")[ , c(2, 4)],
                   stringsAsFactors=FALSE)
    colnames(result) <- c("pararray", "index")
    result$index[result$index == ""] <- "1"
    rownames(result) <- x
    result
}

parsed_parameters_to_parameters <- function(x) {
    structure(x$pararray, names=rownames(x))
}

parsed_parameters_to_skeleton <- function(x) {
    split_indices <- function(indices, n_dim) {
        str_split_fixed(indices, fixed(","), n_dim)
    }
    result <-
        dlply(x, "pararray",
              function(y) {
                  n_dim <- str_count(y$index[1], fixed(",")) + 1
                  if (n_dim > 1) {
                      y_dim <- apply(split_indices(y$index, n_dim),
                                     2, function(z) max(as.integer(z)))
                  } else {
                      y_dim <- max(as.integer(y$index))
                  }
                  zeros(y_dim)
              })
    ## remove unessary attributes created by dlply
    strip_plyr_attr(result)
}

parsed_parameters_to_indices <- function(x) {
    f <- function(x) {
        indices <- x$index
        n_dim <- str_count(x$index[1], fixed(",")) + 1
        dimlist <- c(nrow(x), n_dim)
        result <- matrix(as.integer(str_split_fixed(indices, fixed(","), n_dim)),
                        nrow=nrow(x), ncol=n_dim)
        rownames(result) <- rownames(x)
        result
    }
    strip_plyr_attr(dlply(x, "pararray", f))
}

checkif_bugs_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z.][A-Za-z.0-9]*(\\[\\d(,\\d)*\\])?$")
}

## All Stan parameters are technically BUGS parameters
checkif_stan_parameters <- function(x) {
    str_all_match(x, "^[A-Za-z][A-Za-z0-9_]*(\\.\\d)*$")
}


##' Unflatten mcmc sample vector
##'
##' @rdname mcmcUnflatten
##' @export
setGeneric("mcmcUnflatten",
           function(metadata, x, ...) standardGeneric("mcmcUnflatten"))

mcmc_unflatten <- function(metadata, x, ...) {
    results <- metadata@skeleton
    for (j in names(results)) {
        pars <- metadata@indices[[j]]
        results[[j]][pars] <- x[rownames(pars)]
    }
    results
}

##' @rdname mcmcUnflatten
##' @aliases mcmcUnflatten,McmcParameterMeta,ANY-method
setMethod("mcmcUnflatten", c(metadata="McmcParameterMeta", x="ANY"),
          mcmc_unflatten)

mcmc_unflatten_matrix <- function(metadata, x, ...) {
    alply(x, 1, mcmc_unflatten, metadata=metadata, ...)
}

##' @rdname mcmcUnflatten
##' @aliases mcmcUnflatten,McmcParameterMeta,matrix-method
setMethod("mcmcUnflatten", c(metadata="McmcParameterMeta", x="matrix"),
          mcmc_unflatten_matrix)

