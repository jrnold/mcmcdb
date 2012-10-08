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
##' @rdname parse_parameter_names
##' @export
parse_parameter_names_default <- function(x) {
    result <- data.frame(pararray=x,
                         index=rep("1", length(x)),
                         stringsAsFactors=FALSE)
    rownames(result) <- x
    result
}

##' @rdname parse_parameter_names
##' @export
parse_parameter_names_stan <- function(x) {
    result <- data.frame(str_split_fixed(x, fixed("."), n=2),
                          stringsAsFactors=FALSE)
    colnames(result) <- c("pararray", "index")
    rownames(result) <- x
    result$index <- str_replace_all(result[ , 2], fixed("."), fixed(","))
    result$index[result$index == ""] <- "1"
    result
}

##' @rdname parse_parameter_names
##' @export
parse_parameter_names_bugs <- function(x) {
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


##' MCMC Parameter Metadata
##'
##' This class stores the mapping between the names of MCMC parameters
##' in their flat representation (as returned by samplers like BUGS)
##' and their array representation in the model. This class
##' facilitates transforming parameters from their flat to their array
##' representation.
##'
##' @section Slots:
##'
##' \describe{
##' \item{\code{parameters}}{named character vector. Values are the names of the array parameters;
##' names are the names of the flat parameters.}
##' \item{\code{skeleton}}{List of arrays. Each element has the name of the array parameter, and an
##' array value of the proper shape of that parameter.}
##' \item{\code{indices}}{List of matrices, one for each array parameter. Each row the matrix is a
##' has a rowname of a flat parameter and values that are the index of flat parameter in the array parameter.}
##' }
##'
##' @rdname McmcParameterMeta-class
##' @aliases McmcParameterMeta-class
##' @export
setClass("McmcParameterMeta",
         representation(parameters="character",
                        skeleton="list",
                        indices="list"))

validate_mcmc_parameter_meta <- function(object) {
    par_flat <- names(object@parameters)
    par_array <- unique(object@parameters)
    tmpl_dim <- lapply(object@skeleton, dim)
    indices_max <- lapply(object@indices, function(x) apply(x, 2, max))
    msg <- c()
    ## TODO:
    if (!setequal(par_array, names(object@skeleton))) {
        msg <- c(msg, "names(object@skeleton) disagree with the names in object@parameters")
    }
    if (!setequal(par_array, names(object@indices))) {
        msg <- c(msg, "names(object@indices) disagree with the names in object@parameters")
    }
    if (!setequal(unlist(sapply(object@indices, rownames)), par_flat)) {
        msg <- c(msg, "rownames in object@indices disagree with the parameters in object@parameters")
    }
    if (any(mapply(function(x, y) any(x > y), indices_max, tmpl_dim))) {
        msg <- c(msg, "an index in object@indices is out of range of objects@skeleton")
    }
    if (any(unlist(sapply(object@indices, `<`, y=1)))) {
        msg <- c(msg, "values in object@indices cannot be less than 1")
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}

setValidity("McmcParameterMeta", validate_mcmc_parameter_meta)


### Initialize

##' Create McmcParameterMeta objects
##'
##' @rdname McmcParameterMeta
##' @export
setGeneric("McmcParameterMeta", function(x, ...) standardGeneric("McmcParameterMeta"))

mcmc_parameter_meta_data_frame <- function(x, ...) {
    new("McmcParameterMeta",
        parameters=parsed_parameters_to_parameters(x),
        skeleton=parsed_parameters_to_skeleton(x),
        indices=parsed_parameters_to_indices(x))
}


##' @rdname McmcParameterMeta
##' @aliases McmcParameterMeta,data.frame-method
setMethod("McmcParameterMeta", "data.frame", mcmc_parameter_meta_data_frame)

##' @rdname McmcParameterMeta
##' @aliases McmcParameterMeta,matrix-method
setMethod("McmcParameterMeta", "matrix",
          function(x, ...) {
              callGeneric(as(x, "data.frame"), ...)
          })

##' @rdname McmcParameterMeta
##' @aliases McmcParameterMeta,character-method
setMethod("McmcParameterMeta", "character",
          function(x, fun=parse_parameter_names_default, ...) {
              callGeneric(fun(x, ...))
          })

##' @rdname McmcParameterMeta
##' @aliases McmcParameterMeta,character-method
setMethod("McmcParameterMeta", "factor",
          function(x, fun=parse_parameter_names_default, ...) {
              callGeneric(as.character(x), fun=fun, ...)
          })


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

