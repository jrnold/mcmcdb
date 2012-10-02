##' Mcmc Samples Object with metadata about parameters
##'
##' @section Slots
##' \describe{
##' \item{\code{indices}}{List of matrices which map each flat parameter name to its parameter array.}
##' \item{\code{template}}{List of parameter arrays in the same form as they were in the estimation.}
##' }
##'
##' @section Extends
##' \describe{
##' \item{\code{mcmc.list}}{directly}
##' }
##'
##' @export
setClass("McmcList2", contains="mcmc.list",
         representation(parameters="character",
                        template="list",
                        indices="list"))


mcmc2_default <- function(data, ...,
                          parameter_names=colnames(data[[1]]),
                          fun=parse_parameter_names_default)
{
    ## Put this before parparsed to change data before eval
    data <- mcmc.list(data)
    parparsed <- process_parsed_parameters(fun(parameter_names))
    new("McmcList2", data,
        parameters=parparsed$parameters,
        template=parparsed$template,
        indices=parparsed$indices)
}


##' Create Mcmc2 objects
##'
##' @param parameter_names \code{character} vector of flat parameter names used
##' to get the
##'
##' @export
setGeneric("McmcList2", mcmc2_default)

## TODO: make generic
##' @export
mcmc_to_iterations <- function(object, data=list(), FUN=identity, ...) {
    do_iteration <- function(x, indices, template, innerfun, data, ...) {
        results <- template
        for (j in names(template)) {
            pars <- indices[[j]]
            results[[j]][pars] <- x[rownames(pars)]
        }
        FUN(c(results, data), ...)
    }
    do_chain <- function(x, indices, template, innerfun, data) {
        alply(x, 1, .fun=do_iteration,
              indices=indices,
              template=template,
              data=data, innerfun=innerfun)
    }
    ## element names = be chain.iteration
    n_chains <- length(object)
    n_iter <- sapply(object, nrow)
    listnames <-
        unlist(mapply(function(x, y) paste(x, seq_len(y), sep="."),
                      seq_len(n_chains), n_iter, SIMPLIFY=FALSE))
    ret <- do.call(c, llply(object, do_chain,
                            indices=object@indices,
                            template=object@template,
                            innerfun=FUN, data=data))
    names(ret) <- listnames
    ret
}

## mcmc_to_array <- function(object, ...) {
##     do_iteration <- function(x, indices, template) {
##         template[indices] <- x
##         template
##     }

##     ## This can be done better
##     ## I think object may be being moved around each time.
##     do_parameter <- function(j, object) {
##         columns <- rownames(object@indices[[j]])
##         template <- object@template[[j]]
##         indices <- object@indices[[j]]
##         aaply(object[ , columns], 1, do_iteration, indices=indices, template=template)
##     }
##     llpply(names(object@indices), do_parameter, object=object, ...)

## }



