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
setClass("McmcList2", contains="mcmc.list",
         representation(parameters="character",
                        template="list",
                        indices="list"))


##' Create Mcmc2 objects
setGeneric("Mcmc2", function(data, ...) standardGeneric("Mcmc2"))

mcmc2_mcmc <- function(data, parameter_names=colnames(data), fun=mcmc4_parameter_parser_default, ...) {
    parparsed <- match.fun(fun)(parameter_names)
    new("Mcmc2", data,
        parameters=parparsed$paramters,
        template=parparsed$template,
        indices=parparsed$indices)

}
setMethod("Mcmc2", "mcmc.list", mcmc2_mcmc_list)

mcmc_to_iterations <- function(object, data=NULL, FUN=identity, ...) {
    do_iteration <- function(x, indices, template, data=NULL, FUN=identity) {
        results <- template
        for (j in names(template)) {
            pars <- indices[[j]]
            results[[j]][pars] <- values[rownames(pars)]
        }
        FUN(c(results, data), ...)
    }
    do_chain <- function(x, ...) {
        alply(x, 1, do_iteration, ...)
    }
    do.call(c, llply(object, do_chain,
                     indices=object@indices,
                     template=object@template,
                     data=data, FUN=FUN, ...))
}

mcmc_to_array <- function(object, ...) {
    do_iteration <- function(x, indices, template) {
        template[indices] <- x
        template
    }

    ## This can be done better
    ## I think object may be being moved around each time.
    do_parameter <- function(j, object) {
        columns <- rownames(object@indices[[j]])
        template <- object@template[[j]]
        indices <- object@indices[[j]]
        aaply(object[ , columns], 1, do_iteration, indices=indices, template=template)
    }
    llpply(names(object@indices), do_parameter, object=object, ...)

}



