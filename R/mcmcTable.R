##' @section Slots
##'
##' @export
setClass("McmcTable",
         contains="data.table",
         representation(parameters="list",
                        template="list"))
validate_mcmc_table <- function(object) {
    valid_colnames <- c("parameter", "chain", "iteration", "value")
    parameters <- unique(object$parameter)
    msg <- character()
    if (!all(colnames(object) == valid_colnames)) {
        msg <- append(msg, "Column names are incorrect")
    }
    if (!all(sort(unique(object$parameter))
             == sort(names(object@parameters)))) {
        msg <- append(msg, "object$parameter must match names in object@parameter")
    }
    if (!all(sort(sapply(object@parameters, `[[`, i="parameter"))
             == sort(names(template)))) {
        msg <- append(msg, "object@parameters[[i]]$parameter must be in object@template")
    }
    ## all indices must be <= dimension of corresponding value in the template
    valid_indices <-
        sapply(object@parameters,
               function(x) {
                   all(x$dim <= dim(object@template[[x$parameter]]))
               })
    if (!all(valid_indices)) {
        msg <- append(msg, "Some parameter indices outside of template dimensions.")
    }
    if (!all(sapply(object@template, is.numeric))) {
        msg <- append(msg, "not all elements in object@template are numeric")
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("McmcTable", validate_mcmc_table)

setGeneric("McmcTable", function(data, ...) standardGeneric("McmcTable"))
mcmc_table_data_table <- function(data, parameters) {
    colorder <- c("parameter", "chain", "iteration", "value")
    par_tmpl <- preprocess_parameters(parameters)
    data <- data.table(melt(line)[ , colorder])
    data[ , parameter := as.character(parameter)]
    setkeyv(data, c("parameter", "chain", "iteration"))
    new("McmcTable",
        data, parameters=par_tmpl$parameters, template=par_tmpl$template)
}

setMethod("McmcTable", "data.table", mcmc_table_data_table)
setMethod("McmcTable", "data.frame",
          function(data, ...) {
              callGeneric(data.table(data), ...)
          })
setMethod("McmcTable", "mcmc.list",
          function(data, ...) {
              callGeneric(melt(data), ...)
          })
setMethod("McmcTable", "mcmc",
          function(data, ...) {
              callGeneric(melt(mcmc.list(data)))
          })

## TODO:
## as mcmc -> McmcTable
## as mcmc.list -> McmcTable
## as McmcTable -> mcmc.list

## Iteration


