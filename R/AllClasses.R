##' @exportClass mcmc
##' @exportClass mcmc.list
##' @exportClass summary.mcmc
NULL

setClass("mcmc4", contains="matrix",
         representation(mcpar="numeric"))
setOldClass("mcmc", S4Class="mcmc4")
## removeClass("mcmc4")

Mcmc <- function(x, start=1, end=nrow(x), thin=1) {
    new("mcmc", as.numeric(x),
        mcpar=c(start=start, end=end, thin=thin))
}


setClass("mcmclist4", contains="list")
mcmc_list_validity <- function(object) {
    ## Allow for empty lists
    if (length(object@.Data) == 0) {
        TRUE
    } else {
        allmcmc <- all(sapply(object, is, class2="mcmc"))
        if (allmcmc) {
            TRUE
        } else {
            "Not all elements in the list are mcmc objects."
        }
    }
}
setValidity("mcmclist4", mcmc_list_validity)
setOldClass("mcmc.list", S4Class="mcmclist4")
## removeClass("McmcList4")


setClass("summarymcmc4",
         representation(statistics = "matrix",
                        quantiles = "matrix",
                        start = "numeric",
                        end = "numeric",
                        thin = "numeric",
                        nchain = "numeric"))
setOldClass("summary.mcmc", S4Class="summarymcmc4")
## removeClass("SummaryMcmc4")
