##' @section Slots
##'
##' @export
setClass("McmcTable",
         contains="data.table",
         representation(parameters="list",
                        template="list"))
validate_mcmc_table <- function(object) {
    msg <- character()
    if (colnames(object) != c("parameter", "chain", "iter", "value")) {
        msg <- append(msg, "Column names are incorrect")
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("McmcTable", validate_mcmc_table)

setAs("McmcTable", "mcmc.list",






