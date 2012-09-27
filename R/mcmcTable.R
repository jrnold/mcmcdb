##' @export
setClass("McmcTable", contains="data.table")
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

##' @export
setClass("McmcTable2", contains="McmcTable",
         representation(parameters="list",
                        columns="list"))




