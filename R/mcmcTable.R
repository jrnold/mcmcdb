##' @import data.table

setClass("McmcTable", contains=c("data.table"))

validate_mcmc_table <- function(object) {
    msg <- character()
    if (colnames(obj) != c("parameter", "chain", "iter", "value")) {
        msg <- append(msg, "Column names are incorrect")
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidate("McmcTable", validate_mcmc_table)

setClass("McmcTable2", contains=c("McmcTable"),
         representation(parameters="list",
                        columns="list"))







