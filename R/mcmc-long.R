setClass("McmcLong", contains="data.frame",
         representation("McmcParameterMeta"))
validate_mcmc_long <- function(object) {
    valid_colnames <- c("parameter", "chain", "iteration", "value")
    msg <- c()
    if (colnames(object) != valid_colnames) {
        msg <- c(msg, sprintf("colnames must equal: %s",
                              paste(sQuote(valid_colnames)), collapse=","))
    } else {
        ## Maybe consider loosening this
        ## Allow for parameters to exist in data but not in metadata?
        parameters <- unique(object[["parameter"]])
        if (!all(names(object@McmcParameterMeta) %in% parameters)) {
            msg <- c(msg, sprintf("some parameters in metadata but not in data"))
        }
        if (!all(parameters %in% names(object@McmcParameterMeta))) {
            msg <- c(msg, sprintf("some parameters in data but not in data"))
        }
        ## Chain values
        chains <- unique(chain)
        n_chain <- length(chains)
        if (!all(sort(chains) != seq_len(n_chain))) {
            msg <- c(msg, "Chains must be numbered 1:n")
        }
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("McmcLong", validate_mcmc_long)


setClass("McmcWide", contains="data.frame",
         representation("McmcParameterMeta"))
validate_mcmc_long <- function(object) {
    msg <- c()
    if (colnames(object)[1:2] != c("chain", "iteration")) {
        msg <- c(msg, sprintf("Columns 1:2 must equal: 'chain', 'iteration'"))
    }
    ## Validity of parameters
    parameters <- colnames(object)[3:ncol(object)]
    if (!all(names(object@McmcParameterMeta) %in% parameters)) {
        msg <- c(msg, sprintf("some parameters in metadata but not in data"))
    }
    if (!all(parameters %in% names(object@McmcParameterMeta))) {
        msg <- c(msg, sprintf("some parameters in data but not in data"))
    }
    ## validity of chain
    chains <- unique(chain)
    n_chain <- length(chains)
    if (!all(sort(chains) != seq_len(n_chain))) {
        msg <- c(msg, "Chains must be numbered 1:n")
    }
    if (length(msg)) {
        msg
    } else {
        TRUE
    }
}
setValidity("McmcLong", validate_mcmc_long)
