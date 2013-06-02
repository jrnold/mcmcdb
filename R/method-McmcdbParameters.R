#' @include class-McmcdbParameters.R
#' @include mcmc_parparser.R
#' @exportMethod McmcdbParameters
NULL

# Data frame should have columns idx, flatpar
# x$idx should be comma separated indices
parameters_from_df <- function(x) {
  idx <- do.call(rbind,
                 llply(str_split(x$idx, fixed(",")),
                       as.integer))
  d <- as.integer(aaply(idx, 2, max))
  if (all(x[["scalar"]])) {
    tmpl <- rep(NA_character_, d)
  } else {
    tmpl <- array(NA_character_, d)
  }
  tmpl[idx] <- as.character(x[["flatpar"]])
  tmpl
}

#' @rdname McmcdbParameters-methods
#' @name McmcdbParameters
#' @aliases McmcdbParameters-methods
#' @aliases McmcdbParameters
#' @title Create McmcbParameter objects
#' @description Functions to create \code{\linkS4class{McmcdbParameters}}
#' objects.
#'
#' @param x object
#' @param parser A function that returns an object of \code{\linkS4class{McmcdbFlatpars}}. For example, \code{\link{mcmc_parparser_stan}} or \code{\link{mcmc_parparser_guess}}.
#' @examples
#' McmcdbParameters(c("alpha", "beta.1", "beta.2"))
#' McmcdbParameters(c("alpha", "beta[1]", "beta[2]"),
#'                 parser = mcmc_parparser_bugs)
setGeneric("McmcdbParameters",
           function(x, ...) {
             standardGeneric("McmcdbParameters")
           })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,character-method
setMethod("McmcdbParameters", "character",
          function(x, parser=mcmc_parparser_guess) {
            callGeneric(parser(x))
          })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,data.frame-method
setMethod("McmcdbParameters", "data.frame",
          function(x) callGeneric(McmcdbFlatpars(x)))

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,McmcdbFlatpars-method
setMethod("McmcdbParameters", "McmcdbFlatpars",
          function(x) {
            callGeneric(dlply(x, "parameter", parameters_from_df))
          })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,list-method
setMethod("McmcdbParameters", "list",
          function(x) {
            new("McmcdbParameters", llply(x, ParnameArray))
          })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,missing-method
setMethod("McmcdbParameters", "missing",
          function(x) {
            new("McmcdbParameters")
          })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,McmcdbParameters-method
setMethod("McmcdbParameters", "McmcdbParameters", identity)
