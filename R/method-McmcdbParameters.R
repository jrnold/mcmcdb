#' @include class-McmcdbParameters.R
#' @include mcmc_parparser.R
#' @exportMethod McmcdbParameters
NULL

# Data frame should have columns idx, flatpar
# x$idx should be comma separated indices
pararrays_from_df <- function(x) {
  idx <- do.call(rbind,
                 llply(str_split(x$idx, fixed(",")),
                       as.integer))
  d <- as.integer(aaply(idx, 2, max))
  tmpl <- array(NA_character_, d)
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
#' McmdbParameters(c("alpha", "beta.1", "beta.2"))
#' McmdbParameters(c("alpha", "beta[1]", "beta[2]"),
#'                 parser = mcmc_parparser_bugs)
NULL
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
            callGeneric(dlply(x, "pararray", pararrays_from_df))
          })

#' @rdname McmcdbParameters-methods
#' @aliases McmcdbParameters,list-method
setMethod("McmcdbParameters", "list",
          function(x) {
            callGeneric(ListOfCharArrays(x))
          })

#' @rdname McmcdbParameters-methods
#' @rdname McmcdbParameters,ListOfCharArrays-method
setMethod("McmcdbParameters", "ListOfCharArrays",
          function(x) {
            new("McmcdbParameters", x)
          })

#' @rdname McmcdbParameters-methods
#' @rdname McmcdbParameters,missing-method
setMethod("McmcdbParameters", "missing",
          function(x) {
            new("McmcdbParameters")
          })
