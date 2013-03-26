#' @include utilities.R
#' @include class-misc.R
#' @export mcmc_parparser_bugs
#' @export mcmc_parparser_stan
#' @export mcmc_parparser_underscore
#' @export mcmc_parparser_pattern
#' @export mcmc_parparser_scalar
#' @export mcmc_parparser_guess
NULL

#' @rdname mcmc_parparsers
#' @title Parse MCMC parameter names
#'
#' @description Functions that parse a vector of flat parameter names
#' and return an object of class \code{\linkS4class{McmcdbFlatpars}}.
#'
#' \describe{
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' treating each parameter as a scalar. E.g. \code{"beta.1"}
#' and \code{"beta.2"} will be treated two parameter arrays of
#' size 1.}
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' in the Stan style, e.g. \code{"beta.1.1"}}
#' \item{\code{mcmc_parparser_guess}}{Tries to guess the format of the parameters}
#' \item{\code{mcmc_parparser_pattern}}{Parses parameter names using arbitrary patterns.}
#' }
#'
#' @param x \code{character} vector with flat parameter names.
#' @param pre \code{character} Pattern between parameter name and indices. If a pattern
#' grouping must be used, use "(?: )". 
#' @param sep \code{character} Pattern seperating each index.
#' @param post \code{character} Pattern following the indices.
#' @return Object of class \code{McmcdbFlatpars}
#'
#' @examples
#' mcmc_parparser_bugs(c("beta[1,1]", "beta[1,2]"))
#' mcmc_parparser_stan(c("beta.1.1", "beta.1.2"))
#' mcmc_parparser_underscore(c("beta_1_1", "beta_1_2"))
#' mcmc_parparser_pattern(c("beta<1;1>", "beta<1;2>"), "<", ";", ">")
#' mcmc_parparser_guess(c("beta[1,1]", "beta[1,2]"))
#' mcmc_parparser_guess(c("beta.1.1", "beta.1.2"))
#' mcmc_parparser_scalar(c("beta[1,1]", "beta[1,2]"))
#' # for pattern groups, you must use (?:
#' mcmc_parparser_pattern(c("beta<1;1>", "beta.1,2"), "[<.]", "[;,]", "(?:>|)")
mcmc_parparser_scalar <- function(x) {
  McmcdbFlatpars(data.frame(flatpar = x,
                            pararray = x,
                            idx = as.character("1"),
                            scalar = TRUE,
                            stringsAsFactors = FALSE))
}


#' @rdname mcmc_parparsers
#' @aliases mcmc_parparser_pattern
mcmc_parparser_pattern <- function(x, pre, sep, post) {
  regexp <-
    sprintf("^(.*?)(%s(\\d+(%s\\d+)?)%s)?$", pre, sep, post)
  x_split <- data.frame(str_match(x, regexp)[ , c(1, 2, 4)],
                        stringsAsFactors = FALSE)
  names(x_split) <- c("flatpar", "pararray", "idx")
  x_split$scalar <- (x_split$idx == "")
  x_split$idx[x_split$scalar] <- "1"
  x_split$idx <- gsub(sep, ",", x_split$idx)
  McmcdbFlatpars(x_split)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparser_stan
mcmc_parparser_stan <- function(x) {
  mcmc_parparser_pattern(x, "\\.", "\\.", "")
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparser_bugs
mcmc_parparser_bugs <- function(x) {
  mcmc_parparser_pattern(x, "\\[", ",", "\\]")
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparser_underscore
mcmc_parparser_underscore <- function(x) {
  mcmc_parparser_pattern(x, "_", "_", "")
}

has_bracket_index <- function(x) {
  str_detect(x, "(\\[(\\d+(,\\d+)?)\\])$")
}

has_dots_index <- function(x) {
  str_detect(x, "(\\.(\\d+(\\.\\d+)?))$")
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparser_guess
mcmc_parparser_guess <- function(x) {
  if (any(has_bracket_index(x))) {
    mcmc_parparser_bugs(x)
  } else if (any(has_dots_index(x))) {
    mcmc_parparser_stan(x)
  } else {
    mcmc_parparser_scalar(x)
  }
}
