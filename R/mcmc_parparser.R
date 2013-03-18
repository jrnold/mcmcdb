#' @include utilities.R
#' @include class-misc.R
#' @export mcmc_parparser_bugs
#' @export mcmc_parparser_stan
#' @export mcmc_parparser_scalar
#' @export mcmc_parparser_guess
NULL

#' Parse MCMC parameter names
#'
#' Functions that parse a vector of flat parameter names
#' and return an object of
#'
#' These functions are provided as an argument to \code{\link{mcmc_parse_parnames}}.
#' 
#' \describe{
#' \item{\code{mcmc_parparser_bugs}}{Parses parameter names
#' in the Stan style, e.g. \code{"beta[1,1]"}}
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' treating each parameter as a scalar. E.g. \code{"beta.1"}
#' and \code{"beta.2"} will be treated two parameter arrays of
#' size 1.}
#' \item{\code{mcmc_parparser_stan}}{Parses parameter names
#' in the Stan style, e.g. \code{"beta.1.1"}}
#' \item{\code{mcmc_parparser_guess}}{Tries to guess the format of the parameters}
#' }
#'
#' @param x \code{character} vector with flat parameter names.
#' @return Object of class \code{McmcdbFlatpars}
#'
#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_scalar
#' @examples
#' mcmc_parparser_bugs(c("beta[1,1]", "beta[1,2]"))
#' mcmc_parparser_stan(c("beta.1.1", "beta.1.2"))
#' mcmc_parparser_guess(c("beta[1,1]", "beta[1,2]"))
#' mcmc_parparser_guess(c("beta.1.1", "beta.1.2"))
#' mcmc_parparser_scalar(c("beta[1,1]", "beta[1,2]"))
mcmc_parparser_scalar <- function(x) {
  McmcdbFlatpars(data.frame(flatpar = x,
                            pararray = x,
                            idx = as.character("1"),
                            stringsAsFactors = FALSE))
}


#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_stan
mcmc_parparser_stan <- function(x) {
  x_split <- data.frame(str_split_fixed(x, fixed("."), 2),
                        stringsAsFactors = FALSE)
  names(x_split) <- c("pararray", "idx")
  x_split$idx[x_split$idx == ""] <- "1"
  x_split$idx <- gsub("\\.", ",", x_split$idx)
  x_split$flatpar <- x
  McmcdbFlatpars(x_split[ , c("flatpar", "pararray", "idx")])
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_bugs
mcmc_parparser_bugs <- function(x) {
  regexp <- "([^\\[]+)(\\[([0-9,]+)\\])?"
  x_split <- data.frame(str_match(x, regexp)[ , c(1, 2, 4)],
                        stringsAsFactors = FALSE)
  names(x_split) <- c("flatpar", "pararray", "idx")
  x_split$idx[x_split$idx == ""] <- "1"
  McmcdbFlatpars(x_split)
}

#' @rdname mcmc_parparsers
#' @aliases mcmc_parparsers_guess
mcmc_parparser_guess <- function(x) {
  if (all(valid_mcmc_parnames(x, "stan"))) {
    mcmc_parparser_stan(x)
  } else if (all(valid_mcmc_parnames(x, "bugs"))) {
    mcmc_parparser_bugs(x)
  } else {
    mcmc_parparser_scalar(x)
  }
}
