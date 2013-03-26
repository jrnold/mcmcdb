#' @include package.R
NULL

parnames_from_flatpars <- function(x, pre, sep, post) {
  FUN <- function(flatpar, pararray, idx, scalar) {
    mcmc_parnames_pattern_idx(pararray, as.integer(str_split(idx, ",")[[1]]),
                              scalar, pre, sep, post)
  }
  as.character(dlply(x, "flatpar", splat(FUN)))
}

#' Convert between BUGS/JAGS and Stan style flat parameter names
#'
#' Utility functions to convert flat parameter names between the BUGS (\code{"alpha[1,1]"}) and
#' Stan style (\code{"alpha.1.1"}).
#'
#' @param x \code{character} vector of flat parameter names.
#' @return \code{character} vector of the converted flat parameter names.
#' @export
#' @examples
#' stan_parnames <- c("alpha", "beta.1", "gamma.1.1")
#' bugs_parnames <- c("alpha", "beta[1]", "gamma[1,1]")
#' identical(bugs_to_stan_parnames(bugs_parnames), stan_parnames)
#' identical(stan_to_bugs_parnames(stan_parnames), bugs_parnames)
bugs_to_stan_parnames <- function(x) {
  parnames_from_flatpars(mcmc_parparser_bugs(x), ".", ".", "")
}

#' @rdname bugs_to_stan_parnames
#' @aliases stan_to_bugs_parnamse
#' @export
stan_to_bugs_parnames <- function(x) {
  parnames_from_flatpars(mcmc_parparser_stan(x), "[", ",", "]")
}
