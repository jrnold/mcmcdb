#' @include package.R
#' @export mcmc_parnames_stan
#' @export mcmc_parnames_bugs
NULL

#' Create BUGS/Stan parameter names
#'
#' Given a parameter name and a matrix of index values, generate
#' names for the unlisted parameters.
#'
#' \describe{
#' \item{\code{mcmc_parnames_bugs}}{Writes BUGS/JAGS style flat parameter names, e.g. \code{"alpha[1,2]"}}.
#' \item{\code{mcmc_parnames_stan}}{Writes Stan style flat parameter names, e.g. \code{"alpha.1.2"}}.
#' }
#'
#' @param x \code{character} Parameter name.
#' @param d \code{integer} Dimension of the array.
#' @return \code{character} vector of flat parameter names.
#'
#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_stan
#' @examples
#' mcmc_parnames_bugs("alpha", c(1, 2))
#' mcmc_parnames_stan("alpha", c(1, 2))
mcmc_parnames_stan <- function(x, d) {
  apply(expand_grid_dim(d), 1, function(i) mcmc_parnames_stan_idx(x, i, d))
}

mcmc_parnames_stan_idx <- function(x, idx, d) {
  if (identical(as.integer(d), 1L)) {
    x
  } else {
    paste(x, paste(idx, collapse="."), sep=".")
  }
}

#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_bugs
mcmc_parnames_bugs <- function(x, d) {
  apply(expand_grid_dim(d), 1, function(i) mcmc_parnames_bugs_idx(x, i, d))
}

mcmc_parnames_bugs_idx <- function(x, idx, d) {
  if (identical(as.integer(d), 1L)) {
    x
  } else {
    paste0(x, "[", paste(idx, collapse=","), "]")
  }
}
