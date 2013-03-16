#' @include parameter-parsers.R
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
#' @param dim \code{integer} Dimension of the array.
#'
#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_stan
#' @examples
#' mcmc_parnames_bugs("alpha", c(1, 2))
#' mcmc_parnames_stan("alpha", c(1, 2))
mcmc_parnames_stan <- function(x, dim) {
  apply(expand_grid_dim(dim), 1, function(i) mcmc_parnames_stan_idx(x, i, dim))
}

mcmc_parnames_stan_idx <- function(x, idx, dim) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    paste(x, paste(idx, collapse="."), sep=".")
  }
}

#' @rdname mcmc_create_parnames
#' @aliases mcmc_create_parnames_bugs
mcmc_parnames_bugs <- function(x, dim) {
  apply(expand_grid_dim(dim), 1, function(i) mcmc_parnames_bugs_idx(x, i, dim))
}

mcmc_parnames_bugs_idx <- function(x, idx, dim) {
  if (identical(as.integer(dim), 1L)) {
    x
  } else {
    paste0(x, "[", paste(idx, collapse=","), "]")
  }
}

#' Convert between BUGS/JAGS and Stan style flat parameter names
#'
#' \describe{
#' \item{\code{bugs_to_stan_parnames}}{Convert from BUGS style to Stan style flat parameter names,
#' e.g. \code{"alpha[1,2]"} to \code{"alpha.1.2"}.
#' \item{\code{stan_to_bugs_parnames}}{Convert from Stan style to BUGS style flat parameter names,
#' e.g. \code{"alpha.1.2"} to \code{"alpha[1,2]"}.
#' }
#'
#' @param x
#' @return \code{character} vector of the converted flat parameter names.
#' @export
#' @examples
#' stan_parnames <- c("alpha", "beta.1", "gamma.1.1")
#' bugs_parnames <- c("alpha", "beta[1]", "gamma[1,1]")
#' identical(bugs_to_stan_parnames(bugs_parnames), stan_parnames)
#' identical(stan_to_bugs_parnames(stan_parnames), bugs_parnames)
bugs_to_stan_parnames <- function(x) {
  gsub("]", "", gsub("[\\[,]", ".", x))
}

#' @rdname bugs_to_stan_parnames
#' @aliases stan_to_bugs_parnamse
#' @export
stan_to_bugs_parnames <- function(x) {
  y <- str_split_fixed(x, fixed("."), 2)
  apply(y, 1, function(yi) {
    paste0(yi[1],
           ifelse(yi[2] == "", "", paste0("[", gsub("\\.", ",", yi[2]), "]")))
  })
}

