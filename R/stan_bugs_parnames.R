#' @include package.R
NULL

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
