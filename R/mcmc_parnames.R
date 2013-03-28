#' @include package.R
#' @export mcmc_parnames_stan
#' @export mcmc_parnames_bugs
#' @export mcmc_parnames_underscore
#' @export mcmc_parnames_pattern
NULL

#' @rdname mcmc_parnames
#' @aliases mcmc_parname_pattern
#' @title Create paramter names for flattened parameters
#'
#' @description Given a parameter name and a matrix of index values, generate
#' names for the unlisted parameters.
#'
#' \describe{
#' \item{\code{mcmc_parnames_bugs}}{Writes BUGS/JAGS style flat parameter names, e.g. \code{"alpha[1,2]"}}.
#' \item{\code{mcmc_parnames_stan}}{Writes Stan style flat parameter names, e.g. \code{"alpha.1.2"}}.
#' \item{\code{mcmc_parnames_underscore}}{Writes parameter names with indexes seperated by underscores, e.g. \code{"alpha_1_2"}}.
#' \item{\code{mcmc_parnames_pattern}}{Writes parameter names with arbitrary patterns.}
#' }
#'
#' @param x \code{character} Parameter name.
#' @param d \code{integer} Dimension of the array.
#' @param pre \code{character} String to put before indices.
#' @param sep \code{character} String used to seperate indices.
#' @param post \code{character} String to put after indices.
#' @param colmajor \code{logical}. If \code{TRUE}, then indices are
#' in column-major order (R's default), else row-major.
#' @return \code{character} vector of flat parameter names.
#'
#' @examples
#' mcmc_parnames_bugs("alpha", c(1, 2))
#' mcmc_parnames_stan("alpha", c(1, 2))
#' mcmc_parnames_underscore("alpha", c(1, 2))
#' mcmc_parnames_pattern("alpha", c(1, 2), "<", ";", ">")
mcmc_parnames_pattern <- function(x, d, pre=".", sep=".", post="",
                                  colmajor = TRUE) {
  scalar <- identical(d, 1L)
  FUN <- function(i) {
    mcmc_parnames_pattern_idx(x, i, scalar, pre=pre, sep=sep, post=post,
                              colmajor = colmajor)
  }
  apply(expand_grid_dim(d), 1, FUN)
}

mcmc_parnames_pattern_idx <- function(x, idx, scalar=FALSE, pre=".", sep=".",
                                      post="", colmajor = TRUE) {
  if (scalar) {
    x
  } else {
    if (!colmajor) {
      if (is.matrix(idx)) {
        idx <- apply(idx, 1, rev)
      } else {
        idx <- rev(idx)
      }
    }
    if (is.matrix(idx)) {
      idxstr <- apply(idx, 1, paste, collapse=sep)
    } else {
      idxstr <- paste(idx, collapse=sep)
    }
    paste0(x, pre, idxstr, post)
  }
}

#' @rdname mcmc_parnames
#' @aliases mcmc_parnames_stan
mcmc_parnames_stan <- function(x, d, colmajor=TRUE) {
  mcmc_parnames_pattern(x, d, ".", ".", "", colmajor)
}

#' @rdname mcmc_parnames
#' @aliases mcmc_parnames_bugs
mcmc_parnames_bugs <- function(x, d, colmajor=TRUE) {
  mcmc_parnames_pattern(x, d, "[", ",", "]", colmajor)
}

#' @rdname mcmc_parnames
#' @aliases mcmc_parnames_underscore
mcmc_parnames_underscore <- function(x, d, colmajor=TRUE) {
  mcmc_parnames_pattern(x, d, "_", "_", "", colmajor)
}
