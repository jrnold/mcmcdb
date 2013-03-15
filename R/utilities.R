#' Select first observation
#'
#' @param x \code{vector}
#' @value Returns the first element of \code{x}.
#' @keywords internal
first <- function(x) x[1]

#' Generate indices for all dimensions
#'
#' Create matrix if all indices for a given
#' dimension vector.
#'
#' @param dim Array dimensions
#' @returns \code{matrix} with dimensions \code{c(prod(dim), dim)}.
#' @keywords internal
expand_grid_dim <- function(dim) {
  as.matrix(expand.grid(lapply(as.integer(dim), seq_len)))
}
