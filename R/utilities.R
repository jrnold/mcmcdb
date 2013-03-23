#' @include package.R
NULL

#' Generate indices for all dimensions
#'
#' Create matrix if all indices for a given
#' dimension vector.
#'
#' @param d Array dimensions
#' @return \code{matrix} with dimensions \code{c(prod(dim), dim)}.
#' @keywords internal
expand_grid_dim <- function(d) {
  as.matrix(expand.grid(lapply(as.integer(d), seq_len)))
}


print_rd_model_code <- function(x) {
  sprintf("\\preformatted{%s}\n", mcmcdb_metadata(line_samples)[["model_code"]])
}
