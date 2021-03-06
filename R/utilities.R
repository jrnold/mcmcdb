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
  sprintf("\\preformatted{%s}\n", mcmcdb_metadata(x)[["model_code"]])
}


source_env <- function(file, ..., env.opts = list(parent=parent.frame()) ) {
    e <- do.call(new.env, env.opts)
    sys.source(file, envir=e, ...)
    e
}

source_list <- function(...) {
    as.list(source_env(...))
}

stridx <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    if (length(x) > 1) {
      dstr <- sprintf("[1:%d]", length(x))
    } else {
      dstr <- ""
    }
  } else {
    dstr <- sapply(d,
                   function(di) {
                     if (di == 1) {
                       "1"
                     } else {
                       sprintf("1:%d", di)
                     }
                   })
    dstr <- sprintf("[%s]", paste(dstr, collapse = ", "))
  }
  dstr
}

dimlen <- function(x) {
  d <- dim(x)
  if (is.null(d)) {
    d <- length(x)
  }
  d
}

plyr_fun <- function(input, output) {
  match.fun(sprintf("%s%sply", input, output))
}
