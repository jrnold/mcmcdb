#' @include package.R
#' @exportMethod summary
NULL

#' @name summary-methods
#' @rdname summary-methods
#' @keywords methods
#' @title Methods for function \code{summary}
#'
#' @description Summary methods
#'
#' @param object Object containing the MCMC samples
#' @param .fun Function used to summarize the samples.
#' @param pararrays Parameter arrays to use.
#' @return Named \code{list} of \code{array} objects, one for each
#' parameter array.
#' data(line_samples)
#' summary(line_samples)
setGeneric("summary")

summary.Mcmcdb <- function(object, .fun = mean, pararrays = NULL, ...) {
  .FUN <- function(x) {
    ndim <- length(dim(x))
    margins <- seq_len(ndim - 1)
    aaply(x, margins, .fun)
  }
  llply(mcmcdb_samples_pararrays(object, pararrays = pararrays),
        .fun = .FUN, ...)
}

#' @rdname summary-methods
#' @aliases summary,Mcmcdb-method
#' @family Mcmcdb methods
setMethod("summary", "Mcmcdb", summary.Mcmcdb)
