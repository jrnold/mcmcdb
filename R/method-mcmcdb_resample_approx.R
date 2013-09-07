#' @include class-Mcmcdb.R
#' @include method-mcmcdb_samples_flatpars.R
#' @exportMethod mcmcdb_resample_approx
NULL


#' @rdname mcmcdb_resample_approx
#' @docType methods
#' @aliases mcmcdb_resample_approx
#' @aliases mcmcdb_resample_approx-methods
#' @title Resample from a Posterior Approximation
#'
#' @description Draw samples from a normal or t-distribution approximation
#' of the posterior distribution.
#'
#' @param object object with flat parameters.
#' @param n Number of samples to draw.
#' @param scale_mult Multiplier of scale matrix
#' @param df Degrees of freedom. If \code{Inf}, then a normal distribution is used.
#' Otherwise, a Student t distribution is used.
#' @param ... Passed to \code{\link{mcmcdb_samples_flatpars}}.
setGeneric("mcmcdb_resample_approx",
           function(object, ...) standardGeneric("mcmcdb_resample_approx"))

mcmcdb_resample_approx.matrix <-
  function(object, n = 1, scale_mult = 1, df = Inf) {
    mu <- aaply(object, 2, mean)
    sigma <- cov(object) * scale_mult
    if (is.infinite(df)) {
      rmvnorm(n, mu, sigma)
    } else {
      rmvt(n, sigma = sigma, df = df, delta = mu)
    }
  }

#' @rdname mcmcdb_resample_approx
#' @aliases mcmcdb_resample_approx,matrix-method
setMethod("mcmcdb_resample_approx", "matrix", mcmcdb_resample_approx.matrix)

mcmcdb_resample_approx.Mcmcdb <-
  function(object, n = 1, scale_mult = 1, df = Inf, ...) {
    mcmcdb_resample(mcmcdb_samples_flatpars(object, ...))
  }

#' @rdname mcmcdb_resample_approx
#' @aliases mcmcdb_resample_approx,Mcmcdb-method
setMethod("mcmcdb_resample_approx", "Mcmcdb", mcmcdb_resample_approx.Mcmcdb)
