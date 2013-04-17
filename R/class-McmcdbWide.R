#' @include package.R
#' @include utilities.R
#' @include class-misc.R
#' @include class-matrix.R
#' @include class-McmcdbMem.R
#' @include class-McmcdbParameters.R
#' @exportClass McmcdbWide
NULL

## setClassUnion("listOrNULL", c("namedList", "NULL"))
## setClassUnion("numericOrNULL", c("numeric", "NULL"))

#' @name McmcdbWide-class
#' @rdname McmcdbWide-class
#' @aliases McmcdbWide-class
#' 
#' @title MCMC Samples in wide-format
#'
#' @description Mcmc samples stored as a matrix with (number of chains x number of
#' iterations) rows and (number of flat parameters) columns.
#' 
#' @section Slots:
#'
#' \describe{
#' \item{\code{samples}}{\code{matrix} containing the sample parameter values. This matrix has (number of chains * iterations) rows, and )number of flat parameters) columns.}
#' \item{\code{parameters}}{\linkS4class{McmcdbParameters}.}
#' \item{\code{chains}}{\linkS4class{McmcdbChains}.}
#' \item{\code{iters}}{\linkS4class{McmcdbIters}.}
#' \item{\code{flatpar_chains}}{\linkS4class{McmcdbFlatparChains}.}
#' \item{\code{metadata}}{\code{list} with metadata about the samples.}
#' \item{\code{model_data}}{\code{namedList}. The data used in the model.}
#' \item{\code{version}}{\code{character} version of \pkg{mcmcdb} with which the object was created}
#' }
#' 
#' @docType class
#' @seealso \code{\link{McmcdbWide}} for the method usually used to create these objects.
#' @family Mcmcdb classes
#' @family Mcmcdb methods
#' @family McmcdbWide methods
#' @family McmcdbMem methods
#' @examples
#' showClass("McmcdbWide")
#' 
#' # Example included in the package
#' data("line_samples")
#' print(line_samples)
#' 
#' # Extract samples
#' # As matrix of flat parameters
#' mcmcdb_samples_flatpars(line_samples)
#' mcmcdb_samples_pararrays(line_samples)
#' mcmcdb_samples_iter(line_samples)
#' mcmcdb_samples_long(line_samples)
#' 
#' # extract samples (flat parameters)
#' str(line_samples["beta[1]"])
#' line_samples["beta[1]", 1, 1:10]
#' summary(line_samples[["beta", 1, 1:4, drop=FALSE]])
#' 
#' # extract samples (parameter arrays)
#' str(line_samples[["beta"]])
#' summary(line_samples[["beta", 1, 1:4, drop=FALSE]])
#' 
#' # Chain information
#' mcmcdb_chains(line_samples)
#' mcmcdb_chains(line_samples, drop=FALSE)
#' 
#' # Iteration information
#' summary(mcmcdb_iters(line_samples))
#' 
#' # Paramater information
#' mcmcdb_parameters(line_samples)
#' mcmcdb_pardims(line_samples)
#' mcmcdb_flatpars(line_samples)
#' mcmcdb_paridx(line_samples)
#' 
#' # extract metadata
#' mcmcdb_metadata(line_samples)
setClass("McmcdbWide",
         contains = "McmcdbMem",
         representation(samples="matrix"),
         prototype(samples = matrix()))

validate.McmcdbWide <- function(object) {
  nsamples <- nrow(object@samples)
  parameters <- colnames(object@samples)
  chain_ids <- unique(object@chains$chain_id)

  if (!setequal(parameters, names(mcmcdb_flatpars(object@parameters)))) {
    return("colnames of samples do not match parameters")
  }
  if (nsamples != nrow(object@iters)) {
    return("nrow(object@iters) != nrow(object@samples))")
  }
  if (!setequal(chain_ids, unique(object@iters$chain_id))) {
    return("iters$chain_id does not match chains$chain_id values")
  }
  if (!setequal(unique(object@flatpar_chains$chain_id), chain_ids)) {
    return("flatpar_chains$chain_id does not match chains$chain_id values")
  }
  if (!setequal(levels(object@flatpar_chains$flatpar), parameters)) {
    return("flatpar_chains$flatpar does not match parameters")
  }
  TRUE
}

setValidity("McmcdbWide", validate.McmcdbWide)

show.McmcdbWide <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcdbWide")))
  nsamples <- nrow(object@iters)
  nchains <- nrow(object@chains)
  if (nsamples > 0) {
    iterbychain <- paste(ddply(object@iters, "chain_id",
                               function(x) {
                                 data.frame(niter = nrow(x))
                               })[["niter"]],
                         collapse=", ")
  } else {
    iterbychain <- "0"
  }
  cat(sprintf("%d samples from %d chain%s (%s)\n",
              nsamples, nchains,
              if (nchains != 1) "s" else "",
              iterbychain))
  cat("Parameters:\n")
  parameters <- mcmcdb_parameters(object)
  for (i in seq_along(parameters)) {
    cat(sprintf("$ %s [%s]\n",
                names(parameters)[i], paste(dim(parameters[[i]]), collapse=",")))
  }
}

setMethod("show", "McmcdbWide", show.McmcdbWide)

