#' @include package.R
#' @include utilities.R
#' @include class-misc.R
#' @include class-matrix.R
#' @include class-McmcdbParameters.R
NULL

#' MCMC Samples in wide-format
#'
#' Mcmc samples stored as a matrix with (number of chains x number of
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
#' \item{\code{metadata}}{\code{list} with general data about the samples.}
#' \item{\code{version}}{\code{character} version of \pkg{mcmcdb} with which the object was created}
#' }
#' 
#' @name McmcdbWide-class
#' @rdname McmcdbWide-class
#' @export
#' @aliases McmcdbWide-class
#' @docType class
#' @family McmcdbWide methods
#' @seealso \code{\link{McmcdbWide}} for the method usually used to create these objects.
#' @examples
#' showClass("McmcdbWide")
#' 
#' # Example included in the package
#' data("line_mcmcdbwide")
#' print(line_mcmcdbwide)
#' 
#' # access data
#' mcmcdb_chains(line_mcmcdbwide)
#' mcmcdb_chains(line_mcmcdbwide, drop=TRUE)
#'
#' mcmcdb_parameters(line_mcmcdbwide)
#' mcmcdb_pardims(line_mcmcdbwide)
#' mcmcdb_pararrays(line_mcmcdbwide)
#' mcmcdb_flatpars(line_mcmcdbwide)
#' mcmcdb_par_indices(line_mcmcdbwide)
#' 
#' mcmcdb_iters(line_mcmcdbwide)
#' mcmcdb_flatpar_chains(line_mcmcdbwide)
#' mcmcdb_metadata(line_mcmcdbwide)
setClass("McmcdbWide",
         representation(samples="matrix",
                        parameters="McmcdbParameters",
                        chains="McmcdbChains", # chain_id
                        iters="McmcdbIters", # chain_id, iter
                        flatpar_chains="McmcdbFlatparChainsOrNull", # parname, chain_id
                        metadata="list",
                        version="character"),
         prototype(samples = matrix(),
                   parameters = McmcdbParameters(),
                   chains = McmcdbChains(),
                   iters = McmcdbIters(),
                   metadata = list(),
                   version = VERSION))

validate_McmcdbWide <- function(object) {
  nsamples <- nrow(object@samples)
  if (nsamples != nrow(object@iters)) {
    return("nrow(object@iters) != nrow(object@samples))")
  }
  if (!all(colnames(object@samples) %in% names(object@parameters@flatpars))) {
    return("Columns in object@samples are missing from object@parameters")
  }
  ## Additional tests
  TRUE
}

setValidity("McmcdbWide", validate_McmcdbWide)

show.McmcdbWide <- function(object) {
  cat(sprintf("An object of class %s\n", dQuote("McmcdbWide")))
  nsamples <- nrow(object@samples)
  nchains <- nrow(object@chains)
  cat(sprintf("%d total observations from %d chains\n",
              nsamples, nchains))
  cat(sprintf("Parameter arrays:\n"))
  for (i in names(object@parameters@pararrays)) {
    cat(sprintf("$ %s [%s]\n", i, 
                paste(object@parameters@pararrays[[i]]@dim, collapse=",")))
  }
}

setMethod("show", "McmcdbWide", show.McmcdbWide)

