#' @include class-McmcdbWide.R
#' @include method-mcmcdb_flatpars.R
#' @include method-mcmcdb_parameters.R
NULL

# Miscellaneous utility functions used with McmcdbWide functions
# returns list of flatparameters or TRUE
mcmcdb_wide_select_params <- function(x, flatpars=NULL, pararrays=NULL) {
  flatpars <- union(flatpars,
                    unlist(mcmcdb_parameters(x)[pararrays]))
  if (is.null(flatpars)) {
    flatpars <- TRUE
  }
  flatpars
}

mcmcdb_wide_select_params2 <- function(x, flatpars=NULL, pararrays=NULL) {
  if (is.null(flatpars)) {
    flatpars <- names(mcmcdb_flatpars(x))
  }
  union(flatpars, unlist(mcmcdb_parameters(x)[pararrays]))
}

# returns logical vector 
mcmcdb_wide_select_iters <- function(x, chain_id = NULL, iter = NULL) {
  ## chains
  if (is.null(chain_id)) {
    ischain <- TRUE
  } else {
    ischain <- (x@iters[["chain_id"]] %in% chain_id)
  }
  # iterations
  if (is.null(iter)) {
    isiter <- TRUE
  } else {
    isiter <- (x@iters[["iter"]] %in% iter)
  }
  ischain & isiter
}

mcmcdb_wide_subset <- function(x, pararrays = NULL, flatpars = NULL,
                               chain_id = NULL, iter = NULL, drop=FALSE) {
  params <- mcmcdb_wide_select_params(x, flatpars, pararrays)
  rows <- mcmcdb_wide_select_iters(x, chain_id = unique(chain_id),
                                   iter = unique(iter))
  #iters <- mcmcdb_iters(x, drop=TRUE)[rows, ]
  x@samples[rows, params, drop=drop]
  #rownames(samples) <- paste(iters[["chain_id"]], iters[["iter"]], sep=".")
  #samples
}


