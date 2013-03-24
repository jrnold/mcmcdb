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
  x@samples[rows, params, drop=drop]
}


