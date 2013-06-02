#' @include package.R
#' @include method-mcmcdb_chains.R
#' @include method-mcmcdb_samples_flatpars.R
#' @include method-mcmcdb_samples_parameters.R
NULL

setAs("Mcmcdb", "mcmc.list",
      function(from) {
        chains <- mcmcdb_chains(from, drop=FALSE)
        chain_id <- chains[["chain_id"]]
        to <- llply(chain_id,
                    function(i) {
                      x <- mcmcdb_samples_flatpars(from, chain_id = i)
                      class(x) <- "mcmc"
                      x
                    })
        for (i in nrow(chains)) {
          if ("iter_start" %in% colnames(chains)) {
            mcpar <- as.numeric(chains[i, c("iter_start", "iter_end", "iter_thin")])
            attr(to[[i]], "mcpar") <- mcpar
          } 
        }
        class(to) <- "mcmc.list"
        to
      })

setAs("Mcmcdb", "matrix",
      function(from) {
        mcmcdb_samples_flatpars(from)
      })

setAs("Mcmcdb", "list",
      function(from) {
        mcmcdb_samples_parameters(from)
      })
