# Dummy mcmcdbWide objects
create_McmcdbWide <- function(chain_id, niter, parnames) {
  nchains <- length(chain_id)
  ncol <- length(parnames)
  samples <- matrix(seq_len(nchains * niter * ncol), ncol=ncol)
  colnames(samples) <- parnames
  chains <- McmcdbChains(chain_id = chain_id)
  iters <- McmcdbIters(chain_id = rep(chain_id, each=niter),
                       iter = rep(seq_len(niter), nchains))
  flatpar_chains <-
    McmcdbFlatparChains(expand.grid(flatpar = parnames,
                                    chain_id = chain_id),
                        init = 0)
  McmcdbWide(samples, chains = chains, iters = iters,
             flatpar_chains = flatpar_chains)
}

test_wide2 <- create_McmcdbWide(1:2, 3L, paste("beta", 1:3, sep="."))
test_wide3 <- create_McmcdbWide(1:2, 3L, c("beta", "gamma"))
