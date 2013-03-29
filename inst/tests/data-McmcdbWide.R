# dummy McmcWide object used for testing
test_wide2 <- local({
  nchains <- 2
  niter <- 3
  ncol <- 3
  parnames <- paste("beta", 1:ncol, sep=".")
  samples <- matrix(seq_len(nchains * niter * ncol), ncol=ncol)
  colnames(samples) <- parnames
  chains <- McmcdbChains(chain_id = seq_len(nchains))
  iters <- McmcdbIters(chain_id = rep(seq_len(nchains), each=niter),
                       iter = rep(seq_len(niter), nchains))
  flatpar_chains <-
    McmcdbFlatparChains(expand.grid(flatpar = parnames,
                                    chain_id = seq_len(nchains)),
                        init = 0)
  foo <- McmcdbWide(samples, chains = chains, iters = iters,
                    flatpar_chains = flatpar_chains)
})

test_wide3 <- local({
  nchains <- 2
  niter <- 3
  parnames <- c("beta", "gamma")
  ncol <- length(parnames)
  samples <- matrix(seq_len(nchains * niter * ncol), ncol=ncol)
  colnames(samples) <- parnames
  chains <- McmcdbChains(chain_id = seq_len(nchains))
  iters <- McmcdbIters(chain_id = rep(seq_len(nchains), each=niter),
                       iter = rep(seq_len(niter), nchains))
  flatpar_chains <-
    McmcdbFlatparChains(expand.grid(flatpar = parnames,
                                    chain_id = seq_len(nchains)),
                        init = 0)
  foo <- McmcdbWide(samples, chains = chains, iters = iters,
                    flatpar_chains = flatpar_chains)
})
