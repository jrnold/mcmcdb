# dummy McmcWide object used for testing
test_wide2 <- local({
  nchains <- 2
  niter <- 3
  ncol <- 3
  parnames <- paste("beta", 1:ncol, sep=".")
  samples <- matrix(seq_len(nchains * niter * ncol), ncol=ncol)
  colnames(samples) <- parnames
  chains <- McmcdbChains(data.frame(chain_id = seq_len(nchains)))
  iters <- McmcdbIters(data.frame(chain_id = rep(seq_len(nchains), each=niter),
                                  iter = rep(seq_len(niter), nchains)))
  foo <- McmcdbWide(samples, chains = chains, iters = iters)
})
