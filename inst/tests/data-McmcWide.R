# dummy McmcWide object used for testing
source("data-samples.R")
foo <-new("McmcdbWide",
          samples = samples, parameters = parameters,
          chains = chains, iters = iters,
          flatpar_chains = flatpar_chains)
