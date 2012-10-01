## Data
COLUMNS <- c("alpha", "beta.1.1", "beta.1.2", "beta.2.1", "beta.2.2")
data <- matrix(rnorm(5 * 10), ncol=5)
colnames(data) <- COLUMNS

template <- list(alpha=0, beta=matrix(0, 2, 2))
indices <- list(alpha=matrix(1, 1, 1),
                   beta=matrix(c(1, 1, 1, 2, 2, 1, 2, 2), byrow=FALSE, ncol=2))
rownames(indices$alpha) <- "alpha"
rownames(indices$beta) <- COLUMNS[-1]
parameters <- structure(c("alpha", rep("beta", 4)), names=COLUMNS)

foo <- new("Mcmc2",
           mcmc(data),
           parameters=parameters,
           template=template,
           indices=indices)



