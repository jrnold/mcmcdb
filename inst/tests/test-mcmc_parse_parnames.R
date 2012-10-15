context("parameter name parsers")

COLNAMES <- c("parname", "pararray", "index")
BUGS_COLNAMES <- c("alpha",
                   paste0("beta[", 1:2, "]"),
                   paste0("gamma[", mcmc4:::expand.paste(1:2, 1:3, sep=","), "]"),
                   paste0("delta[", mcmc4:::expand.paste(1:2, 1:3, 1:4, sep=","), "]"))

STAN_ALPHA <- "alpha"
STAN_BETA <- paste("beta", 1:2, sep=".")
STAN_GAMMA <- mcmc4:::expand.paste("gamma", 1:2, 1:3, sep=".")
STAN_DELTA <- mcmc4:::expand.paste("delta", 1:2, 1:3, 1:4, sep=".")
STAN_COLNAMES <- c(STAN_ALPHA, STAN_BETA, STAN_GAMMA, STAN_DELTA)

PARAMETERS <- c("alpha", rep("beta", 2),
                rep("gamma", 2*3), rep("delta", 2*3*4))
INDICES <- c(1, 1:2,
             mcmc4:::expand.paste(1:2, 1:3, sep=","),
             mcmc4:::expand.paste(1:2, 1:3, 1:4, sep=","))

SKELETON <- list(alpha=0, beta=rep(0, 2),
                 delta=mcmc4:::zeros(c(2, 3, 4)),
                 gamma=mcmc4:::zeros(c(2, 3)))

INDICES2 <- list(alpha=matrix(1),
                 beta=matrix(1:2),
                 gamma=as.matrix(expand.grid(1:2, 1:3)),
                 delta=as.matrix(expand.grid(1:2, 1:3, 1:4)))
rownames(INDICES2$alpha) <- STAN_ALPHA
rownames(INDICES2$beta) <- STAN_BETA
rownames(INDICES2$gamma) <- STAN_GAMMA
rownames(INDICES2$delta) <- STAN_DELTA
colnames(INDICES2$gamma) <- NULL
colnames(INDICES2$delta) <- NULL


test_that("default parameter parser works", {
    ret <- mcmc_parse_parname_default(BUGS_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(rownames(ret), BUGS_COLNAMES)
    expect_equal(ret$index, rep("1", length(BUGS_COLNAMES)))
    expect_equal(ret$pararray, BUGS_COLNAMES)
})

test_that("Bugs parameter parser works", {
    ret <- mcmc_parse_parname_bugs(BUGS_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(rownames(ret), BUGS_COLNAMES)
    expect_equal(ret$index, INDICES)
    expect_equal(ret$pararray, PARAMETERS)
})

test_that("Stan parameter parser works", {
    ret <- mcmc_parse_parname_stan(STAN_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(rownames(ret), STAN_COLNAMES)
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(ret$index, INDICES)
    expect_equal(ret$pararray, PARAMETERS)
})


