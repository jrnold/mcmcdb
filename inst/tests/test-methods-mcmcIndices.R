context("mcmcIndices method")

data(line, package="mcmc4")

test_that("mcmcIndices,McmcLong works", {
    linelong <- McmcLong(line, fun=mcmc_parse_parname_stan)
    indices <- mcmcIndices(linelong)
    expected_indices <- 
        structure(list(beta = structure(1:2, .Dim = c(2L, 1L),
                       .Dimnames = list(c("beta.1", "beta.2"), NULL)),
                       lp__ = structure(1L, .Dim = c(1L, 1L),
                       .Dimnames = list("lp__", NULL)),
                       sigma = structure(1L, .Dim = c(1L,  1L),
                       .Dimnames = list("sigma", NULL)),
                       stepsize__ = structure(1L, .Dim = c(1L, 1L),
                       .Dimnames = list("stepsize__", NULL)),
                       treedepth__ = structure(1L, .Dim = c(1L, 1L),
                       .Dimnames = list("treedepth__", NULL))),
                  .Names = c("beta", "lp__", "sigma", "stepsize__", "treedepth__"))
    expect_is(indices, "list")
    expect_equal(indices, expected_indices)
})
          
