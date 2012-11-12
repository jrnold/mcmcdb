context("mcmcIndices method")

data(line, package="mcmc4")

test_that("mcmcIndices,McmcLong works", {
    linelong <- McmcLong(line, fun=mcmc_parse_parname_stan)
    indices <- mcmcIndices(linelong)
    expected_indices <- 
        structure(list(beta = structure(1:2, .Dim = c(2L, 1L),
                       .Dimnames = list(c("beta.1", "beta.2"), NULL)),
                       sigma = structure(1L, .Dim = c(1L, 1L),
                       .Dimnames = list("sigma", NULL))),
                  .Names = c("beta", "sigma"))
    expect_is(indices, "list")
    expect_equal(indices, expected_indices)
})
          
