context("mcmcSkeleton methods")

data(line, package="mcmc4")

test_that("mcmcSkeleton,McmcParray-method works", {
    linelong <- McmcLong(line, fun=mcmc_parse_parname_stan)
    skeleton <-mcmcSkeleton(linelong)
    expected_skeleton <-
        structure(list(beta = c(0, 0), sigma = 0),
                  .Names = c("beta", "sigma"))
    expect_equal(skeleton, expected_skeleton)
})
