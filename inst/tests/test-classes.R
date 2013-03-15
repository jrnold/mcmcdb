context("McmcChains")

test_that("niter, thin, start, end consistency okay", {
  expect_is(McmcChains(data.frame(chain_id=1L, niter=5L, start=1L, end=10L, thin=2L)),
            "McmcChains")
})

test_that("inconsistent niter, thin, start, end throws an error", {
  expect_error(McmcChains(data.frame(chain_id=1L, niter=5L, start=1L, end=15L, thin=2L)),
               "invalid class")
})
