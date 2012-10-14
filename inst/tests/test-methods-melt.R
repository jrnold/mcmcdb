context("melt-methods")

data(line, package="mcmc4")

test_that("signature data=mcmc.list works", {
    foo <- melt(line)
    expect_is(foo, "data.frame")
    expect_equal(colnames(foo), c("parname", "chain_id", "iter", "val"))
    expect_equal(nrow(foo), 1200)
    expect_equal(rownames(foo)[1], "alpha.1.1")
    expect_equal(rownames(foo)[1200], "sigma.2.200")
})

test_that("signature data=mcmc works", {
    foo <- melt(line[[1]])
    expect_is(foo, "data.frame")
    expect_equal(nrow(foo), 600)
    expect_equal(rownames(foo)[1], "alpha.1.1")
    expect_equal(rownames(foo)[600], "sigma.1.200")
    expect_equal(colnames(foo), c("parname", "chain_id", "iter", "val"))
})
