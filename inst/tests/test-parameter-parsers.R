context("parameter name parsers")

BUGS_COLNAMES <- c("alpha",
                   paste0("beta[", 1:2, "]"),
                   paste0("gamma[", expand.paste(1:2, 1:3, sep=","), "]"),
                   paste0("delta[", expand.paste(1:2, 1:3, 1:4, sep=","), "]"))
STAN_COLNAMES <- c("alpha",
                   paste("beta", 1:2, sep="."),
                   expand.paste("gamma", 1:2, 1:3, sep="."),
                   expand.paste("delta", 1:2, 1:3, 1:4, sep="."))

PARAMETERS <- c("alpha", rep("beta", 2),
                rep("gamma", 2*3), rep("delta", 2*3*4))
INDICES <- c(1, 1:2,
             expand.paste(1:2, 1:3, sep=","),
             expand.paste(1:2, 1:3, 1:4, sep=","))
COLNAMES <- c("name", "index")

test_that("default parameter parser works", {
    ret <- parse_parameter_names_default(BUGS_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(rownames(ret), BUGS_COLNAMES)
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(ret$index, rep("1", length(BUGS_COLNAMES)))
    expect_equal(ret$name, BUGS_COLNAMES)
})

test_that("Bugs parameter parser works", {
    ret <- parse_parameter_names_bugs(BUGS_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(rownames(ret), BUGS_COLNAMES)
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(ret$index, INDICES)
    expect_equal(ret$name, PARAMETERS)
})

test_that("Stan parameter parser works", {
    ret <- parse_parameter_names_stan(STAN_COLNAMES)
    expect_is(ret, "data.frame")
    expect_equal(rownames(ret), STAN_COLNAMES)
    expect_equal(colnames(ret), COLNAMES)
    expect_equal(ret$index, INDICES)
    expect_equal(ret$name, PARAMETERS)
})

