context("Plot type parameters")

test_that("Valid pars detects correctly", {
    expect_true(valid_par("pch", "dot"))
    expect_false(valid_par("pch", "dot", "inference"))
    expect_false(valid_par("pch", "hex"))

    expect_true(valid_par("unknown", "dot"))
    expect_warning(expect_true(valid_par("pch", "unknown")))
})
