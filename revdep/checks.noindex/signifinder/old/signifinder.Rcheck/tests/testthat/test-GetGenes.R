library(signifinder)

test_that(".GetGenes works properly", {
    sname <- sample(SignatureNames, 1)
    res <- .GetGenes(sname)
    expect_equal(ncol(res), 2)
    expect_equal(length(unique(res[, 2])), 1)
    expect_equal(unique(res[, 2]), sname)
})
