library(signifinder)
suppressPackageStartupMessages(library(dplyr))

test_that("signatureTable has not double rows", {
    filtTable <- signatureTable[, c("functionName", "tissue", "author")]
    res <- filtTable[duplicated(filtTable), ]
    expect_equal(nrow(res), 0)
    expect_true(all(!is.na(signatureTable)))
    expect_true(all(!signatureTable == 0))
    expect_true(all(!signatureTable == ""))
    expect_s3_class(signatureTable, "data.frame")
    tablename <- colnames(signatureTable)
    tname <- sample(tablename, 1)
    expect_type(signatureTable[, tname], "character")
})
