data(se)
target_protein <- c("ABCA1", "ABCA2")
se <- feature_filter(se,
    target_protein = target_protein,
    filter_method = c("allele", "distance"), filter_allele = 0.3,
    filter_geno = 0.05, ref_position = "TSS"
)
test_that("Results from csQTL have the same length as targeted proteins", {
    se <- csQTL(se)
    expect_equal(
        length(methods::slot(se, "metadata")$TOAST_output),
        length(target_protein)
    )
})