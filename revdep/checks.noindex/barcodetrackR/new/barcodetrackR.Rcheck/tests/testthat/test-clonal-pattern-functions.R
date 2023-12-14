context("Clonal Pattern Functions")

data(wu_subset)

test_that("barcode_ggheatmap works", {
    testthat::expect_type(barcodetrackR::barcode_ggheatmap(wu_subset), "list")
    testthat::expect_s3_class(barcodetrackR::barcode_ggheatmap(wu_subset, return_table = TRUE), "data.frame")
})
# > Test passed 🥳

test_that("barcode_ggheatmap_stat works", {
    testthat::expect_type(barcodetrackR::barcode_ggheatmap_stat(wu_subset[1:100, 1:3],
        sample_size = rep(100, ncol(wu_subset))
    ), "list")
    testthat::expect_s3_class(barcodetrackR::barcode_ggheatmap_stat(wu_subset[1:100, 1:3],
        sample_size = rep(100, ncol(wu_subset)),
        return_table = TRUE
    ), "data.frame")
})
# > Test passed 🥳

test_that("barcode_binary_heatmap works", {
    testthat::expect_type(barcodetrackR::barcode_binary_heatmap(wu_subset), "list")
    testthat::expect_s3_class(barcodetrackR::barcode_binary_heatmap(wu_subset, return_table = TRUE), "data.frame")
})
# > Test passed 🥳

test_that("clonal_contribution works", {
    testthat::expect_type(barcodetrackR::clonal_contribution(wu_subset,
        SAMPLENAME_choice = "ZJ31_20m_T",
        filter_by = "celltype",
        filter_selection = "T",
        plot_over = "months"
    ), "list")
    testthat::expect_s3_class(barcodetrackR::clonal_contribution(wu_subset,
        SAMPLENAME_choice = "ZJ31_20m_T",
        filter_by = "celltype",
        filter_selection = "T",
        plot_over = "months",
        return_table = TRUE
    ), "data.frame")
})
# > Test passed 🥳
