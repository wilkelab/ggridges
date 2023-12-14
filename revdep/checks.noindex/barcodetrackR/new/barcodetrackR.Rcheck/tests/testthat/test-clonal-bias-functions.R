context("Clonal Bias Functions")

data(wu_subset)

test_that("bias_histogram works", {
    testthat::expect_type(barcodetrackR::bias_histogram(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months"
    ), "list")
    testthat::expect_type(barcodetrackR::bias_histogram(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months",
        return_table = TRUE
    ), "list")
})
# > Test passed 🥳

test_that("bias_ridge_plot works", {
    testthat::expect_type(barcodetrackR::bias_ridge_plot(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months"
    ), "list")
    testthat::expect_s3_class(barcodetrackR::bias_ridge_plot(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months",
        return_table = TRUE
    ), "data.frame")
})
# > Test passed 🥳

test_that("bias_lineplot works", {
    testthat::expect_type(barcodetrackR::bias_lineplot(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months",
        remove_unique = TRUE
    ), "list")
    testthat::expect_s3_class(barcodetrackR::bias_lineplot(wu_subset,
        split_bias_on = "celltype",
        bias_1 = "B",
        bias_2 = "Gr",
        split_bias_over = "months",
        remove_unique = TRUE,
        return_table = TRUE
    ), "data.frame")
})
# > Test passed 🥳
