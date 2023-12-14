context("Load data")

# Load sample data
test_data <- read.delim(system.file("extdata", "WuC_etal_appdata/sample_data_ZJ31.txt", package = "barcodetrackR"), row.names = 1)
test_metadata <- read.delim(system.file("extdata", "WuC_etal_appdata/sample_metadata_ZJ31.txt", package = "barcodetrackR"))

test_that("Creating SE works", {
    testthat::expect_s4_class(barcodetrackR::create_SE(
        your_data = test_data,
        meta_data = test_metadata
    ), "SummarizedExperiment")
})
# > Test passed 🥳

# Set threshold
test_threshold <- 0.0000001
test_that("Thresholding data works", {
    testthat::expect_s4_class(barcodetrackR::create_SE(
        your_data = test_data,
        meta_data = test_metadata,
        threshold = test_threshold
    ), "SummarizedExperiment")
})
# > Test passed 🥳


# Threshold Existing SE
test_that("Thresholding SE works", {
    testthat::expect_s4_class(barcodetrackR::threshold_SE(
        your_SE = wu_subset,
        threshold_value = 0.005,
        threshold_type = "relative",
        verbose = TRUE
    ), "SummarizedExperiment")
})
# > Test passed 🥳

# Estimate Barcode Threshold
test_that("Estimate Barcode Threshold works", {
    testthat::expect_type(barcodetrackR::estimate_barcode_threshold(
        capture_efficiency = 0.4,
        population_size = 500000,
        proportion_labeled = 0.3,
        confidence_level = 0.95,
        verbose = FALSE
    ), "double")
})
# > Test passed 🥳
