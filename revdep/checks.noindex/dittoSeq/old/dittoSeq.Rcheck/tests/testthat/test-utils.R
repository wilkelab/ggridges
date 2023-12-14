# Tests for visualization functions
# library(dittoSeq); library(testthat); source("setup.R"); source("test-utils.R")

test_that(".rename_and_or_reorder can rename & reorder",{
    # relabel
    expect_equal(
        as.character(.rename_and_or_reorder(as.character(1:4), relabels = 4:1)),
        as.character(4:1))
    expect_error(
        .rename_and_or_reorder(as.character(1:4), relabels = 3:1),
        "incorrect number of labels provided to 'relabel' input", fixed = TRUE)
    # reorder
    expect_equal(
        levels(.rename_and_or_reorder(as.character(1:4), reorder = 4:1)),
        as.character(4:1))
    expect_error(
        .rename_and_or_reorder(as.character(1:4), reorder = 3:1),
        "incorrect number of indices provided to 'reorder' input", fixed = TRUE)
})
