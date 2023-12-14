library(signifinder)
suppressPackageStartupMessages(library(SummarizedExperiment))

test_that("PyroptosisSign works", {
    pyrnames <- c(
        "Pyroptosis_Ye", "Pyroptosis_Shao", "Pyroptosis_Lin", "Pyroptosis_Li")
    pname <- sample(pyrnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- pyroptosisSign(rmatrix, author = substring(pname, 12))
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
})

test_that("FerroptosysSign work", {
    ferrnames <- c(
        "Ferroptosis_Liang", "Ferroptosis_Li",
        "Ferroptosis_Liu", "Ferroptosis_Ye")
    fname <- sample(ferrnames, 1)
    rmatrix <- .fakeData(fname)
    myres <- ferroptosisSign(rmatrix, author = substring(fname, 13))
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(fname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, fname], ncol(assay(myres)))
    expect_type(colData(myres)[, fname], "double")
})

test_that("LipidMetabolism work", {
    rmatrix <- .fakeData("LipidMetabolism_Zheng")
    myres <- lipidMetabolismSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("LipidMetabolism_Zheng" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "LipidMetabolism_Zheng"], ncol(assay(myres)))
    expect_type(colData(myres)[, "LipidMetabolism_Zheng"], "double")
    expect_message(lipidMetabolismSign(rmatrix), "100")
})

test_that("StemCellCD49f work", {
    rmatrix <- .fakeData("StemCellCD49f_Smith")
    myres <- stemCellCD49fSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("StemCellCD49f_Smith" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "StemCellCD49f_Smith"], ncol(assay(myres)))
    expect_type(colData(myres)[, "StemCellCD49f_Smith"], "double")
    expect_message(stemCellCD49fSign(rmatrix), "100")
})

test_that("glycolysisSign works", {
    pnames <- c("Glycolysis_Zhang", "Glycolysis_Xu")
    pname <- sample(pnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- glycolysisSign(rmatrix, author = substring(pname, 12))
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
    expect_message(glycolysisSign(
        rmatrix, author = substring(pname, 12)), "100")
})

test_that("autophagySign works", {
    pnames <- c("Autophagy_Xu", "Autophagy_Wang", "Autophagy_ChenH")
    pname <- sample(pnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- autophagySign(rmatrix, author = substring(pname, 11))
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
    expect_message(autophagySign(
        rmatrix, author = substring(pname, 11)), "100")
})

test_that("autophagySign ChenM works", {
    pnames <- c("Autophagy_ChenM_OS", "Autophagy_ChenM_DFS")
    pname <- sample(pnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- autophagySign(rmatrix, author = "ChenM")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
})

test_that("TinflamSign work", {
    rmatrix <- .fakeData("Tinflam_Ayers")
    hk_matrix <- matrix(rpois(55, 100), ncol = 5)
    rownames(hk_matrix) <- Tinflam_Ayers$SYMBOL[Tinflam_Ayers$class == "Housekeeping"]
    rmatrix <- rbind(rmatrix, hk_matrix)
    myres <- TinflamSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("Tinflam_Ayers" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "Tinflam_Ayers"], ncol(assay(myres)))
    expect_type(colData(myres)[, "Tinflam_Ayers"], "double")
    expect_message(TinflamSign(rmatrix), "100")
})
