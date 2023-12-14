library(signifinder)
library(testthat)
suppressPackageStartupMessages(library(SummarizedExperiment))

test_that("EMTSign based on Miow's work", {
    pyrnames <- c("EMT_Miow_Epithelial", "EMT_Miow_Mesenchymal")
    pname <- sample(pyrnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- EMTSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
    expect_message(EMTSign(rmatrix), "100")
})

test_that("EMTSign based on Mak's work", {
    rmatrix <- .fakeData("EMT_Mak")
    myresMak <- EMTSign(rmatrix, author = "Mak")
    expect_true(is(myresMak, "SummarizedExperiment"))
    expect_true("EMT_Mak" %in% colnames(colData(myresMak)))
    expect_length(colData(myresMak)[, "EMT_Mak"], ncol(assay(myresMak)))
    expect_type(colData(myresMak)[, "EMT_Mak"], "double")
    expect_message(EMTSign(rmatrix, author = "Mak"), "100")
})

test_that("EMTSign based on Cheng's work", {
    rmatrix <- .fakeData("EMT_Cheng")
    myres <- EMTSign(rmatrix, author = "Cheng")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("EMT_Cheng" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "EMT_Cheng"], ncol(assay(myres)))
    expect_type(colData(myres)[, "EMT_Cheng"], "double")
    expect_message(EMTSign(rmatrix, author = "Cheng"), "100")
})

test_that("ASCSign work", {
    rmatrix <- .fakeData("ASC_Smith")
    myres <- ASCSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("ASC_Smith" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "ASC_Smith"], ncol(assay(myres)))
    expect_type(colData(myres)[, "ASC_Smith"], "double")
    expect_message(ASCSign(rmatrix), "100")
})

test_that("chemokineSign work", {
    rmatrix <- .fakeData("Chemokines_Messina")
    myres <- chemokineSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("Chemokines_Messina" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "Chemokines_Messina"], ncol(assay(myres)))
    expect_type(colData(myres)[, "Chemokines_Messina"], "double")
    expect_message(chemokineSign(rmatrix), "100")
})

test_that("PassONSign work", {
    rmatrix <- .fakeData("PassON_Du")
    myres <- PassONSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("PassON_Du" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "PassON_Du"], ncol(assay(myres)))
    expect_type(colData(myres)[, "PassON_Du"], "double")
})

test_that("CISSign work", {
    rmatrix <- .fakeData("CIS_Robertson")
    myres <- CISSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("CIS_Robertson" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "CIS_Robertson"], ncol(assay(myres)))
    expect_type(colData(myres)[, "CIS_Robertson"], "double")
    expect_message(CISSign(rmatrix), "100")
})

test_that("HRDSSign work", {
    rmatrix <- .fakeData("HRDS_Lu")
    myres <- HRDSSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("HRDS_Lu" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "HRDS_Lu"], ncol(assay(myres)))
    expect_type(colData(myres)[, "HRDS_Lu"], "double")
    expect_message(HRDSSign(rmatrix), "100")
})

test_that("DNArepSign work", {
    rmatrix <- .fakeData("DNArep_Kang")
    myres <- DNArepSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("DNArep_Kang" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "DNArep_Kang"], ncol(assay(myres)))
    expect_type(colData(myres)[, "DNArep_Kang"], "double")
    expect_message(DNArepSign(rmatrix), "100")
})

test_that("IPRESSign work", {
    rmatrix <- .fakeData("IPRES_Hugo")
    myres <- IPRESSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("IPRES_Hugo" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "IPRES_Hugo"], ncol(assay(myres)))
    expect_type(colData(myres)[, "IPRES_Hugo"], "double")
})

test_that("ECM work", {
    pyrnames <- c("ECM_Chakravarthy_up", "ECM_Chakravarthy_down")
    pname <- sample(pyrnames, 1)
    rmatrix <- .fakeData(pname)
    myres <- ECMSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(pname %in% colnames(colData(myres)))
    expect_length(colData(myres)[, pname], ncol(assay(myres)))
    expect_type(colData(myres)[, pname], "double")
    expect_message(ECMSign(rmatrix), "100")
})

test_that("IPSOVSign work", {
    rmatrix  <- .fakeData("IPSOV_Shen")
    myres <- IPSOVSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("IPSOV_Shen" %in% colnames(colData(myres)))
    expect_length(colData(myres)[,"IPSOV_Shen"], ncol(assay(myres)))
    expect_type(colData(myres)[,"IPSOV_Shen"], "double")
    expect_message(IPSOVSign(rmatrix), "100")
})

test_that("glioCellStateSign work", {
    rmatrix <- data.frame()
    for(i in SignatureNames){
        rmatrix <- rbind(rmatrix, as.data.frame(.fakeData(i)))}
    malign <- c(TRUE, TRUE, TRUE, FALSE, TRUE)
    myres <- glioCellStateSign(rmatrix, isMalignant = malign)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("GlioCellState_Neftel_OPC" %in% colnames(colData(myres)))
    expect_length(colData(myres)[,"GlioCellState_Neftel_AC"],ncol(assay(myres)))
    expect_type(colData(myres)[,"GlioCellState_Neftel_MES1"], "double")
})
