library(signifinder)
library(testthat)

suppressPackageStartupMessages(library(SummarizedExperiment))

test_that("TLSSign works", {
    rmatrix <- .fakeData("TLS_Cabrita")
    myres <- TLSSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("TLS_Cabrita" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "TLS_Cabrita"], ncol(assay(myres)))
    expect_type(colData(myres)[, "TLS_Cabrita"], "double")
    expect_message(TLSSign(rmatrix), "100")
})

test_that("ExpandedImmuneSign works", {
    rmatrix <- .fakeData("ExpandedImmune_Ayers")
    myres <- expandedImmuneSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("ExpandedImmune_Ayers" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "ExpandedImmune_Ayers"], ncol(assay(myres)))
    expect_type(colData(myres)[, "ExpandedImmune_Ayers"], "double")
    expect_message(expandedImmuneSign(rmatrix), "100")
})

test_that("IFNSign works", {
    rmatrix <- .fakeData("IFN_Ayers")
    myres <- IFNSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("IFN_Ayers" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "IFN_Ayers"], ncol(assay(myres)))
    expect_type(colData(myres)[, "IFN_Ayers"], "double")
    expect_message(IFNSign(rmatrix), "100")
})

test_that("immuneCytSign based on Rooney's work", {
    rmatrix <- .fakeData("ImmuneCyt_Rooney")
    myres <- immuneCytSign(rmatrix, author = "Rooney")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("ImmuneCyt_Rooney" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "ImmuneCyt_Rooney"], ncol(assay(myres)))
    expect_type(colData(myres)[, "ImmuneCyt_Rooney"], "double")
})

test_that("MitoticIndexSign works", {
    rmatrix <- .fakeData("MitoticIndex_Yang")
    myres <- mitoticIndexSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("MitoticIndex_Yang" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "MitoticIndex_Yang"], ncol(assay(myres)))
    expect_type(colData(myres)[, "MitoticIndex_Yang"], "double")
    expect_message(mitoticIndexSign(rmatrix), "100")
})

test_that("MatrisomeSign works", {
    rmatrix <- .fakeData("Matrisome_Yuzhalin")
    myres <- matrisomeSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("Matrisome_Yuzhalin" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "Matrisome_Yuzhalin"], ncol(assay(myres)))
    expect_type(colData(myres)[, "Matrisome_Yuzhalin"], "integer")
    expect_message(matrisomeSign(rmatrix), "100")
})

test_that("immunoScoreSign based on Roh's work", {
    rmatrix <- .fakeData("ImmunoScore_Roh")
    myres <- immunoScoreSign(rmatrix, author = "Roh")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("ImmunoScore_Roh" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "ImmunoScore_Roh"], ncol(assay(myres)))
    expect_type(colData(myres)[, "ImmunoScore_Roh"], "double")
    expect_message(immunoScoreSign(rmatrix, author = "Roh"), "100")
})

test_that("CINSign works", {
    n <- c("CIN_Carter_25", "CIN_Carter_70")
    pname <- sample(n, 1)
    rmatrix <- .fakeData(pname)
    myres <- CINSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true(all(c("CIN_Carter_25", "CIN_Carter_70") %in% colnames(colData(myres))))
    expect_length(colData(myres)[, "CIN_Carter_25"], ncol(assay(myres)))
    expect_type(colData(myres)[, "CIN_Carter_70"], "double")
    expect_message(CINSign(rmatrix), "100")
})

test_that("hypoxiaSign works", {
    rmatrix <- .fakeData("Hypoxia_Buffa")
    myres <- hypoxiaSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("Hypoxia_Buffa" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "Hypoxia_Buffa"], ncol(assay(myres)))
    expect_type(colData(myres)[, "Hypoxia_Buffa"], "double")
    expect_message(hypoxiaSign(rmatrix), "100")
})

test_that("cellCycleSign based on Lundberg's work", {
    rmatrix <- .fakeData("CellCycle_Lundberg")
    myres <- cellCycleSign(rmatrix, author = "Lundberg")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("CellCycle_Lundberg" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "CellCycle_Lundberg"], ncol(assay(myres)))
    expect_type(colData(myres)[, "CellCycle_Lundberg"], "integer")
    expect_message(cellCycleSign(rmatrix, author = "Lundberg"), "100")
})

test_that("cellCycleSign based on Davoli's work", {
    rmatrix <- .fakeData("CellCycle_Davoli")
    myres <- cellCycleSign(rmatrix, author = "Davoli")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("CellCycle_Davoli" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "CellCycle_Davoli"], ncol(assay(myres)))
    expect_type(colData(myres)[, "CellCycle_Davoli"], "double")
    expect_message(cellCycleSign(rmatrix, author = "Davoli"), "100")
})

test_that("VEGFSign works", {
    rmatrix <- .fakeData("VEGF_Hu")
    myres <- VEGFSign(rmatrix)
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("VEGF_Hu" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "VEGF_Hu"], ncol(assay(myres)))
    expect_type(colData(myres)[, "VEGF_Hu"], "double")
    expect_message(VEGFSign(rmatrix), "100")
})

test_that("immuneCytSign based on Davoli's work", {
    rmatrix <- .fakeData("ImmuneCyt_Davoli")
    myres <- immuneCytSign(rmatrix, author = "Davoli")
    expect_true(is(myres, "SummarizedExperiment"))
    expect_true("ImmuneCyt_Davoli" %in% colnames(colData(myres)))
    expect_length(colData(myres)[, "ImmuneCyt_Davoli"], ncol(assay(myres)))
    expect_type(colData(myres)[, "ImmuneCyt_Davoli"], "double")
    expect_message(immuneCytSign(rmatrix, author = "Davoli"), "100")
})
