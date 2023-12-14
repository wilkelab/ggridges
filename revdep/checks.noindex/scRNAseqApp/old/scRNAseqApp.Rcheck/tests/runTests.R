require("scRNAseqApp") || stop("unable to load Package:scRNAseqApp")
require("Seurat") || stop("unable to load Package::Seurat")
require("testthat") || stop("unable to load testthat")
test_check("scRNAseqApp")
