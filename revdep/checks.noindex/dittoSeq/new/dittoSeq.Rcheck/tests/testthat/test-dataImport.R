# Tests for importDittoBulk function
# library(dittoSeq); library(testthat); source("setup.R"); source("test-dataImport.R")

library(DESeq2)
cnts <- matrix(rnbinom(n=1000, mu=100, size=1/0.5), ncol=10)
cond <- factor(rep(1:2, each=5))
dds <- DESeqDataSetFromMatrix(cnts, DataFrame(cond), ~ cond)
dds <- DESeq(dds)
library(edgeR)
dge <- DGEList(counts=cnts, group=rep(1:2,each=5))

list <- list(counts = counts(sce),
             logcounts = logcounts(sce))
SE <- SummarizedExperiment(assays = list)

test_that("importDittoBulk works for SummarizedExperiments", {
    expect_s4_class(
        importDittoBulk(
            SE),
        "SingleCellExperiment")
})

test_that("importDittoBulk works for lists", {
    expect_s4_class(
        importDittoBulk(
            list),
        "SingleCellExperiment")
})

test_that("importDittoBulk works for DESeqDataSets", {
    expect_s4_class(
        importDittoBulk(
            dds),
        "SingleCellExperiment")
})

test_that("importDittoBulk works for DGEList", {
    expect_s4_class(
        importDittoBulk(
            dge),
        "SingleCellExperiment")
})

# Create potential additions
imported.se <- importDittoBulk(SE)
pca <- prcomp(t(as.matrix(counts(imported.se))), center = TRUE, scale = TRUE)$x
metas <- data.frame(condition = sample(1:4, size = ncol(SE), replace = TRUE),
                    number = seq_len(ncol(SE)))

test_that("importDittoBulk can add reductions", {
    expect_s4_class(
        importDittoBulk(
            SE, reductions = list("pca" = pca)),
        "SingleCellExperiment")
})

test_that("importDittoBulk errors if (any) reductions are not named", {
    expect_error(
        importDittoBulk(
            SE, reductions = list(pca)),
        NULL)
    expect_error(
        importDittoBulk(
            SE, reductions = list(pca = pca,
                                  pca)),
        NULL)
})

test_that("importDittoBulk can add metadata", {
    expect_s4_class(
        importDittoBulk(
            SE, metadata = metas),
        "SingleCellExperiment")
})

# Create from SE with metadta and reductions added
imported.se <- importDittoBulk(
    SE, reductions = list(pca = pca), metadata = metas)

test_that("importDittoBulk gives message when previously included metadata would be overwritten", {
    expect_message(
        importDittoBulk(
            imported.se, metadata = metas),
        NULL)
})

test_that("importDittoBulk with combine_metadata=FALSE ignores previously included metadata", {
    # Check that the warning about repeated metadata is gone.
    expect_message(
        new <- importDittoBulk(
            imported.se, metadata = metas[,1, drop = FALSE], combine_metadata = FALSE),
        NA)
    # Check that one of the metadata is nolonger there
    expect_false(all(isMeta(getMetas(imported.se), new)))
})

test_that("importDittoBulk output works in dittoDimPlot", {
    expect_s3_class(
        dittoDimPlot(imported.se, "condition"),
        "ggplot")
})
