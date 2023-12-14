# Tests for visualization functions
# library(dittoSeq); library(testthat); source("setup.R"); source("test-hover.R")

gene1 <- "gene1"
gene2 <- "gene2"
gene3 <- "gene3"
meta1 <- "score"
meta2 <- "clusters"

test_that("Showing hover.data works for ScatterPlot (with cells.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            dittoScatterPlot(gene1, gene2, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            dittoScatterPlot(gene1, gene2, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                cells.use = rep(c(TRUE,FALSE), length.out = ncol(sce))),
            "plotly")
    } else {
        expect_warning(
            dittoScatterPlot(gene1, gene2, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident")),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("Showing hover.data works for DimPlot (with cells.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            dittoDimPlot(gene1, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            dittoDimPlot(gene1, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                cells.use = rep(c(TRUE,FALSE), length.out = ncol(sce))),
            "plotly")
    } else {
        expect_warning(
            dittoDimPlot(gene1, object = sce, do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident")),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("Showing hover.data works for BarPlot (with cells.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            dittoBarPlot(
                meta1, object = sce,
                group.by = meta2,
                do.hover = TRUE,
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            dittoBarPlot(
                meta1, object = sce,
                group.by = meta2,
                do.hover = TRUE,
                cells.use = rep(c(TRUE,FALSE), length.out = ncol(sce))),
            "plotly")
    } else {
        expect_warning(
            dittoBarPlot(
                meta1, object = sce,
                group.by = meta2,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("Showing hover.data works for Plot (with cells.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            dittoPlot(
                gene1, object = sce,
                group.by = meta2, color.by = meta2,
                do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            dittoBoxPlot(
                gene1, object = sce,
                group.by = meta2, color.by = meta2,
                do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                cells.use = rep(c(TRUE,FALSE), length.out = ncol(sce))),
            "plotly")
    } else {
        expect_warning(
            dittoPlot(
                gene1, object = sce,
                group.by = meta2, color.by = meta2,
                do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident")),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

test_that("Expected hover.data warning for dittoRidgePlot (if plotly available)",{
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_warning(
            dittoRidgePlot(
                gene1, object = sce, plots = c("ridgeplot", "jitter"),
                group.by = meta2, color.by = meta2,
                do.hover = TRUE,
                hover.data = c(gene1,meta1,"ident"),
                data.out = TRUE)[[1]],
            "'do.hover = TRUE' request ignored because plotly does not support ridgeplots.", fixed = TRUE)
    }
})

test_that("Showing hover.data works for VarsAcrossGroups (with cells.use)", {
    if (requireNamespace("plotly", quietly = TRUE)) {
        expect_s3_class(
            dittoPlotVarsAcrossGroups(c(gene1, gene2, gene3),
                object = sce, group.by = meta2,
                do.hover = TRUE,
                data.out = TRUE)[[1]],
            "plotly")
        expect_s3_class(
            dittoPlotVarsAcrossGroups(c(gene1, gene2, gene3),
                object = sce, group.by = meta2,
                do.hover = TRUE,
                cells.use = rep(c(TRUE,FALSE), length.out = ncol(sce))),
            "plotly")
    } else {
        expect_warning(
            dittoPlotVarsAcrossGroups(c(gene1, gene2, gene3),
                object = sce, group.by = meta2,
                do.hover = TRUE),
            "plotly installation required for using hover", fixed = TRUE)
    }
})

## For Future: Could add checking of assay/slot/adjustments
