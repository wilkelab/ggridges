# Tests for multi_dittoDimPlot & multi_dittoPlot functions
# library(dittoSeq); library(testthat); source("setup.R"); source("test-multis.R")

sce$number <- as.numeric(seq_along(colnames(sce)))
cont <- "gene1"
cont2 <- "gene2"
cont3 <- "gene3"
conts_3 <- c(cont, cont2, cont3)
disc <- "groups"
disc2 <- "age"
disc3 <- "clusters"
discs_3 <- c(disc, disc2, disc3)

test_that("multi Plot&DimPlot work for multiple (3) vars", {
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = conts_3),
        "gtable")
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = discs_3),
        "gtable")
    
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, group.by = disc),
        "gtable")
})

test_that("multi Plot&DimPlot work for single (1) vars", {
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = cont),
        "gtable")
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = disc),
        "gtable")
    
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = cont, group.by = disc),
        "gtable")
})

test_that("multi Plot&DimPlot give reasonable error for no (0) vars", {
    expect_error(
        multi_dittoDimPlot(object=sce, vars = NULL),
        "No 'vars' provided.", fixed = TRUE)
    
    expect_error(
        multi_dittoPlot(object=sce, vars = NULL, group.by = disc),
        "No 'vars' provided.", fixed = TRUE)
})

test_that("multi Plot&DimPlot can output plots as a list", {
    expect_type(
        multi_dittoDimPlot(object=sce, conts_3,
            list.out = TRUE),
        "list")
    
    expect_type(
        multi_dittoPlot(object=sce, conts_3, disc,
            list.out = TRUE),
        "list")
})

test_that("multi Plot&DimPlot messaage & output plots as a list when data.out or do.hover given", {
    expect_type(
        multi_dittoDimPlot(object=sce, conts_3,
            data.out = TRUE),
        "list")
    expect_type(
        multi_dittoDimPlot(object=sce, conts_3,
            do.hover = TRUE),
        "list")
    
    expect_type(
        multi_dittoPlot(object=sce, conts_3, disc,
            data.out = TRUE),
        "list")
    expect_type(
        multi_dittoPlot(object=sce, conts_3, disc,
            data.out = TRUE),
        "list")
    
    expect_message(
        multi_dittoDimPlot(object=sce, conts_3,
            do.hover = TRUE),
        "outputting as a list", fixed = TRUE)
    expect_message(
        multi_dittoPlot(object=sce, conts_3, disc,
            data.out = TRUE),
        "outputting as a list", fixed = TRUE)
})

test_that("multi_dittoDimPlot 'axes.labels.show' works", {
    # Manual Check: axes labels hidden in 1st, shown in 2nd.
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = conts_3,
            axes.labels.show = FALSE),
        "gtable")
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = conts_3,
            axes.labels.show = TRUE),
        "gtable")
})

test_that('multi_dittoPlot, giving "var" adjusts main or ylab as stated', {
    # Manual Check: Main titles same in both, ylab has "expression" only in 1st
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, disc,
            main = "make", ylab = "make"),
        "gtable")
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, disc,
            main = "var", ylab = "var"),
        "gtable")
})

test_that("multi Plot&DimPlot ncol & nrow adjust dims", {
    # Manual Check: Should be 2x2
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = conts_3,
            ncol = 2, nrow = 2),
        "gtable")
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, group.by = disc,
            ncol = 2, nrow = 2),
        "gtable")
})

test_that("multi Plot&DimPlot work with additional single-plotter inputs", {
    # Manual Check: Should be 2x2
    expect_s3_class(
        multi_dittoDimPlot(object=sce, vars = conts_3,
            reduction.use = "PCA",
            size = 5,
            opacity = 0.3,
            cells.use = 1:50,
            shape.by = disc,
            split.by = disc2,
            show.axes.numbers = FALSE,
            min.color = "blue",
            max.color = "red",
            min = 0,
            max = 4,
            legend.show = TRUE,
            add.trajectory.lineages = list(1:3),
            trajectory.cluster.meta = "clusters"),
        "gtable")
    
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, group.by = disc,
            color.by = disc2,
            shape.by = disc,
            split.by = disc2,
            cells.use = 1:50,
            min = 0,
            max = 4,
            xlab = "Hello",
            x.labels = 1:5,
            add.line = 2,
            legend.show = TRUE,
            plots = "jitter"),
        "gtable")
    expect_s3_class(
        multi_dittoPlot(object=sce, vars = conts_3, group.by = disc,
            color.by = disc2,
            shape.by = disc,
            split.by = disc2,
            cells.use = 1:50,
            min = 0,
            max = 4,
            xlab = "Hello",
            x.labels = 1:5,
            add.line = 2,
            legend.show = TRUE,
            plots = c("ridgeplot", "jitter")),
        "gtable")
})
