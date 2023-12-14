# Tests for dittoScatterPlot function
# library(dittoSeq); library(testthat); source("setup.R"); source("test-ScatterPlot.R")

####
#### Most ScatterPlot features are used/tested in the test-DimPlot, so this will look light.
####

sce$number <- as.numeric(seq_along(colnames(sce)))
gene <- "gene1"
cont <- "number"
disc <- "groups"
disc2 <- "age"
cells.names <- colnames(sce)[1:40]
cells.logical <- c(rep(TRUE, 40), rep(FALSE,ncells-40))
cols <- c("red", "blue", "yellow", "green", "black", "gray", "white")

test_that("dittoScatterPlot can plot genes or metadata & works for SCE", {
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, object = sce),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            gene, gene, object = sce),
        "ggplot")
})

test_that("dittoScatterPlot can overlay colors, continuous or discrete", {
    expect_true(
        "color" %in%
        names(dittoScatterPlot(gene, cont, cont, object = sce, data.out = TRUE)$Target_data))
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, cont, object = sce),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, disc, object = sce),
        "ggplot")
})

test_that("dittoScatterPlot can overlay shapes", {
    expect_true(
        "shape" %in%
        names(dittoScatterPlot(gene, cont, NULL, disc, object = sce, data.out = TRUE)$Target_data))
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce),
        "ggplot")
})

test_that("dittoScatterPlot can add extra vars to dataframe", {
    df1 <- dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            data.out = TRUE)[[2]]
    expect_s3_class(
        df2 <- dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            extra.vars = c(gene, disc2), data.out = TRUE)[[2]],
        "data.frame")
    expect_equal(ncol(df1), 3)
    expect_equal(ncol(df2), 5)
})

test_that("dittoScatterPlot gene display can utilize different data.types (excluding for hover)", {
    df <- dittoScatterPlot(gene, gene, gene, NULL, object = sce,
        assay.x = "counts",
        assay.y = "counts",
        adjustment.color = "z-score",
        data.out = TRUE)
    expect_equal(
        df$Target_data$X,
        round(df$Target_data$Y,0))
    expect_equal(
        mean(df$Target_data$color),
        0)
})

####################
### Manual Check ###
####################

test_that("dittoScatterPlot can be faceted with split.by (1 or 2 vars)", {
    # MANUAL CHECK: FACETING
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            split.by = disc2),
        "ggplot")
    # horizontal
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            split.by = disc2,
            split.nrow = 1),
        "ggplot")
    # vertical
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            split.by = disc2,
            split.ncol = 1),
        "ggplot")
    # Grid with rows=age, cols=groups
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            split.by = c(disc2,disc)),
        "ggplot")
    # Works with cells.use (should have grey cells)
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, NULL, disc, object = sce,
            split.by = c(disc2,disc),
            cells.use = cells.logical),
        "ggplot")
})

test_that("dittoScatterPlot can show trajectory curves", {
    # MANUAL CHECK: Should have arrows overlaid which are detached from data points
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, object=sce,
            add.trajectory.curves = list(
                data.frame(
                    c(-10,0,-20),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20),
                    c(5:10,9:5,6:10)
                ))
            ),
        "ggplot")
})

test_that("dittoScatterPlot with and without rasterization produces identical plots", {
    # MANUAL CHECK: Should be identical
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, object = sce, do.raster = TRUE),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            gene, gene, object = sce),
        "ggplot")
})

test_that("do.label doesn't cause ignorance of split.by factor level ordering", {
    
    # Set two metadata as factors with non-default levels ordering
    sce$groups_rev <- factor(sce$groups, levels = rev(unique(sce$groups)))
    sce$clusters_rev <- factor(sce$clusters, levels = 4:1)
    
    ### MANUAL CHECK: Should be identical aside from the lack of labels in #2 and #4
    # One `split.by`
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, disc, object = sce,
            split.by = "groups_rev", do.label = FALSE),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, disc, object = sce,
            split.by = "groups_rev", do.label = TRUE),
        "ggplot")
    
    # Two `split.by`
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, disc, object = sce,
            split.by = c("groups_rev", "clusters_rev"), do.label = FALSE),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            gene, cont, disc, object = sce,
            split.by = c("groups_rev", "clusters_rev"), do.label = TRUE),
        "ggplot")
})
