# Tests for dittoHeatmap - ComplexHeatmap output
# library(dittoSeq); library(testthat); source("setup.R"); source("test-Heatmap-complex.R")

sce$number <- as.numeric(seq_along(colnames(sce)))
sce$number2 <- as.numeric(rev(seq_along(colnames(sce))))
genes <- getGenes(sce)[1:9]
metas <- c("score", "score2", "score3")

test_that("Heatmap can be plotted for SCE, both genes and metas", {
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce),
        "Heatmap")
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            metas = metas,
            object = sce),
        "Heatmap")
})

test_that("Heatmap data type can be adjusted", {
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            assay = "counts"),
        "Heatmap")
})

# Set genes1:5 to have all zeros in logcounts in a new object
sce2 <- sce
assay(sce2,"logcounts")[1:5,] <- 0

# Set metadata to zero as well
sce2$score <- 0
test_that("Heatmap gives warnings/errors when genes missing", {
    # Function throws a warning when any genes are not captured in the target cells.
    expect_warning(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce2,
            cells.use = meta("number", sce)<5),
        "Gene(s) or metadata removed due to absence of non-zero values within the 'cells.use' subset", fixed = TRUE)
    # Function throws an error when no genes provided are captured in the target cells.
    expect_error(
        dittoHeatmap(complex = TRUE,
            genes = genes[1:5],
            object = sce2,
            cells.use = meta("number", sce)<10),
        "No target genes/metadata features have non-zero values in the 'cells.use' subset", fixed = TRUE)

    # And now for metadata.
    expect_warning(
        dittoHeatmap(complex = TRUE,
            genes = NULL,
            metas = metas,
            object = sce2,
            cells.use = meta("number", sce)<5),
        "Gene(s) or metadata removed due to absence of non-zero values within the 'cells.use' subset", fixed = TRUE)
    # Function throws an error when no metas provided are captured in the target cells.
    expect_error(
        dittoHeatmap(complex = TRUE,
            genes = NULL,
            metas = metas[1],
            object = sce2,
            cells.use = meta("number", sce)<10),
        "No target genes/metadata features have non-zero values in the 'cells.use' subset", fixed = TRUE)
})

test_that("Heatmap gives error when both genes and metas are not provided", {
    # Function throws an error when no genes or metas are provided.
    expect_error(
        dittoHeatmap(complex = TRUE,
            genes = NULL,
            metas = NULL,
            object = sce),
        "No 'genes' or 'metas' requested", fixed = TRUE)
})

test_that("Heatmap gives error when both highlight.genes and highlight.features are provided", {
    # Function throws an error when no genes or metas are provided.
    expect_error(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            highlight.features = "gene1",
            highlight.genes = "gene1"),
        "you can only specify one of 'highlight.genes' or 'highlight.features'", fixed = TRUE)
})

########################
##### Manual Check #####
########################

test_that("Heatmap title can be adjusted", {
    ### Title chould be "Hello there!"
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            main = "Hello there!"),
        "Heatmap")
})

test_that("Heatmap sample renaming by metadata works", {
    ### Names should be numbers
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            cell.names.meta = "number"),
        "Heatmap")
})

test_that("Heatmap can hide rownames/colnames", {
    ### No colnames
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            show_colnames = TRUE),
        "Heatmap")
    ### No rownames
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            show_rownames = FALSE),
        "Heatmap")
})

test_that("Heatmap highlight genes works", {
    ### Only 1 label, gene1
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            show_colnames = FALSE,
            show_rownames = FALSE,
            highlight.features = "gene1"),
        "Heatmap")
})

test_that("Heatmap can be scaled to max", {
    ### Color bar should go from 0 to 1 and white to red
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            scaled.to.max = TRUE),
        "Heatmap")
})

test_that("Heatmap colors can be adjusted", {
    ### yellow to black to red
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            heatmap.colors = colorRampPalette(c("yellow", "black", "red"))(50)),
        "Heatmap")
    ### black to yellow, 0:1
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            scaled.to.max = TRUE,
            heatmap.colors.max.scaled = colorRampPalette(c("black", "yellow"))(25)),
        "Heatmap")
})

test_that("Heatmap annotations can be given & heatmaps can be ordered by metadata, expression, or user-input vector", {
    # Works for expression
    ### ordered by gene1
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            order.by = "gene1"),
        "Heatmap")
    # Works for metadata
    ### ordered by groups
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            order.by = "groups",
            annot.by = "groups"),
        "Heatmap")
    # Works with vectors provided
    ### Ordered in REVERSE of number2
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = seq_along(colnames(sce))),
        "Heatmap")
})

test_that("Heatmap annotations can be given & ordering can be adjusted and follows defaults", {
    # Annotation bar = clusters
    ### Cells should also be ordered by this (single-cell)
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "clusters"),
        "Heatmap")
    # Samples should not be ordered by this (bulk)
    ### CLustered
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = bulk,
            annot.by = "clusters"),
        "Heatmap")
    # Clusters even though an order.by would be given
    ### Clustered!
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "clusters",
            cluster_cols = TRUE),
        "Heatmap")
    # Ordering, but distinct from the first annotation
    ### ordered by groups.
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = c("clusters", "groups"),
            order.by = "groups"),
        "Heatmap")
})

test_that("Heatmap can be ordered when also subset to certain cells", {
    # Works for expression
    ### Ordered by gene1
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            order.by = "gene1",
            cells.use = meta("number", sce)<20),
        "Heatmap")
    # Works for metadata
    ### ordered by groups metadata
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "groups",
            cells.use = colnames(sce)[meta("number", sce)<20]),
        "Heatmap")
    # Works with vectors provided
    ### ordered in REVERSE of the number annotations
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = seq_along(colnames(sce)),
            cells.use = colnames(sce)[meta("number", sce)<20]),
        "Heatmap")
})

test_that("Heatmap can be subset to certain cells by any method", {
    # By logical
    ### Few cells
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            cells.use = meta("number", sce)<10), # Logical method
        "Heatmap")
    # By names
    ### Same few cells
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            cells.use = colnames(sce)[meta("number", sce)<10]), # names method
        "Heatmap")
    # By indices
    ### Same few cells
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            cells.use = 1:9), # names method
        "Heatmap")
})

test_that("Heatmap annotation colors can be adjusted via annot.colors", {
    # (via adjustment of the color pool)
    ### red, yellow, blue, purple for clusters
    ### green numeric (in order)
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = c("number","clusters"),
            annot.colors = c("red", "yellow", "blue", "purple", "green3")),
        "Heatmap")
})

test_that("Heatmap annotation colors can be adjusted via annotation_colors", {
    color_list <- list(clusters = c('1' = "red",
                                    '2' = "yellow",
                                    '3' = "blue",
                                    '4' = "purple"))
    # Number color should change, but clusters should still be the same custom set as above.
    ### red, yellow, blue, purple for clusters
    ### dittoBlue for number
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = c("number","clusters"),
            annotation_colors = color_list),
        "Heatmap")
    ### When annotation_colors provides all colors for annotation_col.
    color_list <- list(clusters = c('1' = "red",
                                    '2' = "yellow",
                                    '3' = "blue",
                                    '4' = "purple"))
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = c("clusters"),
            annotation_colors = color_list),
        "Heatmap")
})

test_that("Coloring works for discrete column and row annotations", {
    ### column and row annotations, all discrete & all dittoColors
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = c("clusters", "groups"),
            scaled.to.max = TRUE,
            annotation_row = data.frame(
                genes,
                row.names = genes)),
        "Heatmap")
})

test_that("Coloring works for continuous column and row annotations", {
    ### column and row annotations, all numeric.
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "number",
            scaled.to.max = TRUE,
            annotation_row = data.frame(
                lab = seq_along(genes),
                row.names = genes),
            cluster_rows = FALSE,
            cluster_cols = FALSE),
        "Heatmap")
    ### column and row annotations, all numeric.
    ### but column annotation bar flips, while legend stays the same.
    expect_s4_class(
        dittoHeatmap(complex = TRUE,
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = "number",
            scaled.to.max = TRUE,
            annotation_row = data.frame(
                lab = seq_len(9),
                row.names = genes),
            cluster_rows = FALSE,
            cluster_cols = FALSE),
        "Heatmap")
})

test_that("scale and border_color pheatmap inputs function as expected", {
    expect_s4_class(
        dittoHeatmap(complex = TRUE,genes = genes, object = sce,
            scale = "none"),
        "Heatmap")
    expect_s4_class(
        dittoHeatmap(complex = TRUE,genes = genes, object = sce,
            border_color = "red"),
        "Heatmap")
})

test_that("Rasterization works for ComplexHeatmap", {
    # Manual Check: zooming in drastically should reveal unequal row/column widths.
    expect_s4_class(
        dittoHeatmap(complex = TRUE, genes = genes, object = sce,
            use_raster = TRUE, raster_quality = 1),
        "Heatmap")
})

test_that("dittoHeatmap drop_level dropped from args when complex = TRUE",{
    args <- dittoHeatmap(genes = genes, object = sce, data.out = TRUE,
        drop_levels = TRUE, complex = TRUE)
    
    # Manual Check: Only three colors in the legend
    expect_warning(
        dittoHeatmap(genes = genes, object = sce,
            annot.by = "clusters", cells.use = sce$clusters!="4",
            drop_levels = TRUE, complex = TRUE),
        NA)
})
