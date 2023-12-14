# Tests for visualization functions
# library(dittoSeq); library(testthat); source("setup.R"); source("test-Heatmap.R")

# Make Seurat, if can
try(seurat <- Seurat::as.Seurat(sce), silent = TRUE)
seurat_conversion_worked <- exists("seurat")

# Ensure metadata has row.names (a past issue with the fxn)
if (seurat_conversion_worked) {
    rownames(seurat@meta.data) <- colnames(seurat)
}

sce$number <- as.numeric(seq_along(colnames(sce)))
sce$number2 <- as.numeric(rev(seq_along(colnames(sce))))
genes <- getGenes(sce)[1:9]
metas <- c("score", "score2", "score3")

test_that("Heatmap can be plotted for SCE, both genes and metas", {
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce),
        "pheatmap")
    expect_s3_class(
        dittoHeatmap(
            metas = metas,
            object = sce),
        "pheatmap")
})

test_that("Heatmap data type can be adjusted", {
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            assay = "counts"),
        "pheatmap")
})

# Set genes1:5 to have all zeros in logcounts in a new object
sce2 <- sce
assay(sce2,"logcounts")[1:5,] <- 0

# Set metadata to zero as well
sce2$score <- 0
test_that("Heatmap gives warnings/errors when genes missing", {
    # Function throws a warning when any genes are not captured in the target cells.
    expect_warning(
        dittoHeatmap(
            genes = genes,
            object = sce2,
            cells.use = meta("number", sce)<5),
        "Gene(s) or metadata removed due to absence of non-zero values within the 'cells.use' subset", fixed = TRUE)
    # Function throws an error when no genes provided are captured in the target cells.
    expect_error(
        dittoHeatmap(
            genes = genes[1:5],
            object = sce2,
            cells.use = meta("number", sce)<10),
        "No target genes/metadata features have non-zero values in the 'cells.use' subset", fixed = TRUE)

    # And now for metadata.
    expect_warning(
        dittoHeatmap(
            genes = NULL,
            metas = metas,
            object = sce2,
            cells.use = meta("number", sce)<5),
        "Gene(s) or metadata removed due to absence of non-zero values within the 'cells.use' subset", fixed = TRUE)
    # Function throws an error when no metas provided are captured in the target cells.
    expect_error(
        dittoHeatmap(
            genes = NULL,
            metas = metas[1],
            object = sce2,
            cells.use = meta("number", sce)<10),
        "No target genes/metadata features have non-zero values in the 'cells.use' subset", fixed = TRUE)
})

test_that("Heatmap gives error when both genes and metas are not provided", {
    # Function throws an error when no genes or metas are provided.
    expect_error(
        dittoHeatmap(
            genes = NULL,
            metas = NULL,
            object = sce),
        "No 'genes' or 'metas' requested", fixed = TRUE)
})

test_that("Heatmap gives error when both highlight.genes and highlight.features are provided", {
    # Function throws an error when no genes or metas are provided.
    expect_error(
        dittoHeatmap(
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
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            main = "Hello there!"),
        "pheatmap")
})

test_that("Heatmap sample renaming by metadata works", {
    ### Names should be numbers
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            cell.names.meta = "number"),
        "pheatmap")
})

test_that("Heatmap can hide rownames/colnames", {
    ### No colnames
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            show_colnames = TRUE),
        "pheatmap")
    ### No rownames
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            show_rownames = FALSE),
        "pheatmap")
})

test_that("Heatmap highlight genes works", {
    ### Only 1 label, gene1
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            show_colnames = FALSE,
            show_rownames = FALSE,
            highlight.features = "gene1"),
        "pheatmap")
})

test_that("Heatmap can be scaled to max", {
    ### Color bar should go from 0 to 1 and white to red
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            scaled.to.max = TRUE),
        "pheatmap")
})

test_that("Heatmap colors can be adjusted", {
    ### yellow to black to red
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            heatmap.colors = colorRampPalette(c("yellow", "black", "red"))(50)),
        "pheatmap")
    ### black to yellow, 0:1
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            scaled.to.max = TRUE,
            heatmap.colors.max.scaled = colorRampPalette(c("black", "yellow"))(25)),
        "pheatmap")
})

test_that("Heatmap annotations can be given & heatmaps can be ordered by metadata, expression, or user-input vector", {
    # Works for expression
    ### ordered by gene1
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            order.by = "gene1"),
        "pheatmap")
    # Works for metadata
    ### ordered by groups
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            order.by = "groups",
            annot.by = "groups"),
        "pheatmap")
    # Works with vectors provided
    ### Ordered in REVERSE of number2
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = seq_along(colnames(sce))),
        "pheatmap")
    ### Ordered in REVERSE of number2, with out of order cells FIRST
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = c(30:26, 1:25,31:ncol(sce))),
        "pheatmap")
    
    # ordered by multiple metadata
    ### By groups, but then by number2
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            order.by = c("groups","number2"),
            annot.by = c("groups","number2")),
        "pheatmap")
})

test_that("Heatmap annotations can be given & ordering can be adjusted and follows defaults", {
    # Annotation bar = clusters
    ### Cells should also be ordered by this (single-cell)
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "clusters"),
        "pheatmap")
    # Samples should not be ordered by this (bulk)
    ### CLustered
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = bulk,
            annot.by = "clusters"),
        "pheatmap")
    # Clusters even though an order.by would be given
    ### Clustered!
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "clusters",
            cluster_cols = TRUE),
        "pheatmap")
    # Ordering, but distinct from the first annotation
    ### ordered by groups.
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = c("clusters", "groups"),
            order.by = "groups"),
        "pheatmap")
})

test_that("Heatmap can be ordered when also subset to certain cells", {
    # Works for expression
    ### Ordered by gene1
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            order.by = "gene1",
            cells.use = meta("number", sce)<20),
        "pheatmap")
    # Works for metadata
    ### ordered by groups metadata
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "groups",
            cells.use = colnames(sce)[meta("number", sce)<20]),
        "pheatmap")
    # Works with vectors provided
    ### ordered in REVERSE of the number annotations
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "number2",
            order.by = seq_along(colnames(sce)),
            cells.use = colnames(sce)[meta("number", sce)<20]),
        "pheatmap")
})

test_that("Heatmap can be subset to certain cells by any method", {
    # By logical
    ### Few cells
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            cells.use = meta("number", sce)<10), # Logical method
        "pheatmap")
    # By names
    ### Same few cells
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            cells.use = colnames(sce)[meta("number", sce)<10]), # names method
        "pheatmap")
    # By indices
    ### Same few cells
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            cells.use = 1:9), # names method
        "pheatmap")
})

test_that("Heatmap annotation colors can be adjusted via annot.colors", {
    # (via adjustment of the color pool)
    ### red, yellow, blue, purple for clusters
    ### green numeric (in order)
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = c("number","clusters"),
            annot.colors = c("red", "yellow", "blue", "purple", "green3")),
        "pheatmap")
})

test_that("Heatmap annotation colors can be adjusted via annotation_colors", {
    color_list <- list(clusters = c('1' = "red",
                                    '2' = "yellow",
                                    '3' = "blue",
                                    '4' = "purple"))
    # Number color should change, but clusters should still be the same custom set as above.
    ### red, yellow, blue, purple for clusters
    ### dittoBlue for number
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = c("number","clusters"),
            annotation_colors = color_list),
        "pheatmap")
    ### When annotation_colors provides all colors for annotation_col.
    color_list <- list(clusters = c('1' = "red",
                                    '2' = "yellow",
                                    '3' = "blue",
                                    '4' = "purple"))
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = c("clusters"),
            annotation_colors = color_list),
        "pheatmap")
})

test_that("Coloring works for discrete column and row annotations", {
    ### column and row annotations, all discrete & all dittoColors
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = c("clusters", "groups"),
            scaled.to.max = TRUE,
            annotation_row = data.frame(
                genes,
                row.names = genes)),
        "pheatmap")
})

test_that("Coloring works for continuous column and row annotations", {
    ### column and row annotations, all numeric.
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "number",
            scaled.to.max = TRUE,
            annotation_row = data.frame(
                lab = seq_len(9),
                row.names = genes),
            cluster_rows = FALSE,
            cluster_cols = FALSE),
        "pheatmap")
    ### column and row annotations, all numeric.
    ### but column annotation bar flips, while legend stays the same.
    expect_s3_class(
        dittoHeatmap(
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
        "pheatmap")
})

test_that("scale and border_color pheatmap inputs function as expected", {
    expect_s3_class(
        dittoHeatmap(genes = genes, object = sce,
            scale = "none"),
        "pheatmap")
    expect_s3_class(
        dittoHeatmap(genes = genes, object = sce,
            border_color = "red"),
        "pheatmap")
})

test_that("dittoHeatmap swap.rownames works", {
    swap_genes <- paste(genes, "symb", sep = "_")
    
    expect_s3_class(
        dittoHeatmap(genes = swap_genes, object = sce, swap.rownames = "symbol"),
        "pheatmap")
    expect_equivalent(
        rownames(
            dittoHeatmap(
                genes = swap_genes, object = sce, swap.rownames = "symbol",
                data.out = TRUE
            )$mat),
        swap_genes)
})

test_that("dittoHeatmap drops levles from annotation_colors to allow 'drop_levels' to function",{
    full <- dittoHeatmap(genes = genes, object = sce, data.out = TRUE,
        annot.by = "clusters", cells.use = sce$clusters!="4",
        drop_levels = FALSE)$annotation_colors$clusters
    dropped <- dittoHeatmap(genes = genes, object = sce, data.out = TRUE,
        annot.by = "clusters", cells.use = sce$clusters!="4",
        drop_levels = TRUE)$annotation_colors$clusters
    
    expect_equal(length(full), length(dropped)+1)
    
    # Manual Check: Only three colors in the legend
    expect_s3_class(
        dittoHeatmap(genes = genes, object = sce,
            annot.by = "clusters", cells.use = sce$clusters!="4",
            drop_levels = TRUE),
        "pheatmap")
})

test_that("dittoHeatmap works for pre-scaled data", {
    # Generally scale
    assay(sce, "scaled") <- as.matrix(t(scale(t(round(logcounts(sce), 2)))))
    # Ensure some zero rowSums (needed due to rounding)
    assay(sce, "scaled")["gene1",] <- scale(1:ncol(sce), scale = FALSE)
    # Ensure not all genes so always warning generated, not error
    assay(sce, "scaled")["gene2",] <- assay(sce, "scaled")["gene2",]+0.05

    expect_warning(
        h <- dittoHeatmap(genes = genes, object = sce,
                     assay = "scaled", scale = "none",
                     drop_levels = TRUE),
        NA)
    expect_s3_class(h, "pheatmap")
})

### Seurat test
test_that("dittoHeatmap allows annotation by 'ident' as would be expected", {
    # errors if not Seurat
    expect_error(
        dittoHeatmap(
            genes = genes,
            object = sce,
            annot.by = "ident"),
        "ident is not a metadata", fixed = TRUE)

    # works if Seurat
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_s3_class(
        dittoHeatmap(
            genes = genes,
            object = seurat,
            annot.by = "ident"),
        "pheatmap")
})
