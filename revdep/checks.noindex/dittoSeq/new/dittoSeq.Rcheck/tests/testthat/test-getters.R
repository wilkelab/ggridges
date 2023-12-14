# Tests for visualization functions
# library(dittoSeq); library(testthat); source("setup.R"); source("../../R/utils.R"); source("../../R/utils-getters.R"); source("../../R/get.reductions.R"); source("../../R/utils-defaulting.R"); source("test-getters.R")

# Make Seurat, if can
try(seurat <- Seurat::as.Seurat(sce), silent = TRUE)
seurat_conversion_worked <- exists("seurat")

# Ensure metadata has row.names (a past issue with the fxn)
if (seurat_conversion_worked) {
    rownames(seurat@meta.data) <- colnames(seurat)
}

test_that("getMetas works for Seurat and SCE", {
    expect_type(metas <- getMetas(sce),
        "character")
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_true(all(metas %in% getMetas(seurat)))
})

test_that("isMeta works for Seurat and SCE", {
    expect_false(isMeta("HELLO", sce))
    expect_true(isMeta("score", sce))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_false(isMeta("HELLO", seurat))
    expect_true(isMeta("ident", seurat))
    expect_true(isMeta("score", seurat))
})

test_that("meta works for Seurat and SCE (+ adjustment/adj.fxn)", {
    expect_type(
        meta("score", sce),
        "double")
    expect_type(
        meta("score", sce, adjustment = "z-score"),
        "double")
    expect_equal(
        0,
        mean(meta("score", sce, adjustment = "z-score")))
    expect_equal(
        0:1,
        range(meta("score", sce, adjustment = "relative.to.max")))
    expect_equal(
        factor(meta("score", sce)),
        meta("score", sce, adj.fxn = function(x) {factor(x)}))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_equal(
        meta("score", sce),
        meta("score", seurat))
    expect_equal(
        0:1,
        range(meta("score", seurat, adjustment = "relative.to.max")))
})

test_that("metaLevels works for Seurat and SCE (+ cells.use & used.only)", {
    expect_type(groups <- metaLevels("groups", sce),
        "character")
    
    expect_gt(
        length(groups),
        length(metaLevels("groups", sce,
            cells.use = sce$groups!=groups[1]))
    )
    
    sce$groups_fac <- factor(sce$groups)
    expect_gt(
        length(groups),
        length(metaLevels("groups_fac", sce,
            cells.use = sce$groups!=groups[1]))
    )
    expect_equal(
        length(groups),
        length(metaLevels("groups_fac", sce,
            cells.use = sce$groups!=groups[1],
            used.only = FALSE))
    )
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_equal(groups, metaLevels("groups", seurat))
})

test_that("meta and metaLevels give error when given a non-meta", {
    expect_error(meta("a", sce),
        "is not a metadata of 'object'", fixed = TRUE)
    expect_error(metaLevels("a", sce),
        "is not a metadata of 'object'", fixed = TRUE)
})

test_that("getGenes works for Seurat and SCE", {
    expect_type(genes <- getGenes(sce),
        "character")
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_equal(genes, getGenes(seurat))
})

test_that("isGene works for Seurat and SCE", {
    expect_false(isGene("HELLO", sce))
    expect_true(isGene("gene1", sce))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_false(isGene("HELLO", seurat))
    expect_true(isGene("gene1", seurat))
})

test_that("isGene works for different assays / slots", {
    expect_false(isGene("HELLO", sce, assay = "counts"))
    expect_true(isGene("gene1", sce, assay = "counts"))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_false(isGene("HELLO", seurat))
    expect_true(isGene("gene1", seurat))
})

test_that("isGene can return logical or names for test vectors", {
    expect_equivalent(
        isGene(c("HELLO","gene1","gene2"), sce),
        c(FALSE, TRUE, TRUE))
    expect_equivalent(
        isGene(c("HELLO","gene1","gene2"), sce, return.values = TRUE),
        c("gene1","gene2"))
})

test_that("gene works for Seurat and SCE (+ assay/slot & adjustment/adj.fxn)", {
    expect_type(log <- gene("gene1", sce),
        "double")
    expect_type(raw <- gene("gene1", sce, assay = "counts"),
        "double")
    expect_false(identical(log,raw))
    
    expect_equal(
        0,
        mean(gene("gene1", sce, adjustment = "z-score")))
    expect_equal(
        0:1,
        range(gene("gene1", sce, adjustment = "relative.to.max")))
    
    expect_equal(
        gene("gene1", sce)+1,
        gene("gene1", sce,
             adj.fxn = function(x) {x+1}))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_type(log <- gene("gene1", seurat),
        "double")
    expect_type(raw <- gene("gene1", seurat, slot = "counts"),
        "double")
    expect_false(identical(log,raw))
})

test_that("gene gives error when given a non-gene", {
    expect_error(gene("a", sce),
        "is not a gene of 'object'", fixed = TRUE)
})

test_that("gene, isGene, and getGenes work with swap.rownames",{
    
    swap_genes <- paste(rownames(sce), "symb", sep = "_")
    
    expect_equal(
        getGenes(sce, swap.rownames = "symbol"),
        swap_genes)
    
    expect_true(
        isGene("gene1_symb", sce, swap.rownames = "symbol"))
    
    expect_type(
        gene("gene1_symb", sce, swap.rownames = "symbol"),
        "double"
    )
    
})

test_that("getReductions works for Seurat and SCE", {
    expect_type(reductions <- getReductions(sce),
        "character")
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_equal(reductions, getReductions(seurat))
})

test_that(".defult_reduction errors properly when no reductions in object", {
    sce_no_dimreds <- sce
    reducedDims(sce_no_dimreds) <- NULL
    expect_error(
        .default_reduction(sce_no_dimreds),
        "No dimensionality reduction slots"
    )
    
    expect_error(
        .default_reduction(bulk_se),
        "No dimensionality reduction slots"
    )
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    seurat@reductions <- list()
    expect_error(
        .default_reduction(seurat),
        "No dimensionality reduction slots"
    )
    
})

test_that(".var_or_get_meta_or_gene gets metas, genes, spits back var, or errors if wrong length", {
    expect_equal(
        .var_OR_get_meta_or_gene("groups", sce),
        meta("groups", sce))
    expect_equal(
        .var_OR_get_meta_or_gene("gene1", sce),
        gene("gene1", sce))
    expect_equal(
        seq_len(ncol(sce)),
        unname(.var_OR_get_meta_or_gene(
            seq_len(ncol(sce)),
            sce)))
    expect_error(.var_OR_get_meta_or_gene(1,sce),
        "is not a metadata or gene nor equal in length to ncol('object')", fixed = TRUE)
})

test_that("isBulk works properly", {
    expect_true(isBulk(bulk))
    expect_false(isBulk(sce))
    expect_true(isBulk(as(sce, "SummarizedExperiment")))
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_false(isBulk(seurat))
})

test_that("setBulk works properly", {
    expect_true(isBulk(setBulk(bulk)))
    expect_false(isBulk(setBulk(bulk, FALSE)))
    expect_true(isBulk(setBulk(sce)))
})

test_that(".which_cells converts non-string cells.use to string", {
    expect_equal(
        .which_cells(1:10, sce),
        colnames(sce)[1:10])
    
    logical <- rep(FALSE, ncol(sce))
    logical[1:10] <- TRUE
    expect_equal(.which_cells(logical, sce), colnames(sce)[1:10])
})

test_that(".which_cells errors when logical 'cells.use' is the wrong length", {
    expect_error(.which_cells(TRUE, bulk),
        "'cells.use' length must equal the number of cells/samples in 'object' when given in logical form",
        fixed = TRUE)
})

test_that(".which_cells errors when object is missing cell names", {
    colnames(sce) <- NULL
    expect_error(
        .which_cells(NULL, sce),
        "colnames(<object>)", fixed = TRUE
    )

    expect_error(
        dittoPlot(
            sce, "gene1", group.by = "groups"),
        "colnames(<object>)", fixed = TRUE
    )
    expect_error(
        dittoBarPlot(
            sce, "clusters", group.by = "groups"),
        "colnames(<object>)", fixed = TRUE
    )
    expect_error(
        dittoDimPlot(
            sce, "gene1"),
        "colnames(<object>)", fixed = TRUE
    )
})
