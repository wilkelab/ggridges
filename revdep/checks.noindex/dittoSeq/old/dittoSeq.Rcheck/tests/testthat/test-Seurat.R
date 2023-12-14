# Tests for functionality with Seurat objects
# library(dittoSeq); library(testthat); source("setup.R"); source("test-Seurat.R")

sce$number <- as.numeric(seq_along(colnames(sce)))

# Convert
try(seurat <- Seurat::as.Seurat(sce), silent = TRUE)
seurat_conversion_worked <- exists("seurat")

# Ensure metadata has row.names (a past issue with the fxn)
if (seurat_conversion_worked) {
    rownames(seurat@meta.data) <- colnames(seurat)
}

### TESTS
test_that("dittoBarPlot works for a Seurat", {
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
    expect_s3_class(
        dittoBarPlot(
            seurat, "clusters", group.by = "age"),
        "ggplot")
})

test_that("importDemux + demux.SNP.summary & demux.calls.summary work for a Seurat", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    ### Prep
    sce.noDash <- sce
    colnames(sce.noDash) <- sapply(colnames(sce.noDash), function(X) strsplit(X, "-")[[1]][1])
    # Convert that SCE to Seurat
    seurat.noDash <- try(Seurat::as.Seurat(sce.noDash), silent = TRUE)
    
    expect_s4_class(
        seurat.demux <- importDemux(
            object = seurat.noDash, lane.meta = "groups",
            demuxlet.best = "mock_demux.best"),
        "Seurat")
    
    ### All called samples are correct
    # What they should be
    best <- read.table(file = "mock_demux.best", header=TRUE, sep="\t", stringsAsFactors = FALSE)
    samples <- vapply(
        as.character(best$BEST),
        function(X) strsplit(X,'-')[[1]][2],
        FUN.VALUE = character(1))[seq_len(ncol(sce))]
    names(samples) <- NULL
    # Check
    expect_true(all(samples == meta("Sample", seurat.demux)))
    
    ### Vizs
    expect_s3_class(
        demux.SNP.summary(seurat.demux),
        "ggplot")
    expect_s3_class(
        demux.calls.summary(seurat.demux),
        "ggplot")
})
  
test_that("dittoDimPlot work for a Seurat & 'slot' input usable", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(
        dittoDimPlot(
            "groups", object=seurat),
        "ggplot")
    
    df <- dittoDimPlot("gene1", object = seurat, data.out = TRUE,
        slot = "counts")
    expect_equal(
        df$Target_data$color,
        round(df$Target_data$color,0))
})

test_that("dittoDotPlot works with gene and meta data for a Seurat, with 'slot' doing what it should", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")
  
    expect_s3_class(
        print(dittoDotPlot(seurat, group.by = "clusters",
            getGenes(sce)[1:5])),
        "ggplot")
    expect_s3_class(
        dittoDotPlot(seurat, group.by = "clusters",
            c("score", "score2", "score3")),
        "ggplot")
    expect_s3_class(
        dittoDotPlot(seurat, group.by = "clusters",
            c("score", "gene1")),
        "ggplot")
    
    expect_type(
        d_raw <- dittoDotPlot(seurat, getGenes(sce)[1:5], "clusters", data.out = TRUE, scale = FALSE,
            slot = "counts"),
        "list")
    expect_type(
        d_log <- dittoDotPlot(seurat, getGenes(sce)[1:5], "clusters", data.out = TRUE, scale = FALSE,
            slot = "data"),
        "list")
    expect_true(all(
        d_raw$data$color >= d_log$data$color))
})

test_that("Heatmap can be plotted for a Seurat and slot adjusted", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(
        dittoHeatmap(
            genes = getGenes(sce)[1:9],
            object = seurat),
        "pheatmap")
    expect_s3_class(
        dittoHeatmap(
            genes = getGenes(sce)[1:9],
            object = seurat,
            slot = "counts"),
        "pheatmap")
    expect_true(all(
        (dittoHeatmap(
            genes = getGenes(sce)[1:9], object = seurat, data.out = TRUE)$mat) <=
            (dittoHeatmap(
                genes = getGenes(sce)[1:9], object = seurat, data.out = TRUE, slot = "counts")$mat)
    ))
})

test_that("DimHex & ScatterHex work for Seurat", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(dittoDimHex(object=seurat), "ggplot")
    expect_s3_class(dittoDimHex("number", object=seurat), "ggplot")
    # Slot / Assay
    expect_s3_class((p1 <- dittoDimHex(object=seurat, "gene1", data.out = TRUE))[[1]], "ggplot")
    expect_s3_class((p2 <- dittoDimHex("gene1", object=seurat, slot = "counts", data.out = TRUE))$plot, "ggplot")
    expect_s3_class((p3 <- dittoDimHex("gene1", object=sce, assay = "counts", data.out = TRUE))$plot, "ggplot")
    expect_false(identical(p1$data$color, p2$data$color))
    expect_true(identical(p2$data$color, p3$data$color))
    # Scatter - Slot / Assay Adjustment
    expect_s3_class((p <- dittoScatterHex("gene1", "gene1", "gene1", object = seurat, data.out = TRUE,
        slot.x = "counts",
        slot.y = "counts",
        adjustment.color = "z-score"))$plot, "ggplot")
    expect_equal(
        p$data$X,
        round(p$data$Y,0))
    expect_equal(
        mean(p$data$color),
        0)
})

test_that("VaryCells works for a Seurat", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(
        multi_dittoDimPlotVaryCells("gene1", object=seurat, "age"),
        "gtable")
})

test_that("dittoPlot can work for a Seurat", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(
        dittoPlot(
            "gene1", object=seurat, group.by = "clusters",
            plots = c("vlnplot", "boxplot", "jitter")),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups can work for a Seurat", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    seurat$half <- c(rep("a", 40), rep("b", ncells-40))
    seurat$quarter <- seurat$half
    seurat$quarter[21:40] <- "c"
    seurat$quarter[51:ncells] <- "d"
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            getGenes(seurat)[1:5], object=seurat, group.by = "quarter"),
        "ggplot")
})

test_that("dittoScatterPlot can plot genes or metadata for a Seurat, with'slot' adjustable", {
    
    skip_if_not(seurat_conversion_worked, message = "Seurat conversion bug")

    expect_s3_class(
        dittoScatterPlot(
            "gene1", "number", object = seurat),
        "ggplot")
    expect_s3_class(
        dittoScatterPlot(
            "gene1", "gene1", object = seurat),
        "ggplot")
    
    df <- dittoScatterPlot("gene1", "gene1", "gene1", NULL, object = seurat,
        slot.x = "counts",
        slot.y = "counts",
        data.out = TRUE)
    expect_equal(
        df$Target_data$X,
        round(df$Target_data$Y,0))
})
    
