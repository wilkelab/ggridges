# Tests for dittoDimPlot function
# library(dittoSeq); library(testthat); source("setup.R"); source("test-DimPlot-SE.R")

gene <- "gene1"
cont <- "score2"
disc <- "groups"
disc2 <- "age"
cells.names <- colnames(sce)[1:40]
cells.logical <- c(rep(TRUE, 40), rep(FALSE,ncells-40))
cols <- c("red", "blue", "yellow", "green", "black", "gray", "white")

se <- as(sce, 'SummarizedExperiment')
rownames(se) <- rownames(sce)
embeds <- reducedDim(sce, 'PCA')

test_that("dittoDimPlot can plot continuous or discrete data & raw or normalized expression, for SEs", {
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            cont, object = se, reduction.use = embeds),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            gene, object = se, reduction.use = embeds),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            gene, object = se, reduction.use = embeds,
            assay = "counts"),
        "ggplot")
})

test_that("dittoDimPlots can be subset to show only certain cells/samples with any cells.use method, for SEs", {
    expect_s3_class(
        {c1 <- dittoDimPlot(
            disc, object = se, reduction.use = embeds, data.out = TRUE,
            cells.use = cells.names)
        c1$p},
        "ggplot")
    expect_s3_class(
        {c2 <- dittoDimPlot(
            disc, object = se, reduction.use = embeds, data.out = TRUE,
            cells.use = cells.logical)
        c2$p},
        "ggplot")
    c3 <- dittoDimPlot(
        disc, object = se, reduction.use = embeds,
        cells.use = 1:40,
        data.out = TRUE)
    expect_equal(c1$Target_data, c2$Target_data)
    expect_equal(c1$Target_data, c3$Target_data)
    expect_equal(nrow(c3$Target_data), 40)
    # And if we remove an entire grouping...
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            cells.use = meta(disc,sce)!="A"),
        "ggplot")
})

test_that("dittoDimPlot reduction.use dims can be changed, for SEs", {
    ### Manuel Check: these should look obviously distinct
    expect_s3_class(
        dittoDimPlot(
            cont, object = se, reduction.use = embeds),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            cont, object = se, reduction.use = embeds,
            dim.1 = 3, dim.2 = 5),
        "ggplot")
})

test_that("dittoDimPlots discrete labels can be adjusted", {
    # Manual Check: 5:9
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            rename.var.groups = 5:9),
        "ggplot")
    # Manual Check: 3:6
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            shape.by = disc2, rename.shape.groups = 3:6),
        "ggplot")
})

test_that("dittoDimPlot can be labeled or circled", {
    ### Manual Check: Labels should repel in the first two (and move between
    # plots), and 1&3 with background, 2&4 without, 5: smaller labels
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.label = TRUE),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.label = TRUE,
            labels.highlight = FALSE),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.label = TRUE,
            labels.repel = FALSE),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.label = TRUE,
            labels.highlight = FALSE,
            labels.repel = FALSE),
        "ggplot")
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.label = TRUE,
            labels.size = 3),
        "ggplot")
})

test_that("dittoDimPlot trajectory adding works", {
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            add.trajectory.lineages = list(
                c("B","A","C"),
                c("C","A")),
            trajectory.cluster.meta = disc,
            do.label = TRUE),
        "ggplot")
    # Manual Check: Arrows should move & GROW.
    expect_s3_class(
        dittoDimPlot(
            cont, object = se, reduction.use = embeds,
            add.trajectory.lineages = list(
                c("C","A")),
            trajectory.cluster.meta = disc,
            trajectory.arrow.size = 1),
        "ggplot")
    # Manual Check: Arrows should be detached from points
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            add.trajectory.curves = list(
                data.frame(
                    c(-10,0,-20),
                    c(-20,-10,0)),
                data.frame(
                    c(5:20),
                    c(5:10,9:5,6:10)
                )),
            trajectory.cluster.meta = disc),
        "ggplot")
})

test_that("dittoDimPlot lettering works", {
    ### Manual Check: Letters should be added
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.letter = TRUE, size = 3),
        "ggplot")
    ### Manual Check: see through dots and letters
    expect_s3_class(
        dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            do.letter = TRUE, size = 3,
            opacity = 0.5),
        "ggplot")
})

test_that("dittoDimPlot can add extra vars to dataframe", {
    df1 <- dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            data.out = TRUE)[[2]]
    expect_s3_class(
        df2 <- dittoDimPlot(
            disc, object = se, reduction.use = embeds,
            extra.vars = c(gene, disc2), data.out = TRUE)[[2]],
        "data.frame")
    expect_equal(ncol(df1), 3)
    expect_equal(ncol(df2), 5)
})

test_that("dittoDimPlot genes can be different data types", {
    df <- dittoDimPlot(gene, object = se, reduction.use = embeds, data.out = TRUE,
        assay = "counts")
    expect_equal(
        df$Target_data$color,
        round(df$Target_data$color,0))
    df <- dittoDimPlot(gene, object = se, reduction.use = embeds, data.out = TRUE,
        adjustment = "relative.to.max")
    expect_equal(
        0:1,
        range(df$Target_data$color))
})

test_that("dittoDimPlot adding contours", {
    expect_s3_class(dittoDimPlot(object = se, reduction.use = embeds, disc,
        do.contour = TRUE),
        "ggplot")
    
    ### Manual Check: Contour lines light blue and dashed
    expect_s3_class(dittoDimPlot(object = se, reduction.use = embeds, disc,
        do.contour = TRUE,
        contour.color = "lightblue", contour.linetype = "dashed"),
        "ggplot")
})
