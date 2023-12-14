# Tests for dittoPlotVarsAcrossGroups function
# library(dittoSeq); library(testthat); source("setup.R"); source("test-PlotVarsAcrossGroups.R")

sce$number <- as.numeric(seq_along(colnames(sce)))
sce$number2 <- as.numeric(seq_along(colnames(sce)))
sce$half <- c(rep("a", 40), rep("b", ncells-40))
sce$quarter <- sce$half
sce$quarter[21:40] <- "c"
sce$quarter[51:ncells] <- "d"
grp.c <- "quarter"
clr.g <- "half"
genes <- getGenes(sce)[1:5]
grp <- "groups"
clr <- "clusters"
clr2 <- "age"
cells.names <- colnames(sce)[1:40]
cells.logical <- c(rep(TRUE, 40), rep(FALSE,ncells-40))

test_that("dittoPlotVarsAcrossGroups can plot continuous data with all plot types", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter")),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("ridgeplot", "jitter")),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups works with any gene adjustments", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            adjustment = "relative.to.max"),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            adjustment = "z-score"),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            adjustment = NULL),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups errors for single vars or non-numeric vars", {
    expect_error(
        dittoPlotVarsAcrossGroups(sce, group.by = grp,
            c("number")),
        "'vars' must be a vector of at least two", fixed = TRUE)
    
    expect_error(
        dittoPlotVarsAcrossGroups(sce, group.by = grp,
            c("gene1")),
        "'vars' must be a vector of at least two", fixed = TRUE)
    expect_error(
        dittoPlotVarsAcrossGroups(sce, group.by = grp,
            c("gene1", "gene2", grp)),
        "'vars' must be numeric", fixed = TRUE)
})

test_that("dittoPlotVarsAcrossGroups can work for metadata", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            c("number","number2"), object=sce, group.by = grp),
        "ggplot")
})


test_that("dittoPlotVarsAcrossGroups main legend can be removed", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            legend.show = FALSE),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups colors can be distinct from group.by", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp.c,
            plots = c("vlnplot", "boxplot", "jitter"),
            color.by = clr.g),
        "ggplot")
    expect_error(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            color.by = clr2),
        "Unable to interpret 'color.by' input.", fixed = TRUE)
})

test_that("dittoPlotVarsAcrossGroups summary.fxn can be adjusted", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            summary.fxn = median),
        "ggplot")
    expect_error(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"),
            summary.fxn = function(x) x/2),
        NULL)
})

test_that("dittoPlotsVarsAcrossGroups can be subset to show only certain cells/samples with any cells.use method", {
    expect_s3_class(
        {c1 <- dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp, data.out = TRUE,
            plots = c("vlnplot", "boxplot"),
            cells.use = cells.names)
        c1$p},
        "ggplot")
    expect_s3_class(
        {c2 <- dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp, data.out = TRUE,
            plots = c("vlnplot", "boxplot"),
            cells.use = cells.logical)
        c2$p},
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            cells.use = 1:40),
        "ggplot")
    expect_equal(c1$data, c2$data)
    # And if we remove an entire grouping...
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            cells.use = meta(grp,sce)!=0),
        "ggplot")
})

test_that("dittoPlots colors can be adjusted", {
    ### Manual check: These two should look the same.
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            color.panel = dittoColors()[5:1]),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot"),
            colors = 5:1),
        "ggplot")
})

test_that("dittoPlots titles and theme can be adjusted", {
    ### Manual check: All titles should be adjusted.
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            main = "Gotta catch", sub = "em all",
            xlab = "Pokemon", ylab = "Pokedex #s",
            legend.title = "groups"),
        "ggplot")
    ### Manual check: plot should be boxed
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            theme = theme_bw()),
        "ggplot")
})

test_that("dittoPlots y-axis can be adjusted, (x for ridgeplots)", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            min = -5, max = 100),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            y.breaks = seq(10,60,10)),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            min = -2, max = 1, plots = c("ridgeplot","jitter")),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            y.breaks = seq(-1,1.5,0.1), plots = c("ridgeplot","jitter")),
        "ggplot")
})

test_that("dittoPlots x-labels can be adjusted, (y for ridgeplots)", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            x.labels = 3:7),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            x.reorder = 5:1),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            x.labels.rotate = FALSE),
        "ggplot")
    ### Manual Check: L->R, green(5), blue(6), orange(7), with horizontal labels
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            x.labels = 3:7, x.reorder = 5:1, x.labels.rotate = FALSE),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups can have lines added", {
    # Manuel Check: Large blue dots that, in the yplot, look continuous accross groups.
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            add.line = 0),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            add.line = 0, line.linetype = "solid", line.color = "green"),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups jitter adjustments work", {
    # Manuel Check: Large blue dots that, in the yplot, look continuous accross groups.
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp, plots = "jitter",
            jitter.size = 10, jitter.color = "blue", jitter.width = 1),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups boxplot adjustments work", {
    # Manuel Check: Blue boxplots that touch eachother, with jitter visible behind in first plot, outliers shown in second
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp, plots = c("jitter", "boxplot"),
            boxplot.width = 1, boxplot.color = "blue", boxplot.fill = FALSE,
            boxplot.show.outliers = TRUE),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp, plots = c("jitter", "boxplot"),
            boxplot.width = 1, boxplot.color = "blue",
            boxplot.show.outliers = TRUE),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups violin plot adjustments work", {
    # Manuel Check: Almost non-existent lines, with quite overlapping vlns.
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            vlnplot.lineweight = 0.1, vlnplot.width = 5),
        "ggplot")
    # The next three: first two look the same because equal numbers of dots, third should look different:
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            vlnplot.scaling = "count"),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            vlnplot.scaling = "area"),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            vlnplot.scaling = "width"),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups with and without jitter rasterization produces identical plots", {
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter"), do.raster = TRUE),
        "ggplot")
    expect_s3_class(
        dittoPlotVarsAcrossGroups(
            genes, object=sce, group.by = grp,
            plots = c("vlnplot", "boxplot", "jitter")),
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups swap.rownames works", {
    
    swap_genes <- paste(genes, "symb", sep = "_")
    
    no_swap <- dittoPlotVarsAcrossGroups(sce, genes, grp, data.out = TRUE)
    swap <- dittoPlotVarsAcrossGroups(
        sce, swap_genes, grp, data.out = TRUE,
        swap.rownames = "symbol")
    
    expect_equivalent(no_swap$data$color, swap$data$color)
    expect_equivalent(swap$data$var,
                      paste(no_swap$data$var, "symb", sep = "_"))
    
    expect_s3_class(
        no_swap$p,
        "ggplot")
    expect_s3_class(
        swap$p,
        "ggplot")
})

test_that("dittoPlotVarsAcrossGroups split.by works", {
    
    swap_genes <- paste(genes, "symb", sep = "_")
    
    none <- dittoPlotVarsAcrossGroups(
        sce, genes, grp, data.out = TRUE)
    split1 <- dittoPlotVarsAcrossGroups(
        sce, genes, grp, data.out = TRUE,
        split.by = clr)
    split2 <- dittoPlotVarsAcrossGroups(
        sce, genes, grp, data.out = TRUE,
        split.by = c(clr, clr2))
    
    expect_equivalent(
        ncol(none$data)+2,
        ncol(split1$data)+1,
        ncol(split2$data)
    )
    
    expect_s3_class(
        split1$p,
        "ggplot")
    expect_s3_class(
        split2$p,
        "ggplot")
})

## For Future: Could add checking of assay/slot/adjustments
