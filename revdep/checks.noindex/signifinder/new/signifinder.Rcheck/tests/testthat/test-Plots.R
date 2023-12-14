library(signifinder)
library(testthat)
data(ovse)

test_that("oneSignPlot returns a ggplot", {
    sname <- sample(intersect(SignatureNames, names(ovse@colData@listData)), 1)
    res <- oneSignPlot(data = ovse, whichSign = sname)
    expect_true(is(res, "patchwork"))
    expect_true(is(res, "gg"))
    expect_true(is(res, "ggplot"))
})

test_that("geneHeatmapSignPlot returns a HeatmapList", {
    sname <- sample(intersect(SignatureNames, names(ovse@colData@listData)), 1)
    res <- geneHeatmapSignPlot(data = ovse, whichSign = sname)
    expect_true(is(res, "HeatmapList"))
})

test_that("heatmapSignPlot returns a Heatmap", {
    res <- heatmapSignPlot(data = ovse)
    expect_true(is(res, "Heatmap"))

    sname <- sample(intersect(SignatureNames, names(ovse@colData@listData)), 1)
    res <- heatmapSignPlot(data = ovse, clusterBySign = sname)
    expect_true(is(res, "HeatmapList"))
})

test_that("correlationSignPlot returns a list", {
    res <- correlationSignPlot(data = ovse)
    expect_true(is(res, "openair"))
})

test_that("survivalSignPlot returns a ggsurvplot", {
    sname <- sample(intersect(SignatureNames, names(ovse@colData@listData)), 1)
    mysurvData <- cbind(ovse$os, ovse$status)
    rownames(mysurvData) <- rownames(SummarizedExperiment::colData(ovse))
    res <- survivalSignPlot(data = ovse,survData = mysurvData,whichSign = sname)
    expect_true(is(res, "ggsurvplot"))
    expect_true(is(res, "ggsurv"))
    expect_true(is(res, "list"))
})

test_that("ridgelineSignPlot returns a ggplot", {
    res <- ridgelineSignPlot(data = ovse)
    expect_true(is(res, "gg"))
    expect_true(is(res, "ggplot"))
})

test_that("evaluationSignPlot returns a ggplot", {
    res <- evaluationSignPlot(data = ovse)
    expect_true(is(res, "gg"))
    expect_true(is(res, "ggplot"))
})
