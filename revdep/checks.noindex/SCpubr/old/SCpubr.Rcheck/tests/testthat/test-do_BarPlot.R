if (base::isFALSE(dep_check[["do_BarPlot"]])){

  testthat::test_that("do_BarPlot: CRAN essential tests", {
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            position = "stack")
    testthat::expect_type(p, "list")


    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            position = "stack")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            position = "fill")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - one variable - stack", {
    testthat::skip_on_cran()

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            position = "stack")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - two variables - fill - flip", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            position = "fill",
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - two variables - stack - flip", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            position = "stack",
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - two variables - stack - flip - ordered", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "orig.ident",
                            split.by = "seurat_clusters",
                            position = "stack",
                            flip = TRUE,
                            order = TRUE)
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_BarPlot: FAIL - wrong position", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    testthat::expect_error(SCpubr::do_BarPlot(sample = sample,
                                              group.by = "orig.ident",
                                              position = "wrong"))
  })

  testthat::test_that("do_BarPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    testthat::expect_error(SCpubr::do_BarPlot(sample = sample,
                                              group.by = "orig.ident",
                                              font.type = "wrong"))
  })

  testthat::test_that("do_BarPlot: FAIL - column not a factor or character", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    testthat::expect_error(SCpubr::do_BarPlot(sample = sample,
                                              group.by = "nCount_RNA"))
  })

  testthat::test_that("do_BarPlot: PASS - rotate x labels", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "orig.ident",
                            axis.text.x.angle  = 0)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - rotate x labels", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    sample$seurat.clusters.factor <- factor(sample$seurat_clusters)
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat.clusters.factor")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - rotate x labels with group.by", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    sample$seurat.clusters.factor <- factor(sample$seurat_clusters)
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat.clusters.factor",
                            split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BarPlot: PASS - colors.use and group.by", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    sample$seurat.clusters.factor <- factor(sample$seurat_clusters)

    colors <- c("0" = "#001219",
                "1" = "#005f73",
                "2" = "#0a9396",
                "3" = "#94d2bd",
                "4" = "#e9d8a6",
                "5" = "#ee9b00",
                "6" = "#ca6702",
                "7" = "#bb3e03",
                "8" = "#ae2012")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat.clusters.factor",
                            split.by = "seurat_clusters",
                            colors.use = colors)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - colors.use ", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    sample$seurat.clusters.factor <- factor(sample$seurat_clusters)

    colors <- c("0" = "#001219",
                "1" = "#005f73",
                "2" = "#0a9396",
                "3" = "#94d2bd",
                "4" = "#e9d8a6",
                "5" = "#ee9b00",
                "6" = "#ca6702",
                "7" = "#bb3e03",
                "8" = "#ae2012")

    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat.clusters.factor",
                            colors.use = colors)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - one variable - rotate x labels", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "orig.ident",
                            position = "stack",
                            axis.text.x.angle = 0)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - one variable - xlab, ylab and title", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "orig.ident",
                            position = "stack",
                            xlab = "A",
                            ylab = "B",
                            plot.title = "C",
                            plot.subtitle = "D",
                            plot.caption = "E")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - one variable - no legend", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "orig.ident",
                            position = "stack",
                            legend.position = "none")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BarPlot: PASS - group.by factor", {
    testthat::skip_on_cran()

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    sample$factor_seurat_clusters <- factor(sample$seurat_clusters)
    p <- SCpubr::do_BarPlot(sample = sample,
                            group.by = "seurat_clusters",
                            split.by = "factor_seurat_clusters",
                            position = "stack",
                            legend.position = "none")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BarPlot: PASS - labs", {
    testthat::skip_on_cran()

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = NULL,
                            flip = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = NULL,
                            flip = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = TRUE)
    testthat::expect_type(p, "list")



    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = NULL,
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = NULL,
                            ylab = NULL,
                            group.by = "seurat_clusters",
                            split.by = NULL,
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            legend.title = NULL,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            legend.title = NULL,
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = TRUE)
    testthat::expect_type(p, "list")


    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            legend.title = "C",
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BarPlot(sample = sample,
                            xlab = "A",
                            ylab = "B",
                            legend.title = "C",
                            group.by = "seurat_clusters",
                            split.by = "orig.ident",
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })
}


