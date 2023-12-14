if(base::isFALSE(dep_check[["do_ViolinPlot"]])){
  testthat::test_that("do_ViolinPlot: CRAN essentials", {

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - one variable", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.grid = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.grid = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - split.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               split.by = "annotation",
                               plot_boxplot = FALSE)
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_ViolinPlot: PASS - xlab and ylab", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               xlab = "Hi",
                               ylab = "Hi")
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_ViolinPlot: PASS - flip", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               flip = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               flip = FALSE)
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_ViolinPlot: PASS - two variable", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               plot.grid = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               plot.grid = TRUE,
                               share.y.lims = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               plot.grid = TRUE,
                               xlab = c("A", "A"),
                               ylab = c("B", "B"),
                               y_cut = c(400, 400))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               plot.grid = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - group.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.grid = TRUE,
                               group.by = "seurat_clusters")
    testthat::expect_type(p, "list")

    sample$seurat_clusters <- as.character(sample$seurat_clusters)
    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.grid = FALSE,
                               group.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_ViolinPlot: PASS - without boxplot", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot_boxplot = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - rotate axis", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               axis.text.x.angle = 45)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - plot.grid", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.grid = TRUE)
    testthat::expect_type(p, "list")
  })




  testthat::test_that("do_ViolinPlot: PASS - one features ycut", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               axis.text.x.angle = 45,
                               y_cut = 2)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_ViolinPlot: PASS - one features line width", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               axis.text.x.angle = 45,
                               y_cut = 2,
                               line_width = 3)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - one features boxplot width", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               axis.text.x.angle = 45,
                               y_cut = 2,
                               boxplot_width = 0.1)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - change colors", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               axis.text.x.angle = 45,
                               y_cut = 2,
                               boxplot_width = 0.1,
                               colors.use = c("0" = "#001219",
                                              "1" = "#005f73",
                                              "2" = "#0a9396",
                                              "3" = "#94d2bd",
                                              "4" = "#e9d8a6",
                                              "5" = "#ee9b00",
                                              "6" = "#ca6702",
                                              "7" = "#bb3e03",
                                              "8" = "#ae2012"))
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_ViolinPlot: FAIL - split.by", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_ViolinPlot(sample = sample,
                                                 features = "EPC1",
                                                 split.by = "orig.ident"))
  })




  testthat::test_that("do_ViolinPlot: PASS - one variable, group by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               group.by = "orig.ident")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               split.by = "orig.ident",
                               group.by = "annotation",
                               plot_boxplot = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               split.by = "seurat_clusters",
                               plot_boxplot = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: PASS - one variable, xlab y lab", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               xlab = "y",
                               ylab = "x")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               xlab = c("A", "B"),
                               ylab = c("C", "D"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = c("EPC1", "TOX2"),
                               xlab = c(NA, "B"),
                               ylab = c("C", NA))
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_ViolinPlot: PASS - one variable, plot.title, subtitle and caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               features = "EPC1",
                               plot.title = "A",
                               plot.subtitle = "B",
                               plot.caption = "C")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_ViolinPlot: PASS - multiple variables plot.title, subtitle and caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ViolinPlot(sample = sample,
                               "EPC1",
                               group.by = "orig.ident",
                               colors.use = c("Cell" = "red"))
    testthat::expect_type(p, "list")

    sample$orig.ident <- factor(sample$orig.ident)
    p <- SCpubr::do_ViolinPlot(sample = sample,
                               "EPC1",
                               group.by = "orig.ident",
                               colors.use = c("Cell" = "red"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ViolinPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_ViolinPlot(sample = sample,
                                                 features = "EPC1",
                                                 font.type = "wrong"))
  })

  testthat::test_that("do_ViolinPlot: FAIL - split.by with boxplots", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_ViolinPlot(sample = sample,
                                                 features = "EPC1",
                                                 split.by = "annotation"))
  })
}

