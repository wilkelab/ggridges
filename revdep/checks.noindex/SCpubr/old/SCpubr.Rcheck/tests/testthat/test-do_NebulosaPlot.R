if(base::isFALSE(dep_check[["do_NebulosaPlot"]])){
  testthat::test_that("do_NebulosaPlot: CRAN essentials", {
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 joint = TRUE)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_NebulosaPlot: PASS - single feature", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 use_viridis = TRUE,
                                 viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 use_viridis = TRUE,
                                 viridis.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 use_viridis = FALSE,
                                 sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 use_viridis = FALSE,
                                 sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = TRUE,
                                 viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = TRUE,
                                 viridis.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = FALSE,
                                 sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = FALSE,
                                 sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = TRUE,
                                 viridis.direction = 1,
                                 joint = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = TRUE,
                                 viridis.direction = -1,
                                 joint = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = FALSE,
                                 sequential.direction = 1,
                                 joint = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "TOX2"),
                                 use_viridis = FALSE,
                                 sequential.direction = -1,
                                 joint = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - cell_borders", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE)
    testthat::expect_type(p, "list")
    p <- suppressWarnings({SCpubr::do_NebulosaPlot(sample = sample, features = c("EPC1", "PC_1"), plot_cell_borders = TRUE)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - single feature legend normal", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 legend.type = "normal")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - single feature legend colorbar", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 legend.type = "colorbar")
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_NebulosaPlot: FAIL - wrong legend type ", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_NebulosaPlot(sample = sample,
                                                   features = "EPC1",
                                                   legend.type = "wrong"))
  })

  testthat::test_that("do_NebulosaPlot: FAIL - wrong legend position ", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_NebulosaPlot(sample = sample,
                                                   features = "EPC1",
                                                   legend.position = "wrong"))
  })

  testthat::test_that("do_NebulosaPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_NebulosaPlot(sample = sample,
                                                   features = "EPC1",
                                                   font.type = "wrong"))
  })

  testthat::test_that("do_NebulosaPlot: PASS - single feature distinct dims", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 dims = c(2, 1))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - diffusion", {
    testthat::skip_on_cran()


    test <- sample@reductions$umap[[]]
    colnames(test) <- c("DC_1", "DC_2")
    obj <- Seurat::CreateDimReducObject(test, assay = "SCT", key = "DC_")
    sample@reductions$diffusion <- obj
    p <- suppressWarnings(SCpubr::do_NebulosaPlot(sample,
                                                  features = "PC_1",
                                                  reduction = "diffusion"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - several", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - several, joint", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 joint = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - several, joint only joint", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 joint = TRUE,
                                 return_only_joint = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - title", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 joint = TRUE,
                                 return_only_joint = TRUE,
                                 plot.title = "Title")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - subtitle", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 joint = TRUE,
                                 return_only_joint = TRUE,
                                 plot.subtitle = "Subtitle")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 joint = TRUE,
                                 return_only_joint = TRUE,
                                 plot.caption = "Caption")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_NebulosaPlot: PASS - color map", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 viridis.palette = "F")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - legend top", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 legend.position = "left")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - legend top", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 legend.position = "top")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: WARNING - features as list", {
    testthat::skip_on_cran()


    testthat::expect_warning(SCpubr::do_NebulosaPlot(sample = sample,
                                                     features = list("EPC1"),
                                                     viridis.palette = "F"))
  })


  testthat::test_that("do_NebulosaPlot: PASS - no legend", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = "EPC1",
                                 legend.position = "none")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - patchwork title, subtitle and caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 features = c("EPC1", "LTV1"),
                                 plot.title = "A",
                                 plot.subtitle = "B",
                                 plot.caption = "C")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_NebulosaPlot: PASS - plot axis", {
    testthat::skip_on_cran()


    p <- SCpubr::do_NebulosaPlot(sample = sample, plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_NebulosaPlot(sample = sample, reduction = "pca", plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_NebulosaPlot(sample = sample, dims = c(2, 1), plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    sample@reductions$diffusion <- sample@reductions$umap
    p <- SCpubr::do_NebulosaPlot(sample = sample,
                                 reduction = "diffusion",
                                 plot.axes = TRUE,
                                 features = "nCount_RNA")
    testthat::expect_type(p, "list")
  })
}

