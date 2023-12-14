if(base::isFALSE(dep_check[["do_GeyserPlot"]])){

  testthat::test_that("do_GeyserPlot: CRAN essentials", {

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1")
    testthat::expect_type(p, "list")

    sample$orig.ident <- sample(c("A", "B"), ncol(sample), replace = TRUE)
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               split.by = "orig.ident")
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_GeyserPlot: PASS - default parameters", {
    testthat::skip_on_cran()
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "nCount_RNA",
                               scale_type = "categorical",
                               enforce_symmetry = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "nCount_RNA",
                               scale_type = "categorical",
                               enforce_symmetry = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               enforce_symmetry = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               enforce_symmetry = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    sample$seurat_clusters <- as.character(sample$seurat_clusters)
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - flip", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               flip = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               flip = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               flip = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - cutoffs", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               min.cutoff = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               enforce_symmetry = FALSE,
                               use_viridis = FALSE,
                               sequential.direction = 1)
                              
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               enforce_symmetry = FALSE,
                               use_viridis = FALSE,
                               sequential.direction = -1)
    
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               min.cutoff = 1)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               max.cutoff = 2)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               max.cutoff = 2)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               min.cutoff = 1,
                               max.cutoff = 2)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               min.cutoff = 1,
                               max.cutoff = 2)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - default parameters = symmetrical scale", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               enforce_symmetry = FALSE)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_GeyserPlot: PASS - default parameters = categorical scale", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical")
    testthat::expect_type(p, "list")

    sample$seurat_clusters_character <- as.character(sample$seurat_clusters)
    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               group.by = "seurat_clusters_character",
                               colors.use = NULL)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_GeyserPlot: PASS - categorical colors.use", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               group.by = "orig.ident",
                               colors.use = c("Cell" = "green"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "categorical",
                               group.by = "orig.ident",
                               colors.use = NULL)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - order by mean", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               order = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               order = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - plot cell borders", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               plot_cell_borders = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               plot_cell_borders = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - split.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               split.by = "seurat_clusters",
                               plot_cell_borders = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1",
                               scale_type = "continuous",
                               split.by = "seurat_clusters",
                               plot_cell_borders = FALSE)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BarPlot: FAIL - wrong paramters", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_GeyserPlot(sample = sample,
                                                 features = "EPC1",
                                                 scale_type = "wrong"))
    testthat::expect_error(SCpubr::do_GeyserPlot(sample = sample,
                                                 features = "EPC1",
                                                 split.by = "wrong"))
    testthat::expect_error(SCpubr::do_GeyserPlot(sample = sample,
                                                 features = "EPC1",
                                                 group.by = "wrong"))
    testthat::expect_error(SCpubr::do_GeyserPlot(sample = sample,
                                                 features = "EPC1",
                                                 jitter = 1))
  })

  testthat::test_that("do_GeyserPlot: PASS - show legend", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = "EPC1")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GeyserPlot: PASS - several features", {
    testthat::skip_on_cran()


    p <- SCpubr::do_GeyserPlot(sample = sample,
                               features = c("EPC1", "PC_1"))
    testthat::expect_type(p, "list")
    testthat::expect_length(p, 2)
  })

}



