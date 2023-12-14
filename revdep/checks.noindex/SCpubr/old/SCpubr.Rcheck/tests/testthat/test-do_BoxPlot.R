if (base::isFALSE(dep_check[["do_BoxPlot"]])){

  testthat::test_that("do_BoxPlot: CRAN essentials", {
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA")
    testthat::expect_type(p, "list")

    sample$group.by <- as.character(sample$seurat_clusters)
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            group.by = "group.by",
                            split.by = NULL)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            split.by = "orig.ident")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BoxPlot: PASS - default", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA")
    testthat::expect_type(p, "list")

    sample$group.by <- as.character(sample$seurat_clusters)
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            group.by = "group.by",
                            split.by = NULL)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            split.by = "orig.ident")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BoxPlot: PASS - custom_grouping", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            group.by = "orig.ident")
    testthat::expect_type(p, "list")

    sample$orig.ident <- factor(sample$orig.ident)
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            group.by = "orig.ident")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            group.by = "orig.ident",
                            colors.use = c("Cell" = "blue"))
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BoxPlot: PASS - split.by", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters == "0", "C", "B")
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            split.by = "orig.ident")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BoxPlot: PASS - silhouette", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            use_silhouette = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BoxPlot: PASS - silhouette", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            use_test = TRUE,
                            comparisons = list(c("0", "1")))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BoxPlot: PASS - order", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            order = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            order = TRUE,
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_BoxPlot: PASS - flip", {
    testthat::skip_on_cran()
    p <- SCpubr::do_BoxPlot(sample = sample,
                            feature = "nCount_RNA",
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_BoxPlot: FAILS ", {
    testthat::skip_on_cran()
    testthat::expect_error({SCpubr::do_BoxPlot(sample = sample,
                                               feature = "nCount_RNA",
                                               use_test = TRUE,
                                               split.by = "orig.ident")})

    testthat::expect_error({SCpubr::do_BoxPlot(sample = sample,
                                               feature = "nCount_RNA",
                                               use_silhouette = TRUE,
                                               split.by = "orig.ident")})

    testthat::expect_error({SCpubr::do_BoxPlot(sample = sample,
                                               feature = "nCount_RNA",
                                               use_test = TRUE)})

    testthat::expect_error({SCpubr::do_BoxPlot(sample = sample,
                                               feature = "nCount_RNA",
                                               order = TRUE,
                                               split.by = "orig.ident")})
  })
}


