if (base::isFALSE(dep_check[["do_ExpressionHeatmap"]])){

  testthat::test_that("do_ExpressionHeatmap: CRAN essential tests", {

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5])

    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_ExpressionHeatmap: PASS - normal", {
    testthat::skip_on_cran()
    
    p <- SCpubr::do_ExpressionHeatmap(sample = sample,
                                      features = rownames(sample)[1:5],
                                      cluster = TRUE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample = sample,
                                      features = rownames(sample)[1:5],
                                      cluster = FALSE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample = sample,
                                      features = rownames(sample)[1:5],
                                      cluster = TRUE,
                                      features.order = rownames(sample)[c(2, 1, 5, 3, 4)])
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample = sample,
                                      features = rownames(sample)[1:5],
                                      cluster = TRUE,
                                      features.order = rownames(sample)[c(2, 1, 5, 3, 4)],
                                      groups.order = list("Groups" = c("1", "3", "5", "7", "0", "2", "4", "6", "8")))
    testthat::expect_true("ggplot" %in% class(p))
    
    testthat::expect_warning({ p <- SCpubr::do_ExpressionHeatmap(sample,
                                                                 features = list("A" = rownames(sample)[1:5]),
                                                                 group.by = "orig.ident",
                                                                 flip = TRUE)})
    
    testthat::expect_true("ggplot" %in% class(p))
    
    testthat::expect_warning({ p <- SCpubr::do_ExpressionHeatmap(sample,
                                                                 features = list("A" = rownames(sample)[1:5],
                                                                                 "B" = rownames(sample)[6:10]),
                                                                 group.by = "orig.ident",
                                                                 flip = TRUE)})
    
    testthat::expect_true("ggplot" %in% class(p))
    
    testthat::expect_warning({ p <- SCpubr::do_ExpressionHeatmap(sample,
                                                                 features = c("TOX2", "Wront"),
                                                                 group.by = "orig.ident",
                                                                 flip = TRUE)})
    
    testthat::expect_true("ggplot" %in% class(p))
    
    

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      flip = TRUE)

    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      flip = FALSE)
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = c("orig.ident", "seurat_clusters"),
                                      flip = TRUE)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = c("orig.ident", "seurat_clusters"),
                                      flip = FALSE)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      enforce_symmetry = TRUE)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      enforce_symmetry = FALSE,
                                      use_viridis = TRUE,
                                      viridis.direction = 1)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      enforce_symmetry = FALSE,
                                      use_viridis = TRUE,
                                      viridis.direction = -1)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      enforce_symmetry = FALSE,
                                      use_viridis = FALSE,
                                      sequential.direction = 1)
    
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      enforce_symmetry = FALSE,
                                      use_viridis = FALSE,
                                      sequential.direction = -1)
    
    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      flip = FALSE)

    testthat::expect_true("ggplot" %in% class(p))


    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = c("orig.ident", "seurat_clusters"),
                                      flip = TRUE)

    testthat::expect_true("ggplot" %in% class(p))

  })

  testthat::test_that("do_ExpressionHeatmap: PASS - assay", {
    testthat::skip_on_cran()

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      assay = NULL)

    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      assay = "SCT")

    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_ExpressionHeatmap: PASS - legend.position", {
    testthat::skip_on_cran()

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      legend.position = "bottom")

    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      legend.position = "right")

    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_ExpressionHeatmap: PASS - cutoffs", {
    testthat::skip_on_cran()

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      assay = NULL,
                                      min.cutoff = 0.7)

    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      assay = "SCT",
                                      max.cutoff = 0.72)

    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_ExpressionHeatmap(sample,
                                      features = rownames(sample)[1:5],
                                      group.by = "orig.ident",
                                      assay = "SCT",
                                      min.cutoff = 0.7,
                                      max.cutoff = 0.72)

    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_ExpressionHeatmap: FAIL", {
    testthat::skip_on_cran()
    testthat::expect_error({SCpubr::do_ExpressionHeatmap(sample = sample,
                                                         features = "EPC1",
                                                         min.cutoff = -10)})

    testthat::expect_error({SCpubr::do_ExpressionHeatmap(sample = sample,
                                                         features = "EPC1",
                                                         max.cutoff = 200)})

    testthat::expect_error({SCpubr::do_ExpressionHeatmap(sample = sample,
                                                         features = "EPC1",
                                                         max.cutoff = 1,
                                                         min.cutoff = 2)})


  })

}


