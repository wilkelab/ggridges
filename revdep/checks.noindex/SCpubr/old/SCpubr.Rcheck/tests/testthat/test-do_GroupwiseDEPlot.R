if(base::isFALSE(dep_check[["do_GroupwiseDEPlot"]])){
  testthat::test_that("do_GroupwiseDEPlot: CRAN essentials", {
    suppressWarnings({
    sample <- SeuratObject::SetAssayData(object = sample,
                                         assay = "SCT",
                                         slot = "scale.data",
                                         new.data = as.matrix(SeuratObject::GetAssayData(object = sample,
                                                                                         assay = "SCT",
                                                                                         slot = "data")))
    })

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes_scaled,
                                    assay = "SCT",
                                    slot = "scale.data")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_GroupwiseDEPlot: PASS - default", {
    testthat::skip_on_cran()


    suppressWarnings({
      sample <- SeuratObject::SetAssayData(object = sample,
                                           assay = "SCT",
                                           slot = "scale.data",
                                           new.data = as.matrix(SeuratObject::GetAssayData(object = sample,
                                                                                       assay = "SCT",
                                                                                       slot = "data")))
    })

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    use_viridis = FALSE,
                                    sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    use_viridis = FALSE,
                                    sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    use_viridis = TRUE,
                                    viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    use_viridis = TRUE,
                                    viridis.direction = -1)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes_scaled,
                                    assay = "SCT",
                                    slot = "scale.data")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes_scaled,
                                    assay = "SCT",
                                    group.by = "annotation",
                                    slot = "scale.data")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    viridis.direction = 1,
                                    max.cutoff = 1.2,
                                    min.cutoff = 1)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    viridis.direction = 1,
                                    min.cutoff = 1)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    viridis.direction = 1,
                                    max.cutoff = 1.2)
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_GroupwiseDEPlot: PASS - heatmap legend side", {
    testthat::skip_on_cran()

    suppressWarnings({
      sample <- SeuratObject::SetAssayData(object = sample,
                                           assay = "SCT",
                                           slot = "scale.data",
                                           new.data = as.matrix(SeuratObject::GetAssayData(object = sample,
                                                                                       assay = "SCT",
                                                                                       slot = "data")))
    })

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes,
                                    assay = "SCT",
                                    slot = "data",
                                    legend.position = "right")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupwiseDEPlot(sample = sample,
                                    de_genes = de_genes_scaled,
                                    assay = "SCT",
                                    slot = "scale.data",
                                    legend.position = "right")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_GroupwiseDEPlot: FAIL - wrong number of titles", {
    testthat::skip_on_cran()

    suppressWarnings({
      sample <- SeuratObject::SetAssayData(object = sample,
                                           assay = "SCT",
                                           slot = "scale.data",
                                           new.data = as.matrix(SeuratObject::GetAssayData(object = sample,
                                                                                       assay = "SCT",
                                                                                       slot = "data")))
    })

    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes,
                                                       assay = "SCT",
                                                       slot = "data",
                                                       group.by = c("seurat_clusters", "orig.ident"),
                                                       row_title_expression = "a")})
    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes_scaled,
                                                       assay = "SCT",
                                                       slot = "scale.data",
                                                       group.by = c("seurat_clusters", "orig.ident"),
                                                       row_title_expression = "a")})
  })

  testthat::test_that("do_GroupwiseDEPlot: FAIL - wrong direction", {
    testthat::skip_on_cran()

    suppressWarnings({
      sample <- SeuratObject::SetAssayData(object = sample,
                                           assay = "SCT",
                                           slot = "scale.data",
                                           new.data = as.matrix(SeuratObject::GetAssayData(object = sample,
                                                                                       assay = "SCT",
                                                                                       slot = "data")))
    })

    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes,
                                                       assay = "SCT",
                                                       slot = "data",
                                                       viridis.direction = 0)})
    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes_scaled,
                                                       assay = "SCT",
                                                       slot = "scale.data",
                                                       viridis.direction = 0)})
  })

  testthat::test_that("do_ExpressionHeatmap: FAIL", {
    testthat::skip_on_cran()
    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes,
                                                       assay = "SCT",
                                                       slot = "data",
                                                       viridis.direction = 1,
                                                       min.cutoff = -10)})

    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes,
                                                       assay = "SCT",
                                                       slot = "data",
                                                       viridis.direction = 1,
                                                       max.cutoff = 200)})

    testthat::expect_error({SCpubr::do_GroupwiseDEPlot(sample = sample,
                                                       de_genes = de_genes,
                                                       assay = "SCT",
                                                       slot = "data",
                                                       viridis.direction = 1,
                                                       max.cutoff = 1,
                                                       min.cutoff = 2)})

  })

}

