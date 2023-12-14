if (base::isFALSE(dep_check[["do_ChordDiagramPlot"]])){

  testthat::test_that("do_ChordDiagramPlot: CRAN essentials", {
    sample$seurat_clusters_char <- as.character(sample$seurat_clusters)
    sample$orig.ident_char <- sample$orig.ident
    sample$orig.ident <- factor(sample$orig.ident)
    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters_char",
                                     to = "orig.ident_char")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ChordDiagramPlot: PASS - default", {
    testthat::skip_on_cran()

    sample$seurat_clusters_char <- as.character(sample$seurat_clusters)
    sample$orig.ident_char <- sample$orig.ident
    sample$orig.ident <- factor(sample$orig.ident)
    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     z_index = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters_char",
                                     to = "orig.ident_char",
                                     z_index = FALSE)
    testthat::expect_type(p, "list")
    
    # # From df.
    # liana_output <- readRDS(system.file("extdata/liana_output_example.rds", package = "SCpubr"))
    # out <- SCpubr::do_LigandReceptorPlot(liana_output = liana_output, return_interactions = TRUE)
    # 
    # p <- SCpubr::do_ChordDiagramPlot(from_df = TRUE, 
    #                                  df = out$`Group Interactions`, 
    #                                  link.border.color = "black", 
    #                                  z_index = TRUE)
    # testthat::expect_type(p, "list")
  })


  testthat::test_that("do_ChordDiagramPlot: PASS - colors", {
    testthat::skip_on_cran()
    sample$seurat_clusters_char <- as.character(sample$seurat_clusters)
    sample$orig.ident_char <- as.character(sample$orig.ident)
    sample$orig.ident <- factor(sample$orig.ident)
    sample$seurat_clusters <- factor(sample$seurat_clusters)

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident_char",
                                     to = "seurat_clusters_char")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident_char",
                                     to = "seurat_clusters")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters_char")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters",
                                     colors.from = c("Cell" = "blue"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident_char",
                                     to = "seurat_clusters",
                                     colors.from = c("Cell" = "blue"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident_char",
                                     to = "seurat_clusters")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     colors.to = c("Cell" = "blue"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident_char",
                                     colors.to = c("Cell" = "blue"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident_char")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters",
                                     colors.from = c("Cell" = "#345211"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters",
                                     colors.from = c("Cell" = "#345211FF"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters",
                                     colors.from = c("Cell" = "#345211FF"),
                                     highlight_group = "Cell")
    testthat::expect_type(p, "list")

    sample$orig.ident <- ifelse(sample$seurat_clusters == "0", "A", "B")
    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "orig.ident",
                                     to = "seurat_clusters",
                                     colors.from = c("A" = "#345211", "B" = "#345222"),
                                     highlight_group = "A")
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_ChordDiagramPlot: PASS - link border color", {

    testthat::skip_on_cran()

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     link.border.color = "black")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ChordDiagramPlot: PASS - alignment", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     alignment = "vertical")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     alignment = "horizontal")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ChordDiagramPlot: PASS - highlight group", {
    testthat::skip_on_cran()


    p <- SCpubr::do_ChordDiagramPlot(sample = sample,
                                     from = "seurat_clusters",
                                     to = "orig.ident",
                                     highlight_group = "0")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_ChordDiagramPlot: FAILS", {
    testthat::skip_on_cran()


    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        alignment = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        link.arr.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        highlight_group = "0",
                                                        alpha.highlight = 120)})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = NULL,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = NULL,
                                                        to = "orig.ident")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = NULL)})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        directional = 4)})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        direction.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        self.link = 3)})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "CD14",
                                                        to = "orig.ident",
                                                        direction.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "CD14",
                                                        direction.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "nCount_RNA",
                                                        to = "orig.ident",
                                                        direction.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "nCount_RNA",
                                                        direction.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        link.arr.type = "wrong")})

    testthat::expect_error({SCpubr::do_ChordDiagramPlot(sample = sample,
                                                        from = "seurat_clusters",
                                                        to = "orig.ident",
                                                        highlight_group = "0",
                                                        alpha.highlight = 120)})
  })

}

