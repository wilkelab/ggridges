if (base::isFALSE(dep_check[["do_FeaturePlot"]])){
  testthat::test_that("do_FeaturePlot: CRAN essential", {

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - single feature", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA")
    testthat::expect_type(p, "list")
  })
  
  testthat::test_that("do_FeaturePlot: PASS - group.by", {
    testthat::skip_on_cran()
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = TRUE,
                                viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = TRUE,
                                viridis.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = TRUE,
                                use_viridis = TRUE,
                                viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = TRUE,
                                use_viridis = TRUE,
                                viridis.direction = -1)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = TRUE,
                                use_viridis = TRUE,
                                sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = TRUE,
                                use_viridis = FALSE,
                                sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.cell_borders = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.cell_borders = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NA)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL,
                                group.by.colors.use = c("Cell" = "blue"))
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL,
                                group.by.colors.use = c("Cell" = "blue"),
                                group.by.cell_borders = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                idents.highlight = "0",
                                enforce_symmetry = FALSE,
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL,
                                group.by.colors.use = c("Cell" = "blue"),
                                group.by.cell_borders = FALSE)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                use_viridis = FALSE,
                                sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                use_viridis = FALSE,
                                sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("TOX2", "EPC1"),
                                use_viridis = FALSE,
                                sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("TOX2", "EPC1"),
                                use_viridis = FALSE,
                                sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                use_viridis = FALSE,
                                sequential.direction = 1,
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("TOX2", "EPC1"),
                                use_viridis = FALSE,
                                sequential.direction = 1,
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("TOX2", "EPC1"),
                                use_viridis = FALSE,
                                sequential.direction = -1,
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")
    
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "seurat_clusters",
                                group.by.show.dots = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NA)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test",
                                group.by.colors.use = c("Cell" = "blue"))
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = TRUE,
                                viridis.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = TRUE,
                                viridis.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                sequential.direction = 1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                sequential.direction = -1)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "seurat_clusters",
                                group.by.show.dots = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NULL)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = NA)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "seurat_clusters",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test",
                                group.by.colors.use = c("Cell" = "blue"))
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test",
                                group.by.colors.use = c("Cell" = "blue"),
                                group.by.cell_borders = TRUE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test",
                                group.by.colors.use = c("Cell" = "blue"),
                                group.by.cell_borders = FALSE)
    testthat::expect_type(p, "list")
    
    sample$orig.ident <- as.factor(sample$orig.ident)
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "orig.ident",
                                use_viridis = FALSE,
                                enforce_symmetry = FALSE,
                                group.by = "orig.ident",
                                group.by.show.dots = TRUE,
                                group.by.legend = "Test")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - cutoffs", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                min.cutoff = 19500,
                                max.cutoff = 20000)
    testthat::expect_type(p, "list")
    
    testthat::expect_warning({p <- SCpubr::do_FeaturePlot(sample = sample,
                                                          features = c("TOX2", "EPC1"),
                                                          min.cutoff = 1)})
    testthat::expect_type(p, "list")
  
  testthat::expect_warning({p <- SCpubr::do_FeaturePlot(sample = sample,
                                                        features = c("TOX2", "EPC1"),
                                                        max.cutoff = 1)})
  testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "seurat_clusters",
                                min.cutoff = 19500,
                                max.cutoff = 20000)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("nCount_RNA", "EPC1"),
                                min.cutoff = c(19500, 1),
                                max.cutoff = c(20000, 2))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                split.by = "annotation",
                                features = c("nCount_RNA", "EPC1"),
                                min.cutoff = c(19500, 1),
                                max.cutoff = c(20000, 2))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - contour", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                            plot_density_contour = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                            plot_density_contour = TRUE,
                            contour.position = "bottom")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                            plot_density_contour = TRUE,
                            contour.position = "top")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                plot_density_contour = TRUE,
                                contour.position = "bottom",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                plot_density_contour = TRUE,
                                contour.position = "top",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                            split.by = "annotation",
                            raster = TRUE,
                            plot_density_contour = TRUE,
                            contour.position = "top")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                split.by = "annotation",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "bottom")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                split.by = "annotation",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "top",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                split.by = "annotation",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "bottom",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                            idents.highlight = "0",
                            raster = TRUE,
                            plot_density_contour = TRUE,
                            contour.position = "top")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                idents.highlight = "0",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "bottom")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                idents.highlight = "0",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "top",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                feature = "nCount_RNA",
                                idents.highlight = "0",
                                raster = TRUE,
                                plot_density_contour = TRUE,
                                contour.position = "bottom",
                                enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

  })


  testthat::test_that("do_FeaturePlot: PASS - legend.title", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                legend.title = "pepe")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                legend.title = "pepe",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - cell_borders", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE, raster = TRUE, pt.size = 1)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE, idents.highlight = "1")
    testthat::expect_type(p, "list")
    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE, raster = TRUE, idents.highlight = "1", pt.size = 1)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE, split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", plot_cell_borders = TRUE, raster = TRUE, split.by = "seurat_clusters", pt.size = 1)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - enforce_symmetry", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample, features = c("EPC1", "nCount_RNA"), enforce_symmetry = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", enforce_symmetry = TRUE, idents.highlight = c("1", "3"))
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample, features = "EPC1", enforce_symmetry = TRUE, split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - multiple features", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("nCount_RNA", "nFeature_RNA"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - title", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                plot.title = "Feature Plot")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - subtitle", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                plot.subtitle = "Feature Plot")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                plot.caption = "Feature Plot")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - individual titles", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("nCount_RNA", "nFeature_RNA"),
                                individual.titles = c("A", NA))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - individual subtitles ", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("nCount_RNA", "nFeature_RNA"),
                                individual.subtitles = c("A", NA))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - individual captions", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = c("nCount_RNA", "nFeature_RNA"),
                                individual.captions = c("A", NA))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - dims", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample,
                                features = "nCount_RNA",
                                dims = c(2, 1))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - subset of cells", {
    testthat::skip_on_cran()


    cells.plot <- colnames(sample[, !(sample$seurat_clusters %in% c("2", "5", "8"))])
    p <- SCpubr::do_FeaturePlot(sample,
                                features = "EPC1",
                                cells.highlight = cells.plot)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - subset of identities", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "EPC1",
                                idents.highlight = c("1", "2"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - subset of cells and identities", {
    testthat::skip_on_cran()


    cells.plot <- colnames(sample[, !(sample$seurat_clusters %in% c("2", "5", "8"))])
    p <- SCpubr::do_FeaturePlot(sample,
                                features = "EPC1",
                                cells.highlight = cells.plot,
                                idents.highlight = c("1", "2"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "EPC1",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by and idents.keep", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "EPC1",
                                split.by = "seurat_clusters",
                                idents.keep = c("1", "2"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by and idents.keep multiple features", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = c("EPC1", "nCount_RNA"),
                                split.by = "seurat_clusters",
                                idents.keep = c("1", "2"))
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - modify color maps", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "nCount_RNA",
                                viridis.palette = "F")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: WARNING - features as a list", {
    testthat::skip_on_cran()


    testthat::expect_warning(SCpubr::do_FeaturePlot(sample,
                                                    features = list("A" = "nCount_RNA")))
  })

  testthat::test_that("do_FeaturePlot: FAIL - individual titles, subtitles or captions do not match with number of features", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = c("nCount_RNA", "EPC1"),
                                                  individual.titles = "A"))
    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = c("nCount_RNA", "EPC1"),
                                                  individual.subtitles = "A"))
    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = c("nCount_RNA", "EPC1"),
                                                  individual.captions = "A"))
  })

  testthat::test_that("do_FeaturePlot: PASS - legend position = right", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "nCount_RNA",
                                legend.position = "right")
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_FeaturePlot: PASS - ussing diffusion reduction", {
    testthat::skip_on_cran()


    test <- sample@reductions$umap[[]]
    colnames(test) <- c("DC_1", "DC_2")
    obj <- Seurat::CreateDimReducObject(test, assay = "SCT", key = "DC_")
    sample@reductions$diffusion <- obj
    p <- SCpubr::do_FeaturePlot(sample,
                                features = "nCount_RNA",
                                reduction = "diffusion")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - duplicated idents.keep", {
    testthat::skip_on_cran()


    testthat::expect_message(SCpubr::do_FeaturePlot(sample,
                                                    features = "nCount_RNA",
                                                    split.by = "seurat_clusters",
                                                    idents.keep = c("2", "2")))
  })


  testthat::test_that("do_FeaturePlot: PASS - plotting a Dimensional reduction component", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by factor", {
    testthat::skip_on_cran()


    sample$factor_seurat_clusters <- factor(sample$seurat_clusters, levels = c("2", "0", "1", "3", "4", "5", "6", "7", "8"))
    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                split.by = "factor_seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by and plot.title", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                split.by = "seurat_clusters",
                                plot.title = "Title",
                                plot.subtitle = "Subtitle",
                                plot.caption = "Caption")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by and pca", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                split.by = "seurat_clusters",
                                reduction = "pca")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - split.by and diffusion", {
    testthat::skip_on_cran()


    test <- sample@reductions$umap[[]]
    colnames(test) <- c("DC_1", "DC_2")
    obj <- Seurat::CreateDimReducObject(test, assay = "SCT", key = "DC_")
    sample@reductions$diffusion <- obj
    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                split.by = "seurat_clusters",
                                reduction = "diffusion")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - remove legend", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                legend.position = "none")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - normal legend", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                legend.type = "normal")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - colorbar legend", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                legend.type = "colorbar")
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_FeaturePlot: PASS - normal legend - split.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                legend.type = "normal",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_FeaturePlot: PASS - colorbar legend - split.by", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample,
                                features = "PC_1",
                                legend.type = "colorbar",
                                split.by = "seurat_clusters")
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_FeaturePlot: FAIL - wrong legend type", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = "PC_1",
                                                  legend.type = "wrong"))
  })

  testthat::test_that("do_FeaturePlot: FAIL - wrong legend position", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = "PC_1",
                                                  legend.position = "wrong"))
  })
  testthat::test_that("do_FeaturePlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_FeaturePlot(sample,
                                                  features = "PC_1",
                                                  font.type = "wrong"))
  })

  testthat::test_that("do_FeaturePlot: PASS - plot axis", {
    testthat::skip_on_cran()


    p <- SCpubr::do_FeaturePlot(sample = sample, plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample, reduction = "pca", plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_FeaturePlot(sample = sample, dims = c(2, 1), plot.axes = TRUE, features = "nCount_RNA")
    testthat::expect_type(p, "list")

    sample@reductions$diffusion <- sample@reductions$umap
    p <- SCpubr::do_FeaturePlot(sample = sample,
                                reduction = "diffusion",
                                plot.axes = TRUE,
                                features = "nCount_RNA")
    testthat::expect_type(p, "list")
  })
}


