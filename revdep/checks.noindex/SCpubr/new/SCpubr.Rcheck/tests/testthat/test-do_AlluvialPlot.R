if (base::isFALSE(dep_check[["do_AlluvialPlot"]])){

  testthat::test_that("do_AlluvialPlot: CRAN essential tests", {

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 last_group = "seurat_clusters")

    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_EnrichmentHeatmap: PASS - normal", {
    testthat::skip_on_cran()

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters")

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 flip = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 flip = TRUE,
                                 use_labels = TRUE,
                                 repel = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 flip = TRUE,
                                 use_labels = TRUE,
                                 repel = FALSE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 flip = TRUE,
                                 use_labels = FALSE,
                                 repel = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 flip = TRUE,
                                 use_labels = FALSE,
                                 repel = FALSE)

    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_AlluvialPlot: factors", {
    sample$orig.ident <- as.factor(sample$orig.ident)
    sample$seurat_clusters <- as.factor(sample$seurat_clusters)
    sample$annotation <- as.factor(sample$annotation)
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters")

    testthat::expect_type(p, "list")

    sample$orig.ident <- as.character(sample$orig.ident)
    sample$seurat_clusters <- as.character(sample$seurat_clusters)
    sample$annotation <- as.character(sample$annotation)
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters")

    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_AlluvialPlot: geom_flow", {
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_geom_flow = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_geom_flow = FALSE)

    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_AlluvialPlot: stratum.fill.conditional", {
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_geom_flow = FALSE,
                                 stratum.fill.conditional = FALSE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_geom_flow = FALSE,
                                 stratum.fill.conditional = TRUE)

    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_AlluvialPlot: use_viridis", {
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = "annotation",
                                 last_group = "seurat_clusters",
                                 use_geom_flow = FALSE,
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_AlluvialPlot: colors.use", {
    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "seurat_clusters",
                                 last_group = "orig.ident",
                                 colors.use = c("Cell" = "blue"))

    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_AlluvialPlot: test_numbers", {
    sample$annotation2 <- sample$annotation
    sample$annotation3 <- sample$annotation
    sample$annotation4 <- sample$annotation
    sample$annotation5 <- sample$annotation
    sample$annotation6 <- sample$annotation
    sample$annotation7 <- sample$annotation
    sample$annotation8 <- sample$annotation

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3",
                                                   "annotation4"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3",
                                                   "annotation4",
                                                   "annotation5"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3",
                                                   "annotation4",
                                                   "annotation5",
                                                   "annotation6"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3",
                                                   "annotation4",
                                                   "annotation5",
                                                   "annotation6",
                                                   "annotation7"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_AlluvialPlot(sample,
                                 first_group = "orig.ident",
                                 middle_groups = c("annotation",
                                                   "annotation2",
                                                   "annotation3",
                                                   "annotation4",
                                                   "annotation5",
                                                   "annotation6",
                                                   "annotation7",
                                                   "annotation8"),
                                 last_group = "seurat_clusters",
                                 use_viridis = TRUE)

    testthat::expect_type(p, "list")

  })
}


