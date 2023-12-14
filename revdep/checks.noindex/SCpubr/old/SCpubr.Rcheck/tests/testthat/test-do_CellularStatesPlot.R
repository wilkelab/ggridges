if (base::isFALSE(dep_check[["do_CellularStatesPlot"]])){

  testthat::test_that("do_CellularStatesPlot: CRAN essentials", {

    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       plot_enrichment_scores = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       raster = TRUE,
                                       pt.size = 1,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - cell_borders", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       raster = TRUE,
                                       pt.size = 1,
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       raster = TRUE,
                                       pt.size = 1,
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       raster = TRUE,
                                       pt.size = 1,
                                       plot_cell_borders = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - continuous feature", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       plot_enrichment_scores = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       plot_enrichment_scores = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       plot_features = TRUE,
                                       features = "EPC1",
                                       plot_enrichment_scores = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_cell_borders = TRUE,
                                       plot_features = FALSE,
                                       plot_enrichment_scores = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables marginal", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables marginal marginal.size", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.size = 8,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables marginal marginal.group FALSE", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.group = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables marginal distribution types", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.type = "density",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.type = "histogram",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.type = "boxplot",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       marginal.type = "violin",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")

    #p <- SCpubr::do_CellularStatesPlot(sample = sample,
    #                                   input_gene_list = genes,
    #                                   x1 = "A",
    #                                   y1 = "B",
    #                                   plot_marginal_distributions = TRUE,
    #                                   plot_cell_borders = FALSE,
    #                                   marginal.type = "densigram",
    #                                   nbin = 1,
    #                                   ctrl = 10)
    #testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 2 variables marginal wrong marginal.type", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error({SCpubr::do_CellularStatesPlot(sample = sample,
                                                          input_gene_list = genes,
                                                          x1 = "A",
                                                          y1 = "B",
                                                          plot_marginal_distributions = TRUE,
                                                          marginal.type = "wrong",
                                                          nbin = 1,
                                                          ctrl = 10)})
  })


  testthat::test_that("do_CellularStatesPlot: PASS - title, subtitle and caption", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       plot.title = "A",
                                       plot.subtitle = "B",
                                       plot.caption = "C",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables enforce symmetry", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables, colors.use", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    Seurat::Idents(sample) <- sample$orig.ident
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       colors.use = c("A" = "black", "B" = "red"),
                                       x1 = "A",
                                       y1 = "B",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables, group.by", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       group.by = "orig.ident",
                                       x1 = "A",
                                       y1 = "B",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables remove axis ticks", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       axis.ticks = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables remove axis text", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       axis.text = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 2 variables, group.by and colors.use", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       group.by = "orig.ident",
                                       colors.use = c("A" = "black", "B" = "red"),
                                       x1 = "A",
                                       y1 = "B",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 2 variables same parameters", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "A",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 2 variables x1 not in the list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "Not in list",
                                                         y1 = "A",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 2 variables y1 not in the list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "Not in list",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 2 variables provide features", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "B",
                                                         plot_features = TRUE,
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 3 variables", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       raster = TRUE,
                                       pt.size = 1,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 3 variables marginal", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 3 variables enforce symmetry", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_CellularStatesPlot: FAIL - 3 variables duplicated parameters", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "A",
                                                         x2 = "B",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 3 variables x1 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "Not in list",
                                                         y1 = "A",
                                                         x2 = "B",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 3 variables x2 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "B",
                                                         y1 = "A",
                                                         x2 = "Not in list",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 3 variables y1 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "Not in list",
                                                         x2 = "B",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 4 variables", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       raster = TRUE,
                                       pt.size = 1,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 4 variables marginal", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       plot_marginal_distributions = TRUE,
                                       plot_cell_borders = FALSE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_CellularStatesPlot: PASS - 4 variables enforce symmetry", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    p <- SCpubr::do_CellularStatesPlot(sample = sample,
                                       input_gene_list = genes,
                                       x1 = "A",
                                       y1 = "B",
                                       x2 = "C",
                                       y2 = "D",
                                       enforce_symmetry = TRUE,
                                       nbin = 1,
                                       ctrl = 10)
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_CellularStatesPlot: FAIL - 4 variables repeated parameters", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "A",
                                                         x2 = "A",
                                                         y2 = "A",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 4 variables x1 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "Not in list",
                                                         y1 = "B",
                                                         x2 = "C",
                                                         y2 = "D",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 4 variables y1 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "Not in list",
                                                         x2 = "C",
                                                         y2 = "D",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 4 variables x2 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "B",
                                                         x2 = "Not in list",
                                                         y2 = "D",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - 4 variables y2 not in list", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "B",
                                                         x2 = "C",
                                                         y2 = "Not in list",
                                                         nbin = 1,
                                                         ctrl = 10))
  })

  testthat::test_that("do_CellularStatesPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15],
                  "D" = Seurat::VariableFeatures(sample)[16:20])


    testthat::expect_error(SCpubr::do_CellularStatesPlot(sample = sample,
                                                         input_gene_list = genes,
                                                         x1 = "A",
                                                         y1 = "Not in list",
                                                         x2 = "B",
                                                         font.type = "wrong",
                                                         nbin = 1,
                                                         ctrl = 10))
  })
}

