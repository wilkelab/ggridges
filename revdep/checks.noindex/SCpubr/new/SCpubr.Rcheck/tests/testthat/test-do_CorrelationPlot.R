if (base::isFALSE(dep_check[["do_CorrelationPlot"]])){

  testthat::test_that("do_CorrelationPlot: CRAN essentials", {
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15])

    p <- SCpubr::do_CorrelationPlot(sample = sample, legend.position = "top")
    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_CorrelationPlot: PASS - normal", {

    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")

    p <- SCpubr::do_CorrelationPlot(sample = sample, legend.position = "top", group.by = "orig.ident")
    testthat::expect_true("ggplot" %in% class(p))

    p <- SCpubr::do_CorrelationPlot(sample = sample, legend.position = "right")
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(sample = sample, legend.position = "right", cluster = TRUE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(sample = sample, legend.position = "right", cluster = FALSE)
    testthat::expect_true("ggplot" %in% class(p))
  })
  
  testthat::test_that("do_CorrelationPlot: PASS - jaccard", {
    
    testthat::skip_on_cran()
    sample$orig.ident <- ifelse(sample$seurat_clusters %in% c("1", "2"), "A", "B")
    
    genes <- list("A" = rownames(sample)[1:5],
                  "B" = rownames(sample)[3:8],
                  "C" = rownames(sample)[5:13])
    
    p <- SCpubr::do_CorrelationPlot(input_gene_list = genes, mode = "jaccard", legend.position = "top", cluster = FALSE, use_viridis = TRUE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(input_gene_list = genes, mode = "jaccard", legend.position = "top", cluster = TRUE, use_viridis = FALSE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(input_gene_list = genes, mode = "jaccard", legend.position = "top", remove.diagonal = TRUE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(input_gene_list = genes, mode = "jaccard", legend.position = "top", remove.diagonal = FALSE)
    testthat::expect_true("ggplot" %in% class(p))
    
    p <- SCpubr::do_CorrelationPlot(input_gene_list = genes, mode = "jaccard", legend.position = "right")
    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_CorrelationPlot: PASS - group.by", {
    testthat::skip_on_cran()

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15])

    p <- SCpubr::do_CorrelationPlot(sample = sample,
                                    group.by = "seurat_clusters")
    testthat::expect_true("ggplot" %in% class(p))
  })

  testthat::test_that("do_CorrelationPlot: PASS - group.by - rotate axis labels", {
    testthat::skip_on_cran()

    genes <- list("A" = Seurat::VariableFeatures(sample)[1:5],
                  "B" = Seurat::VariableFeatures(sample)[6:10],
                  "C" = Seurat::VariableFeatures(sample)[11:15])

    p <- SCpubr::do_CorrelationPlot(sample = sample,
                                    group.by = "seurat_clusters",
                                    axis.text.x.angle = 0)
    testthat::expect_true("ggplot" %in% class(p))
  })
}
