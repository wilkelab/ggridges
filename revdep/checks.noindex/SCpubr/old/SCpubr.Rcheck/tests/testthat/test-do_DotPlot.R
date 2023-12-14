if (isFALSE(dep_check[["do_DotPlot"]])){

  testthat::test_that("do_DotPlot: CRAN essentials", {

    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - one variable", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE,
                            legend.position = "right")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE,
                            legend.position = "top")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE,
                            legend.position = "none")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = TRUE,
                            legend.position = "none")
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = TRUE,
                            legend.position = "none",
                            use_viridis = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = TRUE,
                            legend.position = "none",
                            use_viridis = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE,
                            legend.position = "none",
                            use_viridis = FALSE)
    testthat::expect_type(p, "list")
    
    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            dot_border = FALSE,
                            legend.position = "none",
                            use_viridis = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - plot grid", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            plot.grid = TRUE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            plot.grid = FALSE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - use_viridis", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            use_viridis = TRUE,
                            dot_border = FALSE)
    testthat::expect_type(p, "list")

    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            use_viridis = TRUE,
                            dot_border = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - one variable legend normal", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            legend.type = "normal")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - one variable legend colorbar", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            legend.type = "colorbar")
    testthat::expect_type(p, "list")
  })





  testthat::test_that("do_DotPlot: FAIL - wrong legend type", {
    testthat::skip_on_cran()


    testthat::expect_error(suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                                                features = "EPC1",
                                                                flip = TRUE,
                                                                legend.type = "wrong")}))

  })

  testthat::test_that("do_DotPlot: FAIL - wrong legend position", {
    testthat::skip_on_cran()


    testthat::expect_error(suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                                                features = "EPC1",
                                                                flip = TRUE,
                                                                legend.position = "wrong")}))

  })

  testthat::test_that("do_DotPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()


    testthat::expect_error(suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                                                features = "EPC1",
                                                                flip = TRUE,
                                                                font.type = "wrong")}))

  })

  testthat::test_that("do_DotPlot: PASS - one variable flip", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            flip = TRUE)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - multiple features", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    genes <- unname(unlist(genes))
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - multiple features flip", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    genes <- unname(unlist(genes))
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes,
                                              flip = TRUE)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - multiple features flip rotate x labels", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    genes <- unname(unlist(genes))
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes,
                                              flip = TRUE,
                                              axis.text.x.angle = 45)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - list of features", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - list of features cluster idents", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes,
                                              cluster = TRUE)})
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_DotPlot: PASS - list of features cluster idents modify colors", {
    testthat::skip_on_cran()


    genes <- list("Naive CD4+ T" = Seurat::VariableFeatures(sample)[1:2],
                  "EPC1+ Mono" = Seurat::VariableFeatures(sample)[3:4],
                  "Memory CD4+" = Seurat::VariableFeatures(sample)[5],
                  "B" = Seurat::VariableFeatures(sample)[6],
                  "CD8+ T" = Seurat::VariableFeatures(sample)[7],
                  "FCGR3A+ Mono" = Seurat::VariableFeatures(sample)[8:9],
                  "NK" = Seurat::VariableFeatures(sample)[10:11],
                  "DC" = Seurat::VariableFeatures(sample)[12:13],
                  "Platelet" = Seurat::VariableFeatures(sample)[14])
    p <- suppressWarnings({SCpubr::do_DotPlot(sample = sample,
                                              features = genes,
                                              cluster = TRUE,
                                              colors.use = c("#001219", "#e9d8a6"))})
    testthat::expect_type(p, "list")
  })



  testthat::test_that("do_DotPlot: PASS - one variable xlab, ylab, title, subtitle, caption", {
    testthat::skip_on_cran()


    p <- SCpubr::do_DotPlot(sample = sample,
                            features = "EPC1",
                            xlab = "A",
                            ylab = "B",
                            plot.title = "C",
                            plot.subtitle = "D",
                            plot.caption = "E")
    testthat::expect_type(p, "list")
  })
}

