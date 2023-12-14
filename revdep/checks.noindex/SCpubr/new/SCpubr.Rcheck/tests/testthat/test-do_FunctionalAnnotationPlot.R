if (base::isFALSE(dep_check[["do_FunctionalAnnotationPlot"]])){

  testthat::test_that("do_FunctionalAnnotationPlot: CRAN essential tests", {

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO")

    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_FunctionalAnnotationPlot: PASS - normal", {
    testthat::skip_on_cran()
  
    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO",
                                             return_matrix = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO")

    testthat::expect_type(p, "list")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO",
                                             legend.position = "top")

    testthat::expect_type(p, "list")


    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO",
                                             legend.position = "right")

    testthat::expect_type(p, "list")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = c("CCL1", "CCL4", "CENPE", "CENPK", "OLIG1"),
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO",
                                             min.overlap = 1)

    testthat::expect_type(p, "list")


    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO",
                                             legend.position = "none")

    testthat::expect_type(p, "list")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = c("CCL1", "CCL4", "CENPE", "CENPK", "OLIG1"),
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "GO")

    testthat::expect_type(p, "character")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "KEGG")

    testthat::expect_type(p, "character")


    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "KEGG",
                                             legend.position = "top")

    testthat::expect_type(p, "character")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "KEGG",
                                             legend.position = "right")

    testthat::expect_type(p, "character")

    p <- SCpubr::do_FunctionalAnnotationPlot(genes = "MBP",
                                             org.db = org.Hs.eg.db,
                                             organism = "hsa",
                                             database = "KEGG",
                                             legend.position = "none")

    testthat::expect_type(p, "character")
  })
}
