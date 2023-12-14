if (base::isFALSE(dep_check[["do_GroupedGOTermPlot"]])){
  testthat::test_that("do_GroupedGOTermPlot: CRAN essential tests", {

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      levels.use = c(1, 2),
                                      verbose = FALSE,
                                      min.overlap = 1)

    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_GroupedGOTermPlot: PASS", {
    testthat::skip_on_cran()
    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      min.overlap = 1)

    testthat::expect_type(p, "list")


    suppressMessages({
      p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                        org.db = org.db,
                                        GO_ontology = "BP",
                                        verbose = TRUE,
                                        min.overlap = 1)
    })

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2),
                                      return_matrices = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = c("CCL1", "CCL4", "CENPE", "CENPK", "OLIG1"),
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2))

    testthat::expect_type(p, "list")


    suppressMessages({
      p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                        org.db = org.db,
                                        GO_ontology = "BP",
                                        verbose = TRUE,
                                        levels.use = c(1, 2),
                                        min.overlap = 1)
    })

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2),
                                      min.overlap = 1,
                                      flip = FALSE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2),
                                      min.overlap = 1,
                                      flip = TRUE)

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2),
                                      min.overlap = 1,
                                      flip = FALSE,
                                      legend.position = "right")

    testthat::expect_type(p, "list")

    p <- SCpubr::do_GroupedGOTermPlot(genes = "MBP",
                                      org.db = org.db,
                                      GO_ontology = "BP",
                                      verbose = FALSE,
                                      levels.use = c(1, 2),
                                      min.overlap = 1,
                                      flip = FALSE,
                                      legend.position = "none")

    testthat::expect_type(p, "list")
  })

}
