if(base::isFALSE(dep_check[["do_TermEnrichmentPlot"]])){

  testthat::test_that("do_TermEnrichmentPlot: CRAN essentials", {

    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2)
    testthat::expect_type(p, "list")

    enriched_terms$GO_Cellular_Component_2021 <- NULL
    enriched_terms$Azimuth_Cell_Types_2021 <- NULL
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS - legend position = right", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       legend.position = "right")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS - number of terms", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2)
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_TermEnrichmentPlot: PASS - length of terms", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       nchar_wrap = 20)
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS - modify colors", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       colors.use = c("#e9d8a6", "#9b2226"))
    testthat::expect_type(p, "list")
  })


  testthat::test_that("do_TermEnrichmentPlot: PASS - single database legend types", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       colors.use = c("#e9d8a6", "#9b2226"),
                                       legend.type = "normal")
    testthat::expect_type(p, "list")

    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       colors.use = c("#e9d8a6", "#9b2226"),
                                       legend.type = "colorbar")
    testthat::expect_type(p, "list")



  testthat::test_that("do_TermEnrichmentPlot: FAIL - wrong legend.type", {
    testthat::skip_on_cran()
    testthat::expect_error(SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                                         nterms = 2,
                                                         legend.type = "wrong"))
  })

  testthat::test_that("do_TermEnrichmentPlot: FAIL - wrong legend.position", {
    testthat::skip_on_cran()
    testthat::expect_error(SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                                         nterms = 2,
                                                         legend.position = "wrong"))
  })

  testthat::test_that("do_TermEnrichmentPlot: FAIL - wrong font.type", {
    testthat::skip_on_cran()
    testthat::expect_error(SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                                         nterms = 2,
                                                         font.type = "wrong"))
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS - single database legend normal - one pvalue", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       colors.use = c("#e9d8a6", "#9b2226"),
                                       legend.type = "normal")
    testthat::expect_type(p, "list")
  })

  testthat::test_that("do_TermEnrichmentPlot: PASS - single database legend colorbar - one pvalue", {
    testthat::skip_on_cran()
    p <- SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                       nterms = 2,
                                       colors.use = c("#e9d8a6", "#9b2226"),
                                       legend.type = "colorbar")
    testthat::expect_type(p, "list")
  })




  testthat::test_that("do_TermEnrichmentPlot: FAIL - provide more colors than needed", {
    testthat::skip_on_cran()


    testthat::expect_error(SCpubr::do_TermEnrichmentPlot(enriched_terms = enriched_terms,
                                                         nterms = 2,
                                                         colors.use = c("#e9d8a6", "#9b2226", "red")))
  })
  })
}

