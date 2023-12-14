context("Testing gene set scoring")

test_that("Scores are calculated and plotted", {
  gss_mat <- gs_scores(
    se = vst_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive[1:20, ],
    annotation_obj = anno_df
  )
  expect_is(gss_mat, "matrix")
  p <- gs_scoresheat(gss_mat)
  expect_is(p, "gg")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive[1:20, ],
    annotation_obj = anno_df
  )

  gss_gtl <- gs_scores(
    se = vst_macrophage,
    gtl = gtl_macrophage
  )
  expect_is(gss_gtl, "matrix")

  expect_equal(gss_mat, gss_gtl)
})
