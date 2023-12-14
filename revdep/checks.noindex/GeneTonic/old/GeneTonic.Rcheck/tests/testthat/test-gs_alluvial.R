context("Testing the alluvial plots")

test_that("Alluvial plot (interactive) is generated", {
  p <- gs_alluvial(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 5
  )
  expect_is(p, "plotly")
  expect_is(p, "htmlwidget")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  p1 <- gs_alluvial(
    gtl = gtl_macrophage,
    n_gs = 5
  )
  expect_is(p1, "plotly")
})
