context("Testing the GO volcano plot and related functionality")

test_that("Plot is generated", {
  expect_error(
    gs_volcano(res_enrich_IFNg_vs_naive)
  )

  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  expect_is(gs_volcano(res_enrich_withscores), "gg")
  
  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_withscores[1:200, ],
    annotation_obj = anno_df
  )
  expect_is(gs_volcano(gtl = gtl_macrophage), "gg")

  expect_error(
    gs_volcano(
      res_enrich_withscores,
      color_by = "fake_col"
    )
  )
})

test_that("mds plot with custom genesets", {
  mygenesets <- res_enrich_IFNg_vs_naive$gs_id[c(1, 10, 20)]
  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  expect_is(
    gs_volcano(res_enrich_withscores,
      gs_ids = mygenesets,
      plot_title = "mytitle-volcano"
    ),
    "gg"
  )
})
