context("Testing the geneset simplify feature based on overlap")

test_that("Dataset is simplified", {
  res_simp <- gs_simplify(res_enrich_IFNg_vs_naive,
    gs_overlap = 0.75
  )
  expect_is(res_simp, "data.frame")
  expect_true(nrow(res_simp) < nrow(res_enrich_IFNg_vs_naive))

  res_megasimp <- gs_simplify(res_enrich_IFNg_vs_naive,
    gs_overlap = 0
  )
  expect_equal(nrow(res_megasimp), 11)

  res_simp <- get_aggrscores(res_simp,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  expect_is(gs_volcano(res_simp), "gg")
})
