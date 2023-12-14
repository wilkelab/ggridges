context("Testing the gene set radar plot and co.")

test_that("radar plot is generated", {
  expect_warning(gs_radar(res_enrich_IFNg_vs_naive))
  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  p <- gs_radar(res_enrich = res_enrich_withscores)
  expect_is(p, "plotly")

  res_enrich2 <- res_enrich_withscores[1:60, ]
  set.seed(42)
  shuffled_ones <- sample(seq_len(60)) # to generate permuted p-values
  res_enrich2$gs_pvalue <- res_enrich2$gs_pvalue[shuffled_ones]
  # ideally, I would also permute the z scores and aggregated scores
  p2 <- gs_radar(
    res_enrich = res_enrich_withscores,
    res_enrich2 = res_enrich2
  )
  expect_is(p2, "plotly")


  r2 <- res_enrich_IFNg_vs_naive
  expect_warning(
    gs_radar(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_enrich2 = r2
    )
  )

  r1only <- res_enrich_withscores[1:50, ]
  r2only <- res_enrich_withscores[51:150, ]
  expect_error(
    gs_radar(
      res_enrich = r1only,
      res_enrich2 = r2only
    )
  )
})
