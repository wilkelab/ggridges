context("Testing the gene set summaries and related functionality")

test_that("summary_heat plot is generated", {
  p <- gs_summary_heat(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 20
  )
  expect_is(p, "gg")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )

  p2 <- gs_summary_heat(
    gtl = gtl_macrophage,
    n_gs = 20
  )
  expect_is(p2, "gg")
})

test_that("summary plots are generated", {
  expect_error(gs_summary_overview(res_enrich_IFNg_vs_naive))
  expect_error(gs_summary_overview_pair(res_enrich_IFNg_vs_naive))
  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  
  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_withscores[1:200, ],
    annotation_obj = anno_df
  )

  # generating a shuffled dataset
  res_enrich2 <- res_enrich_withscores[1:20, ]
  set.seed(42)
  shuffled_ones <- sample(seq_len(20)) # to generate permuted p-values
  res_enrich2$gs_pvalue <- res_enrich2$gs_pvalue[shuffled_ones]
  res_enrich2$z_score <- res_enrich2$z_score[shuffled_ones]
  res_enrich2$aggr_score <- res_enrich2$aggr_score[shuffled_ones]

  p1 <- gs_summary_overview(res_enrich_withscores)
  p1_bar <- gs_summary_overview(res_enrich_withscores, return_barchart = TRUE)
  p1_nocol <- gs_summary_overview(res_enrich_withscores, color_by = NULL)
  p1_bar_nocol <- gs_summary_overview(res_enrich_withscores,
    color_by = NULL,
    return_barchart = TRUE
  )
  p1_gtl <- gs_summary_overview(gtl = gtl_macrophage)
  
  expect_is(p1, "gg")
  expect_is(p1_bar, "gg")
  expect_is(p1_nocol, "gg")
  expect_is(p1_bar_nocol, "gg")
  expect_is(p1_gtl, "gg")
  
  p2 <- gs_summary_overview_pair(res_enrich_withscores, res_enrich2)
  expect_is(p2, "gg")

  res_enrich2 <- res_enrich_withscores[1:42, ]
  res_enrich3 <- res_enrich_withscores[1:42, ]
  res_enrich4 <- res_enrich_withscores[1:42, ]

  set.seed(2 * 42)
  shuffled_ones_2 <- sample(seq_len(42)) # to generate permuted p-values
  res_enrich2$gs_pvalue <- res_enrich2$gs_pvalue[shuffled_ones_2]
  res_enrich2$z_score <- res_enrich2$z_score[shuffled_ones_2]
  res_enrich2$aggr_score <- res_enrich2$aggr_score[shuffled_ones_2]

  set.seed(3 * 42)
  shuffled_ones_3 <- sample(seq_len(42)) # to generate permuted p-values
  res_enrich3$gs_pvalue <- res_enrich3$gs_pvalue[shuffled_ones_3]
  res_enrich3$z_score <- res_enrich3$z_score[shuffled_ones_3]
  res_enrich3$aggr_score <- res_enrich3$aggr_score[shuffled_ones_3]

  set.seed(4 * 42)
  shuffled_ones_4 <- sample(seq_len(42)) # to generate permuted p-values
  res_enrich4$gs_pvalue <- res_enrich4$gs_pvalue[shuffled_ones_4]
  res_enrich4$z_score <- res_enrich4$z_score[shuffled_ones_4]
  res_enrich4$aggr_score <- res_enrich4$aggr_score[shuffled_ones_4]

  compa_list <- list(
    scenario2 = res_enrich2,
    scenario3 = res_enrich3,
    scenario4 = res_enrich4
  )

  p3a <- gs_horizon(res_enrich_withscores,
    compared_res_enrich_list = compa_list,
    n_gs = 50,
    sort_by = "clustered"
  )
  p3b <- gs_horizon(res_enrich_withscores,
    compared_res_enrich_list = compa_list,
    n_gs = 20,
    sort_by = "first_set"
  )
  expect_is(p3a, "gg")
  expect_is(p3b, "gg")


  # for the pairs...
  expect_error(
    gs_summary_overview_pair(
      res_enrich = res_enrich2,
      res_enrich2 = res_enrich_IFNg_vs_naive, # no z score there
      color_by = "z_score"
    )
  )

  expect_error(
    gs_summary_overview_pair(
      res_enrich = res_enrich_withscores[1:30, ],
      res_enrich2 = res_enrich_withscores[31:60, ]
    )
  )

  # for the horizon...
  expect_error(
    gs_horizon(
      res_enrich = topgoDE_macrophage_IFNg_vs_naive,
      compared_res_enrich_list = compa_list,
      n_gs = 50,
      sort_by = "clustered"
    )
  )

  expect_error(
    gs_horizon(res_enrich_withscores,
      compared_res_enrich_list = compa_list,
      n_gs = 0,
      sort_by = "clustered"
    )
  )

  expect_error(
    gs_horizon(res_enrich_withscores,
      compared_res_enrich_list = res_enrich2,
      n_gs = 50,
      sort_by = "clustered"
    )
  )

  expect_message(
    gs_horizon(res_enrich_withscores,
      compared_res_enrich_list = unname(compa_list),
      n_gs = 20,
      sort_by = "first_set"
    )
  )

  compa_list2 <- compa_list
  compa_list2[[3]] <- topgoDE_macrophage_IFNg_vs_naive
  expect_error(
    gs_horizon(res_enrich_withscores,
      compared_res_enrich_list = compa_list2,
      n_gs = 50,
      sort_by = "clustered"
    )
  )

  compa_list3 <- compa_list
  compa_list3[[3]] <- res_enrich_IFNg_vs_naive # no z_score in it
  expect_error(
    gs_horizon(res_enrich_withscores,
      compared_res_enrich_list = compa_list3,
      n_gs = 50,
      sort_by = "clustered"
    )
  )

  compa_list4 <- compa_list
  res_other_pvalue <- res_enrich_withscores
  res_other_pvalue$gs_pval_weight <- res_other_pvalue$gs_pvalue
  expect_error(
    gs_horizon(res_other_pvalue,
      compared_res_enrich_list = compa_list4,
      n_gs = 50,
      p_value_column = "gs_pval_weight",
      sort_by = "clustered"
    )
  )

  re1 <- res_enrich_withscores[1:30, ]
  compa_list5 <- list(re2 = res_enrich2[51:70, ])
  expect_error(
    gs_horizon(re1,
      compared_res_enrich_list = compa_list5,
      n_gs = 50,
      sort_by = "clustered"
    )
  )
})
