context("Testing enrichmentmap and related functionality")

test_that("Graph is generated", {
  g <- enrichment_map(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 50
  )
  expect_is(g, "igraph")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  g1 <- enrichment_map(gtl = gtl_macrophage)
  expect_is(g1, "igraph")

  expect_true(identical_graphs(g, g1))

  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  g2 <- enrichment_map(
    res_enrich = res_enrich_withscores,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    color_by = "z_score",
    n_gs = 50
  )
  expect_is(g2, "igraph")

  res_pos_z <- na.omit(res_enrich_withscores[res_enrich_withscores$z_score >= 0, ])
  g3 <- enrichment_map(
    res_enrich = res_pos_z,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    color_by = "z_score",
    n_gs = 50
  )
  expect_is(g3, "igraph")


  library(magrittr)
  vi <- visNetwork::visIgraph(g) %>%
    visOptions(
      highlightNearest = list(
        enabled = TRUE,
        degree = 1,
        hover = TRUE
      ),
      nodesIdSelection = TRUE
    )

  expect_is(vi, "visNetwork")
  expect_is(vi, "htmlwidget")
})

test_that("Errors and warnings are thrown", {
  expect_error(
    enrichment_map(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_de = res_macrophage_IFNg_vs_naive,
      annotation_obj = anno_df,
      color_by = "pvalue",
      n_gs = 50
    )
  )
})
