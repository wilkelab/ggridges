context("Testing ggs_graph and related functionality")

test_that("Graph is generated", {
  g <- ggs_graph(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 20
  )
  expect_is(g, "igraph")
  g2 <- ggs_graph(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 30,
    prettify = FALSE
  )
  expect_is(g2, "igraph")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  g3 <- ggs_graph(
    gtl = gtl_macrophage,
    n_gs = 20
  )
  expect_is(g3, "igraph")

  hub_df <- summarize_ggs_hubgenes(g3)
  expect_is(hub_df, "data.frame")

  expect_true(identical_graphs(g, g3))

  alt_pal <- scales::alpha(
    colorRampPalette(RColorBrewer::brewer.pal(name = "RdYlBu", 11))(50), 0.4
  )
  g3 <- ggs_graph(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 10,
    genes_graph_colpal = alt_pal
  )
  expect_is(g3, "igraph")

  expect_error(
    ggs_graph(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_de = res_macrophage_IFNg_vs_naive,
      annotation_obj = anno_df,
      genes_graph_colpal = list("blue", "red"),
      n_gs = 20
    )
  )

  expect_error(
    ggs_graph(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_de = res_macrophage_IFNg_vs_naive,
      annotation_obj = anno_df,
      genes_graph_colpal = c("blue", "whitesss", "red"),
      n_gs = 20
    )
  )
})


test_that("Backbone functionality up and running", {
  res_enrich_nozscore <- res_enrich_IFNg_vs_naive
  res_enrich_IFNg_vs_naive <- get_aggrscores(
    res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    anno_df
  )
  bbg <- ggs_backbone(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 10,
    bb_on = "genesets"
  )
  expect_is(bbg, "igraph")
  bbg2 <- ggs_backbone(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 20,
    bb_on = "features",
    bb_method = "fixedrow",
    bb_remove_singletons = FALSE,
    bb_fullinfo = TRUE
  )
  expect_is(bbg2, "list")
  expect_is(bbg2$bbgraph, "igraph")
  expect_is(bbg2$ggs, "igraph")

  gtl_macrophage <- list(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  bbg3 <- ggs_backbone(
    gtl = gtl_macrophage,
    n_gs = 10
  )
  expect_is(bbg3, "igraph")

  expect_true(identical_graphs(bbg, bbg3))

  # this few genesets, as this method would take quite long to run
  bbg4 <- ggs_backbone(
    gtl = gtl_macrophage,
    bb_method = "fdsm",
    n_gs = 4
  )
  expect_is(bbg4, "igraph")
  expect_true(vcount(bbg4) <= 2)

  expect_error(
    ggs_backbone(
      res_enrich = res_enrich_nozscore,
      res_de = res_macrophage_IFNg_vs_naive,
      annotation_obj = anno_df,
      n_gs = 10,
      bb_on = "genesets"
    )
  )

  res_de_nolfc <- res_macrophage_IFNg_vs_naive
  res_de_nolfc$log2FoldChange <- NULL

  expect_error(
    ggs_backbone(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_de = res_de_nolfc,
      annotation_obj = anno_df,
      n_gs = 10,
      bb_on = "features"
    )
  )

  expect_error(
    ggs_backbone(
      res_enrich = res_enrich_IFNg_vs_naive,
      res_de = res_macrophage_IFNg_vs_naive,
      annotation_obj = anno_df,
      n_gs = 10,
      bb_on = "genesets",
      color_by_geneset = "some_column_not_there"
    )
  )
})
