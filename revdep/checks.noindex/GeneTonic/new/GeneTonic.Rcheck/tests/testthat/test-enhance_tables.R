context("Testing enhance_tables and related functionality")

test_that("Enhanced table is created", {
  p <- enhance_table(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 50,
    chars_limit = 60
  )
  expect_is(p, "gg")

  pl <- ggplotly(p)
  expect_is(pl, "plotly")
  expect_is(pl, "htmlwidget")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  p_gtl <- enhance_table(
    gtl = gtl_macrophage,
    n_gs = 20
  )
  expect_is(p_gtl, "gg")

  with_scores <- get_aggrscores(gtl = gtl_macrophage)
  expect_is(with_scores, "data.frame")

  p2 <- enhance_table(with_scores,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 5,
    chars_limit = 60,
    plot_title = "My custom title - with scores"
  )
  expect_is(p2, "gg")
  
  p_ridge <- enhance_table(with_scores,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 5,
    plot_style = "ridgeline"
  )
  expect_is(p_ridge, "gg")
  
  p_ridge_zscorecol <- enhance_table(with_scores,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 5,
    plot_style = "ridgeline",
    ridge_color = "gs_score"
  )
  expect_is(p_ridge_zscorecol, "gg")
  
  expect_message({
    p_ridge_fallback <- enhance_table(
      gtl = gtl_macrophage,
      n_gs = 5,
      plot_style = "ridgeline",
      ridge_color = "gs_score")
  })
  
  
  re_modified <- res_enrich_IFNg_vs_naive
  # patching up some letters to mess up the name of a gene
  re_modified$gs_genes[1] <- 
    paste0(re_modified$gs_genes[1], "AAAAAAAA")
  
  expect_message({
    p3 <- enhance_table(re_modified,
                        res_macrophage_IFNg_vs_naive,
                        annotation_obj = anno_df,
                        n_gs = 5,
                        chars_limit = 60,
                        plot_title = "After modifying the genes assigned to gs1"
    )
  })
  expect_is(p3, "gg")
})

test_that("The distillery is up and running", {
  d_en <- distill_enrichment(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 30,
    cluster_fun = "cluster_markov"
  )

  expect_is(d_en, "list")
  expect_is(d_en$distilled_table, "data.frame")
  expect_is(d_en$distilled_em, "igraph")

  library("igraph")

  d_en_louvain <- distill_enrichment(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 30,
    cluster_fun = "cluster_louvain"
  )

  d_en_walktrap <- distill_enrichment(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 30,
    cluster_fun = "cluster_walktrap"
  )

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  d_gtl <- distill_enrichment(
    gtl = gtl_macrophage,
    n_gs = 30
  )

  expect_is(d_en_louvain, "list")
  expect_is(d_en_walktrap, "list")
  expect_is(d_gtl, "list")

  expect_equal(d_en$distilled_table, d_gtl$distilled_table)
  expect_equal(d_en$res_enrich, d_gtl$res_enrich)
  expect_true(identical_graphs(d_en$distilled_em, d_gtl$distilled_em))

  expect_error(distill_enrichment(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 100,
    cluster_fun = "cluster_wrong_function"
  ))
})
