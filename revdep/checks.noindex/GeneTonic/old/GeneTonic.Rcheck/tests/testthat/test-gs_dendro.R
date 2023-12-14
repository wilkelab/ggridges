context("Testing the gene sets dendrogram")

test_that("Gene set dendrogram is created", {
  res_enrich_withscores <- get_aggrscores(res_enrich_IFNg_vs_naive,
    res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    aggrfun = mean
  )
  my_dend <- gs_dendro(res_enrich_withscores, n_gs = 20)
  expect_is(my_dend, "dendrogram")
  my_dend2 <- gs_dendro(res_enrich_withscores,
    n_gs = 15,
    gs_dist_type = "jaccard", color_branches_by = NULL
  )
  my_dend3 <- gs_dendro(res_enrich_withscores,
    n_gs = 20,
    color_leaves_by = NULL, size_leaves_by = NULL
  )
  my_dend4 <- gs_dendro(res_enrich_withscores,
    n_gs = 20,
    color_leaves_by = NULL, size_leaves_by = NULL,
    create_plot = FALSE
  )
  expect_is(my_dend2, "dendrogram")
  expect_is(my_dend3, "dendrogram")
  expect_is(my_dend4, "dendrogram")
  
  my_dend_pval <- gs_dendro(res_enrich_withscores, n_gs = 20,
                            color_leaves_by = "gs_pvalue")
  expect_is(my_dend_pval, "dendrogram")
  
  re_subset <- res_enrich_withscores[res_enrich_withscores[1:12, ]$z_score >= 0, ]
  my_dend_subset <- gs_dendro(re_subset, n_gs = 12, color_leaves_by = "z_score")
  expect_is(my_dend_subset, "dendrogram")

  expect_warning(gs_dendro(res_enrich_withscores,
    n_gs = 5,
    color_leaves_by = NULL, size_leaves_by = NULL,
    create_plot = FALSE
  ))

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_withscores,
    annotation_obj = anno_df
  )
  my_dend_gtl <- gs_dendro(
    gtl = gtl_macrophage,
    n_gs = 20
  )
  expect_is(my_dend_gtl, "dendrogram")

  expect_equal(my_dend, my_dend_gtl)

  expect_error(
    gs_dendro(res_enrich_withscores,
      n_gs = 8,
      color_leaves_by = "mean_score"
    )
  )
  expect_error(
    gs_dendro(res_enrich_withscores,
      n_gs = 6,
      size_leaves_by = "pvalue"
    )
  )
})
