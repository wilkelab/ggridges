context("Testing clustering of gene sets and related functionality")

test_that("Clustering is generated", {
  em <- enrichment_map(
    res_enrich = res_enrich_IFNg_vs_naive,
    res_de = res_macrophage_IFNg_vs_naive,
    annotation_obj = anno_df,
    n_gs = 50
  )
  expect_is(em, "igraph")

  emc <- cluster_markov(em)

  expect_is(emc, "communities")
  expect_equal(length(emc), 9)

  expect_error(cluster_markov(res_enrich_IFNg_vs_naive))

  emc_s <- cluster_markov(em, allow_singletons = FALSE, return_esm = TRUE)

  expect_error(emc_e <- cluster_markov(em, add_self_loops = FALSE))

  expect_error(cluster_markov(em, add_self_loops = 3))
  expect_error(cluster_markov(em, loop_value = -1))
  expect_error(cluster_markov(em, mcl_expansion = 0.5))
})
