context("Testing gene set fuzzy clustering")

test_that("Fuzzy clustering on example set", {
  fake_re <- res_enrich_IFNg_vs_naive[1:8, ]
  # replacing assigned genes with the combination as in the example
  fake_re$gs_genes <- c(
    "gene1,gene2,gene3,gene4,gene5,gene6,gene7,gene8,gene9,gene10",
    "gene1,gene2,gene3,gene4,gene5,gene6,gene7,gene8,gene9,gene10",
    "gene1,gene2,gene3,gene4,gene5,gene6,gene7,gene8,gene9,gene10",
    "gene5,gene6,gene7,gene8,gene9,gene10,gene11",
    "gene6,gene7,gene8,gene9,gene10,gene11,gene12,gene13,gene14,gene15",
    "gene6,gene7,gene8,gene9,gene10,gene11,gene12,gene13,gene14,gene15",
    "gene6,gene7,gene8,gene9,gene10,gene11,gene12,gene13,gene14,gene15",
    "gene4,gene8,gene12"
  )

  fuzzy_davidexample <- gs_fuzzyclustering(fake_re,
    n_gs = nrow(fake_re),
    gs_ids = NULL,
    similarity_matrix = NULL,
    similarity_threshold = 0.34,
    fuzzy_seeding_initial_neighbors = 3,
    fuzzy_multilinkage_rule = 0.5
  )

  expect_is(fuzzy_davidexample, "data.frame")
  expect_equal(
    fuzzy_davidexample$gs_fuzzycluster,
    c(
      rep(1, 4),
      rep(2, 4),
      3
    )
  )

  expect_equal(
    fuzzy_davidexample$gs_cluster_status,
    c(
      "Representative", "Member", "Member", "Member",
      "Representative", "Member", "Member", "Member",
      "Representative"
    )
  )

  expect_warning(
    gs_fuzzyclustering(fake_re,
      n_gs = nrow(fake_re),
      gs_ids = NULL,
      similarity_matrix = NULL,
      similarity_threshold = 0.2
    )
  )
})

test_that("Fuzzy clustering on GeneTonic set", {
  res_enrich <- res_enrich_IFNg_vs_naive[1:200, ]

  fuzzy_gtexample <- gs_fuzzyclustering(
    res_enrich = res_enrich,
    n_gs = nrow(res_enrich),
    gs_ids = NULL,
    similarity_matrix = NULL,
    similarity_threshold = 0.35,
    fuzzy_seeding_initial_neighbors = 3,
    fuzzy_multilinkage_rule = 0.5
  )
  expect_is(fuzzy_gtexample, "data.frame")
  expect_equal(sum(fuzzy_gtexample$gs_cluster_status == "Representative"), 113)
  
  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive[1:200, ],
    annotation_obj = anno_df
  )
  
  fuzzy_gtexample_gtl <- gs_fuzzyclustering(
    gtl = gtl_macrophage,
    n_gs = nrow(res_enrich),
    gs_ids = NULL,
    similarity_matrix = NULL,
    similarity_threshold = 0.35,
    fuzzy_seeding_initial_neighbors = 3,
    fuzzy_multilinkage_rule = 0.5
  )
  expect_is(fuzzy_gtexample_gtl, "data.frame")
  
})
