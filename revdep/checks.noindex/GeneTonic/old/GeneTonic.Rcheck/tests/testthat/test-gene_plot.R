context("Testing function for plotting the gene expression levels")

test_that("Basic gene plot is generated", {
  p <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    annotation_obj = anno_df,
    transform = TRUE,
    labels_repel = TRUE
  )
  expect_is(p, "gg")

  p2_noanno_normallabels_untransformed <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    transform = FALSE,
    labels_repel = FALSE
  )
  expect_is(p2_noanno_normallabels_untransformed, "gg")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  p3_gtl <- gene_plot(
    gtl = gtl_macrophage,
    gene = "ENSG00000285982"
  )
  expect_is(p3_gtl, "gg")
  
  expect_error({
    gene_plot(
      dds = dds_macrophage,
      gene = "ENSG00000285982",
      assay = "counts",
      intgroup = "factor_not_there",
      annotation_obj = anno_df,
      transform = TRUE,
      labels_repe
    )
  })
})

test_that("Enforcing plot types", {
  p_jitter <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    plot_type = "jitteronly"
  )
  p_boxplot <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    plot_type = "boxplot"
  )
  p_violin <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    plot_type = "violin"
  )
  p_sina <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    plot_type = "sina"
  )
  expect_is(p_jitter, "gg")
  expect_is(p_boxplot, "gg")
  expect_is(p_violin, "gg")
  expect_is(p_sina, "gg")
})

test_that("Data instead of plot is returned", {
  df_jitter <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    return_data = TRUE
  )
  expect_is(df_jitter, "data.frame")
})

test_that("Assays are correctly accessed", {
  p_non_norm_counts <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "counts",
    intgroup = "condition",
    normalized = FALSE
  )
  expect_is(p_non_norm_counts, "gg")
  p_tpm <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "abundance",
    intgroup = "condition",
    normalized = FALSE
  )
  expect_is(p_tpm, "gg")

  p_other_assay <- gene_plot(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    assay = "avgTxLength",
    intgroup = "condition",
    normalized = FALSE
  )
  expect_is(p_other_assay, "gg")
})

test_that("Extraction of expression values works", {
  df_simple <- get_expression_values(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    intgroup = "condition",
    assay = "counts"
  )
  expect_is(df_simple, "data.frame")
  
  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive[1:200, ],
    annotation_obj = anno_df
  )
  df_simple_gtl <- get_expression_values(
    gtl = gtl_macrophage,
    gene = "ENSG00000285982",
    intgroup = "condition",
    assay = "counts"
  )
  expect_is(df_simple_gtl, "data.frame")
  
  expect_error(get_expression_values(
    dds = dds_macrophage,
    gene = "ENSG00000285982",
    intgroup = "condition",
    assay = "count"
  ))
  df_unnormalized <- get_expression_values(
    dds = dds_unnormalized,
    gene = "ENSG00000285982",
    intgroup = "condition",
    assay = "counts"
  )
  expect_is(df_unnormalized, "data.frame")
})
