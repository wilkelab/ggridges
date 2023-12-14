context("Testing gene set heatmap and related functionality")

test_that("Geneset heatmap is created", {
  cur_gsid <- res_enrich_IFNg_vs_naive$gs_id[1]
  p <- gs_heatmap(
    se = vst_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
    de_only = FALSE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    center_mean = TRUE,
    scale_row = TRUE
  )
  expect_is(p, "HeatmapList")
  p2 <- gs_heatmap(
    se = vst_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
    de_only = TRUE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    center_mean = TRUE,
    scale_row = TRUE
  )
  expect_is(p2, "HeatmapList")

  vst_macrophage$condition_char <- as.character(vst_macrophage$condition)
  vst_macrophage$some_numbers <- seq_len(ncol(vst_macrophage))
  vst_macrophage$also_negative_numbers <- vst_macrophage$some_numbers * c(-1, 1)

  p3 <- gs_heatmap(
    se = vst_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
    de_only = TRUE,
    cluster_rows = TRUE,
    cluster_columns = TRUE,
    center_mean = TRUE,
    scale_row = TRUE,
    winsorize_threshold = 2,
    anno_col_info = c(
      "condition_char",
      "some_numbers",
      "also_negative_numbers"
    ),
    plot_title = "Just this as title"
  )
  expect_is(p3, "HeatmapList")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  p4 <- gs_heatmap(
    se = vst_macrophage,
    gtl = gtl_macrophage,
    geneset_id = cur_gsid
  )
  expect_is(p4, "HeatmapList")

  # enforcing id not present in the object
  mycustomlist <- c(
    rownames(vst_macrophage)[1:10],
    "ENSmadeUPid"
  )

  expect_warning(
    p4 <- gs_heatmap(
      se = vst_macrophage,
      res_de = res_macrophage_IFNg_vs_naive,
      res_enrich = res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df,
      genelist = mycustomlist,
      FDR = 0.05,
      de_only = FALSE,
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      center_mean = TRUE,
      scale_row = TRUE,
      anno_col_info = "condition"
    )
  )
  
  expect_error(
    p5 <- gs_heatmap(
      se = vst_macrophage,
      res_de = res_macrophage_IFNg_vs_naive,
      res_enrich = res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df,
      genelist = mycustomlist,
      FDR = 0.05,
      de_only = FALSE,
      cluster_rows = TRUE,
      cluster_columns = TRUE,
      center_mean = TRUE,
      scale_row = TRUE,
      winsorize_threshold = -3,
      anno_col_info = "condition"
    )
  )

  file.remove("Rplots.pdf")
})
