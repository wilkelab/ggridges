context("Testing gene set signature volcano and related functionality")

test_that("Geneset signature volcano is created", {
  cur_gsid <- res_enrich_IFNg_vs_naive$gs_id[1]
  p <- signature_volcano(
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
  )
  expect_is(p, "gg")
  p2 <- signature_volcano(
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    color = "red"
  )
  expect_is(p2, "gg")
  p3 <- signature_volcano(
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
    plot_title = "Random Title"
  )
  expect_is(p3, "gg")

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  p4 <- signature_volcano(
    gtl = gtl_macrophage,
    geneset_id = cur_gsid,
  )
  expect_is(p4, "gg")

  p5 <- signature_volcano(
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df,
    geneset_id = cur_gsid,
    FDR = 0.05,
    volcano_labels = 35
  )
  expect_is(p5, "gg")

  # enforcing id not present in the object
  mycustomlist <- c(
    rownames(vst_macrophage)[1:10],
    "ENSmadeUPid"
  )

  expect_warning(
    p6 <- signature_volcano(
      res_de = res_macrophage_IFNg_vs_naive,
      res_enrich = res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df,
      genelist = mycustomlist,
    )
  )
})
