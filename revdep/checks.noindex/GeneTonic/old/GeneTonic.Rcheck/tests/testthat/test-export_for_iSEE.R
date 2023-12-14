context("Testing export to SE for iSEE")

test_that("Export SE", {
  se <- export_for_iSEE(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive
  )
  expect_is(se, "SummarizedExperiment")
  expect_equal(nrow(se), 17806)
  expect_equal(ncol(se), 24)

  expect_true("DESeq2_condition_IFNg_vs_naive" %in% colnames(rowData(se)))

  dds_cutout <- dds_macrophage[1:10000, ]
  expect_message(
    se2 <- export_for_iSEE(
      dds = dds_cutout,
      res_de = res_macrophage_IFNg_vs_naive
    )
  )

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )

  se_gtl <- export_for_iSEE(gtl = gtl_macrophage)
  expect_is(se_gtl, "SummarizedExperiment")

  expect_equal(se, se_gtl)
})
