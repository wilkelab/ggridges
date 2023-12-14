library("GeneTonic")

context("Check that the Shiny app is generated")

dds <- DESeq2::makeExampleDESeqDataSet(n = 100, m = 8)

test_that("Shiny app is generated", {
  # expect_is(GeneTonic(), "shiny.appobj")
  # expect_is(GeneTonic(dds), "shiny.appobj")
  # expect_is(GeneTonic(dds_macrophage, res_macrophage_IFNg_vs_naive), "shiny.appobj")
  expect_is(
    GeneTonic(dds_macrophage,
      res_macrophage_IFNg_vs_naive,
      res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df
    ),
    "shiny.appobj"
  )

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  expect_is(GeneTonic(gtl = gtl_macrophage), "shiny.appobj")
})
