context("Input main parameter checking works")

test_that("Early fails are triggered", {

  # providing a count matrix
  expect_error(GeneTonic(counts(dds_macrophage),
    res_macrophage_IFNg_vs_naive,
    res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  ))

  # providing a simple data frame
  expect_error(GeneTonic(dds_macrophage,
    deseqresult2df(res_macrophage_IFNg_vs_naive),
    res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  ))

  # providing data frame with missing key columns
  expect_error(GeneTonic(dds_macrophage,
    res_macrophage_IFNg_vs_naive,
    res_enrich_IFNg_vs_naive[, -1],
    annotation_obj = anno_df
  ))

  # providing data frame with missing key columns
  expect_error(GeneTonic(dds_macrophage,
    res_macrophage_IFNg_vs_naive,
    res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df[, -1]
  ))
})

test_that("Warnings are thrown correctly", {
  expect_warning(
    checkup_GeneTonic(
      dds_unnormalized,
      res_macrophage_IFNg_vs_naive,
      res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df
    )
  )

  dds_mod <- dds_macrophage[-c(1:50), ]
  expect_warning(
    checkup_GeneTonic(
      dds_mod,
      res_macrophage_IFNg_vs_naive,
      res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df
    )
  )
})

test_that("Messages are returned correctly", {
  expect_message(
    checkup_GeneTonic(
      dds_macrophage,
      res_macrophage_IFNg_vs_naive,
      res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df,
      verbose = TRUE
    )
  )
})


test_that("List-based input checks", {
  gtl <- list(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )

  gtl_missing <- list(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    # res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )

  expect_message(
    checkup_gtl(gtl, verbose = TRUE)
  )

  expect_error(
    checkup_gtl(gtl_missing)
  )

  expect_error(
    checkup_gtl(dds_macrophage)
  )
})
