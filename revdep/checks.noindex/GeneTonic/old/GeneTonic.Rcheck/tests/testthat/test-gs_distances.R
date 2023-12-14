context("Testing create_kappa_matrix and related functionality")

test_that("Kappa matrix is created", {
  kmat <- create_kappa_matrix(res_enrich_IFNg_vs_naive, n_gs = 30)
  expect_true(all(diag(kmat) == 1))

  kmat2 <- create_kappa_matrix(res_enrich_IFNg_vs_naive,
    n_gs = 20,
    gs_ids = res_enrich_IFNg_vs_naive$gs_id[21:30]
  )
  expect_true(identical(kmat, kmat2))

  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )

  kmat_gtl <- create_kappa_matrix(
    gtl = gtl_macrophage,
    n_gs = 30
  )

  expect_equal(kmat, kmat_gtl)

  jmat_gtl <- create_jaccard_matrix(
    gtl = gtl_macrophage,
    n_gs = 60
  )
  expect_is(jmat_gtl, "matrix")
})
