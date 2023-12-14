context("Testing gs_upset and related functionality")

test_that("upset data is generated", {
  ugsg <- create_upsetdata(res_enrich_IFNg_vs_naive[1:20, ])
  dim(ugsg)

  expect_is(ugsg, "data.frame")
  expect_equal(dim(ugsg), c(246, 20))

  ugsg_ids <- create_upsetdata(res_enrich_IFNg_vs_naive[1:5, ], use_ids = TRUE)

  expect_true(
    all(grepl(pattern = "^GO", colnames(ugsg_ids)))
  )
})


test_that("gs_upset functionality up and running", {

  p <- gs_upset(res_enrich_IFNg_vs_naive,
                n_gs = 10)
  expect_is(p, "gg")

  p_anno <- gs_upset(res_enrich_IFNg_vs_naive, res_de = res_macrophage_IFNg_vs_naive, annotation_obj = anno_df,
                     n_gs = 8,
                     add_de_direction = TRUE, add_de_gsgenes = TRUE)
  expect_is(p_anno, "gg")

  p_gtl <- gs_upset(gtl = gtl_macrophage,
                    n_gs = 15,
                    add_de_direction = TRUE, add_de_gsgenes = TRUE)
  expect_is(p_gtl, "gg")

  p_gtl_df <- gs_upset(gtl = gtl_macrophage,
                    n_gs = 15,
                    return_upsetgsg = TRUE)
  expect_is(p_gtl_df, "data.frame")

  # triggering exceptions
  expect_error({
    p_gtl <- gs_upset(res_enrich = gtl_macrophage,
                      n_gs = 15,
                      add_de_direction = TRUE, add_de_gsgenes = TRUE)
  })

  expect_error({
    p_node <- gs_upset(res_enrich = res_enrich_IFNg_vs_naive,
                      n_gs = 15,
                      add_de_direction = TRUE, add_de_gsgenes = FALSE)
  })

  expect_error({
    p_noanno <- gs_upset(res_enrich = res_enrich_IFNg_vs_naive,
                      n_gs = 15,
                      add_de_direction = FALSE, add_de_gsgenes = TRUE)
  })
})
