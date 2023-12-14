test_that("If plot_sbp returns 0.",{
  mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                      chi11$cluster_col, chi11$stim_label,
                                      chi11$unstim_label, seed_val = 123,
                                      umap = FALSE)

  s <- plot_K_Fisher(mapped_data, path = NULL, verbose = TRUE)
  expect_type(s, "list")
})

test_that("If plot_umap returns 0.",{
  mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                      chi11$cluster_col, chi11$stim_label,
                                      chi11$unstim_label, seed_val = 123,
                                      umap = TRUE, umap_cells = 50)

  u <- plot_umap(mapped_data, path = NULL, verbose = TRUE)
  expect_type(u, "list")
})

test_that("If plot_kde returns 0.",{
  mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                      chi11$cluster_col, chi11$stim_label,
                                      chi11$unstim_label, seed_val = 123,
                                      umap = FALSE)

  k <- plot_exprs(mapped_data, path = NULL, verbose = TRUE)
  expect_type(k, "list")
})
