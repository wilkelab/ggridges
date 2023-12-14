if(R.Version()$major == "4"){
  test_that("Test Boruta output.",{
    mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                        chi11$cluster_col, chi11$stim_label,
                                        chi11$unstim_label, seed_val = 123,
                                        umap = FALSE)

    attribute_stats <- marker_ranking_boruta(mapped_data, path = NULL,
                                             n_cells = 1000, max_runs = 20, seed_val = 123,
                                             verbose = FALSE)

    expect_type(attribute_stats, "list")

  })
}
