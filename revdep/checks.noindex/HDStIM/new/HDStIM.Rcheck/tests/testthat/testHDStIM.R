if(R.Version()$major == "3" & R.Version()$minor == "6.3"){
  test_that("Whether HDStIM maps the same number of cells.",{
    mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                        chi11$cluster_col, chi11$stim_label,
                                        chi11$unstim_label, seed_val = 123,
                                        umap = FALSE, umap_cells = 50,
                                        verbose = FALSE)
    resp_data <- filter(mapped_data$response_mapping_main, stim_type != "U" & k_cluster_id == responding_cluster)
    expect_equal(nrow(resp_data), 717)

  })
}

if(R.Version()$major == "4"){
  test_that("Whether HDStIM maps the same number of cells.",{
    mapped_data <- HDStIM(chi11$expr_data, chi11$state_markers,
                                        chi11$cluster_col, chi11$stim_label,
                                        chi11$unstim_label, seed_val = 123,
                                        umap = TRUE, umap_cells = 50,
                                        verbose = FALSE)
    resp_data <- filter(mapped_data$response_mapping_main, stim_type != "U" & k_cluster_id == responding_cluster)
    expect_equal(nrow(resp_data), 717)

  })
}
