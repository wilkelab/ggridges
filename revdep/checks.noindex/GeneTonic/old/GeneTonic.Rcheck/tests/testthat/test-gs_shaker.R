context("Pre-shaking the enrichment results")

test_that("Converting from topGOtable results", {
  res_enrich_IFNg_vs_naive <- shake_topGOtableResult(topgoDE_macrophage_IFNg_vs_naive)
  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_enrich_IFNg_vs_naive)))

  topgo_not_all_columns <- topgoDE_macrophage_IFNg_vs_naive[, -1]
  expect_error(
    shake_topGOtableResult(topgo_not_all_columns)
  )
  expect_error(
    shake_topGOtableResult(topgoDE_macrophage_IFNg_vs_naive, p_value_column = "p.value_weight")
  )
  topgo_nogenes <- topgoDE_macrophage_IFNg_vs_naive[, -9]
  expect_error(
    shake_topGOtableResult(topgo_nogenes)
  )
})

test_that("Converting from clusterProfiler", {
  # from enrichGO
  res_enrich_IFNg_vs_naive_cp <- shake_enrichResult(ego_IFNg_vs_naive)
  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_enrich_IFNg_vs_naive_cp)))
  
  expect_error(shake_enrichResult(topgoDE_macrophage_IFNg_vs_naive))
  expect_error(shake_enrichResult(as.data.frame(ego_IFNg_vs_naive)))
  
  ego_mod <- ego_IFNg_vs_naive
  ego_mod@result$geneID <- NULL
  expect_error(shake_enrichResult(ego_mod))
  
  # from gseGO
  res_gsenrich_IFNg_vs_naive_cp <- shake_gsenrichResult(gsego_IFNg_vs_naive)
  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_gsenrich_IFNg_vs_naive_cp)))
  
  expect_error(shake_gsenrichResult(topgoDE_macrophage_IFNg_vs_naive))
  expect_error(shake_gsenrichResult(as.data.frame(ego_IFNg_vs_naive)))
  
  gsego_mod <- gsego_IFNg_vs_naive
  gsego_mod@result$core_enrichment <- NULL
  expect_error(shake_gsenrichResult(gsego_mod))
  
})


test_that("Converting from the output of DAVID", {
  david_output <- system.file("extdata", "david_output_chart_BPonly_ifng_vs_naive.txt", package = "GeneTonic")
  res_enrich_IFNg_vs_naive_david <- shake_davidResult(david_output)
  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_enrich_IFNg_vs_naive_david)))

  expect_error(shake_davidResult("non_existing_file.txt"))
  expect_error(shake_davidResult(topgoDE_macrophage_IFNg_vs_naive))
  expect_error(shake_davidResult(as.data.frame(ego_IFNg_vs_naive)))

  david_tempfile <- tempfile()
  david_full_set <- read.delim(david_output, header = TRUE, sep = "\t")
  # dropping a column
  david_without_a_column <- david_full_set[, -1]
  write.table(david_without_a_column, file = david_tempfile, quote = FALSE, sep = "\t")

  expect_error(shake_davidResult(david_tempfile))
})


test_that("Converting from the output of g:Profiler", {
  gprofiler_output_file <- system.file(
    "extdata",
    "gProfiler_hsapiens_5-25-2020_tblexport_IFNg_vs_naive.csv",
    package = "GeneTonic"
  )
  res_from_gprofiler <- shake_gprofilerResult(gprofiler_output_file = gprofiler_output_file)

  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_from_gprofiler)))
  expect_true(nrow(res_from_gprofiler) == 5593)
  expect_true(ncol(res_from_gprofiler) == 7)

  data(gostres_macrophage, package = "GeneTonic")
  res_from_gprofiler_2 <- shake_gprofilerResult(
    gprofiler_output = gostres_macrophage$result
  )
  expect_true(all(required_colnames %in% colnames(res_from_gprofiler_2)))
  expect_true(nrow(res_from_gprofiler_2) == 5593)
  expect_true(ncol(res_from_gprofiler_2) == 8)

  expect_error(shake_gprofilerResult("non_existing_file.txt"))
  expect_error(shake_gprofilerResult(topgoDE_macrophage_IFNg_vs_naive))

  expect_error(shake_gprofilerResult(gprofiler_output = list(res_from_gprofiler_2)))
  expect_error(shake_gprofilerResult(gprofiler_output = gostres_macrophage))

  gostres_macrophage_gonewrong <- gostres_macrophage
  colnames(gostres_macrophage_gonewrong$result)[3] <- "any_wrong_name"
  expect_error(shake_gprofilerResult(gprofiler_output = gostres_macrophage_gonewrong$result))


  gprofiler_tempfile <- tempfile()
  gprofiler_full_set <- read.delim(gprofiler_output_file, header = TRUE, sep = ",")
  # dropping a column
  gprofiler_without_a_column <- gprofiler_full_set[, -1]
  write.table(gprofiler_without_a_column, file = gprofiler_tempfile, quote = TRUE, sep = ",")

  expect_error(shake_gprofilerResult(gprofiler_output_file = gprofiler_tempfile))
})


test_that("Converting from the output of enrichR", {
  enrichr_output_file <- system.file("extdata",
    "enrichr_tblexport_IFNg_vs_naive.txt",
    package = "GeneTonic"
  )
  res_from_enrichr <- shake_enrichrResult(enrichr_output_file = enrichr_output_file)
  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")

  expect_true(all(required_colnames %in% colnames(res_from_enrichr)))
  expect_true(nrow(res_from_enrichr) == 2734)
  expect_true(ncol(res_from_enrichr) == 7)

  data(enrichr_output_macrophage, package = "GeneTonic")
  res_from_enrichr2 <- shake_enrichrResult(
    enrichr_output = enrichr_output_macrophage[["GO_Biological_Process_2018"]]
  )

  expect_true(all(required_colnames %in% colnames(res_from_enrichr2)))
  expect_true(nrow(res_from_enrichr2) == 2734)
  expect_true(ncol(res_from_enrichr2) == 7)


  expect_error(shake_enrichrResult("non_existing_file.txt"))
  expect_error(shake_enrichrResult(enrichr_output = topgoDE_macrophage_IFNg_vs_naive))

  expect_error(shake_enrichrResult(enrichr_output = enrichr_output_macrophage))

  enrichr_output_macrophage_gonewrong <- enrichr_output_macrophage
  colnames(enrichr_output_macrophage_gonewrong$GO_Biological_Process_2018)[3] <- "any_wrong_name"
  expect_error(shake_enrichrResult(enrichr_output = enrichr_output_macrophage_gonewrong$GO_Biological_Process_2018))
})

test_that("Converting from the output of fgsea", {
  data(fgseaRes, package = "GeneTonic")
  res_from_fgsea <- shake_fgseaResult(fgseaRes)

  required_colnames <- c("gs_id", "gs_description", "gs_pvalue", "gs_genes", "gs_de_count", "gs_bg_count")
  expect_true(all(required_colnames %in% colnames(res_from_fgsea)))
  expect_true(nrow(res_from_fgsea) == 7341)
  expect_true(ncol(res_from_fgsea) == 8)

  expect_error(shake_fgseaResult(non_existing_object))
  expect_error(shake_fgseaResult(topgoDE_macrophage_IFNg_vs_naive))
  expect_error(shake_fgseaResult(fgsea_output = list(res_from_fgsea)))

  fgseaRes_gonewrong <- fgseaRes
  fgseaRes_gonewrong$leadingEdge <- vapply(
    fgseaRes_gonewrong$leadingEdge,
    function(arg) paste(arg, collapse = ","), character(1)
  )

  expect_error(shake_fgseaResult(fgseaRes_gonewrong))
})
