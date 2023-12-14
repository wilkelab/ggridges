if (base::isFALSE(dep_check[["do_CopyNumberVariantPlot"]])){

  testthat::test_that("do_BarPlot: CRAN essentials", {
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })

  testthat::test_that("do_BarPlot: PASS - normal cells all chromosomes", {

    testthat::skip_on_cran()

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations,
                                            flip = TRUE)
    testthat::expect_type(out, "list")
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations,
                                            flip = TRUE,
                                            group.by = c("seurat_clusters", "orig.ident", "annotation"))
    testthat::expect_type(out, "list")
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations,
                                            flip = FALSE)
    testthat::expect_type(out, "list")
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations,
                                            flip = FALSE,
                                            group.by = c("seurat_clusters", "orig.ident", "annotation"))
    testthat::expect_type(out, "list")
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations,
                                            flip = FALSE,
                                            return_object = TRUE)
    testthat::expect_type(out, "list")
    
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            group.by = c("seurat_clusters", "orig.ident"),
                                            chromosome_locations = human_chr_locations,
                                            flip = TRUE)
    testthat::expect_type(out, "list")
    
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            group.by = c("seurat_clusters", "orig.ident"),
                                            chromosome_locations = human_chr_locations,
                                            flip = FALSE,
                                            return_object = TRUE)
    testthat::expect_type(out, "list")
  })

  testthat::test_that("do_BarPlot: PASS - normal cells one chromosome", {

    testthat::skip_on_cran()
    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object,
                                            using_metacells = FALSE,
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })

  testthat::test_that("do_BarPlot: PASS - metacells all chromosomes", {
    testthat::skip_on_cran()

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })

  

  testthat::test_that("do_BarPlot: PASS - group.by", {
    testthat::skip_on_cran()

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            group.by = "orig.ident",
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })


  testthat::test_that("do_BarPlot: PASS - legend.position", {
    testthat::skip_on_cran()

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            legend.position = "right",
                                            legend.title = "test",
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            legend.position = "bottom",
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })

  testthat::test_that("do_BarPlot: PASS - legend.position", {
    testthat::skip_on_cran()

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            legend.type = "normal",
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")

    out <- SCpubr::do_CopyNumberVariantPlot(sample = sample,
                                            infercnv_object = infercnv_object_metacells,
                                            using_metacells = TRUE,
                                            metacell_mapping = metacell_mapping,
                                            legend.type = "colorbar",
                                            chromosome_locations = human_chr_locations)
    testthat::expect_type(out, "list")
  })
}

