if (base::isFALSE(dep_check[["utils"]])){
  # CHECK SUGGESTS
  testthat::test_that("utils: check_suggests - FAIL - Wrong function", {
    testthat::skip_on_cran()
    testthat::expect_error(check_suggests("wrong_name"))
  })

  testthat::test_that("utils: check_suggests - FAIL - Package not installed", {
    testthat::skip_on_cran()
    testthat::expect_error(check_suggests("testing"))
  })

  testthat::test_that("utils: check_suggests - PASS - Correct function", {
    testthat::skip_on_cran()
    testthat::expect_silent(check_suggests("do_DimPlot"))
  })

  testthat::test_that("utils: check_suggests - PASS - Correct function", {
    testthat::skip_on_cran()
    testthat::expect_silent(check_suggests("do_DimPlot", passive = TRUE))
    testthat::expect_silent(check_suggests("do_DimPlot", passive = FALSE))
  })


  # STATE DEPENDENCIES

  
  # PACKAGE REPORT
  testthat::test_that("utils: package_report - PASS - general", {
    testthat::skip_on_cran()
    suppressMessages({testthat::expect_message(SCpubr::package_report(startup = FALSE))})
    suppressMessages({testthat::expect_message(SCpubr::package_report(startup = TRUE))})
  })
  
  
  # CHECK SEURAT

  # CHECK SUGGESTS
  testthat::test_that("utils: check_Seurat - FAIL - Not Seurat object", {
    testthat::skip_on_cran()
    testthat::expect_error(check_Seurat("not a Seurat object"))
  })

  testthat::test_that("utils: check_suggests - PASS - Seurat object", {
    testthat::skip_on_cran()


    testthat::expect_silent(check_Seurat(sample))
  })


  # CHECK COLORS
  testthat::test_that("utils: check_colors - FAIL - wrong color", {
    testthat::skip_on_cran()
    testthat::expect_error(check_colors("not_a_color"))

  })

  testthat::test_that("utils: check_colors - FAIL - wrong color in a vector of colors", {
    testthat::skip_on_cran()
    testthat::expect_error(check_colors(c("not_a_color", "red", "blue")))
  })

  testthat::test_that("utils: check_colors - PASS - One color", {
    testthat::skip_on_cran()
    testthat::expect_silent(check_colors("red"))
  })

  testthat::test_that("utils: check_colors - PASS - Several colors", {
    testthat::skip_on_cran()
    testthat::expect_silent(check_colors(c("red", "blue")))
  })




  # CHECK CONSISTENCY COLORS AND NAMES

  testthat::test_that("utils: check_consistency_colors_and_names - FAIL - more colors provided", {
    testthat::skip_on_cran()


    testthat::expect_error(check_consistency_colors_and_names(sample = sample,
                                                                       colors = c("a" = "red", "b" = "blue"),
                                                                       grouping_variable = "orig.ident"))
  })

  testthat::test_that("utils: check_consistency_colors_and_names - FAIL - names of colors not matching", {
    testthat::skip_on_cran()


    testthat::expect_error(check_consistency_colors_and_names(sample = sample,
                                                                       colors = c("a" = "red"),
                                                                       grouping_variable = "orig.ident"))
  })

  testthat::test_that("utils: check_consistency_colors_and_names - FAIL - less colors provided", {
    testthat::skip_on_cran()


    testthat::expect_error(check_consistency_colors_and_names(sample = sample,
                                                                       colors = c("1" = "red"),
                                                                       grouping_variable = "seurat_clusters"))
  })

  testthat::test_that("utils: check_consistency_colors_and_names - PASS - Colors matching", {
    testthat::skip_on_cran()


    testthat::expect_silent(check_consistency_colors_and_names(sample = sample,
                                                                        colors = c("0" = "red",
                                                                                   "1" = "red",
                                                                                   "2" = "red",
                                                                                   "3" = "red",
                                                                                   "4" = "red",
                                                                                   "5" = "red",
                                                                                   "6" = "red",
                                                                                   "7" = "red",
                                                                                   "8" = "red")))
  })

  testthat::test_that("utils: check_consistency_colors_and_names - PASS - Colors matching, grouping variable", {
    testthat::skip_on_cran()


    testthat::expect_silent(check_consistency_colors_and_names(sample = sample,
                                                                        colors = c("Cell" = "red"),
                                                                        grouping_variable = "orig.ident"))
  })


  # GENERATE COLOR SCALE
  testthat::test_that("utils: generate_color_scale - PASS - equal length of output", {
    testthat::skip_on_cran()
    names_use <- c("a", "b", "c")
    colors <- colorspace::qualitative_hcl(length(names_use), palette = "Dark 3")
    testthat::expect_length(colors, length(names_use))
  })

  # COMPUTE SCALES LIMITS

  testthat::test_that("utils: compute_scale_limits - PASS - using a gene", {

    testthat::skip_on_cran()

    output <- compute_scale_limits(sample = sample,
                                            feature = "EPC1")
    testthat::expect_length(output, 2)
  })

  testthat::test_that("utils: compute_scale_limits - PASS - using a metadata variable", {
    testthat::skip_on_cran()


    output <- compute_scale_limits(sample = sample,
                                            feature = "orig.ident")
    testthat::expect_length(output, 2)
  })

  testthat::test_that("utils: compute_scale_limits - PASS - using dimensional reduction variable", {
    testthat::skip_on_cran()


    output <- compute_scale_limits(sample = sample,
                                            feature = "PC_1")
    testthat::expect_length(output, 2)
  })

  # CHECK FEATURE

  testthat::test_that("utils: check_feature - FAIL - using the wrong gene", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                         features = "NOTEPC1"))
  })

  testthat::test_that("utils: check_feature - FAIL - using the wrong metadata", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                                  features = "oris.ident"))
  })

  testthat::test_that("utils: check_feature - FAIL - using the wrong dimensional reduction variable", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                                  features = "UMAP_38"))
  })

  testthat::test_that("utils: check_feature - FAIL - all features failing while in permissive mode", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                                  features = c("NOTEPC1", "UMAP_38"),
                                                  permissive = TRUE))
  })

  testthat::test_that("utils: check_feature - WARNING - using one wrong gene and one good", {
    testthat::skip_on_cran()


    testthat::expect_warning(check_feature(sample = sample,
                                                    features = c("NOTEPC1", "EPC1"),
                                                    permissive = TRUE))
  })

  testthat::test_that("utils: check_feature - WARNING - using one wrong metadata variable and one good", {
    testthat::skip_on_cran()


    testthat::expect_warning(check_feature(sample = sample,
                                                    features = c("oris.ident", "orig.ident"),
                                                    permissive = TRUE))
  })

  testthat::test_that("utils: check_feature - WARNING - using one wrong dimensional reduction variable and one good", {
    testthat::skip_on_cran()


    testthat::expect_warning(check_feature(sample = sample,
                                                    features = c("UMAP_38", "PC_1"),
                                                    permissive = TRUE))
  })

  testthat::test_that("utils: check_feature - PASS - dump reduction names", {
    testthat::skip_on_cran()


    dim_names <- check_feature(sample = sample,
                                        features = "PC_1",
                                        dump_reduction_names = TRUE)
    expected_output <- 0
    for (dim_red in names(sample@reductions)){
      expected_output <- expected_output + length(colnames(sample@reductions[[dim_red]][[]]))
    }
    testthat::expect_length(dim_names, expected_output)
  })

  testthat::test_that("utils: check_feature - PASS - permissive check length of output", {
    testthat::skip_on_cran()


    testthat::expect_warning({
      features <- check_feature(sample = sample,
                                         features = c("PC_1", "PC_99"),
                                         permissive = TRUE)
      testthat::expect_length(features, 1)
    })
  })

  testthat::test_that("utils: check_feature - PASS - permissive check length of output when both permissive and dump_reduction_names are present.", {
    testthat::skip_on_cran()


    output <- check_feature(sample = sample,
                                     features = "PC_1",
                                     dump_reduction_names = TRUE,
                                     permissive = TRUE)
    testthat::expect_length(output, 2)
  })

  testthat::test_that("utils: check_feature - ERROR - using the wrong enforcer", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                                  features = "EPC1",
                                                  enforce_check = "Gene",
                                                  enforce_parameter = "group.by"))
  })

  testthat::test_that("utils: check_feature - ERROR - using the wrong feature for the selected enforcer", {
    testthat::skip_on_cran()


    testthat::expect_error(check_feature(sample = sample,
                                                  features = "EPC1",
                                                  enforce_check = "reductions",
                                                  enforce_parameter = "group.by"))
  })


  # REMOVE NOT FOUND FEATURES
  testthat::test_that("utils: remove_not_found_features - PASS - 0 features removed - character", {
    testthat::skip_on_cran()
    features <- c("a", "b")
    not_found_features <- ""
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output, 2)
    testthat::expect_type(output, "character")
  })

  testthat::test_that("utils: remove_not_found_features - PASS - 1 features removed - character", {
    testthat::skip_on_cran()
    features <- c("a", "b")
    not_found_features <- "a"
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output, 1)
    testthat::expect_type(output, "character")
  })

  testthat::test_that("utils: remove_not_found_features - PASS - 2 features removed - character", {
    testthat::skip_on_cran()
    features <- c("a", "b")
    not_found_features <- c("a", "b")
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output, 0)
    testthat::expect_type(output, "character")
  })

  testthat::test_that("utils: remove_not_found_features - PASS - 0 features removed - list", {
    testthat::skip_on_cran()
    features <- list("A" = "a",
                     "B" = "b")
    not_found_features <- ""
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output$A, 1)
    testthat::expect_length(output$B, 1)
    testthat::expect_type(output, "list")
  })

  testthat::test_that("utils: remove_not_found_features - PASS - 1 features removed - list", {
    testthat::skip_on_cran()
    features <- list("A" = "a",
                     "B" = "b")
    not_found_features <- "a"
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output$A, 0)
    testthat::expect_length(output$B, 1)
    testthat::expect_type(output, "list")
  })

  testthat::test_that("utils: remove_not_found_features - PASS - 2 features removed - list", {
    testthat::skip_on_cran()
    features <- list("A" = "a",
                     "B" = "b")
    not_found_features <- c("a", "b")
    output <- remove_not_found_features(features = features, not_found_features = not_found_features)
    testthat::expect_length(output$A, 0)
    testthat::expect_length(output$B, 0)
    testthat::expect_type(output, "list")
  })


  # REMOVE DUPLICATED FEATURES

  testthat::test_that("utils: remove_duplicated_features - WARNING - having duplicated features - character", {
    testthat::skip_on_cran()
    features <- c("a", "a")
    testthat::expect_warning(remove_duplicated_features(features))
    output <- suppressWarnings({remove_duplicated_features(features)})
    testthat::expect_type(output, "character")
  })

  testthat::test_that("utils: remove_duplicated_features - WARNING - having duplicated features across lists - list", {
    testthat::skip_on_cran()
    features <- list("A" = "a",
                     "B" = "a")
    testthat::expect_warning(remove_duplicated_features(features))
    output <- suppressWarnings({remove_duplicated_features(features)})
    testthat::expect_type(output, "list")
  })

  testthat::test_that("utils: remove_duplicated_features - WARNING - having duplicated features within lists - list", {
    testthat::skip_on_cran()
    features <- list("A" = c("a", "a"),
                     "B" = "b")
    testthat::expect_warning(remove_duplicated_features(features))
    output <- suppressWarnings({remove_duplicated_features(features)})
    testthat::expect_type(output, "list")
  })

  testthat::test_that("utils: remove_duplicated_features - WARNING - having duplicated features across and between lists - list", {
    testthat::skip_on_cran()
    features <- list("A" = c("a", "a"),
                     "B" = "a")
    suppressWarnings({testthat::expect_warning(remove_duplicated_features(features))})
    output <- suppressWarnings({remove_duplicated_features(features)})
    testthat::expect_type(output, "list")

  })


  # CHECK IDENTITY

  testthat::test_that("utils: check_identity - FAIL - wrong identity", {
    testthat::skip_on_cran()


    testthat::expect_error(check_identity(sample, "wrong_identity"))
  })

  testthat::test_that("utils: check_identity - PASS - right identity", {
    testthat::skip_on_cran()


    testthat::expect_silent(check_identity(sample, "0"))
  })


  # CHECK AND SET REDUCTION

  testthat::test_that("utils: check_and_set_reduction - FAIL - no reductions", {
    testthat::skip_on_cran()


    test <- sample
    test@reductions[["pca"]] <- NULL
    test@reductions[["umap"]] <- NULL
    testthat::expect_error(check_and_set_reduction(sample = test, reduction = "umap"))
  })

  testthat::test_that("utils: check_and_set_reduction - FAIL - wrong reductions", {

    testthat::skip_on_cran()

    testthat::expect_error(check_and_set_reduction(sample = sample, reduction = "wrong_reduction"))
  })

  testthat::test_that("utils: check_and_set_reduction - PASS - null reduction, check that the output is the last computed reduction", {
    testthat::skip_on_cran()

    sample@reductions$ref.umap <- NULL
    sample@reductions$umap <- NULL
    output <- check_and_set_reduction(sample = sample)
    last_reduction <- names(sample@reductions)[length(names(sample@reductions))]
    testthat::expect_identical(output, last_reduction)
  })

  testthat::test_that("utils: check_and_set_reduction - PASS - provide a reduction", {
    testthat::skip_on_cran()

    sample@reductions$ref.umap <- NULL
    output <- check_and_set_reduction(sample = sample, reduction = "umap")
    reduction_check <- "umap"
    testthat::expect_identical(output, reduction_check)
  })

  testthat::test_that("utils: check_and_set_reduction - PASS - umap not in reductions", {
    testthat::skip_on_cran()


    sample@reductions$umap <- NULL
    sample@reductions$ref.umap <- NULL
    sample@reductions$diffusion <- NULL
    output <- check_and_set_reduction(sample = sample)
    reduction_check <- "pca"
    testthat::expect_identical(output, reduction_check)
  })


  # CHECK AND SET DIMENSIONS

  testthat::test_that("utils: check_and_set_dimensions - FAIL - dims not being a pair of values", {
    testthat::skip_on_cran()


    testthat::expect_error(check_and_set_dimensions(sample = sample, reduction = "umap", dims = "wrong_input"))
  })

  testthat::test_that("utils: check_and_set_dimensions - FAIL - dims not being a pair of integers", {
    testthat::skip_on_cran()


    testthat::expect_error(check_and_set_dimensions(sample = sample, reduction = "umap", dims = c(1, "wrong_input")))
  })

  testthat::test_that("utils: check_and_set_dimensions - FAIL - dims not being in the available list of dims", {
    testthat::skip_on_cran()


    testthat::expect_error(check_and_set_dimensions(sample = sample, reduction = "umap", dims = c(1, 20)))
  })

  testthat::test_that("utils: check_and_set_dimensions - FAIL - reduction only having 1 dim", {
    testthat::skip_on_cran()


    test <- sample
    obj <- Seurat::CreateDimReducObject(test@reductions$umap[[]][, "UMAP_1", drop = FALSE], key = "UMAP_", assay = "SCT")
    test@reductions$umap <- obj
    testthat::expect_error(check_and_set_dimensions(sample = test, reduction = "umap", dims = c(1, 2)))
  })

  testthat::test_that("utils: check_and_set_dimensions - PASS - NULL parameters", {
    testthat::skip_on_cran()


    output <- check_and_set_dimensions(sample = sample)
    testthat::expect_identical(output, c(1, 2))
  })

  testthat::test_that("utils: check_and_set_dimensions - PASS - NULL dimension but provided dims", {
    testthat::skip_on_cran()


    output <- check_and_set_dimensions(sample = sample, dims = c(2, 1))
    testthat::expect_identical(output, c(2, 1))
  })

  testthat::test_that("utils: check_and_set_dimensions - PASS - provided dimension and dims", {
    testthat::skip_on_cran()


    output <- check_and_set_dimensions(sample = sample, reduction = "pca", dims = c(20, 11))
    testthat::expect_identical(output, c(20, 11))
  })


  # CHECK AND SET ASSAY
  testthat::test_that("utils: check_and_set_assay - FAIL - wrong assay type", {
    testthat::skip_on_cran()


    testthat::expect_error(check_and_set_assay(sample = sample, assay = FALSE))
  })

  testthat::test_that("utils: check_and_set_assay - FAIL - no assays in object", {
    testthat::skip_on_cran()


    test <- sample
    test@assays$RNA <- NULL
    test@assays$SCT <- NULL
    testthat::expect_error(check_and_set_assay(sample = test))
  })

  testthat::test_that("utils: check_and_set_assay - FAIL - assay not present", {
    testthat::skip_on_cran()


    testthat::expect_error(check_and_set_assay(sample = sample, assay = "ATAC"))
  })

  testthat::test_that("utils: check_and_set_assay - PASS - null parameters", {
    testthat::skip_on_cran()


    output <- check_and_set_assay(sample = sample)
    testthat::expect_identical(output$assay, Seurat::DefaultAssay(sample))
  })

  testthat::test_that("utils: check_and_set_assay - PASS - providing assay", {
    testthat::skip_on_cran()


    output <- check_and_set_assay(sample = sample, assay = "SCT")
    testthat::expect_identical(output$assay, "SCT")
  })

  testthat::test_that("utils: check_and_set_assay - PASS - providing non defaultassay", {
    testthat::skip_on_cran()


    sample@assays$RNA <- sample@assays$SCT
    output <- check_and_set_assay(sample = sample, assay = "RNA")
    testthat::expect_identical(output$assay, "RNA")
  })


  # CHECK TYPE

  testthat::test_that("utils: check_type - FAIL - wrong type", {
    testthat::skip_on_cran()
    parameters <- c("first" = 1,
                    "second" = 2,
                    "third" = "a")
    testthat::expect_error(check_type(parameters = parameters, required_type = "numeric", test_function = is.numeric))
  })

  testthat::test_that("utils: check_type - PASS - numeric", {
    testthat::skip_on_cran()
    parameters <- c("first" = 1,
                    "second" = 2)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "numeric", test_function = is.numeric))
  })

  testthat::test_that("utils: check_type - PASS - numeric with NULL", {
    testthat::skip_on_cran()
    parameters <- c("first" = 1,
                    "second" = 2,
                    "third" = NULL)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "numeric", test_function = is.numeric))
  })

  testthat::test_that("utils: check_type - PASS - character", {
    testthat::skip_on_cran()
    parameters <- c("first" = "a",
                    "second" = "b")
    testthat::expect_silent(check_type(parameters = parameters, required_type = "character", test_function = is.character))
  })

  testthat::test_that("utils: check_type - PASS - character with NULL", {
    testthat::skip_on_cran()
    parameters <- c("first" = "a",
                    "second" = "b",
                    "third" = NULL)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "character", test_function = is.character))
  })

  testthat::test_that("utils: check_type - PASS - logical", {
    testthat::skip_on_cran()
    parameters <- c("first" = TRUE,
                    "second" = FALSE)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "logical", test_function = is.logical))
  })

  testthat::test_that("utils: check_type - PASS - logical with NULL", {
    testthat::skip_on_cran()
    parameters <- c("first" = TRUE,
                    "second" = FALSE,
                    "third" = NULL)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "logical", test_function = is.logical))
  })

  testthat::test_that("utils: check_type - PASS - list", {
    testthat::skip_on_cran()
    parameters <- c("first" = list(),
                    "second" = list())
    testthat::expect_silent(check_type(parameters = parameters, required_type = "list", test_function = is.list))
  })

  testthat::test_that("utils: check_type - PASS - list with NULL", {
    testthat::skip_on_cran()
    parameters <- c("first" = list(),
                    "second" = list(),
                    "third" = NULL)
    testthat::expect_silent(check_type(parameters = parameters, required_type = "list", test_function = is.list))
  })


  # CHECK AND SET THE SLOT

  testthat::test_that("utils: check_and_set_slot - FAIL - wrong slot", {
    testthat::skip_on_cran()
    testthat::expect_error(check_and_set_slot("wrong_slot"))
  })

  testthat::test_that("utils: check_and_set_slot - PASS - counts", {
    testthat::skip_on_cran()
    output <- check_and_set_slot("counts")
    testthat::expect_identical(output, "counts")
  })

  testthat::test_that("utils: check_and_set_slot - PASS - data", {

    output <- check_and_set_slot("data")
    testthat::expect_identical(output, "data")
  })

  testthat::test_that("utils: check_and_set_slot - PASS - scale.data", {
    testthat::skip_on_cran()
    output <- check_and_set_slot("scale.data")
    testthat::expect_identical(output, "scale.data")
  })


  # CHECK LIMITS
  testthat::test_that("utils: check_and_set_slot - FAIL - wrong limit", {
    testthat::skip_on_cran()


    testthat::expect_error(check_limits(sample = sample, feature = "EPC1", value_name = "scale.end", value = 30))
  })

  testthat::test_that("utils: check_and_set_slot - PASS - good limit", {
    testthat::skip_on_cran()


    testthat::expect_silent(check_limits(sample = sample, feature = "EPC1", value_name = "scale.end", value = 2))
  })


  # COMPUTE FACTOR LEVELS

  testthat::test_that("utils: compute_factor_levels - FAIL - wrong position", {
    testthat::skip_on_cran()


    testthat::expect_error(compute_factor_levels(sample = sample, feature = "seurat_clusters", position = "upper"))
  })

  testthat::test_that("utils: compute_factor_levels - PASS - order.by and group.by", {
    testthat::skip_on_cran()
    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "seurat_clusters",
                                                         position = "fill",
                                                         group.by = NULL), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "seurat_clusters",
                                                         position = "fill",
                                                         group.by = "orig.ident",
                                                         order = TRUE), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "seurat_clusters",
                                                         position = "fill",
                                                         group.by = "orig.ident"), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "EPC1",
                                                         position = "fill",
                                                         group.by = "orig.ident",
                                                         order = TRUE), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "EPC1",
                                                         position = "fill",
                                                         group.by = "orig.ident"), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "seurat_clusters",
                                                         position = "stack",
                                                         group.by = "orig.ident",
                                                         order = TRUE), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "seurat_clusters",
                                                         position = "stack",
                                                         group.by = "orig.ident"), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "EPC1",
                                                         position = "stack",
                                                         group.by = "orig.ident",
                                                         order = TRUE), "character")

    testthat::expect_type(compute_factor_levels(sample = sample,
                                                         feature = "EPC1",
                                                         position = "stack",
                                                         group.by = "orig.ident"), "character")
  })


  # CHECK LENGTH

  testthat::test_that("utils: check_length - FAIL - distinct length", {
    testthat::skip_on_cran()
    vector_parameters <- c(1, 2)
    vector_features <- 1
    parameters_name <- "A"
    features_name <- "B"
    testthat::expect_error(check_length(vector_of_parameters = vector_parameters,
                                                 vector_of_features = vector_features,
                                                 parameters_name = parameters_name,
                                                 features_name = features_name))
  })

  testthat::test_that("utils: check_length - PASS - correct length", {
    testthat::skip_on_cran()
    vector_parameters <- c(1, 2)
    vector_features <- c(1, 2)
    parameters_name <- "A"
    features_name <- "B"
    testthat::expect_silent(check_length(vector_of_parameters = vector_parameters,
                                                  vector_of_features = vector_features,
                                                  parameters_name = parameters_name,
                                                  features_name = features_name))
  })



  # ADD SCALE
  testthat::test_that("utils: add_scale - PASS - checks", {



    p <- SCpubr::do_FeaturePlot(sample, features = "EPC1")
    output <- add_scale(p = p, scale = "color", function_use = ggplot2::scale_color_viridis_b())
    testthat::expect_true("ggplot" %in% class(output))
    
    p <- SCpubr::do_FeaturePlot(sample, features = rownames(sample)[1:5])
    output <- add_scale(p = p, scale = "color", function_use = ggplot2::scale_color_viridis_b(), num_plots = 5)
    testthat::expect_true("ggplot" %in% class(output))
  })



  # MODIFY STRING

  testthat::test_that("utils: modify_string - PASS - checks", {
    testthat::skip_on_cran()
    output <- modify_string("This is a string to cut")
    testthat::expect_type(output, "character")
  })


  # COMPUTE ENRICHMENT SCORES

  testthat::test_that("utils: compute_enrichment_scores - PASS - checks", {
    testthat::skip_on_cran()


    output <- compute_enrichment_scores(sample = sample, input_gene_list = list("test" = "EPC1"), nbin = 1, ctrl = 10)
    testthat::expect_true("Seurat" %in% class(output))
    testthat::expect_true("test" %in% colnames(output@meta.data))

    output <- compute_enrichment_scores(sample = sample, input_gene_list = list("test" = "EPC1"), nbin = 1, ctrl = 10, flavor = "UCell")
    testthat::expect_true("Seurat" %in% class(output))
    testthat::expect_true("test" %in% colnames(output@meta.data))

    output <- compute_enrichment_scores(sample = sample, input_gene_list = list("test" = "EPC1"), nbin = 1, ctrl = 10, flavor = "AUCell")
    testthat::expect_true("Seurat" %in% class(output))
    testthat::expect_true("test" %in% colnames(output@meta.data))

    output <- compute_enrichment_scores(sample = sample, input_gene_list = list("test" = "EPC1"), verbose = TRUE, nbin = 1, ctrl = 10)
    testthat::expect_true("Seurat" %in% class(output))
    testthat::expect_true("test" %in% colnames(output@meta.data))

    output <- compute_enrichment_scores(sample = sample, input_gene_list = "EPC1", nbin = 1, ctrl = 10)
    testthat::expect_true("Seurat" %in% class(output))
    testthat::expect_true("Input" %in% colnames(output@meta.data))
  })

  # GET DATA COLUMN

  testthat::test_that("utils: get data column - PASS ", {
    testthat::skip_on_cran()


    data <- get_data_column(sample = sample, feature = "EPC1", assay = "SCT", slot = "data")
    testthat::expect_true("data.frame" %in% class(data))
    testthat::expect_true("feature" %in% colnames(data))

    data <- get_data_column(sample = sample, feature = "nCount_RNA", assay = "SCT", slot = "data")
    testthat::expect_true("data.frame" %in% class(data))
    testthat::expect_true("feature" %in% colnames(data))

    data <- get_data_column(sample = sample, feature = "PC_1", assay = "SCT", slot = "data")
    testthat::expect_true("data.frame" %in% class(data))
    testthat::expect_true("feature" %in% colnames(data))
  })

  # CHECK PARAMETERS
  testthat::test_that("utils: check parameters - FAIL ", {
    testthat::skip_on_cran()
    testthat::expect_error({check_parameters(parameter = -2, parameter_name = "viridis.direction")})
    testthat::expect_error({check_parameters(parameter = "ERROR", parameter_name = "viridis.palette")})
  })

  # GET AXIS PARAMETERS
  testthat::test_that("utils: check get_axis_parameters - PASS ", {
    testthat::skip_on_cran()
    out <- get_axis_parameters(angle = 0, flip = FALSE)
    testthat::expect_type(out, "list")

    out <- get_axis_parameters(angle = 0, flip = TRUE)
    testthat::expect_type(out, "list")

    out <- get_axis_parameters(angle = 45, flip = FALSE)
    testthat::expect_type(out, "list")

    out <- get_axis_parameters(angle = 45, flip = TRUE)
    testthat::expect_type(out, "list")

    out <- get_axis_parameters(angle = 90, flip = FALSE)
    testthat::expect_type(out, "list")

    out <- get_axis_parameters(angle = 90, flip = TRUE)
    testthat::expect_type(out, "list")
  })
}







