de_genes <- readRDS(system.file("extdata/de_genes_example.rds", package = "SCpubr"))
# nolint start
if (requireNamespace("Seurat", quietly = TRUE)) {
 suppressMessages(library("Seurat"))
}

if (requireNamespace("magrittr", quietly = TRUE)) {
 suppressMessages(library("magrittr"))
}

if (requireNamespace("dplyr", quietly = TRUE)) {
 suppressMessages(library("dplyr"))
  de_genes_scaled <- dplyr::rename(.data = de_genes,
                                   "avg_diff" = "avg_log2FC")
}
# nolint end

sample <- readRDS(system.file("extdata/seurat_dataset_example.rds", package = "SCpubr"))

if (isTRUE(getOption("SCpubr.v5"))){
  suppressWarnings(sample[["SCT"]] <- as(object = sample[["SCT"]], Class = "Assay5"))
}

metacell_mapping <- readRDS(system.file("extdata/metacell_mapping_example.rds", package = "SCpubr"))
infercnv_object <- readRDS(system.file("extdata/infercnv_object_example.rds", package = "SCpubr"))
infercnv_object_metacells <- readRDS(system.file("extdata/infercnv_object_metacells_example.rds", package = "SCpubr"))
human_chr_locations <- SCpubr::human_chr_locations
progeny_activities <- readRDS(system.file("extdata/progeny_activities_example.rds", package = "SCpubr"))
dorothea_activities <- readRDS(system.file("extdata/dorothea_activities_example.rds", package = "SCpubr"))
enriched_terms <- readRDS(system.file("extdata/enriched_terms_example.rds", package = "SCpubr"))


# Get packages.
dependencies <- SCpubr:::return_dependencies()

dependencies[["utils"]] <- c("Seurat",
                             "rlang",
                             "dplyr",
                             "magrittr",
                             "dplyr",
                             "tidyr",
                             "tibble",
                             "stringr",
                             "plyr",
                             "grDevices",
                             "stats",
                             "grid",
                             "assertthat",
                             "ComplexHeatmap")

# Check them.
dep_check <- list()
for (func in names(dependencies)){
  packages <- c(dependencies[[func]], dependencies[["Essentials"]])
  value <- FALSE
  for (pkg in packages){
    if (!requireNamespace(pkg, quietly = TRUE)) {
        value <- TRUE
    }
  }
  dep_check[[func]] <- value
}

# nolint start
if (base::isFALSE(dep_check[["do_GroupedGOTermPlot"]]) | base::isFALSE(dep_check[["do_FunctionalAnnotationPlot"]])){
  if (requireNamespace("org.Hs.eg.db", quietly = TRUE)) {
    suppressMessages(library("org.Hs.eg.db"))
  }

  if (requireNamespace("AnnotationDbi", quietly = TRUE)) {
    suppressMessages(library("AnnotationDbi"))
    org.db <- AnnotationDbi::loadDb(system.file("./extdata/org.Hs.eg.sqlite", package = "org.Hs.eg.db"))
  }
}
# nolint end

# Remove this for publication in CRAN.
# if (base::isFALSE(dep_check[["do_LigandReceptorPlot"]])){
#   liana_output <- readRDS(system.file("extdata/liana_output_example.rds", package = "SCpubr"))
# }
# 
# if (base::isFALSE(dep_check[["do_DimPlot"]]) &
#     base::isFALSE(dep_check[["do_CorrelationPlot"]]) &
#     base::isFALSE(dep_check[["do_ChordDiagramPlot"]]) &
#     isTRUE(requireNamespace(pkg, quietly = TRUE)) &
#     base::isFALSE(dep_check[["save_Plot"]])){
#   p <- SCpubr::do_DimPlot(sample)
#   data <- data.frame("A" = stats::runif(n = 10),
#                      "B" = stats::runif(n = 10),
#                      "C" = stats::runif(n = 10),
#                      "D" = stats::runif(n = 10))
#   data <- as.matrix(data)
#   p.pheatmap <- pheatmap::pheatmap(data, cluster_rows = FALSE, cluster_cols = FALSE)
#   p.heatmap <- ComplexHeatmap::Heatmap(data, cluster_rows = FALSE, cluster_columns = FALSE)
#   p.chord <- SCpubr::do_ChordDiagramPlot(sample = sample, from = "seurat_clusters", to = "orig.ident")
#   figure_path <- getwd()
# }


#monocle_sample <- sample
#monocle_cds <- test.data$monocle_cds
