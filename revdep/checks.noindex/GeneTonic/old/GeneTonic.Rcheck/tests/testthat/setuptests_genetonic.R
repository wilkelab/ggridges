library("GeneTonic")

message("--- Loading packages...")
suppressPackageStartupMessages({
  library("macrophage")
  library("DESeq2")
  library("org.Hs.eg.db")
  library("AnnotationDbi")
  library("clusterProfiler")
})
message("- Done!")

message("--- Generating objects for the testing setup...")

# dds --------------------------------------------------------------------------
data(gse)
dds_macrophage <- DESeqDataSet(gse, design = ~ line + condition)
rownames(dds_macrophage) <- substr(rownames(dds_macrophage), 1, 15)

# annotation -------------------------------------------------------------------
anno_df <- data.frame(
  gene_id = rownames(dds_macrophage),
  gene_name = mapIds(org.Hs.eg.db,
    keys = rownames(dds_macrophage),
    column = "SYMBOL",
    keytype = "ENSEMBL"
  ),
  stringsAsFactors = FALSE,
  row.names = rownames(dds_macrophage)
)
# alternatively, one could use the wrapper in ...
# anno_df <- pcaExplorer::get_annotation_orgdb(dds_macrophage, "org.Hs.eg.db", "ENSEMBL")

# res_de -----------------------------------------------------------------------
## using counts and average transcript lengths from tximeta
keep <- rowSums(counts(dds_macrophage) >= 10) >= 6
dds_macrophage <- dds_macrophage[keep, ]
dds_unnormalized <- dds_macrophage

dds_macrophage <- DESeq(dds_macrophage)
vst_macrophage <- vst(dds_macrophage)
res_macrophage_IFNg_vs_naive <- results(dds_macrophage,
  contrast = c("condition", "IFNg", "naive"),
  lfcThreshold = 1, alpha = 0.05
)
summary(res_macrophage_IFNg_vs_naive)
res_macrophage_IFNg_vs_naive$SYMBOL <- rowData(dds_macrophage)$SYMBOL

# res_enrich -------------------------------------------------------------------
de_symbols_IFNg_vs_naive <- res_macrophage_IFNg_vs_naive[(!(is.na(res_macrophage_IFNg_vs_naive$padj))) & (res_macrophage_IFNg_vs_naive$padj <= 0.05), "SYMBOL"]
bg_ids <- rowData(dds_macrophage)$SYMBOL[rowSums(counts(dds_macrophage)) > 0]

# library("topGO")
# topgoDE_macrophage_IFNg_vs_naive <-
#   pcaExplorer::topGOtable(de_symbols_IFNg_vs_naive,
#                           bg_ids,
#                           ontology = "BP",
#                           mapping = "org.Hs.eg.db",
#                           geneID = "symbol",
#                           topTablerows = 500)
# write.table(topgoDE_macrophage_IFNg_vs_naive,
#             "inst/extdata/topgotable_res_IFNg_vs_naive.txt",
#             sep = "\t")
topgoDE_macrophage_IFNg_vs_naive <-
  read.table(system.file("extdata", "topgotable_res_IFNg_vs_naive.txt", package = "GeneTonic"),
    stringsAsFactors = FALSE
  )
message("- Done!")

message("--- Running enrichGO...")
ego_IFNg_vs_naive <- enrichGO(
  gene = de_symbols_IFNg_vs_naive,
  universe = bg_ids,
  keyType = "SYMBOL",
  OrgDb = org.Hs.eg.db,
  ont = "BP",
  pAdjustMethod = "BH",
  pvalueCutoff = 0.01,
  qvalueCutoff = 0.05,
  readable = FALSE
)

message("--- Running gseGO...")
sorted_genes <- sort(
  setNames(res_macrophage_IFNg_vs_naive$log2FoldChange,
           res_macrophage_IFNg_vs_naive$SYMBOL),
  decreasing = TRUE
)

suppressWarnings({
  gsego_IFNg_vs_naive <- gseGO(
    geneList = sorted_genes,
    ont = "BP",
    OrgDb = org.Hs.eg.db,
    keyType = "SYMBOL",
    minGSSize = 10,
    maxGSSize = 500,
    pvalueCutoff = 0.05,
    verbose = TRUE
  )
})

# save(dds_macrophage, res_macrophage_IFNg_vs_naive, vst_macrophage, topgoDE_macrophage_IFNg_vs_naive, anno_df, ego_IFNg_vs_naive, file ="quick_startup.RData")

# load("/Users/fede/Development/GeneTonic/quick_startup.RData")
dds_unnormalized <- dds_macrophage
assays(dds_unnormalized)[["normalizationFactors"]] <- NULL
res_enrich_IFNg_vs_naive <- shake_topGOtableResult(topgoDE_macrophage_IFNg_vs_naive)[1:200, ]
message("- Done!")

# also creating the gtl container...
gtl_macrophage <- GeneTonic_list(
  dds = dds_macrophage,
  res_de = res_macrophage_IFNg_vs_naive,
  res_enrich = res_enrich_IFNg_vs_naive,
  annotation_obj = anno_df
)

message("--- Test setup script completed!")
