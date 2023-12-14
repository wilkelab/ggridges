context("Testing extra functions/helpers for GeneTonic")

test_that("Creating and describing gtl", {
  gtl_macrophage <- GeneTonicList(
    dds = dds_macrophage,
    res_de = res_macrophage_IFNg_vs_naive,
    res_enrich = res_enrich_IFNg_vs_naive,
    annotation_obj = anno_df
  )
  expect_is(gtl_macrophage, "list")

  expect_message({
    GeneTonicList(
      dds = dds_macrophage,
      res_de = res_macrophage_IFNg_vs_naive,
      res_enrich = res_enrich_IFNg_vs_naive,
      annotation_obj = anno_df
    )
  })
})


test_that("Overlap functions work", {
  set1 <- letters[1:10]
  set2 <- letters[1:15]
  set3 <- letters[5:20]
  ol_1_2 <- overlap_coefficient(set1, set2)
  ol_1_2_ji <- overlap_jaccard_index(set1, set2)

  expect_equal(ol_1_2, 1)
  expect_equal(ol_1_2_ji, 2 / 3)
})

test_that("JS code for DT is generated", {
  simplest_df <- data.frame(
    a = c(rep("a", 9)),
    value = c(-4, -3, -2, -1, 0, 1, 2, 3, 4)
  )

  bg_jscode <- styleColorBar_divergent(
    simplest_df$value,
    scales::alpha("forestgreen", 0.4),
    scales::alpha("gold", 0.4)
  )

  expect_is(bg_jscode, "JS_EVAL")
})

test_that("map2color works", {
  mypal <- rev(scales::alpha(
    colorRampPalette(RColorBrewer::brewer.pal(name = "RdYlBu", 11))(50), 0.4
  ))
  my_vals <- res_macrophage_IFNg_vs_naive$log2FoldChange[1:20]
  m2c <- map2color(x = my_vals, pal = mypal, limits = c(-4, 4))
  m2c_nolimits <- map2color(x = my_vals, pal = mypal)
  # plot(1:20, col = m2c, pch = 20, cex = 5)

  expect_length(m2c, 20)
  expect_length(m2c_nolimits, 20)
})

test_that("color check works", {
  mypal <- c("steelblue", "#FF1100")
  expect_true(all(check_colors(mypal)))

  mypal2 <- rev(
    scales::alpha(
      colorRampPalette(RColorBrewer::brewer.pal(name = "RdYlBu", 11))(50), 0.4
    )
  )
  expect_true(all(check_colors(mypal2)))

  my_non_pal <- c("green", "gren", "hmm", "whoops")
  check_colors(my_non_pal)
  expect_true(!all(check_colors(my_non_pal)))
})

test_that("results to data frame conversion works", {
  res_df <- deseqresult2df(res_macrophage_IFNg_vs_naive, FDR = 1)
  res_df2 <- deseqresult2df(res_macrophage_IFNg_vs_naive, FDR = 0.05)
  res_df3 <- deseqresult2df(res_macrophage_IFNg_vs_naive)
  expect_is(res_df, "data.frame")
  expect_error(deseqresult2df(res_df))
})

test_that("Exporting to sif format works", {
  library("igraph")
  g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
  g <- add_edges(g, c(1, 6, 1, 11, 6, 11))
  siffile <- export_to_sif(g, tempfile())

  expect_is(siffile, "character")

  expect_error(export_to_sif(V(g)))
  expect_error(export_to_sif(g, 1))
  expect_error(export_to_sif(g, sif_file = c(tempfile(), "there_sif.txt")))
})

test_that("Retrieving info on GO term", {
  out <- go_2_html("GO:0032729")
  expect_is(out, "character")
  expect_is(out, "html")
  expect_equal(go_2_html("GO:00"), HTML("Gene Ontology term not found!"))

  res_enrich <- get_aggrscores(res_enrich_IFNg_vs_naive, res_de = res_macrophage_IFNg_vs_naive, annotation_obj = anno_df)
  out2 <- go_2_html("GO:0032729", res_enrich = res_enrich)
  expect_is(out2, "character")
  expect_is(out2, "html")
})

test_that("Retrieving info on gene", {
  out <- geneinfo_2_html("Xist")
  expect_is(out, "character")
  expect_is(out, "html")

  # using a gene name present in the res_de
  out2 <- geneinfo_2_html("IRF1", res_de = res_macrophage_IFNg_vs_naive)
  expect_is(out2, "character")
  expect_is(out2, "html")

  # using a gene name which is not in the res_de
  expect_message(
    out3 <- geneinfo_2_html("Irf1", res_de = res_macrophage_IFNg_vs_naive)
  )
  expect_is(out3, "character")
  expect_is(out3, "html")
  expect_true(grepl("not found", out3))
})

test_that("Linking to AmiGO database", {
  out <- .link2amigo("GO:0032729")
  expect_is(out, "character")
})

test_that("Linking to NCBI database", {
  out <- .link2ncbi("Actb")
  expect_is(out, "character")
})

test_that("Linking to GeneCards database", {
  out <- .link2genecards("Gapdh")
  expect_is(out, "character")
})

test_that("Button correctly created", {
  out <- gt_downloadButton(
    "start_happyhour",
    "Start the happy hour!",
    class = "biocdlbutton",
    icon = "cocktail"
  )
  expect_is(out, "shiny.tag")
})

test_that("Content from the editor is converted to vector", {
  in_editor <- "a\nb  \nc\nd"
  out_vector <- editor_to_vector_sanitized(in_editor)
  expect_equal(out_vector, c("a", "b", "c", "d"))
})
