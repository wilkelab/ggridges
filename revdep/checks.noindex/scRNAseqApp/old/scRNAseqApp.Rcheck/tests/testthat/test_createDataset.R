test_that("createDataset works not correct", {
  appconf <- createAppConfig(
    title="pbmc_small",
    destinationFolder = "pbmc_small",
    species = "Homo sapiens",
    doi="10.1038/nbt.3192",
    datatype = "scRNAseq",
    keywords = c("pbmc", "testdata"))
  tmp <- tempdir()
  createDataSet(appconf, pbmc_small, datafolder=tmp)
  p <- system.file("extdata", "data", "pbmc_small",
                   package = "scRNAseqApp")
  for(f in dir(p)){
    expect_true(file.exists(file.path(tmp, 'pbmc_small', f)))
  }
})
