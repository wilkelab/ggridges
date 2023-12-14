test_that("utilities works not correct", {
  app_path=tempdir()
  scInit(app_path=app_path)
  expect_true(file.exists(file.path(app_path, 'doc.txt')))
  expect_true(file.exists(file.path(app_path, .globals$credential_path)))
  expect_true(file.exists(file.path(app_path, 'www', 'counter.tsv')))
  setwd(app_path)
  scRNAseqApp()
})
