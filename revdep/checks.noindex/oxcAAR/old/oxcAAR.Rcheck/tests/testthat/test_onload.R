context("onload")

test_that("onload for oxcAAR worked", {
  oxcAAR:::.onLoad()
  expect_true("oxcAAR.oxcal_path" %in% names(options()))
})

