testthat::context("data")

testthat::test_that("loading MIsim works", {
  data("MIsim", package = "rsimsum")
  testthat::expect_s3_class(object = MIsim, class = "data.frame")
})

testthat::test_that("loading MIsim2 works", {
  data("MIsim2", package = "rsimsum")
  testthat::expect_s3_class(object = MIsim2, class = "data.frame")
})

testthat::test_that("MIsim and MIsim2 are the same (if we forget about the methods stuff)", {
  data("MIsim", package = "rsimsum")
  attr(x = MIsim, which = "label") <- NULL
  data("MIsim2", package = "rsimsum")
  testthat::expect_equal(object = nrow(MIsim), expected = nrow(MIsim2))
  .columns <- c("dataset", "b", "se")
  MIsim_small <- MIsim[, .columns]
  MIsim2_small <- MIsim2[, .columns]
  testthat::expect_equal(object = MIsim_small, expected = MIsim2_small)
})

testthat::test_that("loading relhaz works", {
  data("relhaz", package = "rsimsum")
  testthat::expect_s3_class(object = relhaz, class = "data.frame")
})

testthat::test_that("loading frailty works", {
  data("frailty", package = "rsimsum")
  testthat::expect_s3_class(object = frailty, class = "data.frame")
})

testthat::test_that("loading frailty2 works", {
  data("frailty2", package = "rsimsum")
  testthat::expect_s3_class(object = frailty2, class = "data.frame")
})

testthat::test_that("frailty and frailty2 are the same (if we forget about the model stuff)", {
  data("frailty", package = "rsimsum")
  data("frailty2", package = "rsimsum")
  testthat::expect_equal(object = nrow(frailty), expected = nrow(frailty2))
  .columns <- c("i", "b", "se", "par", "fv_dist")
  frailty_small <- frailty[, .columns]
  frailty2_small <- frailty2[, .columns]
  testthat::expect_equal(object = frailty_small, expected = frailty2_small)
})

testthat::test_that("loading nlp works", {
  data("nlp", package = "rsimsum")
  testthat::expect_s3_class(object = nlp, class = "data.frame")
})

testthat::test_that("loading tt works", {
  data("tt", package = "rsimsum")
  testthat::expect_s3_class(object = tt, class = "data.frame")
  testthat::expect_equal(object = dim(tt), expected = c(4000, 8))
})
