testthat::context("tidy")

testthat::test_that("tidy returns a data.frame", {
  data("MIsim", package = "rsimsum")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_s3_class(object = tidy(x), class = "data.frame")
  xs <- summary(x)
  testthat::expect_s3_class(object = tidy(xs), class = "data.frame")
  data("frailty", package = "rsimsum")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_s3_class(object = tidy(ms), class = "data.frame")
  sms <- summary(ms)
  testthat::expect_s3_class(object = tidy(sms), class = "data.frame")
})

testthat::test_that("tidy with 'stats' results only 'stats'", {
  data("MIsim", package = "rsimsum")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  out <- rsimsum::tidy(x, stats = "bias")
  testthat::expect_true(object = all(out$stat == "bias"))
  out <- rsimsum::tidy(x, stats = c("bias", "cover"))
  testthat::expect_true(object = all(out$stat %in% c("bias", "cover")))
})

testthat::test_that("tidy with wrong 'stats' throws an error", {
  data("MIsim", package = "rsimsum")
  x <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_error(object = rsimsum::tidy(x, stats = "42"))
  testthat::expect_error(object = rsimsum::tidy(x, stats = TRUE))
  testthat::expect_error(object = rsimsum::tidy(x, stats = 42))
})
