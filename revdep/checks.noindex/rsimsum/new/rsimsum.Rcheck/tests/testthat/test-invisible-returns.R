test_that("Invisible returns of expected size for the single-parameter case", {
  data("relhaz", package = "rsimsum")
  s2 <- simsum(data = relhaz, estvarname = "theta", true = -0.50, se = "se", methodvar = "model", by = c("baseline", "n"))
  out <- print(summary(s2, stats = "bias"))
  testthat::expect_equal(object = length(out), expected = 1)
  out <- print(summary(s2, stats = c("bias", "cover")))
  testthat::expect_equal(object = length(out), expected = 2)
  out <- print(summary(s2, stats = c("bias", "cover", "mse")))
  testthat::expect_equal(object = length(out), expected = 3)
})

test_that("Invisible returns of expected size for the multiple-parameter case", {
  data("frailty", package = "rsimsum")
  ms <- multisimsum(
    data = frailty, par = "par", true = c(
      trt = -0.50,
      fv = 0.75
    ), estvarname = "b", se = "se", methodvar = "model",
    by = "fv_dist"
  )
  out <- print(summary(ms, stats = "bias"))
  testthat::expect_equal(object = length(out), expected = length(unique(frailty$par)))
  for (i in seq_along(out)) {
    testthat::expect_equal(object = length(out[[i]]), expected = 1)
  }
  out <- print(summary(ms, stats = c("bias", "cover")))
  testthat::expect_equal(object = length(out), expected = length(unique(frailty$par)))
  for (i in seq_along(out)) {
    testthat::expect_equal(object = length(out[[i]]), expected = 2)
  }
  out <- print(summary(ms, stats = c("bias", "cover", "mse")))
  testthat::expect_equal(object = length(out), expected = length(unique(frailty$par)))
  for (i in seq_along(out)) {
    testthat::expect_equal(object = length(out[[i]]), expected = 3)
  }
})
