testthat::context("multisimsum")

testthat::test_that("multisimsum prints ok", {
  data("frailty")
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")))
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", by = "fv_dist")))
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model")))
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se")))
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = FALSE))))
  testthat::expect_output(print(multisimsum(data = frailty, par = "par", estvarname = "b", se = "se")))
})

testthat::test_that("multisimsum returns an object of class multisimsum", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_s3_class(ms, "multisimsum")
})

testthat::test_that("summ slot of a multisimsum object is a data.frame", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_s3_class(ms$summ, "data.frame")
})

testthat::test_that("not passing estvarname throws an error", {
  testthat::expect_error(
    {
      data("frailty")
      ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), se = "se", methodvar = "model", by = "fv_dist")
    },
    'argument "estvarname" is missing, with no default'
  )
})

testthat::test_that("specifying ref and not methodvar throws a warning", {
  testthat::expect_warning(
    {
      data("frailty")
      ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", ref = "Cox, Gamma", by = "fv_dist")
    },
    "'ref' method is specified while 'methodvar' is not: 'ref' will be ignored"
  )
})

testthat::test_that("specifying methodvar and not ref shows a message", {
  testthat::expect_message(
    {
      data("frailty")
      ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
    },
    "'ref' method was not specified, Cox, Gamma set as the reference"
  )
})

testthat::test_that("running multisimsum on frailty return summaries of the correct dimension", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_equal(dim(ms$summ), expected = c(224, 6))
})

testthat::test_that("multisimsum with mcse option returns mcse", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = TRUE))
  testthat::expect_true("mcse" %in% names(ms$summ))
})

testthat::test_that("multisimsum without mcse option does not returns mcse", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = FALSE))
  testthat::expect_false("mcse" %in% names(ms$summ))
})

testthat::test_that("multisimsum with by factors returns error when 'by' name is not a variable in data", {
  testthat::expect_error({
    data("frailty")
    ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "not_in_data")
  })
})

testthat::test_that("multisimsum with by factors returns a data.frame with results", {
  data("frailty")
  ms <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_s3_class(object = ms$summ, class = "data.frame")
})

testthat::test_that("multisimsum with x = FALSE does not return data", {
  data("frailty")
  s <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", x = FALSE)
  testthat::expect_null(object = s$x)
})

testthat::test_that("multisimsum with x = TRUE does return data", {
  data("frailty")
  s <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", x = TRUE)
  testthat::expect_s3_class(object = s$x, class = "data.frame")
  testthat::expect_equal(object = nrow(s$x), expected = nrow(frailty))
})

testthat::test_that("multisimsum adds ci.limits if requested", {
  data("frailty")
  s <- suppressWarnings(multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", ci.limits = c(-0.55, -0.45)))
  testthat::expect_equal(object = s$ci.limits, expected = c(-0.55, -0.45))
})

testthat::test_that("multisimsum argument checks", {
  data("frailty")
  testthat::expect_error(object = rsimsum::multisimsum(data = frailty, par = 1, true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
  testthat::expect_error(object = rsimsum::multisimsum(data = frailty, par = TRUE, true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
  testthat::expect_error(object = rsimsum::multisimsum(data = frailty, par = NULL, true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
  testthat::expect_error(object = rsimsum::multisimsum(data = frailty, par = "par", true = c(1, 1), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
  testthat::expect_error(object = rsimsum::multisimsum(data = frailty, par = "par", true = TRUE, estvarname = "b", se = "se", methodvar = "model", by = "fv_dist"))
})

testthat::test_that("multisimsum without 'true' does not compute bias, cover, mse", {
  data("frailty")
  s <- multisimsum(data = frailty, par = "par", estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_false(object = any(c("bias", "cover", "mse") %in% s$summ$stat))
})

testthat::test_that("multisimsum without 'se' does not compute se2mean, se2median, modelse, relerror, cover, becover, power", {
  data("frailty")
  s <- multisimsum(data = frailty, par = "par", estvarname = "b", true = c(trt = -0.50, fv = 0.75), methodvar = "model", by = "fv_dist")
  testthat::expect_false(object = any(c("se2mean", "se2median", "modelse", "relerror", "cover", "becover", "power") %in% s$summ$stat))
})

testthat::test_that("multisimsum without 'se' nor 'true' does not compute se2mean, se2median, modelse, relerror, cover, becover, power, bias, mse", {
  data("frailty")
  s <- multisimsum(data = frailty, par = "par", estvarname = "b", methodvar = "model", by = "fv_dist")
  testthat::expect_false(object = any(c("se2mean", "se2median", "modelse", "relerror", "cover", "becover", "power", "bias", "mse") %in% s$summ$stat))
})
