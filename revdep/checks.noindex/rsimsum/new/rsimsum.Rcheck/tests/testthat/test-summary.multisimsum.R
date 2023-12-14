testthat::context("summary.multisimsum")

testthat::test_that("summarising a simsum object works fine and prints ok", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_output(print(summary(x)))
  testthat::expect_output(print(summary(x, ci_level = 0.99)))
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model")
  testthat::expect_output(print(summary(x)))
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", by = "fv_dist")
  testthat::expect_output(print(summary(x)))
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se")
  testthat::expect_output(print(summary(x)))
  testthat::expect_output(print(summary(x), mcse = TRUE))
  testthat::expect_output(print(summary(x), mcse = FALSE))
  testthat::expect_output(print(summary(x), mcse = NULL))
  testthat::expect_output(print(summary(x, stats = c("bias", "becover"))))
  testthat::expect_error(print(summary(x, stats = "wrong")))
  testthat::expect_error(print(summary(x), digits = -1))
})

testthat::test_that("summary.multisimsum returns an object of class summary.multisimsum", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  s <- summary(x)
  testthat::expect_s3_class(object = s, class = "summary.multisimsum")
})

testthat::test_that("summary.multisimsum returns confidence intervals when mcse = TRUE", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = TRUE))
  s <- summary(x)
  testthat::expect_true(object = all(c("lower", "upper") %in% names(s$summ)))
})

testthat::test_that("summary.multisimsum does not return confidence intervals when mcse = FALSE", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = FALSE))
  s <- summary(x)
  testthat::expect_true(object = all(!(c("lower", "upper") %in% names(s$summ))))
})

testthat::test_that("summary.multisimsum with wrong arguments throws an error", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_error(object = summary(x, ci_level = -1))
  testthat::expect_error(object = summary(x, ci_level = 2))
  testthat::expect_error(object = summary(x, ci_level = "0.05"))
})

testthat::test_that("summary.multisimsum with t distribution, results differ with default settings", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_false(object = all(tidy(summary(x))[["lower"]] == tidy(summary(x, df = 3))[["lower"]]))
  testthat::expect_false(object = all(tidy(summary(x, df = 3))[["lower"]] == tidy(summary(x, df = 10))[["lower"]]))
  testthat::expect_false(object = all(tidy(summary(x))[["upper"]] == tidy(summary(x, df = 3))[["upper"]]))
  testthat::expect_false(object = all(tidy(summary(x, df = 3))[["upper"]] == tidy(summary(x, df = 10))[["upper"]]))
})

testthat::test_that("summary.multisimsum returns selected stats only", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist")
  testthat::expect_true(object = all(tidy(summary(x, stats = "bias"))[["stat"]] == "bias"))
  testthat::expect_true(object = all(tidy(summary(x, stats = c("bias", "cover")))[["stat"]] %in% c("bias", "cover")))
})

testthat::test_that("print.summary.multisimsum complains if users ask for the moon", {
  data("frailty")
  x <- multisimsum(data = frailty, par = "par", true = c(trt = -0.50, fv = 0.75), estvarname = "b", se = "se", methodvar = "model", by = "fv_dist", control = list(mcse = FALSE))
  testthat::expect_message(object = print(summary(x, mcse = TRUE)))
  testthat::expect_message(object = print(summary(x, mcse = FALSE)))
  testthat::expect_message(object = print(summary(x, mcse = NULL)))
})
