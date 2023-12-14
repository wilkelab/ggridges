### New tests for issue #23
testthat::context("#28")

testthat::test_that("regex check new output options of multisimsum", {
  data("frailty", package = "rsimsum")
  frailty$true <- ifelse(frailty$par == "trt", -0.50, 0.75)
  ms0 <- multisimsum(data = frailty, par = "par", estvarname = "b", true = c(trt = -0.50, fv = 0.75))
  ms1 <- multisimsum(data = frailty, par = "par", estvarname = "b", true = "true")
  ms2 <- multisimsum(data = frailty, par = "par", estvarname = "b", true = 1)

  testthat::expect_output(object = print(ms0), regexp = "True values: fv = -0.5, trt = 0.75")
  testthat::expect_output(object = print(ms1), regexp = "True values from column 'true'")
  testthat::expect_output(object = print(ms2), regexp = "True values fixed at value 1 ")
})

testthat::test_that("regex check new output options of simsum", {
  data("MIsim")
  MIsim$true <- 0.5
  x0 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  x1 <- simsum(data = MIsim, estvarname = "b", true = "true", se = "se", methodvar = "method")

  testthat::expect_output(object = print(x0), regexp = "True value of the estimand: 0.5")
  testthat::expect_output(object = print(x1), regexp = "True value of the estimand from column 'true'")
})
