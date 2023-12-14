### New tests for PR #33
testthat::context("#33")

testthat::test_that("Test that power_df is actually used", {
  data(MIsim, package = "rsimsum")
  s1 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(power_df = NULL))
  s2 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(power_df = 3))
  testthat::expect_false(object = identical(x = tidy(s1)$est[tidy(s1)$stat == "power"], y = tidy(s2)$est[tidy(s2)$stat == "power"]))

  s3 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  testthat::expect_false(object = identical(x = tidy(s3)$est[tidy(s3)$stat == "power"], y = tidy(s2)$est[tidy(s2)$stat == "power"]))
  testthat::expect_identical(object = tidy(s1)$est[tidy(s1)$stat == "power"], expected = tidy(s3)$est[tidy(s3)$stat == "power"])

  s4 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method", control = list(power_df = 1000))
  testthat::expect_identical(object = tidy(s1)$est[tidy(s1)$stat == "power"], expected = tidy(s4)$est[tidy(s4)$stat == "power"])
})
