### New tests for PR #31
testthat::context("#31")

testthat::test_that("Splitting and not-splitting 'method' in MIsim leads to the same results", {
  data(MIsim, package = "rsimsum")
  data(MIsim2, package = "rsimsum")
  s1 <- simsum(data = MIsim, estvarname = "b", true = 0.5, se = "se", methodvar = "method")
  s2 <- simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = c("m1", "m2"))
  testthat::expect_identical(object = tidy(s1)$stat, expected = tidy(s2)$stat)
  testthat::expect_identical(object = tidy(s1)$est, expected = tidy(s2)$est)
  testthat::expect_identical(object = tidy(s1)$mcse, expected = tidy(s2)$mcse)
})

testthat::test_that("multisimsum with multiple columns as 'methodvar' yields same results", {
  data("frailty")
  s1 <- multisimsum(data = frailty, par = "par", estvarname = "b", methodvar = "model", by = "fv_dist")
  data("frailty2")
  s2 <- multisimsum(data = frailty2, par = "par", estvarname = "b", methodvar = c("m_baseline", "m_frailty"), by = "fv_dist")
  testthat::expect_equal(object = tidy(s2)$est, expected = tidy(s1)$est)
  testthat::expect_equal(object = tidy(s2)$mcse, expected = tidy(s1)$mcse)
})

testthat::test_that("New code prints ok", {
  data(MIsim2, package = "rsimsum")
  s1 <- simsum(data = MIsim2, estvarname = "b", true = 0.5, se = "se", methodvar = c("m1", "m2"))
  testthat::expect_output(print(s1))
  testthat::expect_output(print(summary(s1)))

  data("frailty2")
  s2 <- multisimsum(data = frailty2, par = "par", estvarname = "b", methodvar = c("m_baseline", "m_frailty"), by = "fv_dist")
  testthat::expect_output(print(s2))
  testthat::expect_output(print(summary(s2)))
})
