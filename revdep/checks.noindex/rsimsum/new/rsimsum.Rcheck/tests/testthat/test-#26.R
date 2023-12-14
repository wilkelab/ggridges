### New tests for issue #23
testthat::context("#26")

testthat::test_that("argument checks for new option", {
  data("tt", package = "rsimsum")

  testthat::expect_s3_class(object = simsum(data = tt, estvarname = "diff", se = "se", df = "df", true = -1), class = "simsum")
  testthat::expect_error(object = simsum(data = tt, estvarname = "diff", se = "se", df = 42, true = -1))
  testthat::expect_error(object = simsum(data = tt, estvarname = "diff", se = "se", df = NA, true = -1))
  testthat::expect_error(object = simsum(data = tt, estvarname = "diff", se = "se", df = "NULL", true = -1))
})

testthat::test_that("expect equivalent values when passing lower, upper (based on df) and df", {
  data("tt", package = "rsimsum")

  s6 <- simsum(data = tt, estvarname = "diff", se = "se", df = "df", true = -1)
  s7 <- simsum(data = tt, estvarname = "diff", se = "se", ci.limits = c("lower", "upper"), true = -1)
  testthat::expect_equal(object = tidy(s6), expected = tidy(s7))

  s6 <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", df = "df", true = -1)
  s7 <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", ci.limits = c("lower", "upper"), true = -1)
  testthat::expect_equal(object = tidy(s6), expected = tidy(s7))

  s6 <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", df = "df", true = -1)
  s7 <- simsum(data = tt, estvarname = "diff", se = "se", methodvar = "method", by = "dgm", ci.limits = c("lower", "upper"), true = -1)
  testthat::expect_equal(object = tidy(s6), expected = tidy(s7))
})
