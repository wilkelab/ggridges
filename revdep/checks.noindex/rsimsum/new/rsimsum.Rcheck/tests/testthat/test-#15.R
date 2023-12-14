### New tests for issue #15
testthat::context("#15")

testthat::test_that("simsum with 'true' as a column returns the same results as using a scalar value if each row has the same value", {
  data(tt, package = "rsimsum")
  tt$true <- -1

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_identical(object = tidy(s1), expected = tidy(s2))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
  testthat::expect_identical(object = tidy(s1), expected = tidy(s2))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c(-1.5, -0.5), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c(-1.5, -0.5), methodvar = "method", by = "dgm")
  testthat::expect_identical(object = tidy(s1), expected = tidy(s2))
})

testthat::test_that("simsum with 'true' as a column returns different results compared to using a scalar value if each row has a different value", {
  data(tt, package = "rsimsum")
  tt$true <- stats::rnorm(n = nrow(tt), mean = -1, sd = 1)

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(tidy(s1), tidy(s2)))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(tidy(s1), tidy(s2)))

  s1 <- simsum(data = tt, estvarname = "diff", true = "true", se = "se", ci.limits = c(-1.5, -0.5), methodvar = "method", by = "dgm")
  s2 <- simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c(-1.5, -0.5), methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(tidy(s1), tidy(s2)))
})

testthat::test_that("multisimsum with 'true' as a column returns the same results as using a scalar value if each row has the same value", {
  data(tt, package = "rsimsum")
  tt$true <- -1
  tt <- dplyr::bind_rows(
    dplyr::mutate(tt, par = "diff1"),
    dplyr::mutate(tt, diff = diff + 1, true = true + 1, par = "diff2")
  )

  m1 <- multisimsum(data = tt, estvarname = "diff", par = "par", true = c(diff1 = -1, diff2 = 0), se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  m2 <- multisimsum(data = tt, estvarname = "diff", par = "par", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_identical(object = tidy(m1), expected = tidy(m2))
})

testthat::test_that("multisimsum with 'true' as a column returns different results compared to using a scalar value if each row has a different value", {
  data(tt, package = "rsimsum")
  tt$true <- stats::rnorm(n = tt, mean = -1, sd = 1)
  tt <- dplyr::bind_rows(
    dplyr::mutate(tt, par = "diff1"),
    dplyr::mutate(tt, diff = diff + 1, true = true + 1, par = "diff2")
  )

  m1 <- multisimsum(data = tt, estvarname = "diff", par = "par", true = c(diff1 = -1, diff2 = 0), se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  m2 <- multisimsum(data = tt, estvarname = "diff", par = "par", true = "true", se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  testthat::expect_false(object = identical(tidy(m1), tidy(m2)))
})
