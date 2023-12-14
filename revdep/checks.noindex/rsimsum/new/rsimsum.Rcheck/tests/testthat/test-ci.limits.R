testthat::context("ci.limits")
data(tt, package = "rsimsum")

testthat::test_that("ci.limits breaks if columns not in data", {
  testthat::expect_error(rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower.missing", "upper.missing"), methodvar = "method", by = "dgm"))
})

testthat::test_that("ci.limits breaks if logic vector", {
  testthat::expect_error(rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c(TRUE, FALSE), methodvar = "method", by = "dgm"))
})

testthat::test_that("ci.limits works ok with string or numeric vectors", {
  testthat::expect_s3_class(object = rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm"), class = "simsum")
  testthat::expect_s3_class(object = rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c(-2, 0), methodvar = "method", by = "dgm"), class = "simsum")
})

testthat::test_that("ci.limits with string vector yields different values of coverage than the default", {
  s <- rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", ci.limits = c("lower", "upper"), methodvar = "method", by = "dgm")
  s <- tidy(s, stats = "cover")
  sdef <- rsimsum::simsum(data = tt, estvarname = "diff", true = -1, se = "se", methodvar = "method", by = "dgm")
  sdef <- tidy(sdef, stats = "cover")

  testthat::expect_false(object = all(s == sdef))
})
