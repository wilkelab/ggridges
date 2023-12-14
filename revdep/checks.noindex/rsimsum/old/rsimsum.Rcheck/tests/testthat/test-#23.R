### New tests for issue #23
testthat::context("#23")

testthat::test_that("expect error when autoplot 'vs' plots and no methods are defined", {
  data("tt", package = "rsimsum")
  s6 <- simsum(data = tt, estvarname = "diff", se = "se", true = -1, x = TRUE)

  testthat::expect_error(object = autoplot(s6, type = "est"))
  testthat::expect_error(object = autoplot(s6, type = "se"))
  testthat::expect_error(object = autoplot(s6, type = "est_ba"))
  testthat::expect_error(object = autoplot(s6, type = "se_ba"))
  testthat::expect_error(object = autoplot(s6, type = "est_density"))
  testthat::expect_error(object = autoplot(s6, type = "se_density"))
  testthat::expect_error(object = autoplot(s6, type = "est_hex"))
  testthat::expect_error(object = autoplot(s6, type = "se_hex"))
})

testthat::test_that("output from autoplot without methods is ok for other types of plot", {
  data("tt", package = "rsimsum")
  s6 <- simsum(data = tt, estvarname = "diff", se = "se", true = -1, x = TRUE)
  s7 <- simsum(data = tt, estvarname = "diff", se = "se", by = "dgm", true = -1, x = TRUE)

  testthat::expect_s3_class(object = autoplot(s6), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s6, type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(summary(s6)), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(summary(s6), type = "lolly"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s6, type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s6, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s6, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s6, type = "se_ridge"), class = c("gg", "ggplot"))

  testthat::expect_s3_class(object = autoplot(s7, type = "heat"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s7, type = "zip"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s7, type = "est_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s7, type = "se_ridge"), class = c("gg", "ggplot"))
  testthat::expect_s3_class(object = autoplot(s7, type = "nlp"), class = c("gg", "ggplot"))
})
