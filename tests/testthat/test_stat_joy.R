context("stat_joy")

test_that("no ecdf or quantiles by default", {
  df <- data.frame(x = rnorm(100))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_joy())

  expect_false("ecdf" %in% names(out))
  expect_false("quantile" %in% names(out))
})

test_that("calculation of ecdf and quantiles can be turned on", {
  df <- data.frame(x = rnorm(100))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_joy(calc_ecdf = TRUE, quantiles = 5))

  expect_true("ecdf" %in% names(out))
  expect_true("quantile" %in% names(out))
  expect_length(unique(out$quantile), 5)
})
