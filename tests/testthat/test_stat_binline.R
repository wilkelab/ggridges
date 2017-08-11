context("stat_binline")

test_that("binning works", {
  df <- data.frame(x = sample(c(rep(1, 10), rep(2, 5), rep(3, 2), rep(4, 1))))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_binline(binwidth = 1))

  expect_equal(out$count, c(0, 0, 10, 10, 5, 5, 2, 2, 1, 1, 0, 0))

  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_binline(binwidth = 1, pad = FALSE))
  expect_equal(out$count, c(10, 10, 5, 5, 2, 2, 1, 1))
})
