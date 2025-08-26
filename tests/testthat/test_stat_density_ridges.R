context("stat_density_ridges")

test_that("no ecdf or quantiles by default", {
  df <- data.frame(x = rnorm(20))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges())

  expect_false("ecdf" %in% names(out))
  expect_false("quantile" %in% names(out))
})

test_that("from and to arguments work", {
  df <- data.frame(x = rnorm(20))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(from = -2, to = 2))

  expect_equal(-2, min(out$x))
  expect_equal(2, max(out$x))
})

test_that("calculation of ecdf and quantiles can be turned on", {
  df <- data.frame(x = rnorm(20))
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(calc_ecdf = TRUE, quantiles = 5))

  expect_true("ecdf" %in% names(out))
  expect_true("quantile" %in% names(out))
  expect_length(unique(out$quantile), 5)

  # either calc_ecdf = TRUE or quantile_lines = TRUE switches on quantile lines
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(quantile_lines = TRUE, quantiles = 5))

  expect_true("ecdf" %in% names(out))
  expect_true("quantile" %in% names(out))
  expect_length(unique(out$quantile), 5)
})


test_that("jittered points and quantile lines can be turned on and off", {
  df <- data.frame(x = rnorm(20))

  # no point or vline data type by default
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges())
  expect_equal(unique(out$datatype), "ridgeline")

  # data points can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(jittered_points = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)

  # quantile lines can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline"))
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

  # quantile lines and data points can be turned on at once
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges(jittered_points = TRUE, quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

  ## now repeat everything with geom_density_ridges and geom_density_ridges_gradient

  # no points or vlines data type by default
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges())
  expect_equal(unique(out$datatype), "ridgeline")

  # data points can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges(jittered_points = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)

  # quantile lines can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges(quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline"))
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

  # quantile lines and data points can be turned on at once
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges_gradient(jittered_points = TRUE, quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

  # no points or vlines data type by default
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges_gradient())
  expect_equal(unique(out$datatype), "ridgeline")

  # data points can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges_gradient(jittered_points = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)

  # quantile lines can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges_gradient(quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline"))
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

  # quantile lines and data points can be turned on at once
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) + geom_density_ridges_gradient(jittered_points = TRUE, quantile_lines = TRUE))
  expect_setequal(out$datatype, c("ridgeline", "vline", "point"))
  expect_equal(out$x[out$datatype=="point"], df$x)
  expect_equal(out$x[out$datatype=="vline"], unname(quantile(df$x)[2:4]))

})


test_that("alternative quantile function can be provided", {
  df <- data.frame(x = rnorm(20))

  # quantile lines can be turned on
  out <- layer_data(ggplot(df, aes(x = x, y = 0)) +
                      geom_density_ridges(quantile_lines = TRUE, quantile_fun = mean))
  expect_setequal(out$datatype, c("ridgeline", "vline"))
  expect_equal(out$x[out$datatype=="vline"], mean(df$x))
})

test_that("unweighted densities are calculated correctly", {
  df <- data.frame(x = rnorm(100), wts = runif(100))
  df$wts <- df$wts / sum(df$wts)

  gg_no_wts <- layer_data(ggplot(df, aes(x = x, y = 0)) + stat_density_ridges())
  d_no_wts <- stats::density(df$x)

  expect_equal(gg_no_wts$density, d_no_wts$y)
})

test_that("weighted densities are calculated correctly", {
  df <- data.frame(x = rnorm(100), wts = runif(100))
  df$wts <- df$wts / sum(df$wts)

  gg_wts <- layer_data(ggplot(df, aes(x = x, y = 0, weight = wts)) + stat_density_ridges(bandwidth = 0.3))
  d_wts <- stats::density(df$x, weights = df$wts, bw = 0.3)

  expect_equal(gg_wts$density, d_wts$y)
})
