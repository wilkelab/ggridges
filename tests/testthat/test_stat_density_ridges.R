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

# data with two large groups (n = 30) and one small group (n = 2) in the middle
small_group_df <- function(nC = 2L) {
  data.frame(
    g = factor(rep(c("a", "b", "c"), times = c(30, nC, 30)), levels = c("a", "b", "c")),
    x = c(seq(-1, 1, length.out = 30), rep(0, nC), seq(2, 4, length.out = 30))
  )
}

test_that("groups smaller than min_obs are retained, not dropped", {
  df <- small_group_df(2L)

  # the small group (index 2) is present whether or not points are jittered,
  # and its presence does not depend on the jittered_points flag
  out_f <- suppressMessages(
    layer_data(ggplot(df, aes(x, g)) + geom_density_ridges(jittered_points = FALSE))
  )
  out_t <- suppressMessages(
    layer_data(ggplot(df, aes(x, g)) + geom_density_ridges(jittered_points = TRUE))
  )
  expect_setequal(unique(out_f$group), c(1, 2, 3))
  expect_setequal(unique(out_f$group), unique(out_t$group))

  # group indices are preserved (the small group keeps index 2, not renumbered)
  expect_true(2 %in% out_f$group)

  # the small group's density is omitted (NA placeholder, draws nothing)
  small_ridge <- out_f[out_f$group == 2 & out_f$datatype == "ridgeline", ]
  expect_true(all(is.na(small_ridge$density)))

  # but the large groups still get a real density
  big_ridge <- out_f[out_f$group == 1 & out_f$datatype == "ridgeline", ]
  expect_true(all(is.finite(big_ridge$density)))
})

test_that("raw points of small groups are drawn when jittered_points = TRUE", {
  df <- small_group_df(2L)
  out <- suppressMessages(
    layer_data(ggplot(df, aes(x, g)) + geom_density_ridges(jittered_points = TRUE))
  )
  pts <- out[out$datatype == "point", ]
  # every observation, including the two in the small group, is carried over
  expect_equal(nrow(pts), nrow(df))
  expect_equal(sum(pts$group == 2), 2)
})

test_that("small-group points have finite plotting coordinates (actually render)", {
  # regression guard: small-group points must end up with finite ymin/ymax after
  # the position adjustment, otherwise they are present in the data but invisible
  df <- small_group_df(2L)
  for (geom in list(geom_density_ridges, geom_density_ridges_gradient)) {
    out <- suppressMessages(
      ggplot_build(ggplot(df, aes(x, g)) + geom(jittered_points = TRUE))$data[[1]]
    )
    small_pts <- out[out$group == 2 & out$datatype == "point", ]
    expect_equal(nrow(small_pts), 2)
    expect_true(all(is.finite(small_pts$ymin)))
    expect_true(all(is.finite(small_pts$ymax)))
  }
})

test_that("panels with only small groups build and draw without warnings", {
  # every group too small for a density (degenerate but legal)
  df <- data.frame(x = c(0, 5, 10), g = factor(c("a", "b", "c")))
  for (geom in list(geom_density_ridges, geom_density_ridges_gradient)) {
    expect_no_warning(
      out <- suppressMessages(
        ggplot_build(ggplot(df, aes(x, g)) + geom(jittered_points = TRUE))$data[[1]]
      )
    )
    pts <- out[out$datatype == "point", ]
    expect_equal(nrow(pts), 3)
    expect_true(all(is.finite(pts$ymin)))
  }
})

test_that("min_obs controls the density threshold", {
  df <- small_group_df(2L)

  # default threshold is 3, so a 2-observation group gets no density
  out3 <- suppressMessages(layer_data(ggplot(df, aes(x, g)) + geom_density_ridges()))
  expect_true(all(is.na(out3$density[out3$group == 2])))

  # min_obs = 1 forces a density estimate for every group
  out1 <- suppressMessages(layer_data(ggplot(df, aes(x, g)) + geom_density_ridges(min_obs = 1)))
  small_ridge <- out1[out1$group == 2 & out1$datatype == "ridgeline", ]
  expect_true(any(is.finite(small_ridge$density)))
})

test_that("small groups work together with calc_ecdf and quantile_lines", {
  df <- small_group_df(2L)

  out_e <- suppressMessages(
    layer_data(ggplot(df, aes(x, g)) + stat_density_ridges(calc_ecdf = TRUE, jittered_points = TRUE))
  )
  expect_true(all(c("ecdf", "quantile") %in% names(out_e)))
  expect_true(2 %in% out_e$group)

  out_q <- suppressMessages(
    layer_data(ggplot(df, aes(x, g)) + stat_density_ridges(quantile_lines = TRUE, jittered_points = TRUE))
  )
  expect_true(2 %in% out_q$group)
  # quantile lines need a density, so the small group has none
  expect_false(any(out_q$group == 2 & out_q$datatype == "vline"))
})

test_that("small groups survive per-panel scaling under faceting", {
  # exercises the per-panel (vector hmax) branch of the geom's internal scaling
  # with a small group present in each facet
  df <- rbind(
    transform(small_group_df(2L), f = "p1"),
    transform(small_group_df(2L), f = "p2")
  )
  for (geom in list(geom_density_ridges, geom_density_ridges_gradient)) {
    expect_no_warning(
      out <- suppressMessages(
        ggplot_build(
          ggplot(df, aes(x, g)) + geom(jittered_points = TRUE) + facet_wrap(~f)
        )$data[[1]]
      )
    )
    # both panels keep the small group's two points, with finite coordinates
    small_pts <- out[out$group == 2 & out$datatype == "point", ]
    expect_equal(nrow(small_pts), 4)
    expect_true(all(is.finite(small_pts$ymin)))
  }
})

test_that("min_obs = 1 degrades gracefully when no bandwidth can be estimated", {
  # all groups are singletons: a joint bandwidth is not estimable, so even with
  # min_obs = 1 no density is computed. This must not error; points still draw.
  df <- data.frame(x = c(0, 5, 10), g = factor(c("a", "b", "c")))
  for (geom in list(geom_density_ridges, geom_density_ridges_gradient)) {
    expect_no_error(
      out <- suppressMessages(
        ggplot_build(ggplot(df, aes(x, g)) + geom(min_obs = 1, jittered_points = TRUE))$data[[1]]
      )
    )
    expect_true(all(is.na(out$density[out$datatype == "ridgeline"])))
    pts <- out[out$datatype == "point", ]
    expect_equal(nrow(pts), 3)
    expect_true(all(is.finite(pts$ymin)))
  }
})
