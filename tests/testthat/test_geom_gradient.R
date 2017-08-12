context("geom_*_gradient")


# Visual tests ------------------------------------------------------------
test_that("visual appearance", {
  df <- data.frame(x = c(1, 2, 3, 4, 5, 6),
                   height = c(1, 2, 3, 2, 1, 2),
                   type = c("A", "A", "B", "B", "C", "C"))
  p <- ggplot(df, aes(x, y = 0, height = height, group = 0, fill = type)) + geom_ridgeline_gradient()
  vdiffr::expect_doppelganger("geom_ridgeline_gradient basic fill pattern", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..x..)) + geom_joy_gradient()
  vdiffr::expect_doppelganger("geom_joy_gradient continuous fill", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..ecdf..)) +
    geom_joy_gradient(calc_ecdf = TRUE)
  vdiffr::expect_doppelganger("geom_joy_gradient ecdf fill", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..quantile..)) +
    geom_joy_gradient(calc_ecdf = TRUE, quantiles = 5)
  vdiffr::expect_doppelganger("geom_joy_gradient quintiles", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..quantile..)) +
    geom_joy_gradient(calc_ecdf = TRUE, quantiles = c(0.05, 0.95))
  vdiffr::expect_doppelganger("geom_joy_gradient probability tails", p)
})
