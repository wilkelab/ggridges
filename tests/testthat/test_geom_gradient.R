context("geom_*_gradient")


# Visual tests ------------------------------------------------------------
test_that("visual appearance of gradient geoms", {
  testthat::skip_on_cran()
  testthat::skip("skip for now, something's broken")

  df <- data.frame(x = c(1, 2, 3, 4, 5, 6),
                   height = c(1, 2, 3, 2, 1, 2),
                   type = c("A", "A", "B", "B", "C", "C"))
  p <- ggplot(df, aes(x, y = 0, height = height, group = 0, fill = type)) + geom_ridgeline_gradient()
  vdiffr::expect_doppelganger("geom_ridgeline_gradient basic fill pattern", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..x..)) + geom_density_ridges_gradient()
  vdiffr::expect_doppelganger("geom_density_ridges_gradient continuous fill", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..ecdf..)) +
    geom_density_ridges_gradient(calc_ecdf = TRUE)
  vdiffr::expect_doppelganger("geom_density_ridges_gradient ecdf fill", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..quantile..)) +
    geom_density_ridges_gradient(calc_ecdf = TRUE, quantiles = 5)
  vdiffr::expect_doppelganger("geom_density_ridges_gradient quintiles", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..quantile..)) +
    geom_density_ridges_gradient(calc_ecdf = TRUE, quantiles = c(0.05, 0.95))
  vdiffr::expect_doppelganger("geom_density_ridges_gradient probability tails", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..quantile..)) +
    geom_density_ridges_gradient(calc_ecdf = TRUE, quantiles = 5, quantile_lines = TRUE)
  vdiffr::expect_doppelganger("geom_density_ridges_gradient quantile lines match quantile shading", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species, fill = ..x..)) +
    geom_density_ridges_gradient(jittered_points = TRUE)
  vdiffr::expect_doppelganger("geom_density_ridges_gradient jittered points can be turned on", p)

})
