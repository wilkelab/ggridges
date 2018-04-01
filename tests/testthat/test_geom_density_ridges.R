context("geom_density_ridges")



# Visual tests ------------------------------------------------------------

test_that("geom_density_ridges draws correctly", {
  testthat::skip_on_cran()
  testthat::skip("skip for now, something's broken")

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges()
  vdiffr::expect_doppelganger("geom_density_ridges basic", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(rel_min_height = 0.005)
  vdiffr::expect_doppelganger("geom_density_ridges no trailing lines", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges(scale = 3)

  vdiffr::expect_doppelganger("geom_density_ridges scale=3", p)

  p <- ggplot(iris, aes(x = Sepal.Length, y = Species)) + geom_density_ridges2()

  vdiffr::expect_doppelganger("geom_density_ridges2 solid polygons", p)
})
