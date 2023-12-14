test_that("returns expected plot when no parameters are passed", {
  skip_on_cran()
  p <- displacement_plot()
  vdiffr::expect_doppelganger("bare displacement plot", p)
})

test_that("returns expected plot when interpolated curve is passed", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(522623, 6526182)), crs = 32632)
  target_curve <- interpolate_curve(target_point)
  p <- displacement_plot(target_curve)
  vdiffr::expect_doppelganger("plot with interpolated curve", p)
})

test_that("returns expected plot with renamed interpolated curve", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(522623, 6526182)), crs = 32632)
  target_curve <- interpolate_curve(target_point)
  p <- displacement_plot(target_curve, target_name = "Example name")
  vdiffr::expect_doppelganger("plot renamed interpolated curve", p)
})

test_that("returns expected plot with greyscale = TRUE", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(522623, 6526182)), crs = 32632)
  target_curve <- interpolate_curve(target_point)
  p <- displacement_plot(target_curve, greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale plot", p)
})

test_that("returns expected plot with greyscale = TRUE and no passed curve", {
  skip_on_cran()
  p <- displacement_plot(greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale plot, no curve", p)
})

