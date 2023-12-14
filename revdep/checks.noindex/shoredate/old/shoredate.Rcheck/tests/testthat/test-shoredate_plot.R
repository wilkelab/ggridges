test_that("skips the plotting of dates that are out of bounds and warns how many have been skipped", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)

  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R. This is therefore suppressed.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                 elevation = c(46, 100, 200)))
  warn <- expect_warning(shoredate_plot(target_dates))
  expect_equal(warn$message, "Skipped one date that was out of bounds.")
})

test_that("skips the plotting of dates that are out of bounds and warns how many have been skipped when multiplot = TRUE", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)

  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R. This is therefore suppressed.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                                  elevation = c(46, 200, 100)))
  warn <- expect_warning(shoredate_plot(target_dates, multiplot = TRUE))
  expect_equal(warn$message, "Skipped one date that was out of bounds.")
})

test_that("multiple isobases causes error with multiplot", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)
  target_dates <- shoreline_date(sites = target_points,
                                 elevation = c(46, 70, 100),
                                 isobase_direction = c(327, 333))
  err <- expect_error(shoredate_plot(target_dates, multiplot = TRUE))
  expect_equal(err$message, "The parameter setting multiplot = TRUE is not compatible with more than one isobase direction." )
})

test_that("dates omitted as out of bounds throws warning", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)
  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                 elevation = c(70, 46, 86, 200)))
  warn <- expect_warning(shoredate_plot(target_dates, multiplot = TRUE))
  expect_equal(warn$message, "Skipped one date that was out of bounds.")
})

# Create date for multiple tests below
target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
target_date <- shoreline_date(site = target_point, elevation = 70)

test_that("returns expected plot when a single date is passed", {
  skip_on_cran()
  p <- shoredate_plot(target_date)
  vdiffr::expect_doppelganger("plot with a single date", p)
})

test_that("returns expected plot when multiple directions are summed", {
  skip_on_cran()
  target_date <- shoreline_date(site = target_point,
                                elevation = 70,
                                isobase_direction = c(327, 333),
                                sum_isobase_directions = TRUE)
  p <- shoredate_plot(target_date, isobase_direction = TRUE)
  vdiffr::expect_doppelganger("plot with a summed directions", p)
})

test_that("returns expected plot when model = 'none'", {
  skip_on_cran()
  target_date <- shoreline_date(site = target_point,
                                elevation = 70,
                                model = "none")
  p <- shoredate_plot(target_date)
  vdiffr::expect_doppelganger("plot with no distance model", p)
})

test_that("returns expected plot without probability distribution of date", {
  skip_on_cran()
  p <- shoredate_plot(target_date, date_probability = FALSE)
  vdiffr::expect_doppelganger("plot without probability", p)
})

test_that("returns expected plot without HDR", {
  skip_on_cran()
  p <- shoredate_plot(target_date, highest_density_region = FALSE)
  vdiffr::expect_doppelganger("plot without HDR", p)
})

test_that("returns expected plot in greyscale", {
  skip_on_cran()
  p <- shoredate_plot(target_date, greyscale = TRUE)
  vdiffr::expect_doppelganger("plot in greyscale", p)
})

test_that("returns only site name, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, site_name = TRUE)
  vdiffr::expect_doppelganger("plot with name", p)
})

test_that("returns site name and model parameters, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, site_name = TRUE, parameters = TRUE)
  vdiffr::expect_doppelganger("plot with name & model parameters", p)
})

test_that("returns site name and isobase directions, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, site_name = TRUE, isobase_direction = TRUE)
  vdiffr::expect_doppelganger("plot with name & isobase directions", p)
})

test_that("returns model parameters and isobase directions, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, parameters = TRUE, isobase_direction = TRUE)
  vdiffr::expect_doppelganger("plot with model parameters & isobase directions", p)
})

test_that("returns only model parameters, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, parameters = TRUE)
  vdiffr::expect_doppelganger("plot with model parameters", p)
})

test_that("returns only isobase directions, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, isobase_direction = TRUE)
  vdiffr::expect_doppelganger("plot with isobase directions", p)
})

test_that("returns site name, model parameteres and isobase directions, when specified", {
  skip_on_cran()
  p <- shoredate_plot(target_date, site_name = TRUE,
                      parameters = TRUE, isobase_direction = TRUE)
  vdiffr::expect_doppelganger("plot with all elements of title", p)
})

test_that("returns expected multiplot when multiple dates are passed", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(572985, 6563115)))
  target_points <- sf::st_set_crs(target_points, 32632)
  target_dates <- shoreline_date(sites = target_points, elevation = c(46, 60))
  p <- shoredate_plot(target_dates, multiplot = TRUE)
  vdiffr::expect_doppelganger("plot with multiplot", p)
})

test_that("changing graphical parameters works", {
  skip_on_cran()
  p <- shoredate_plot(target_date,
                      date_col = NA,
                      date_fill = "gold",
                      displacement_col = "blue",
                      displacement_fill = "blue",
                      site_elevation_col = "darkgreen",
                      site_elevation_fill = "darkgreen",
                      hdr_col = "hotpink",
                      hdr_label_xadj = 0.5,
                      hdr_label_yadj = 0.5)
  vdiffr::expect_doppelganger("graphical parameters", p)
})
