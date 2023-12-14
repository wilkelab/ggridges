target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                            sf::st_point(c(538300, 6544250)))
target_points <- sf::st_as_sf(target_points, crs = 32632)

target_dates <- shoreline_date(target_points,
                               elevation = c(65, 70),
                               cal_reso = 100)

test_that("expected sumplot is produced", {
  skip_on_cran()
  target_sum <- sum_shoredates(target_dates)
  p <- shoredate_sumplot(target_sum)
  vdiffr::expect_doppelganger("Plot of sum of multiple dates", p)
})

test_that("passing a shoreline_date object and not shoredates_sum throws error", {
  skip_on_cran()
  err <- expect_error(shoredate_sumplot(target_dates))
  expect_equal(err$message, "Sum to be plotted must be of class shoredates_sum, as returned from sum_shoredates()")
})
