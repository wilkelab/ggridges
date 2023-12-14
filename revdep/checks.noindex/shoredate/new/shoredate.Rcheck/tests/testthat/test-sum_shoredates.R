target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                            sf::st_point(c(538300, 6544250)))
target_points <- sf::st_as_sf(target_points, crs = 32632)


test_that("cut-off level outside range between 0 and 1 throws error", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62))
  err <- expect_error(sum_shoredates(target_dates, cut_off_level = 2))
  expect_equal(err$message,
            "Probability level for cut-off should be a value between 0 and 1.")
})

test_that("setting normalised = FALSE returns expected result", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62))
  expect_snapshot(sum_shoredates(target_dates,
                                 normalise = FALSE))
})

test_that("summing sparse dates", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62), sparse = TRUE)
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 2)
})

test_that("summing dates with multiple isobase directions", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62),
                                 isobase_direction = c(327, 338))
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 4)
})

test_that("summing sparse dates with multiple isobase directions", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points,
                                 elevation = c(70, 62),
                                 isobase_direction = c(327, 338),
                                 sparse = TRUE)
  target_sum <- sum_shoredates(target_dates)
  expect_equal(target_sum$dates_n, 4)
})

test_that("date with more than 50% prob mass above 2500 BCE is excluded", {
  skip_on_cran()
  target_dates <- shoreline_date(target_points, elevation = c(19, 62))
  target_sum <- sum_shoredates(target_dates, cut_off_level = 0.5)
  expect_equal(target_sum$dates_n, 1)
})


test_that("handles one or more NA dates", {
  skip_on_cran()
  target_points <- sf::st_sfc(sf::st_point(c(538310, 6544255)),
                              sf::st_point(c(538300, 6544250)),
                              sf::st_point(c(538250, 6544250)))
  target_points <- sf::st_set_crs(target_points, 32632)

  # shoreline_date() gives an expected warning here, which is already tested in
  # test-shoreline_date.R. This is therefore suppressed.
  target_dates <- suppressWarnings(shoreline_date(sites = target_points,
                                                  elevation = c(46, 200, 100),
                                                  sparse = TRUE))
  target_sum <- sum_shoredates(target_dates)
  expect_equal(length(target_sum), 2)
})
