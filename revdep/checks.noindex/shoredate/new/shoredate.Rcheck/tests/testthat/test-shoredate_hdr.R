test_that("returns a list of expected length", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  target_date <- shoreline_date(sites = target_point, elevation = 65)

  shdr <- shoredate_hdr(target_date[[1]][[1]]$date$bce,
                                 target_date[[1]][[1]]$date$probability,
                                 target_date[[1]][[1]]$site_name,
                                 target_date[[1]][[1]]$cal_reso)
  expect_equal(length(shdr), 5)
})
