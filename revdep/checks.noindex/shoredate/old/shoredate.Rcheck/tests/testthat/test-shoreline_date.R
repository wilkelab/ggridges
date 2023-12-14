test_that("returns list of class shoreline_date", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_equal(class(target_date), c("shoreline_date", "list"))
})

test_that("first column is returned as site name", {
  skip_on_cran()
  target_point <- sf::st_as_sf(sf::st_sfc(sf::st_point(c(538310, 6544255)),
                                          crs = 32632))
  target_point$name <- "Example"
  target_date <- shoreline_date(site = target_point, elevation = 46)
  expect_equal(target_point$name, target_date[[1]][[1]]$site_name)
})

test_that("progress is printed with verbose = TRUE", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point,
                                 elevation = 46, verbose = TRUE ))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150))
})

test_that("gives warning if the elevation of a site implies a date that is out of bounds,
          and specifies isobase direction if others than the default are provided", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(538310, 6544255)), crs = 32632)
  expect_warning(shoreline_date(site = target_point, elevation = 150,
                                isobase_direction = 338))
})

test_that("undefined CRS throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, "Undefined coordinate reference system. This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632).")
})

test_that("wrong CRS throws error and that this is printed", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 4326)
  err <- expect_error(shoreline_date(site = target_point, elevation = 46))
  expect_equal(err$message, paste0("Target has coordinate reference system with EPSG ",
                                   sf::st_crs(target_point)$epsg,
                                   ". This needs to be set to WGS84 / UTM zone 32N (EPSG: 32632)."))
})

test_that("gives warning if displacement is to be interpolated to a site located outside the limit of the study area", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 46))
  expect_equal(warn$message, "Target location is not within the study area for which the interpolation method was derived.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE and is returned as NA.")
})

test_that("gives warning and returns NA if date has a later start than 2500 BCE and non-default isobase direction", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  warn <- expect_warning(shoreline_date(site = target_point, elevation = 17, isobase_direction = 338))
  expect_equal(warn$message, "Site 1 has a younger possible start date than 2500 BCE with an isobase direction of 338 and is returned as NA.")
})

test_that("lack of both elevation value and elevation raster throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point))
  expect_equal(err$message, "Numeric values specifying the site elevations or an elevation raster must be provided.")
})

test_that("providing different number of elevation values than number of sites throws error", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  err <- expect_error(shoreline_date(site = target_point, elevation = c(56, 64)))
  expect_equal(err$message, "Specify one elevation value per site. 2 elevation values and 1 sites were provided.")
})

test_that("using no model instead of the gamma works", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point,
                                 elevation = 60,
                                 model = "none"))
})

test_that("summing multiple isobase directions works", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  expect_snapshot(shoreline_date(site = target_point, elevation = 60,
                      isobase_direction = c(327, 338),
                      sum_isobase_directions = TRUE))
})

test_that("finding site elevation from a raster works", {
  skip_on_cran()
  # Having issues with R CMD check on GitHub with progress
  skip_if_not_installed("progress")
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  target_wgs84 <- sf::st_transform(target_point, crs = 4326)
  elev_raster <- suppressWarnings(elevatr::get_elev_raster(target_wgs84,
                                                         z = 14,  src = "aws"))
  elev_raster <- terra::project(terra::rast(elev_raster), "epsg:32632")
  expect_snapshot(shoreline_date(target_point, elevation = elev_raster))
})

test_that("precomputing interpolation and passing site as a site name", {
  skip_on_cran()
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  precompiled_curve <- interpolate_curve(target_point)
  expect_snapshot(shoreline_date(site = "Example site", elevation = 60,
                                 target_curve = precompiled_curve))
})

test_that("passing displacement curve with different time interval and no isobase", {
  skip_on_cran()
  orland_disp <- get(load(system.file("extdata/orland_displacement_curve.rda",
                                      package = "shoredate")))
  expect_snapshot(shoreline_date(site = "Example site", elevation = 17,
                                 target_curve = orland_disp))
})

test_that("NA date with no isobase direction above elevation limit", {
  skip_on_cran()
  orland_disp <- get(load(system.file("extdata/orland_displacement_curve.rda",
                                      package = "shoredate")))
  warn <- expect_warning(shoreline_date(site = "Example site", elevation = 40,
                                 target_curve = orland_disp))
  expect_equal(warn$message, "The elevation of site Example site implies an earliest possible date older than -4050 BCE and is out of bounds. The date is returned as NA.")
})
