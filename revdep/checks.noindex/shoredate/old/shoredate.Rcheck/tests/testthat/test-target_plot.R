test_that("returns expected plot when no targets are passed", {
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot()
  vdiffr::expect_doppelganger("no targets", p)
})

# For label placement
set.seed(123)

# To be reused
target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)

test_that("throws error if CRS is undefined for targets", {
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)))
  err <- expect_error(target_plot(target_point))
  expect_equal(err$message,
      "The provided targets do not have a defined coordinate reference system.")
})

test_that("throws error if CRS is undefined for isobases", {
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  pts <- rbind(c(579570, 6582982), c(578000, 6582000))
  isobase <- sf::st_linestring(pts)
  err <- expect_error(target_plot(isobases = isobase))
  expect_equal(err$message,
    "The provided isobases do not have a defined coordinate reference system.")
})

test_that("throws error if CRS is undefined for basemap", {
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  pts1 <- rbind(c(579570, 6582982), c(578000, 6582000),
                c(578100, 6582100), c(579570, 6582982))
  pts2 <- rbind(c(570000, 6580000), c(578500, 6582500),
                c(578700, 6582700), c(570000, 6580000))
  basemap <- sf::st_polygon(list(pts1, pts2))
  err <- expect_error(target_plot(basemap = basemap))
  expect_equal(err$message,
    "The provided basemap does not have a defined coordinate reference system.")
})

test_that("basemap is reprojected to correct CRS with a warning", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  basemap = sf::st_read(
    system.file("extdata/naturalearth_basemap.gpkg",
                package = "shoredate",
                mustWork = TRUE), quiet = TRUE)
  basemap <- sf::st_transform(basemap, 4326)

  warn <- expect_warning(target_plot(basemap = basemap))
  expect_equal(warn$message,
               "Reprojecting basemap from CRS with EPSG code 4326 to EPSG 32632.")

  p <- suppressWarnings(target_plot(basemap = basemap))
  vdiffr::expect_doppelganger("reprojected basemap", p)
})

test_that("isobases are reprojected to correct CRS with a warning", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  pts <- rbind(c(579570, 6582982), c(570000, 6580000))
  isobase <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts)), crs = 32632)
  isobase <- sf::st_transform(isobase, 4326)
  isobase$name = "Isobase"

  warn <- expect_warning(target_plot(isobases = isobase,
                   isobase_line = c("Isobase" = "solid"),
                   isobase_col = c("Isobase" = "red")))
  expect_equal(warn$message,
           "Reprojecting isobases from CRS with EPSG code 4326 to EPSG 32632.")

  p <- suppressWarnings(target_plot(isobases = isobase,
              isobase_line = c("Isobase" = "solid"),
              isobase_col = c("Isobase" = "red")))
  vdiffr::expect_doppelganger("reprojected isobases", p)
})

test_that("targets are reprojected to correct CRS with a warning", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  target_point <- sf::st_sfc(sf::st_point(c(579570, 6582982)), crs = 32632)
  target_point <- sf::st_transform(target_point, 4326)

  warn <- expect_warning(target_plot(targets = target_point))
  expect_equal(warn$message,
               "Reprojecting targets from CRS with EPSG code 4326 to EPSG 32632.")

  p <- suppressWarnings(target_plot(targets = target_point))
  vdiffr::expect_doppelganger("reprojected targets", p)
})

test_that("returns expected plot when a target is passed", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point)
  vdiffr::expect_doppelganger("target point", p)
})

test_that("returns expected plot with a target with multiple columns", {
  set.seed(123)
  skip_on_cran()
  target_point <- sf::st_sf(target_point)
  target_point$name <- "Example"
  target_point$additional_column <- "should not be plotted"

  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point)
  vdiffr::expect_doppelganger("multicolumn target", p)
})

test_that("returns expected plot when isobases are excluded", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, isobases = NA)
  vdiffr::expect_doppelganger("exclude isobases", p)
})

test_that("returns expected plot when basemap is excluded", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, basemap = NA)
  vdiffr::expect_doppelganger("exclude basemap", p)
})

test_that("returns expected plot when only targets are plotted", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, isobases = NA, basemap = NA)
  vdiffr::expect_doppelganger("only targets", p)
})

test_that("returns expected plot when only isobases are plotted", {
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(targets = NA, basemap = NA)
  vdiffr::expect_doppelganger("only isobases", p)
})

test_that("returns expected plot when only basemap is plotted", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(targets = NA, isobases = NA)
  vdiffr::expect_doppelganger("only basemap", p)
})

test_that("returns expected plot when graphical parameters are adjusted", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point,
                   target_shape = 24,
                   target_col = "chartreuse",
                   target_fill = "black",
                   target_size = 2.25,
                   isobase_line = c("Horten" = "dashed",
                                    "Porsgrunn" = "dashed",
                                    "Tvedestrand" = "dashed",
                                    "Arendal" = "dashed"),
                   isobase_col = c("Arendal" = "gold",
                                   "Porsgrunn" = "hotpink",
                                   "Tvedestrand" = "firebrick2",
                                   "Horten" = "thistle1"))
  vdiffr::expect_doppelganger("graphical parameters", p)
})

test_that("returns expected plot in greyscale when a target is passed", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  p <- target_plot(target_point, greyscale = TRUE)
  vdiffr::expect_doppelganger("greyscale", p)
})

test_that("plotting with naturalearthdata works", {
  set.seed(123)
  # skip_on_cran()
  skip("Skipped due to R CMD check failure on GitHub")
  target_point <- sf::st_sfc(sf::st_point(c(532719, 7065723)), crs = 32632)
  p <- suppressWarnings(target_plot(target_point, naturalearth_basemap = TRUE))
  vdiffr::expect_doppelganger("naturalearth basemap", p)
})


test_that("gives warning targets are located outside the basemap", {
    skip_on_cran()
    target_point <- sf::st_sfc(sf::st_point(c(458310, 6544255)), crs = 32632)

    warn <- expect_warning(target_plot(target_point))
    expect_equal(warn$message, "Basemap and targets do not intersect.")

    warn <- expect_warning(target_plot(target_point, isobases = NA))
    expect_equal(warn$message, "Basemap and targets do not intersect.")
})

test_that("gives warning if basemap and isobases do not intersect", {
  set.seed(123)
  skip_on_cran()
  # skip("Skipped due to R CMD check failure on GitHub")
  pts <- rbind(c(5795, 6582), c(5700, 65800))
  isobase <- sf::st_as_sf(sf::st_sfc(sf::st_linestring(pts)), crs = 32632)
  isobase$name = "Isobase"

  warn <- expect_warning(target_plot(isobases = isobase,
                                     isobase_line = c("Isobase" = "solid"),
                                     isobase_col = c("Isobase" = "red")))
  expect_equal(warn$message,
               "Basemap and isobases do not intersect.")
})
