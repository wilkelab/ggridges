context("screening list of harmonies")

x <- tsibbledata::vic_elec

library(gravitas)
library(ggplot2)
library(dplyr)
library(tsibble)

cricket_tsibble <- cricket %>%
  dplyr::mutate(data_index = row_number()) %>%
  tsibble::as_tsibble(index = data_index)


hierarchy_model <- tibble::tibble(
  units = c("index", "over", "inning", "match"),
  convert_fct = c(1, 20, 2, 1))

harmony_tbl <- tibble::tibble(facet_variable = c("hour_day","wknd_wday",
               "wknd_wday", "hhour_hour",
               "wknd_wday","hhour_hour",
"hour_day"),
x_variable = c("hhour_hour","hhour_hour",
                   "hhour_day", "hour_day",
                   "hour_day","wknd_wday",
                   "wknd_wday"),
facet_levels = c(24, 2, 2, 2, 2, 2, 24),
x_levels = c(2, 2, 48, 24, 24, 2, 2))

harmony_tbl$facet_levels <- as.integer(harmony_tbl$facet_levels)
harmony_tbl$x_levels <- as.integer(harmony_tbl$x_levels)

# common tests for temporal and non-temporal data


test_that("tsibble input", {
  expect_is(x, c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})


test_that("tibble output", {
  expect_is(harmony(x, lgran = "hour", ugran = "week"), c("tbl_df", "tbl", "data.frame"))
})


test_that("norun check runs in create_gran",{
expect_equal(smart_meter10 %>%
   harmony(ugran = "day",
           filter_in = "wknd_wday"),harmony_tbl)
})

test_that("harmony error with null input", {
  expect_error(harmony(), "argument \".data\" is missing, with no default")
})

test_that("harmony throws error when only one granularity can be formed.", {
  expect_error(harmony(x, lgran = "hour", ugran = "day"), "Only one granularity hour_day can be formed. Function requires checking compatibility for bivariate granularities")
})

test_that("harmony throws error when no hierarchy table provided for non-temporal data.", {
  expect_error(harmony(cricket_tsibble), "Hierarchy table must be provided when class of index of the tsibble is not date-time")
})


test_that("harmony throws error when incorrect lgran provided.", {
  expect_error(harmony(x, lgran = "hours", ugran = "day"), "lowest unit must be listed as an element in the hierarchy table")
})

test_that("harmony throws error when incorrect ugran provided.", {
  expect_error(harmony(x, lgran = "hour", ugran = "daysu"), "highest unit must be listed as an element in the  hierarchy table")
})
