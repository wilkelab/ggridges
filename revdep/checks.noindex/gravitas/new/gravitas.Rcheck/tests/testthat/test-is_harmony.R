context("check if two granularities are harmonies")

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

# common tests for temporal and non-temporal data

test_that("tsibble input", {
  expect_is(x, c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})


test_that("character output", {
  expect_is(
    is_harmony(
      x,
      "hour_day",
      "day_week"
    ),
    "character"
  )
})


test_that("is_harmony error with null input", {
  expect_error(
    is_harmony(),
    "argument \".data\" is missing, with no default"
  )
})

test_that("throws error with just first granularity", {
  expect_error(
    is_harmony(x, gran1 = "hour_day"),
    "argument \"gran2\" is missing, with no default"
  )
})


test_that("throws error with just second granularity", {
  expect_error(
    is_harmony(x, gran2 = "hour_day"),
    "argument \"gran1\" is missing, with no default"
  )
})

test_that("throws error with no hierarchy table
          provided for non temporal data", {
  expect_error(
    is_harmony(
      cricket_tsibble,
      "inning_match",
      "over_match"
    ),
    "Hierarchy table must be provided\n           when class of index of the tsibble\n           is not date-time"
  )
})

test_that("throws error with incorrect input for granularity 1", {
  expect_error(
    is_harmony(
      cricket_tsibble,
      "innings_match",
      "over_match",
      hierarchy_model
    ),
    "lower part of granularity must be\n           listed as an element in the hierarchy table"
  )
})


test_that("throws error with incorrect input for granularity 2", {
  expect_error(
    is_harmony(
      cricket_tsibble,
      "inning_match",
      "over_matchgh",
      hierarchy_model
    ),
    "upper part of granularity must be\n           listed as an element in the hierarchy table"
  )
})
