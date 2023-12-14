context("search all granularities")

x <- tsibbledata::vic_elec

library(gravitas)
library(ggplot2)
library(dplyr)
library(tsibble)

cricket_tsibble <- cricket %>%
  dplyr::mutate(data_index = row_number()) %>%
  tsibble::as_tsibble(index = data_index)

hierarchy_model <- tibble::tibble(
  units = c(
    "index",
    "ball",
    "over",
    "inning",
    "match"
  ),
  convert_fct = c(1, 6, 20, 2, 1)
)

# common tests for temporal and non-temporal data

test_that("tsibble input", {
  expect_is(x, c(
    "tbl_ts", "tbl_df",
    "tbl",
    "data.frame"
  ))
})

test_that("search_gran outputs a character", {
  expect_is(
    search_gran(x, "hour", "week"),
    "character"
  )
})

test_that("search_gran error with null input", {
  expect_error(
    search_gran(),
    "argument \".data\" is missing, with no default"
  )
})


# temporal data


test_that("search_gran hour to week expected output", {
  expect_equal(search_gran(
    x,
    "hour",
    "week"
  ), c(
    "hour_day",
    "hour_week",
    "day_week"
  ))
})


test_that("search_gran error with
          finer and coarser unit swapped", {
  expect_error(
    search_gran(
      x,
      "week",
      "hour"
    ),
    "granularities should be of the form finer to coarser. Try swapping the order of the units."
  )
})

test_that("search_gran error with finer
          and coarser unit swapped", {
  expect_error(
    search_gran(x, "week", "hour"),
    "granularities should be of the form finer to coarser. Try swapping the order of the units."
  )
})


test_that("search_gran error input for highest
          unit doesn't feature in predefined
          lookup table", {
  expect_error(
    search_gran(x, "hour", "weeks"),
    "highest unit must be listed as an element in the  hierarchy table"
  )
})


test_that("search_gran error input
          for lowest unit doesn't feature
          in predefined lookup table", {
  expect_error(
    search_gran(x, "millisecond", "week"),
    "lowest unit must be listed as an element in the hierarchy table"
  )
})


# non-temporal data

test_that("search_gran of
          non-temporal expected output", {
  expect_equal(
    search_gran(cricket_tsibble,
      lowest_unit = "ball",
      highest_unit = "inning",
      hierarchy_tbl = hierarchy_model
    ),
    c(
      "ball_over",
      "ball_inning",
      "over_inning"
    )
  )
})

test_that("search_gran error with finer
          and coarser unit swapped
          in non-temporal data", {
  expect_error(
    search_gran(cricket_tsibble,
      lowest_unit = "inning",
      highest_unit = "over",
      hierarchy_tbl =
        hierarchy_model
    ),
    "granularities should be of the form finer to coarser. Try swapping the order of the units."
  )
})

test_that("search_gran error when no hierarchy table
          supplied for non-temporal data", {
  expect_error(
    search_gran(cricket_tsibble,
      lowest_unit = "ball",
      highest_unit = "inning"
    ),
    "Hierarchy table must be provided when class of index of the tsibble is not date-time"
  )
})


test_that("search_gran error input for highest unit
          doesn't feature in hierarchy table", {
  expect_error(
    search_gran(cricket_tsibble,
      lowest_unit = "ball",
      highest_unit = "innings",
      hierarchy_tbl = hierarchy_model
    ),
    "highest unit must be listed as an element in the  hierarchy table"
  )
})


test_that("search_gran error input for lowest
          unit doesn't feature in hierarchy table", {
  expect_error(
    search_gran(cricket_tsibble,
      lowest_unit = "balls",
      highest_unit = "inning",
      hierarchy_tbl = hierarchy_model
    ),
    "lowest unit must be listed as an element in the hierarchy table"
  )
})
