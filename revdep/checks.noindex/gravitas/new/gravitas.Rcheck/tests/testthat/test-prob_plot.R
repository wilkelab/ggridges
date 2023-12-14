context("creates plot")

library(gravitas)
library(ggplot2)
library(dplyr)
library(tsibble)
x <- tsibbledata::vic_elec

cricket_tsibble <- cricket %>%
  dplyr::mutate(data_index = row_number()) %>%
  tsibble::as_tsibble(index = data_index)

hierarchy_model <- tibble::tibble(
   units = c("index", "over", "inning", "match"),
   convert_fct = c(1, 20, 2, 1))

# common tests for temporal and non-temporal data

test_that("prob_plot error with null input", {
  expect_error(prob_plot(), "argument \".data\" is missing, with no default")
})


test_that("tsibble input", {
  expect_is(x, c("tbl_ts", "tbl_df", "tbl", "data.frame"))
})

test_that("character output", {
  expect_is(prob_plot(x, "hour_day", "hhour_hour"), c("gg", "ggplot"))
})


test_that("throws error when granularities to be plotted are not specified", {
  expect_error(prob_plot(x), "Specify the granularities that are to be plotted")
})


test_that("throws error with just one granularity", {
  expect_error(prob_plot(x, gran2 = "hour_day"), "Specify the granularities that are to be plotted")
})


test_that("throws error with no hierarchy table specified for non temporal data", {
  expect_error(
    prob_plot(cricket_tsibble, "inning_match", "over_inning"),
    "Hierarchy table must be provided\n           when class of index of the tsibble\n           is not date-time"
  )
})

test_that("throws error with incorrect input for granularities", {
  expect_error(
    prob_plot(cricket_tsibble, "innings_match", "over_inning", hierarchy_model),
    "lower part of granularity must be\n           listed as an element in the hierarchy table"
  )
})
