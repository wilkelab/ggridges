context("gran_convert")

test_that("gran_convert is numeric", {
  expect_is(
    gran_convert("hour", "day"),
    "numeric"
  )
})

test_that("gran_convert has length 1", {
  expect_equal(length(gran_convert(
    "hour",
    "day"
  )), 1)
})

test_that("gran_convert output for hour, week is 168", {
  expect_equal(gran_convert(
    "hour",
    "week"
  ), 168)
})

test_that("gran_convert output for week, hour is error", {
  expect_error(
    gran_convert("week", "hour"),
    "Second temporal resolution should be higher\n           in order than the first one.\n           Try reversing their position"
  )
})


test_that("gran_convert output for unknown temporal granularity(s) is error", {
  expect_error(
    gran_convert("lightyear", "hour"),
    "granularity lightyear and hour both should be one of second, minute, qhour, hhour, hour, day, week, fortnight, month, quarter, semester, year"
  )
})
