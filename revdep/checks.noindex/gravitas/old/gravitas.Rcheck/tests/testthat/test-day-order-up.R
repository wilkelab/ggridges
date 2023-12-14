context("day order functions")

x <- lubridate::ymd_hms("2014-12-30 23:31:15 UTC")


test_that("second_minute inputs", {
  expect_is(x, c("POSIXct", "POSIXt"))
})


# day_fortnight
test_that("day_fortnight outputs a numeric value", {
  expect_is(day_fortnight(x), "numeric")
})

test_that("day_fortnight expected output", {
  expect_equal(day_fortnight(x), 14)
})

test_that("day_fortnight output length equals input length", {
  expect_length(day_fortnight(x), length(x))
})

test_that("day_fortnight error with null input", {
  expect_error(day_fortnight(), "argument \"x\" is missing, with no default")
})


# day_semester

test_that("day_semester outputs a numeric value", {
  expect_is(day_semester(x), "numeric")
})

test_that("day_semester expected output", {
  expect_equal(day_semester(x), 183)
})

test_that("day_semester output length equals input length", {
  expect_length(day_semester(x), length(x))
})

test_that("day_semester error with null input", {
  expect_error(day_semester(), "argument \"x\" is missing, with no default")
})

# qhour_day
test_that("qhour_day outputs a numeric value", {
  expect_is(qhour_day(x), "numeric")
})

test_that("qhour_day expected output", {
  expect_equal(qhour_day(x), 95)
})

test_that("qhour_day output length equals input length", {
  expect_length(qhour_day(x), length(x))
})

test_that("qhour_day error with null input", {
  expect_error(qhour_day(), "argument \"x\" is missing, with no default")
})


# hhour_day

test_that("hhour_day outputs a numeric value", {
  expect_is(hhour_day(x), "numeric")
})

test_that("hhour_day expected output", {
  expect_equal(hhour_day(x), 47)
})

test_that("hhour_day output length equals input length", {
  expect_length(hhour_day(x), length(x))
})

test_that("hhour_day error with null input", {
  expect_error(hhour_day(), "argument \"x\" is missing, with no default")
})


# minute_day


test_that("minute_day outputs a numeric value", {
  expect_is(minute_day(x), "numeric")
})

test_that("minute_day expected output", {
  expect_equal(minute_day(x), 1411)
})

test_that("minute_day output length equals input length", {
  expect_length(minute_day(x), length(x))
})

test_that("minute_day error with null input", {
  expect_error(minute_day(), "argument \"x\" is missing, with no default")
})

# second_day


test_that("second_day outputs a numeric value", {
  expect_is(second_day(x), "numeric")
})

test_that("second_day expected output", {
  expect_equal(second_day(x), 84675)
})

test_that("second_day output length equals input length", {
  expect_length(second_day(x), length(x))
})

test_that("second_day error with null input", {
  expect_error(second_day(), "argument \"x\" is missing, with no default")
})
