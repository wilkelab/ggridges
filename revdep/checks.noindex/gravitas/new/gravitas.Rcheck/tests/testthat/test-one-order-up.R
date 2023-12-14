context("one order up functions")

x <- lubridate::ymd_hms("2014-12-31 23:31:15 UTC")


test_that("second_minute inputs", {
  expect_is(x, c("POSIXct", "POSIXt"))
})



# second_minute

test_that("second_minute outputs a numeric value", {
  expect_is(second_minute(x), "numeric")
})

test_that("second_minute expected output", {
  expect_equal(second_minute(x), 15)
})

test_that("second_minute output length equals input length", {
  expect_length(second_minute(x), length(x))
})

test_that("second_minute error with null input", {
  expect_error(second_minute(), "argument \"x\" is missing, with no default")
})


# minute_qhour
test_that("minute_qhour outputs a numeric value", {
  expect_is(minute_qhour(x), "numeric")
})


test_that("minute_qhour expected output", {
  expect_equal(minute_qhour(x), 2)
})

test_that("minute_qhour output length equals input length", {
  expect_length(minute_qhour(x), length(x))
})


test_that("minute_qhour error with null input", {
  expect_error(minute_qhour(), "argument \"x\" is missing, with no default")
})



# qhour_hhour

test_that("qhour_hhour outputs a numeric value", {
  expect_is(qhour_hhour(x), "numeric")
})


test_that("qhour_hhour expected output", {
  expect_equal(qhour_hhour(x), 1)
})


test_that("qhour_hhour output length equals input length", {
  expect_length(qhour_hhour(x), length(x))
})

test_that("qhour_hhour error with null input", {
  expect_error(qhour_hhour(), "argument \"x\" is missing, with no default")
})

# hhour_hour



test_that("hhour_hour outputs a numeric value", {
  expect_is(hhour_hour(x), "numeric")
})


test_that("hhour_hour expected output", {
  expect_equal(hhour_hour(x), 2)
})


test_that("hhour_hour output length equals input length", {
  expect_length(hhour_hour(x), length(x))
})


test_that("hhour_hour error with null input", {
  expect_error(hhour_hour(), "argument \"x\" is missing, with no default")
})
# week_fortnight

test_that("week_fortnight outputs a numeric value", {
  expect_is(week_fortnight(x), "numeric")
})


test_that("week_fortnight expected output", {
  expect_equal(week_fortnight(x), 1)
})

test_that("week_fortnight output length equals input length", {
  expect_length(week_fortnight(x), length(x))
})


test_that("week_fortnight error with null input", {
  expect_error(week_fortnight(), "argument \"x\" is missing, with no default")
})
# month_quarter

test_that("month_quarter outputs a numeric value", {
  expect_is(month_quarter(x), "numeric")
})


test_that("month_quarter expected output", {
  expect_equal(month_quarter(x), 3)
})


test_that("month_quarter output length equals input length", {
  expect_length(month_quarter(x), length(x))
})


test_that("month_quarter error with null input", {
  expect_error(month_quarter(), "argument \"x\" is missing, with no default")
})
# quarter_semester

test_that("quarter_semester outputs a numeric value", {
  expect_is(quarter_semester(x), "numeric")
})


test_that("quarter_semester expected output", {
  expect_equal(quarter_semester(x), 2)
})


test_that("quarter_semester output length equals input length", {
  expect_length(quarter_semester(x), length(x))
})

test_that("quarter_semester error with null input", {
  expect_error(quarter_semester(), "argument \"x\" is missing, with no default")
})
# semester_year

test_that("semester_year outputs a integer value", {
  expect_is(semester_year(x), "integer")
})


test_that("semester_year expected output", {
  expect_equal(semester_year(x), 2)
})

test_that("semester_year output length equals input length", {
  expect_length(semester_year(x), length(x))
})

test_that("semester_year error with null input", {
  expect_error(semester_year(), "argument \"x\" is missing, with no default")
})
