context("build_gran")

x <- lubridate::ymd_hms("2018-11-04 18:37:04 EST")

# test_that("build_gran inputs", {
#   expect_is(x,c("POSIXct", "POSIXt"))
# })

test_that("build_gran output length equals input length of time vector", {
  expect_length(build_gran(x, "hour", "week"), length(x))
})
#
# test_that("build_gran error with null input", {
#   expect_error(build_gran(x, "hour"), "function requires both gran1 and gran2 to be specified")
# })

test_that("build_gran outputs a numeric value", {
  expect_is(build_gran(x, "hour", "week"), "numeric")
})


test_that("build_gran expected output hour_week", {
  expect_equal(build_gran(x, "hour", "week"), 18)
})

test_that("build_gran expected output minute_hhour", {
  expect_equal(build_gran(x, "minute", "hhour"), 8)
})

test_that("build_gran expected output day_month", {
  expect_equal(build_gran(x, "day", "month"), 4)
})

test_that("build_gran expected output month_semester", {
  expect_equal(build_gran(x, "month", "semester"), 5)
})

test_that("build_gran expected output week_quarter", {
  expect_equal(build_gran(x, "week", "quarter"), 5)
})

test_that("build_gran expected output week_semester", {
  expect_equal(build_gran(x, "week", "semester"), 19)
})

test_that("build_gran expected output second_hhour", {
  expect_equal(build_gran(x, "second", "hhour"), 424)
})
