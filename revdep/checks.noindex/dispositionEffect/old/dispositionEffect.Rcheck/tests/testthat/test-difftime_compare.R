context("difftime_compare")

from <- as.POSIXct("2021-01-04 09:00:00", tz = "UTC")
to <- as.POSIXct("2021-01-04 18:00:00", tz = "UTC")

test_that("difftime_compare works", {
	expect_equal(difftime_compare(from, to, "5 mins"), "greater")
	expect_equal(difftime_compare(from, to, "5 hours"), "greater")
	expect_equal(difftime_compare(from, to, "5 days"), "smaller")
	expect_error(difftime_compare(from, to, "5days"))
})
