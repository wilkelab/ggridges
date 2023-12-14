context("difftime_financial")

from <- as.POSIXct("2021-01-04 09:00:00", tz = "UTC")
to <- as.POSIXct("2021-01-04 18:00:00", tz = "UTC")

test_that("difftime_financial works (same date)", {
	expect_equal(difftime_financial(from, to), 9)
	expect_equal(difftime_financial(from, to, units = "mins"), 9 * 60)
	expect_equal(difftime_financial(from, to, units = "days"), 9 / 24)
})

from <- as.POSIXct("2021-01-04 09:00:00", tz = "UTC")
to <- as.POSIXct("2021-01-05 18:00:00", tz = "UTC")

test_that("difftime_financial works (different dates)", {
	expect_equal(difftime_financial(from, to), 23)
	expect_equal(difftime_financial(from, to, units = "mins"), 23 * 60)
	expect_equal(difftime_financial(from, to, units = "days"), 23 / 24)
})

from <- as.POSIXct("2021-01-09 23:00:00", tz = "UTC")
to <- as.POSIXct("2021-01-10 06:00:00", tz = "UTC")

test_that("difftime_financial works (weekends)", {
	expect_warning(difftime_financial(from, to))
})

