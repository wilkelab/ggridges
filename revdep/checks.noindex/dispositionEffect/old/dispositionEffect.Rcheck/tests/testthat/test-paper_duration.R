context("paper_duration's arguments")

test_that("paper_duration works (zero quantity)", {
  expect_equal(paper_duration(0, 0, 0, datetime_difference = 5, allow_short = TRUE),
  						 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(0, 15, 10, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(0, 10, 15, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})

test_that("paper_duration works (long)", {
	expect_equal(paper_duration(2, 0, 0, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(2, 15, 10, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 5))
	expect_equal(paper_duration(2, 10, 15, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 5, "PL_duration" = 0))
})

test_that("paper_duration works (short)", {
	expect_equal(paper_duration(-2, 0, 0, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(-2, 15, 10, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 5, "PL_duration" = 0))
	expect_equal(paper_duration(-2, 10, 15, datetime_difference = 5, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 5))
})

test_that("paper_duration works (allow_short = FALSE)", {
	expect_equal(paper_duration(2, 0, 0, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(2, 15, 10, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 5))
	expect_equal(paper_duration(2, 10, 15, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 5, "PL_duration" = 0))
	expect_equal(paper_duration(-2, 0, 0, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(-2, 15, 10, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(paper_duration(-2, 10, 15, datetime_difference = 5, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})
