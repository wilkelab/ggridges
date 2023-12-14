context("paper_value's arguments")

test_that("paper_value works (zero quantity)", {
  expect_equal(paper_value(0, 0, 0, allow_short = TRUE),
  						 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(0, 15, 10, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(0, 10, 15, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})

test_that("paper_value works (long)", {
	expect_equal(paper_value(2, 0, 0, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(2, 15, 10, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	expect_equal(paper_value(2, 10, 15, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
})

test_that("paper_value works (short)", {
	expect_equal(paper_value(-2, 0, 0, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(-2, 15, 10, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = -(10 - 15) / 15, "PL_value" = 0))
	expect_equal(paper_value(-2, 10, 15, allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = -(15 - 10) / 10))
})

test_that("paper_value works (allow_short = FALSE)", {
	expect_equal(paper_value(2, 0, 0, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(2, 15, 10, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	expect_equal(paper_value(2, 10, 15, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
	expect_equal(paper_value(-2, 0, 0, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(-2, 15, 10, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(paper_value(-2, 10, 15, allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})
