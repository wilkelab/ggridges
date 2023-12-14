context("paper_count's arguments")

test_that("paper_count works (zero quantity)", {
  expect_equal(paper_count(0, 0, 0, allow_short = TRUE),
  						 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(0, 15, 10, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(0, 10, 15, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})

test_that("paper_count works (long)", {
	expect_equal(paper_count(2, 0, 0, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(2, 15, 10, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
	expect_equal(paper_count(2, 10, 15, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
})

test_that("paper_count works (short)", {
	expect_equal(paper_count(-2, 0, 0, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(-2, 15, 10, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	expect_equal(paper_count(-2, 10, 15, allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
})

test_that("paper_count works (allow_short = FALSE)", {
	expect_equal(paper_count(2, 0, 0, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(2, 15, 10, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
	expect_equal(paper_count(2, 10, 15, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	expect_equal(paper_count(-2, 0, 0, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(-2, 15, 10, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(paper_count(-2, 10, 15, allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})
