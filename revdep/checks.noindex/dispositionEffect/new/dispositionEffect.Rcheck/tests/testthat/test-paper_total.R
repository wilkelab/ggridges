context("paper_total's arguments")

test_that("paper_total works (zero quantity)", {
  expect_equal(paper_total(0, 0, 0, allow_short = TRUE),
  						 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(0, 15, 10, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(0, 10, 15, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})

test_that("paper_total works (long)", {
	expect_equal(paper_total(2, 0, 0, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(2, 15, 10, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 2))
	expect_equal(paper_total(2, 10, 15, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 2, "PL_total" = 0))
})

test_that("paper_total works (short)", {
	expect_equal(paper_total(-2, 0, 0, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(-2, 15, 10, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 2, "PL_total" = 0))
	expect_equal(paper_total(-2, 10, 15, allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 2))
})

test_that("paper_total works (allow_short = FALSE)", {
	expect_equal(paper_total(2, 0, 0, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(2, 15, 10, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 2))
	expect_equal(paper_total(2, 10, 15, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 2, "PL_total" = 0))
	expect_equal(paper_total(-2, 0, 0, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(-2, 15, 10, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(paper_total(-2, 10, 15, allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})
