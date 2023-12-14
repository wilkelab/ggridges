context("realized_total's arguments")

# portfolio_quantity, portfolio_price, transaction_quantity, transaction_price, transaction_type

test_that("realized_total works (zero quantity)", {
  expect_equal(realized_total(0, 0, 0, 0, "B", allow_short = TRUE),
  						 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	expect_equal(realized_total(0, 0, 0, 0, "S", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})

test_that("realized_total works (long)", {
	# paper loss
	expect_equal(realized_total(2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 2))
	# paper gain
	expect_equal(realized_total(2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 2, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-1), "PG_total" = 0, "PL_total" = (2 - 1)))
	# realized gain + paper gain
	expect_equal(realized_total(2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_total" = abs(-1), "RL_total" = 0, "PG_total" = (2 - 1), "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(2, 15, -2, 10, "S", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-2) + (2 - 2), "PG_total" = 0, "PL_total" = 0))
	# realized gain
	expect_equal(realized_total(2, 10, -2, 15, "S", allow_short = TRUE),
							 c("RG_total" = abs(-2) + (2 - 2), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})

test_that("realized_total works (short)", {
	# paper gain
	expect_equal(realized_total(-2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = abs(-2), "PL_total" = 0))
	# paper loss
	expect_equal(realized_total(-2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = abs(-2)))
	# realized gain + paper gain
	expect_equal(realized_total(-2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_total" = 1, "RL_total" = 0, "PG_total" = abs(-2 + 1), "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(-2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 1, "PG_total" = 0, "PL_total" = abs(-2 + 1)))
	# realized gain
	expect_equal(realized_total(-2, 15, 2, 10, "B", allow_short = TRUE),
							 c("RG_total" = 1 - (-2 + 1), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(-2, 10, 2, 15, "B", allow_short = TRUE),
							 c("RG_total" = 0, "RL_total" = 1 - (-2 + 1), "PG_total" = 0, "PL_total" = 0))
})

test_that("realized_total works (allow_short = FALSE)", {
	# long
	# paper
	expect_equal(realized_total(2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 2))
	# paper gain
	expect_equal(realized_total(2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 2, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = abs(-1), "PG_total" = 0, "PL_total" = (2 - 1)))
	# realized gain + paper gain
	expect_equal(realized_total(2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_total" = abs(-1), "RL_total" = 0, "PG_total" = (2 - 1), "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(2, 15, -2, 10, "S", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = abs(-2) + (2 - 2), "PG_total" = 0, "PL_total" = 0))
	# realized  gain
	expect_equal(realized_total(2, 10, -2, 15, "S", allow_short = FALSE),
							 c("RG_total" = abs(-2) + (2 - 2), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# short
	# paper gain
	expect_equal(realized_total(-2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# paper loss
	expect_equal(realized_total(-2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized gain + paper gain
	expect_equal(realized_total(-2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(-2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized gain
	expect_equal(realized_total(-2, 15, 2, 10, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(-2, 10, 2, 15, "B", allow_short = FALSE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})

test_that("realized_total works (realized_only = TRUE)", {
	# allow_short = TRUE
	# long
	# paper loss
	expect_equal(realized_total(2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# paper gain
	expect_equal(realized_total(2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-1), "PG_total" = 0, "PL_total" = 0))
	# realized gain + paper gain
	expect_equal(realized_total(2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = abs(-1), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(2, 15, -2, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-2) + (2 - 2), "PG_total" = 0, "PL_total" = 0))
	# realized gain
	expect_equal(realized_total(2, 10, -2, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = abs(-2) + (2 - 2), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# short
	# paper gain
	expect_equal(realized_total(-2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# paper loss
	expect_equal(realized_total(-2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized gain + paper gain
	expect_equal(realized_total(-2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 1, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(-2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 1, "PG_total" = 0, "PL_total" = 0))
	# realized gain
	expect_equal(realized_total(-2, 15, 2, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 1 - (-2 + 1), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(-2, 10, 2, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 1 - (-2 + 1), "PG_total" = 0, "PL_total" = 0))
	#  allow_short = FALSE
	# long
	# paper loss
	expect_equal(realized_total(2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# paper gain
	expect_equal(realized_total(2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-1), "PG_total" = 0, "PL_total" = 0))
	# realized gain + paper gain
	expect_equal(realized_total(2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = abs(-1), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(2, 15, -2, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = abs(-2) + (2 - 2), "PG_total" = 0, "PL_total" = 0))
	# realized  gain
	expect_equal(realized_total(2, 10, -2, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = abs(-2) + (2 - 2), "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# short
	# paper gain
	expect_equal(realized_total(-2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# paper loss
	expect_equal(realized_total(-2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized gain + paper gain
	expect_equal(realized_total(-2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss + paper loss
	expect_equal(realized_total(-2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized gain
	expect_equal(realized_total(-2, 15, 2, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
	# realized loss
	expect_equal(realized_total(-2, 10, 2, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_total" = 0, "RL_total" = 0, "PG_total" = 0, "PL_total" = 0))
})
