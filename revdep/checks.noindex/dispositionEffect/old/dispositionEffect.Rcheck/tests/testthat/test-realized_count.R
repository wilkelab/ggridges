context("realized_count's arguments")

# portfolio_quantity, portfolio_price, transaction_quantity, transaction_price, transaction_type

test_that("realized_count works (zero quantity)", {
  expect_equal(realized_count(0, 0, 0, 0, "B", allow_short = TRUE),
  						 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	expect_equal(realized_count(0, 0, 0, 0, "S", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})

test_that("realized_count works (long)", {
	# paper loss
	expect_equal(realized_count(2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
	# paper gain
	expect_equal(realized_count(2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1))
	# realized gain + paper gain
	expect_equal(realized_count(2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(2, 15, -2, 10, "S", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized  gain
	expect_equal(realized_count(2, 10, -2, 15, "S", allow_short = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})

test_that("realized_count works (short)", {
	# paper gain
	expect_equal(realized_count(-2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# paper loss
	expect_equal(realized_count(-2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
	# realized gain + paper gain
	expect_equal(realized_count(-2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(-2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1))
	# realized gain
	expect_equal(realized_count(-2, 15, 2, 10, "B", allow_short = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(-2, 10, 2, 15, "B", allow_short = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
})

test_that("realized_count works (allow_short = FALSE)", {
	# long
	# paper loss
	expect_equal(realized_count(2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 1))
	# paper gain
	expect_equal(realized_count(2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 1))
	# realized gain + paper gain
	expect_equal(realized_count(2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 1, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(2, 15, -2, 10, "S", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized  gain
	expect_equal(realized_count(2, 10, -2, 15, "S", allow_short = FALSE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# short
	# paper gain
	expect_equal(realized_count(-2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# paper loss
	expect_equal(realized_count(-2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized gain + paper gain
	expect_equal(realized_count(-2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(-2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized gain
	expect_equal(realized_count(-2, 15, 2, 10, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(-2, 10, 2, 15, "B", allow_short = FALSE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})

test_that("realized_count works (realized_only = TRUE)", {
	# allow_short = TRUE
	# long
	# paper loss
	expect_equal(realized_count(2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# paper gain
	expect_equal(realized_count(2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized gain + paper gain
	expect_equal(realized_count(2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(2, 15, -2, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized  gain
	expect_equal(realized_count(2, 10, -2, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# short
	# paper gain
	expect_equal(realized_count(-2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# paper loss
	expect_equal(realized_count(-2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized gain + paper gain
	expect_equal(realized_count(-2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(-2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized gain
	expect_equal(realized_count(-2, 15, 2, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(-2, 10, 2, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	#  allow_short = FALSE
	# long
	# paper loss
	expect_equal(realized_count(2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# paper gain
	expect_equal(realized_count(2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized gain + paper gain
	expect_equal(realized_count(2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(2, 15, -2, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 1, "PG_count" = 0, "PL_count" = 0))
	# realized  gain
	expect_equal(realized_count(2, 10, -2, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 1, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# short
	# paper gain
	expect_equal(realized_count(-2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# paper loss
	expect_equal(realized_count(-2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized gain + paper gain
	expect_equal(realized_count(-2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss + paper loss
	expect_equal(realized_count(-2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized gain
	expect_equal(realized_count(-2, 15, 2, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
	# realized loss
	expect_equal(realized_count(-2, 10, 2, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_count" = 0, "RL_count" = 0, "PG_count" = 0, "PL_count" = 0))
})
