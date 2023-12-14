context("realized_value's arguments")

# portfolio_quantity, portfolio_price, transaction_quantity, transaction_price, transaction_type
# Er = (trx_prz - portf_prz) / portf_prz

test_that("realized_value works (zero quantity)", {
  expect_equal(realized_value(0, 0, 0, 0, "B", allow_short = TRUE),
  						 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	expect_equal(realized_value(0, 0, 0, 0, "S", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})

test_that("realized_value works (long)", {
	# paper loss
	expect_equal(realized_value(2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	# paper gain
	expect_equal(realized_value(2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	# realized gain + paper gain
	expect_equal(realized_value(2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(2, 15, -2, 10, "S", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(2, 10, -2, 15, "S", allow_short = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})

test_that("realized_value works (short)", {
	# paper loss
	expect_equal(realized_value(-2, 15, -1, 10, "S", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = -(10 - 15) / 15, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(-2, 10, -1, 15, "S", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = -(15 - 10) / 10))
	# realized loss + paper loss
	expect_equal(realized_value(-2, 15, 1, 10, "B", allow_short = TRUE),
							 c("RG_value" = -(10 - 15) / 15, "RL_value" = 0, "PG_value" = -(10 - 15) / 15, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(-2, 10, 1, 15, "B", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = -(15 - 10) / 10, "PG_value" = 0, "PL_value" = -(15 - 10) / 10))
	# realized loss
	expect_equal(realized_value(-2, 15, 2, 10, "B", allow_short = TRUE),
							 c("RG_value" = -(10 - 15) / 15, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(-2, 10, 2, 15, "B", allow_short = TRUE),
							 c("RG_value" = 0, "RL_value" = -(15 - 10) / 10, "PG_value" = 0, "PL_value" = 0))
})

test_that("realized_value works (allow_short = FALSE)", {
	# long
	# paper loss
	expect_equal(realized_value(2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	# paper gain
	expect_equal(realized_value(2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = (10 - 15) / 15))
	# realized gain + paper gain
	expect_equal(realized_value(2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = (15 - 10) / 10, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(2, 15, -2, 10, "S", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(2, 10, -2, 15, "S", allow_short = FALSE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# short
	# paper loss
	expect_equal(realized_value(-2, 15, -1, 10, "S", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(-2, 10, -1, 15, "S", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(-2, 15, 1, 10, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(-2, 10, 1, 15, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(-2, 15, 2, 10, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(-2, 10, 2, 15, "B", allow_short = FALSE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})

test_that("realized_value works (realized_only = TRUE)", {
	# allow_short = TRUE
	# long
	# paper loss
	expect_equal(realized_value(2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(2, 15, -2, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(2, 10, -2, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# short
	# paper loss
	expect_equal(realized_value(-2, 15, -1, 10, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(-2, 10, -1, 15, "S", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(-2, 15, 1, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = -(10 - 15) / 15, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(-2, 10, 1, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = -(15 - 10) / 10, "PG_value" = 0, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(-2, 15, 2, 10, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = -(10 - 15) / 15, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(-2, 10, 2, 15, "B", allow_short = TRUE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = -(15 - 10) / 10, "PG_value" = 0, "PL_value" = 0))
	#  allow_short = FALSE
	# long
	# paper loss
	expect_equal(realized_value(2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(2, 15, -2, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = (10 - 15) / 15, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(2, 10, -2, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = (15 - 10) / 10, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# short
	# paper loss
	expect_equal(realized_value(-2, 15, -1, 10, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# paper gain
	expect_equal(realized_value(-2, 10, -1, 15, "S", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss + paper loss
	expect_equal(realized_value(-2, 15, 1, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized gain + paper gain
	expect_equal(realized_value(-2, 10, 1, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized loss
	expect_equal(realized_value(-2, 15, 2, 10, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
	# realized  gain
	expect_equal(realized_value(-2, 10, 2, 15, "B", allow_short = FALSE, realized_only = TRUE),
							 c("RG_value" = 0, "RL_value" = 0, "PG_value" = 0, "PL_value" = 0))
})
