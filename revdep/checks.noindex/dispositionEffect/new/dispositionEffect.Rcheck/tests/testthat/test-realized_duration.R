context("realized_duration's arguments")

# portfolio_quantity, portfolio_price, transaction_quantity, transaction_price, transaction_type
# previous_transaction_datetime, previous_datetime, transaction_datetime,

pt_dtt <- as.POSIXct("2021-01-04 10:00:00", tz = "UTC")
p_dtt <- as.POSIXct("2021-01-05 10:00:00", tz = "UTC")
t_dtt <- as.POSIXct("2021-01-06 10:00:00", tz = "UTC")

dtt_diff <- 14 # difftime_financial(p_dtt, t_dtt)
dtt_diff0 <- 28 # difftime_financial(pt_dtt, t_dtt)

test_that("realized_duration works (zero quantity)", {
  expect_equal(realized_duration(0, 0, 0, 0, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
  						 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	expect_equal(realized_duration(0, 0, 0, 0, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})

test_that("realized_duration works (long)", {
	# paper loss
	expect_equal(realized_duration(2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# paper gain
	expect_equal(realized_duration(2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# realized gain + paper gain
	expect_equal(realized_duration(2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(2, 15, -2, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(2, 10, -2, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})

test_that("realized_duration works (short)", {
	# paper loss
	expect_equal(realized_duration(-2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(-2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# realized loss + paper loss
	expect_equal(realized_duration(-2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(-2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# realized loss
	expect_equal(realized_duration(-2, 15, 2, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(-2, 10, 2, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
})

test_that("realized_duration works (allow_short = FALSE)", {
	# long
	# paper loss
	expect_equal(realized_duration(2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# paper gain
	expect_equal(realized_duration(2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = dtt_diff))
	# realized gain + paper gain
	expect_equal(realized_duration(2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = dtt_diff, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(2, 15, -2, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(2, 10, -2, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# short
	# paper loss
	expect_equal(realized_duration(-2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(-2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(-2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(-2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(-2, 15, 2, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(-2, 10, 2, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})

test_that("realized_duration works (realized_only = TRUE)", {
	# allow_short = TRUE
	# long
	# paper loss
	expect_equal(realized_duration(2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(2, 15, -2, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(2, 10, -2, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# short
	# paper loss
	expect_equal(realized_duration(-2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(-2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(-2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(-2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(-2, 15, 2, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(-2, 10, 2, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = TRUE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	#  allow_short = FALSE
	# long
	# paper loss
	expect_equal(realized_duration(2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(2, 15, -2, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = dtt_diff0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(2, 10, -2, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = dtt_diff0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# short
	# paper loss
	expect_equal(realized_duration(-2, 15, -1, 10, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# paper gain
	expect_equal(realized_duration(-2, 10, -1, 15, "S", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss + paper loss
	expect_equal(realized_duration(-2, 15, 1, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized gain + paper gain
	expect_equal(realized_duration(-2, 10, 1, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized loss
	expect_equal(realized_duration(-2, 15, 2, 10, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
	# realized  gain
	expect_equal(realized_duration(-2, 10, 2, 15, "B", pt_dtt, p_dtt, t_dtt, allow_short = FALSE, realized_only = TRUE),
							 c("RG_duration" = 0, "RL_duration" = 0, "PG_duration" = 0, "PL_duration" = 0))
})
