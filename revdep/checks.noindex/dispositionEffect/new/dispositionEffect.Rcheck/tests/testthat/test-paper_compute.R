context("paper_compute's arguments")

df <- data.frame("asset" = LETTERS[1:5],
								 "quantity" = c(0, 2, 2, -2, -2),
								 "price" = c(0, 15, 10, 15, 10),
								 "market_price" = c(0, 10, 15, 10, 15))
res <- data.frame("asset" = LETTERS[1:5],
									"RG_count" = c(0, 0, 0, 0, 0),
									"RL_count" = c(0, 0, 0, 0, 0),
									"PG_count" = c(0, 0, 1, 1, 0),
									"PL_count" = c(0, 1, 0, 0, 1),
									"RG_total" = c(0, 0, 0, 0, 0),
									"RL_total" = c(0, 0, 0, 0, 0),
									"PG_total" = c(0, 0, 2, 2, 0),
									"PL_total" = c(0, 2, 0, 0, 2),
									"RG_value" = c(0, 0, 0, 0, 0),
									"RL_value" = c(0, 0, 0, 0, 0),
									"PG_value" = c(0, 0, (15 - 10) / 10, -(10 - 15) / 15, 0),
									"PL_value" = c(0, (10 - 15) / 15, 0, 0, -(15 - 10) / 10),
									"RG_duration" = c(0, 0, 0, 0, 0),
									"RL_duration" = c(0, 0, 0, 0, 0),
									"PG_duration" = c(0, 0, 14, 14, 0),
									"PL_duration" = c(0, 14, 0, 0, 14))
prev_dtt <- as.POSIXct("2021-01-04 10:00:00", tz = "UTC")
trx_dtt <- as.POSIXct("2021-01-05 10:00:00", tz = "UTC")

test_that("paper_compute works (method = count)", {
  expect_equal(paper_compute(df$quantity, df$price, df$market_price, prev_dtt, trx_dtt,
  													 df$asset, allow_short = TRUE, method = "count"),
  						 res[, c(1, 2:5)])
})

test_that("paper_compute works (method = total)", {
	expect_equal(paper_compute(df$quantity, df$price, df$market_price, prev_dtt, trx_dtt,
														 df$asset, allow_short = TRUE, method = "total"),
							 res[, c(1, 6:9)])
})

test_that("paper_compute works (method = value)", {
	expect_equal(paper_compute(df$quantity, df$price, df$market_price, prev_dtt, trx_dtt,
														 df$asset, allow_short = TRUE, method = "value"),
							 res[, c(1, 10:13)])
})

test_that("paper_compute works (method = duration)", {
	expect_equal(paper_compute(df$quantity, df$price, df$market_price, prev_dtt, trx_dtt,
														 df$asset, allow_short = TRUE, method = "duration"),
							 res[, c(1, 14:17)])
})

test_that("paper_compute works (method = all)", {
	expect_equal(paper_compute(df$quantity, df$price, df$market_price, prev_dtt, trx_dtt,
														 df$asset, allow_short = TRUE, method = "all"),
							 res)
})
