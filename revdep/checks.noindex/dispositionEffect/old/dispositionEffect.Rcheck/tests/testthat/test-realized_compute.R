context("realized_compute's arguments")

pt_dtt <- as.POSIXct("2021-01-04 10:00:00", tz = "UTC")
p_dtt <- as.POSIXct("2021-01-05 10:00:00", tz = "UTC")
t_dtt <- as.POSIXct("2021-01-06 10:00:00", tz = "UTC")

erg <- (15 - 10) / 10
erl <- (10 - 15) / 15
d <- 14 # difftime_financial(p_dtt, t_dtt)
d0 <- 28 # difftime_financial(pt_dtt, t_dtt)

df <- data.frame("asset" = LETTERS[1:14],
								 "ptf_quantity" = c(0, 0, 2, 2, 2, 2, 2, 2, -2, -2, -2, -2, -2, -2),
								 "ptf_price"    = c(0, 0, 15, 10, 15, 10, 15, 10, 15, 10, 15, 10, 15, 10),
								 "trx_quantity" = c(0, 0, 1, 1, -1, -1, -2, -2, -1, -1, 1, 1, 2, 2),
								 "trx_price"    = c(0, 0, 10, 15, 10, 15, 10, 15, 10, 15, 10, 15, 10, 15),
								 "trx_type"     = c("B", "S", "B", "B", "S", "S", "S", "S", "S", "S", "B", "B", "B", "B"))
res <- data.frame("asset" = LETTERS[1:14],
									"RG_count"    = c(0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0),
									"RL_count"    = c(0, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1),
									"PG_count"    = c(0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0),
									"PL_count"    = c(0, 0, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 0, 0),
									"RG_total"    = c(0, 0, 0, 0, 0, 1, 0, 2, 0, 0, 1, 0, 2, 0),
									"RL_total"    = c(0, 0, 0, 0, 1, 0, 2, 0, 0, 0, 0, 1, 0, 2),
									"PG_total"    = c(0, 0, 0, 2, 0, 1, 0, 0, 2, 0, 1, 0, 0, 0),
									"PL_total"    = c(0, 0, 2, 0, 1, 0, 0, 0, 0, 2, 0, 1, 0, 0),
									"RG_value"    = c(0, 0, 0, 0, 0, erg, 0, erg, 0, 0, -erl, 0, -erl, 0),
									"RL_value"    = c(0, 0, 0, 0, erl, 0, erl, 0, 0, 0, 0, -erg, 0, -erg),
									"PG_value"    = c(0, 0, 0, erg, 0, erg, 0, 0, -erl, 0, -erl, 0, 0, 0),
									"PL_value"    = c(0, 0, erl, 0, erl, 0, 0, 0, 0, -erg, 0, -erg, 0, 0),
									"RG_duration" = c(0, 0, 0, 0, 0, d0, 0, d0, 0, 0, d0, 0, d0, 0),
									"RL_duration" = c(0, 0, 0, 0, d0, 0, d0, 0, 0, 0, 0, d0, 0, d0),
									"PG_duration" = c(0, 0, 0, d, 0, d, 0, 0, d, 0, d, 0, 0, 0),
									"PL_duration" = c(0, 0, d, 0, d, 0, 0, 0, 0, d, 0, d, 0, 0))

test_that("realized_compute works (method = count)", {
  expect_equal(
  	dplyr::bind_rows(
  		purrr::pmap(
  			list(df$ptf_quantity, df$ptf_price, df$trx_quantity, df$trx_price, df$trx_type, "transaction_asset" = df$asset),
  			realized_compute,
  			pt_dtt, p_dtt, t_dtt, allow_short = TRUE, method = "count"
  		)
  	),
  	res[, c(1, 2:5)])
})

test_that("realized_compute works (method = total)", {
	expect_equal(
		dplyr::bind_rows(
			purrr::pmap(
				list(df$ptf_quantity, df$ptf_price, df$trx_quantity, df$trx_price, df$trx_type, "transaction_asset" = df$asset),
				realized_compute,
				pt_dtt, p_dtt, t_dtt, allow_short = TRUE, method = "total"
			)
		),
		res[, c(1, 6:9)])
})

test_that("realized_compute works (method = value)", {
	expect_equal(
		dplyr::bind_rows(
			purrr::pmap(
				list(df$ptf_quantity, df$ptf_price, df$trx_quantity, df$trx_price, df$trx_type, "transaction_asset" = df$asset),
				realized_compute,
				pt_dtt, p_dtt, t_dtt, allow_short = TRUE, method = "value"
			)
		),
		res[, c(1, 10:13)])
})

test_that("realized_compute works (method = duration)", {
	expect_equal(
		dplyr::bind_rows(
			purrr::pmap(
				list(df$ptf_quantity, df$ptf_price, df$trx_quantity, df$trx_price, df$trx_type, "transaction_asset" = df$asset),
				realized_compute,
				pt_dtt, p_dtt, t_dtt, allow_short = TRUE, method = "duration"
			)
		),
		res[, c(1, 14:17)])
})

test_that("realized_compute works (method = all)", {
	expect_equal(
		dplyr::bind_rows(
			purrr::pmap(
				list(df$ptf_quantity, df$ptf_price, df$trx_quantity, df$trx_price, df$trx_type, "transaction_asset" = df$asset),
				realized_compute,
				pt_dtt, p_dtt, t_dtt, allow_short = TRUE, method = "all"
			)
		),
		res)
})
