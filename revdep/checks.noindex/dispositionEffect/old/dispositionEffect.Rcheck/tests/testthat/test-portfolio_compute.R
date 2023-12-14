context("portfolio_compute")

test_that("portfolio_compute works (arg method)", {
	# skip_on_cran()
	expect_equal(portfolio_compute(investor, marketprices, method = "none"), portfolio_results[, c(1:5)])
	expect_equal(portfolio_compute(investor, marketprices, method = "none", progress = TRUE), portfolio_results[, c(1:5)])
	expect_equal(portfolio_compute(investor, marketprices, method = "count"), portfolio_results[, c(1:5, 6:9)])
	expect_equal(portfolio_compute(investor, marketprices, method = "total"), portfolio_results[, c(1:5, 10:13)])
	expect_equal(portfolio_compute(investor, marketprices, method = "value"), portfolio_results[, c(1:5, 14:17)])
	expect_equal(portfolio_compute(investor, marketprices, method = "duration"), portfolio_results[, c(1:5, 18:21)], tolerance = 0.001)
	expect_equal(portfolio_compute(investor, marketprices, method = "all"), portfolio_results, tolerance = 0.001)
})

test_that("portfolio_compute works (arg allow_short)", {
	# skip_on_cran()
	expect_type(portfolio_compute(investor, marketprices, allow_short = FALSE), "list")
})

test_that("portfolio_compute works (arg exact_market_prices)", {
	# skip_on_cran()
	expect_equal(
		portfolio_compute(investor, marketprices, exact_market_prices = TRUE),
		portfolio_compute(investor, marketprices, exact_market_prices = FALSE)
	)
})

test_that("portfolio_compute works (arg time_threshold)", {
	# skip_on_cran()
	expect_equal(portfolio_compute(investor, marketprices, time_threshold = "5 mins"), portfolio_results[, 1:9], tolerance = 0.001)
	expect_type(portfolio_compute(investor, marketprices, time_threshold = "30 days"), "list")
})

test_that("portfolio_compute works (arg portfolio_driven_DE)", {
	# skip_on_cran()
	expect_type(portfolio_compute(investor, marketprices, method = "all", portfolio_driven_DE = TRUE), "list")
})

test_that("portfolio_compute works (arg time_series_DE)", {
	# skip_on_cran()
	expect_type(portfolio_compute(investor, marketprices, time_series_DE = TRUE), "list")
	expect_type(portfolio_compute(investor, marketprices, method = "value", time_series_DE = TRUE), "list")
	expect_type(portfolio_compute(investor, marketprices, method = "all", time_series_DE = TRUE), "list")
})

test_that("portfolio_compute works (arg assets_time_series_DE)", {
	# skip_on_cran()
	expect_type(
		portfolio_compute(investor, marketprices, time_series_DE = TRUE, assets_time_series_DE = "ACO"),
		"list"
	)
	expect_type(
		portfolio_compute(investor, marketprices, method = "value", time_series_DE = TRUE, assets_time_series_DE = "ACO"),
		"list"
	)
	expect_type(
		portfolio_compute(investor, marketprices, method = "all", time_series_DE = TRUE, assets_time_series_DE = "ACO"),
		"list"
	)
})

investor_error <- investor
names(investor_error) <- c("investors", "types", "asset", "quantity", "price", "datetime")
investor_error2 <- investor
investor_error2$type[1] <- "Buy"
marketprices_error <- marketprices
names(marketprices_error) <- c("assets", "datetime", "price")
marketprices_error2 <- marketprices
marketprices_error2 <- marketprices_error2[-c(11:60), ]

test_that("portfolio_compute works (initial checks)", {
	# skip_on_cran()
	expect_error(portfolio_compute(investor_error, marketprices))
	expect_error(portfolio_compute(investor, marketprices_error))
	expect_error(portfolio_compute(investor_error2, marketprices))
	expect_error(portfolio_compute(investor, marketprices, method = "new"))
	expect_error(portfolio_compute(investor, marketprices, time_threshold = "1 month"))
	expect_warning(portfolio_compute(investor, marketprices, method = "total", time_series_DE = TRUE))
	expect_warning(portfolio_compute(investor, marketprices, method = "duration", time_series_DE = TRUE))
	expect_warning(portfolio_compute(investor, marketprices, method = "none", time_series_DE = TRUE))
	expect_warning(portfolio_compute(investor, marketprices, portfolio_driven_DE = TRUE, time_series_DE = TRUE))
	expect_warning(portfolio_compute(investor, marketprices_error2))
	expect_warning(portfolio_compute(investor, marketprices, assets_time_series_DE = "AC"))
	expect_error(portfolio_compute(investor, marketprices, time_series_DE = TRUE, assets_time_series_DE = "AC"))
})
