context("disposition_effect")

test_that("disposition_effect works", {
	expect_equal(disposition_effect(1, 1, .5, .5), 0)
	expect_equal(disposition_effect(c(1, 2), c(2, 1), c(4, 8), c(8, 4)), c(0, 0))
})

test_that("disposition_difference works", {
	expect_equal(disposition_difference(1, 1), 0)
	expect_equal(disposition_difference(c(1, 2), c(2, 1)), c(-1, 1))
})

res <- data.frame(
	"investor" = "4273N",
	"asset" = c("ACO", "AST"),
	"DE_count" = c(0.1428571, -1),
	"DE_total" = c(0.04094631, -1),
	"DE_value" = c(0.22469836, -1),
	"DD_duration" = c(439.9000, -165.1167)
)

test_that("disposition_compute works", {
	expect_equal(disposition_compute(portfolio_results)[1:2, ], res, tolerance = 0.00001)
	expect_equal(
		disposition_compute(portfolio_results[1:2, ], aggregate_fun = mean, na.rm = TRUE),
		cbind(data.frame("investor" = res$investor[1]), t(colMeans(res[3:6], na.rm = TRUE))),
		tolerance = 0.00001
	)
	expect_error(disposition_compute(portfolio_results[, 1:5]))
})

test_that("disposition_compute_ts works", {
	expect_equal(disposition_compute_ts(portfolio_results)[1:2, ], res[, c(2, 3, 5)], tolerance = 0.00001)
	expect_equal(
		disposition_compute_ts(portfolio_results[1:2, ], aggregate_fun = mean, na.rm = TRUE),
		as.data.frame(t(colMeans(res[c(3, 5)], na.rm = TRUE))),
		tolerance = 0.00001
	)
	expect_error(disposition_compute_ts(portfolio_results[, 1:5]))
})

test_that("disposition_summary works", {
	expect_type(disposition_summary(portfolio_results), "list")
})

test_that("disposition_summary_ts works", {
	expect_type(disposition_summary_ts(portfolio_results_ts), "list")
})
