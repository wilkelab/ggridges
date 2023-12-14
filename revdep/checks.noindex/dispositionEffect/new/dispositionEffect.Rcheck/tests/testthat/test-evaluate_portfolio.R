context("evaluate_portfolio")

p <- data.frame("asset" = LETTERS[1:6], "quantity" = c(0, 2, 2, 2, 2, NA), "price" = c(0, 15, 10, 15, 10, NA))
m <- data.frame("asset" = LETTERS[1:6], "price" = c(5, 10, 15, 10, 15, 5))

test_that("evaluate_portfolio works", {
	expect_equal(evaluate_portfolio(p, m), 0)
	expect_equal(evaluate_portfolio(p[c(1, 2, 4), ], m), -20)
	expect_equal(evaluate_portfolio(p[c(1, 3, 5), ], m), 20)
})
