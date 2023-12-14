context("closest_market_price")

m <- data.frame(
	"asset" = c("A", "A", "A", "B", "B", "B"),
	"datetime" = as.POSIXct(c("2021-01-01 10:00:00", "2021-01-01 10:45:00", "2021-02-01 10:00:00",
														"2021-01-01 10:00:00", "2021-01-01 10:45:00", "2021-02-01 10:00:00")),
	"price" = c(10, 100, 150, 2, 0.5, 1000)
)

test_that("closest_market_price works", {
	expect_equivalent(closest_market_price("A", as.POSIXct("2021-01-01 10:40:00"), m), m[1, ])
	expect_equivalent(closest_market_price("A", as.POSIXct("2021-01-01 10:45:00"), m), m[2, ])
	expect_equivalent(closest_market_price("B", as.POSIXct("2021-02-01 10:00:00"), m), m[6, ])
	expect_equivalent(closest_market_price("B", as.POSIXct("2022-01-01 10:45:00"), m), m[6, ])
	expect_equal(closest_market_price("A", as.POSIXct("2021-01-01 10:40:00"), m, price_only = TRUE), 10)
})
