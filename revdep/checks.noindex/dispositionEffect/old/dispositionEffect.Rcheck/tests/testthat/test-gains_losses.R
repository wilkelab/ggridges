context("gains_losses")

p <- data.frame(
	"investor" = c("INV", "INV"),
	"asset" = c("A", "B"),
	"quantity" = c(2, -2),
	"price" = c(10, 20),
	"datetime" = as.POSIXct(c("2021-01-01 10:00:00", "2021-02-01 10:00:00"))
)
m <- data.frame("asset" = c("A", "B"), "price" = c(15, 25))
res <- data.frame(
	"investor" = c("INV", "INV"),
	"asset" = c("A", "B"),
	"RG_count" = c(1, 0),
	"RL_count" = c(0, 0),
	"PG_count" = c(1, 0),
	"PL_count" = c(0, 0)
)

test_that("gains_losses works", {
	expect_type(gains_losses(p, m, "S", "A", 5, 15, as.POSIXct("2021-03-01 10:00:00"), as.POSIXct("2021-02-26 10:00:00")), "list")
	expect_type(gains_losses(p, m, "S", "A", 5, 15, as.POSIXct("2021-03-01 10:00:00"), as.POSIXct("2021-02-26 10:00:00"), time_threshold = "30 days"), "list")
	expect_equal(gains_losses(p, m, "S", "A", 5, 15, as.POSIXct("2021-03-01 10:00:00"), as.POSIXct("2021-02-26 10:00:00"), method = "count"), res)
})
