test_that("test quantile.Distribution", {
  set.seed(2344L)

  dist <- dist_normal(mean = 0.0, sd = 1.0)
  x <- c(0, -Inf, Inf, dist$sample(100L))
  p <- dist$probability(x)
  expect_equal(quantile(dist, p), x)
  # force numerical quantile estimation
  dist$.__enclos_env__$private$.caps <- c("probability", "density")
  expect_equal(quantile(dist, p, start = 0.0), x, tolerance = 1e-4)
})
