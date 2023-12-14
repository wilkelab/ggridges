test_that("test dist_trunc", {
  set.seed(1337L)
  dist <- dist_trunc(dist_exponential(), min = 1.0, max = 3.0)
  params <- list(dist = list(rate = 0.5))
  x <- dist$sample(100L, with_params = params)

  # Force rejection sampling algorithm
  caps <- dist$default_params$dist$.__enclos_env__$private$.caps
  dist$default_params$dist$.__enclos_env__$private$.caps <- "sample"
  x <- c(x, dist$sample(100L, with_params = params))
  dist$default_params$dist$.__enclos_env__$private$.caps <- caps

  expect_silent(fit(dist, x))
  expect_identical(dist$get_type(), "continuous")
  expect_density(dist, function(x, log = FALSE, dist) {
    d <- dexp(x, rate = dist$rate) /
      diff(pexp(c(1.0, 3.0), rate = dist$rate))
    if (log) d <- log(d)
    d
  }, params, x)
  expect_probability(dist, function(q, log.p = FALSE, lower.tail = TRUE, dist) {
    p <- (pexp(q, rate = dist$rate) - pexp(1.0, rate = dist$rate)) /
      diff(pexp(c(1.0, 3.0), rate = dist$rate))
    if (!lower.tail) p <- 1 - p
    pmax(0, pmin(1, p))
    if (log.p) p <- log(p)
    p
  }, params, x)
  expect_quantile(dist, function(p, log.p = FALSE, lower.tail = TRUE, dist) {
    if (log.p) p <- exp(p)
    if (!lower.tail) p <- 1 - p
    pt <- pexp(1.0, rate = dist$rate) +
      p * diff(pexp(c(1.0, 3.0), rate = dist$rate))
    qexp(pt, rate = dist$rate)
  }, params)
  expect_identical(dist$is_in_support(x), rep_len(TRUE, length(x)))
  # TODO implement gradients
  # expect_diff_density(dist, x, params)
  # expect_diff_density(dist, x, list(dist = list(rate = 4.0)))
  # expect_diff_probability(dist, x, params)
  # expect_diff_probability(dist, x, list(dist = list(rate = 4.0)))
  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)

  expect_iprobability(dist, params, x, x + 1.0)
  expect_iprobability(dist, params, 0, x)
  expect_iprobability(dist, params, x, Inf)

  expect_tf_fit(dist, params, interval(1.0, 3.0))
})
