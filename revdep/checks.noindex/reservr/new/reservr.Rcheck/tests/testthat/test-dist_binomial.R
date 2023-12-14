test_that("binomial distribution works", {
  set.seed(1337L)
  dist <- dist_binomial()
  params <- list(size = 20L, prob = 0.5)
  x <- dist$sample(100L, with_params = params)

  dist$default_params$size <- fit_dist_start(dist, x)$size
  expect_silent(fit(dist, trunc_obs(x, tmin = 0)))
  dist$default_params["size"] <- list(NULL)
  expect_identical(dist$get_type(), "discrete")
  expect_density(dist, dbinom, params, x)
  expect_probability(dist, pbinom, params, x)
  expect_quantile(dist, qbinom, params)
  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )
  expect_diff_density(dist, x, params, vars = "prob")
  expect_diff_density(dist, x, list(size = 30L, prob = 0.1), vars = "prob")
  expect_diff_density(dist, x, list(size = 10L, prob = 0.9), vars = "prob")
  expect_diff_probability(dist, x, params, vars = "prob")
  expect_diff_probability(dist, x, list(size = 30L, prob = 0.1), vars = "prob")
  expect_diff_probability(dist, x, list(size = 10L, prob = 0.9), vars = "prob")

  # TODO somehow check diff_* for size
  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, params$size)

  dist$default_params$size <- params$size
  expect_tf_fit(dist, params["prob"], interval(0L, params$size, integer = TRUE))
})
