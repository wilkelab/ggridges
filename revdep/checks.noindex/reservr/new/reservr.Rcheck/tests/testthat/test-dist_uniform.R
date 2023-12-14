test_that("uniform distribution works", {
  set.seed(1337L)
  dist <- dist_uniform()
  params <- list(min = 1, max = 4)
  x <- dist$sample(100L, with_params = params)

  expect_silent(fit(dist, x))
  expect_identical(dist$get_type(), "continuous")
  expect_density(dist, dunif, params, x)
  expect_probability(dist, punif, params, x)
  expect_quantile(dist, qunif, params)
  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )
  expect_diff_density(dist, x, params)
  expect_diff_density(dist, x, list(min = 0, max = 2))
  expect_diff_probability(dist, x, params)
  expect_diff_probability(dist, x, list(min = 0, max = 2))

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, Inf)

  expect_tf_fit(dist, params, interval(1.0, 4.0))
})
