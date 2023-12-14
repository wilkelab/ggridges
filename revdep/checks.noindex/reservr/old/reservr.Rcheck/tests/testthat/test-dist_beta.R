test_that("beta distribution works", {
  set.seed(1337L)
  dist <- dist_beta(ncp = 0.0)
  params <- list(shape1 = 2.0, shape2 = 3.0)
  x <- dist$sample(100L, with_params = params)

  expect_silent(fit_dist(dist, x))
  expect_identical(dist$get_type(), "continuous")
  expect_density(dist, dbeta, params, x)
  expect_probability(dist, pbeta, params, x)
  expect_quantile(dist, qbeta, params)
  expect_identical(dist$is_in_support(x), rep_len(TRUE, length(x)))
  # TODO enable once diff_* is implemented
  # expect_diff_density(dist, x, params)
  # expect_diff_probability(dist, x, params)
  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 0.1)
  expect_tf_logprobability(dist, params, x - 0.1, x)

  expect_tf_fit(dist, params, I_UNIT_INTERVAL)
})
