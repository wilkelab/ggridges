test_that("negative binomial distribution works", {
  set.seed(1337L)
  dist <- dist_negbinomial()
  params <- list(size = 5.0, mu = 10.0)
  x <- dist$sample(100L, with_params = params)

  expect_silent(fit(dist, x))
  expect_identical(dist$get_type(), "discrete")
  expect_density(dist, dnbinom, params, x)
  expect_probability(dist, pnbinom, params, x)
  expect_quantile(dist, qnbinom, params)
  expect_identical(
    dist$is_in_support(x, with_params = params), rep_len(TRUE, length(x))
  )
  expect_diff_density(dist, x, params)
  expect_diff_density(dist, x, list(size = 1.0, mu = 0.1))

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, Inf)

  expect_tf_fit(dist, params, I_NATURALS)
})
