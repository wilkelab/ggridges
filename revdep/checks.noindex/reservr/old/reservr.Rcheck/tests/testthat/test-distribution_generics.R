test_that("fit_dist_direct works", {
  set.seed(1444L)

  dist <- dist_blended(list(dist_normal(), dist_normal()))

  true_params <- list(
    dists = list(
      list(mean = -1, sd = 1),
      list(mean = 1, sd = 1)
    ),
    breaks = list(0),
    bandwidths = list(1),
    probs = list(0.5, 0.5)
  )

  x <- dist$sample(100, with_params = true_params)

  expect_silent(fit_result <- fit_dist(dist, x))
  expect_silent(direct_fit_result <- fit_dist_direct(dist, x))
  # because fit_dist.BlendedDistribution falls back to direct fitting if
  # breaks or bandwidths are free parameters.
  expect_equal(fit_result, direct_fit_result)
})
