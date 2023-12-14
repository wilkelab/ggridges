test_that("test dist_translate", {
  set.seed(1337L)

  dist <- dist_translate(
    dist_exponential(),
    offset = 1.0
  )

  params <- list(dist = list(rate = 2.0))

  x <- dist$sample(100L, with_params = params)

  expect_silent(fit(dist, x))
  expect_identical(dist$get_type(), "continuous")
  expect_length(dist$get_components(), 1L)

  dist_ref <- dist_genpareto(u = 1.0, sigmau = 1 / params$dist$rate, xi = 0.0)

  expect_density(
    dist,
    function(x, log = FALSE, ...) {
      dist_ref$density(x, log = log)
    },
    params,
    x
  )

  expect_probability(
    dist,
    function(q, log.p = FALSE, lower.tail = TRUE, ...) {
      dist_ref$probability(q, log.p = log.p, lower.tail = lower.tail)
    },
    params,
    x
  )

  expect_quantile(
    dist,
    function(p, log.p = FALSE, lower.tail = TRUE, ...) {
      dist_ref$quantile(p, log.p = log.p, lower.tail = lower.tail)
    },
    params
  )

  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )

  expect_diff_density(dist, x, params)
  expect_diff_probability(dist, x, params)

  params$offset <- 1.0
  params$multiplier <- 1.0

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, Inf)

  expect_tf_fit(dist, params, interval(1.0, Inf))
})
