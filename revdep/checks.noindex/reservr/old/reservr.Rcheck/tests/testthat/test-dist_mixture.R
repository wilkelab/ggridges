test_that("test dist_mixture", {
  set.seed(1337L)

  dist <- dist_mixture(
    list(
      dist_dirac(0.0),
      dist_exponential()
    )
  )

  params <- list(
    dists = list(
      list(),
      list(rate = 1.0)
    ),
    probs = list(0.1, 0.9)
  )

  x <- dist$sample(100L, with_params = params)

  expect_silent(fit(dist, x))
  expect_identical(dist$get_type(), "mixed")
  expect_length(dist$get_components(), 2L)

  expect_density(
    dist,
    function(x, log = FALSE, ...) {
      params <- list(...)
      dens <- ifelse(
        x == 0.0,
        params$probs[[1L]],
        params$probs[[2L]] * dexp(x, rate = params$dists[[2L]]$rate)
      )
      if (log) log(dens) else dens
    },
    params,
    x
  )

  expect_probability(
    dist,
    function(q, log.p = FALSE, lower.tail = TRUE, ...) {
      params <- list(...)
      pr <- params$probs[[1L]] + params$probs[[2L]] *
        pexp(q, rate = params$dists[[2L]]$rate)

      if (!lower.tail) pr <- 1 - pr
      if (log.p) pr <- log(pr)
      pr
    },
    params,
    x
  )

  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, Inf)

  expect_iprobability(dist, params, x, x + 1.0)
  expect_iprobability(dist, params, 0, x)
  expect_iprobability(dist, params, x, Inf)

  expect_tf_fit(dist, params, I_POSITIVE_REALS)
})
