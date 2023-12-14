test_that("discrete distribution works", {
  set.seed(1337L)
  dist <- dist_discrete(size = 4L)
  params <- list(probs = list(0.5, 0.25, 0.15, 0.1))
  x <- dist$sample(100L, with_params = params)

  expect_silent(fit(dist, trunc_obs(x, tmin = 0)))
  expect_identical(dist$get_type(), "discrete")
  expect_density(dist, function(x, log = FALSE, probs) {
    res <- as.numeric(probs)[x]
    if (log) res <- log(res)
    res
  },
  params, x)
  expect_probability(
    dist,
    function(q, lower.tail = TRUE, log.p = FALSE, probs) {
      res <- cumsum(as.numeric(probs))[q]
      if (!lower.tail) res <- 1.0 - res
      if (log.p) res <- log(res)
      res
    },
    params,
    x
  )
  expect_quantile(dist, function(p, lower.tail = TRUE, log.p = FALSE, probs) {
    pr <- c(0, cumsum(as.numeric(probs)))
    if (!lower.tail) pr <- rev(1.0 - pr)
    if (log.p) p <- exp(p)
    res <- findInterval(p, pr, left.open = TRUE, all.inside = TRUE)
    if (!lower.tail) res <- length(probs) - res + 1L
    res
  }, params)
  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, length(params$probs))

  expect_iprobability(dist, params, x, x + 1.0)
  expect_iprobability(dist, params, 0, x)
  expect_iprobability(dist, params, x, length(params$probs))

  expect_tf_fit(dist, params, interval(1L, length(params$probs), integer = TRUE))
})
