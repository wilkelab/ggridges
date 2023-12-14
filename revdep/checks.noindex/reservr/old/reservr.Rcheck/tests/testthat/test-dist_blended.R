test_that("test dist_blended", {
  set.seed(1337L)

  dist <- dist_blended(
    list(
      dist_exponential(),
      dist_genpareto()
    )
  )

  params <- list(
    dists = list(
      list(rate = 2.0),
      list(u = 1.5, sigmau = 1.0, xi = 0.2)
    ),
    breaks = list(1.5),
    bandwidths = list(0.3),
    probs = list(0.9, 0.1)
  )

  x <- dist$sample(100L, with_params = params)

  dist$default_params$breaks <- params$breaks
  dist$default_params$bandwidths <- params$bandwidths
  expect_identical(dist$get_type(), "continuous")
  expect_length(dist$get_components(), 2L)

  p_lower <- pexp(params$breaks[[1L]], rate = params$dists[[1L]]$rate)

  x_lhs <- x[x < params$breaks[[1L]] - params$bandwidths[[1L]]]
  x_rhs <- x[x > params$breaks[[1L]] + params$bandwidths[[1L]]]

  # Necessary for compiled tests: params must contain all parameters in a fixed sequence and only those that are free.
  free_params <- params[c("dists", "probs")]

  expect_density(
    dist,
    function(x, log = FALSE, ...) {
      if (log) {
        log(list(...)$probs[[1L]]) +
          dexp(x, rate = list(...)$dists[[1L]]$rate, log = TRUE) -
          pexp(params$breaks[[1L]], rate = list(...)$dists[[1L]]$rate, log = TRUE)
      } else {
        list(...)$probs[[1L]] *
          dexp(x, rate = list(...)$dists[[1L]]$rate) /
          pexp(params$breaks[[1L]], rate = list(...)$dists[[1L]]$rate)
      }
    },
    free_params,
    x_lhs
  )

  expect_density(
    dist,
    function(x, log = FALSE, ...) {
      params_gpd <- list(...)$dists[[2L]]
      if (log) {
        log(list(...)$probs[[2L]]) +
          do.call(dgpd, c(list(x = x, log = TRUE), params_gpd)) -
          do.call(pgpd, c(list(q = params$breaks[[1L]], lower.tail = FALSE, log = TRUE), params_gpd))
      } else {
        list(...)$probs[[2L]] *
          do.call(dgpd, c(list(x = x), params_gpd)) /
          do.call(pgpd, c(list(q = params$breaks[[1L]], lower.tail = FALSE), params_gpd))
      }
    },
    free_params,
    x_rhs
  )

  expect_probability(
    dist,
    function(q, log.p = FALSE, lower.tail = TRUE, ...) {
      pr <- list(...)$probs[[1L]] *
        pexp(q, rate = list(...)$dists[[1L]]$rate) /
        pexp(params$breaks[[1L]], rate = list(...)$dists[[1L]]$rate)
      if (!lower.tail) pr <- 1 - pr
      if (log.p) pr <- log(pr)
      pr
    },
    free_params,
    x_lhs
  )

  expect_probability(
    dist,
    function(q, log.p = FALSE, lower.tail = TRUE, ...) {
      params_gpd <- list(...)$dists[[2L]]
      pr <- (
        list(...)$probs[[1L]] +
          list(...)$probs[[2L]] *
          do.call(pgpd, c(list(q = q), params_gpd)) /
            do.call(pgpd, c(list(q = params$breaks[[1L]], lower.tail = FALSE), params_gpd))
      )
      if (!lower.tail) pr <- 1 - pr
      if (log.p) pr <- log(pr)
      pr
    },
    free_params,
    x_rhs
  )

  expect_identical(
    dist$is_in_support(x, with_params = params),
    rep_len(TRUE, length(x))
  )

  expect_tf_logdensity(dist, params, x)
  expect_tf_logprobability(dist, params, x, x + 1.0)
  expect_tf_logprobability(dist, params, 0, x)
  expect_tf_logprobability(dist, params, x, Inf)
  expect_iprobability(dist, free_params, x, x + 1.0)
  expect_iprobability(dist, free_params, 0, x)
  expect_iprobability(dist, free_params, x, Inf)

  skip_on_cran()
  expect_silent(fit(dist, x))

  expect_tf_fit(dist, free_params, I_POSITIVE_REALS, global_fit_args = free_params)
})

test_that("blending works for discrete distributions", {
  dist <- dist_blended(
    dists = list(dist_dirac(1), dist_dirac(2), dist_dirac(3)),
    breaks = list(1.5, 2.5),
    bandwidths = list(0.3, 0.3),
    probs = as.list(c(1, 1, 1) / 3)
  )

  expect_identical(dist$get_type(), "discrete")
  expect_length(dist$get_components(), 3L)

  x <- dist$sample(100L)
  equivalent_dist <- dist_discrete(3, probs = as.list(c(1, 1, 1) / 3))
  expect_density(
    dist,
    function(x, ..., log = FALSE) equivalent_dist$density(x, log = log),
    list(NULL),
    x
  )
  # FIXME Evaluation error: non-conformable arrays.
  #> expect_probability(
  #>   dist,
  #>   function(q, ..., lower.tail = TRUE, log.p = FALSE)
  #>     equivalent_dist$probability(q, lower.tail = lower.tail, log.p = log.p),
  #>   list(NULL),
  #>   x
  #> )

  expect_identical(dist$is_in_support(x), rep_len(TRUE, length(x)))
  # TODO enable once > 2 components are supported
  #> expect_tf_logdensity(dist, list(), x)
  #> expect_tf_logprobability(dist, list(), x, x + 1.0)
  #> expect_tf_logprobability(dist, list(), 0, x)
  #> expect_tf_logprobability(dist, list(), x, Inf)
  expect_iprobability(dist, list(NULL), x, x + 1.0)
  expect_iprobability(dist, list(NULL), 0, x)
  expect_iprobability(dist, list(NULL), x, Inf)
})
