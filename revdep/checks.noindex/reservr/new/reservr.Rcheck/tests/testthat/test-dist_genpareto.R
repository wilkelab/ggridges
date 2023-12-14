test_that("generalized pareto distribution works", {
  set.seed(1337L)

  genpareto <- dist_genpareto()
  test_grid <- expand.grid(
    u = c(1.0, -1.0),
    sigmau = c(1.0, 2.0),
    xi = c(-1.0, 0.0, 0.5)
  )
  xparams <- lapply(seq_len(nrow(test_grid)), function(i) {
    list(u = test_grid$u[i], sigmau = test_grid$sigmau[i], xi = test_grid$xi[i])
  })

  x1 <- genpareto$sample(100L, with_params = list(u = 0, sigmau = 1, xi = 1))
  x2 <- genpareto$sample(100L, with_params = list(u = 0, sigmau = 1, xi = 0))
  x3 <- genpareto$sample(100L, with_params = list(u = 0, sigmau = 1, xi = -1))

  my_pgpd <- function(q, u, sigmau, xi, lower.tail = TRUE, log.p = FALSE) {
    res <- evmix::pgpd(q = q, u = u, sigmau = sigmau, xi = xi,
                       lower.tail = lower.tail)
    if (log.p) log(res) else res
  }

  my_qgpd <- function(p, u, sigmau, xi, lower.tail = TRUE, log.p = FALSE) {
    if (log.p) p <- exp(p)
    evmix::qgpd(p = p, u = u, sigmau = sigmau, xi = xi, lower.tail = lower.tail)
  }

  expect_identical(genpareto$get_type(), "continuous")
  for (i in seq_len(nrow(test_grid))) {
    xx <- genpareto$sample(100L, with_params = xparams[[i]])
    xx_inner <- sort(xx)[6L:94L]
    expect_density(genpareto, evmix::dgpd, xparams[[i]], xx)
    expect_probability(genpareto, my_pgpd, xparams[[i]], xx)
    expect_quantile(genpareto, my_qgpd, xparams[[i]])
    expect_identical(
      genpareto$is_in_support(xx, with_params = xparams[[!!i]]),
      rep_len(TRUE, length(xx))
    )
    expect_diff_density(genpareto, xx_inner, xparams[[i]])
    expect_diff_probability(genpareto, xx_inner, xparams[[i]])
    expect_tf_logdensity(genpareto, xparams[[i]], xx, tolerance = 1.0e-6)
    expect_tf_logprobability(genpareto, xparams[[i]], xx, xx + 1.0)
    expect_tf_logprobability(genpareto, xparams[[i]], 0, xx)
    expect_tf_logprobability(genpareto, xparams[[i]], xx, Inf)

  }
  expect_tf_fit(genpareto, list(u = 0, sigmau = 1, xi = 1), I_POSITIVE_REALS)

  expect_diff_density(genpareto, x1, xparams[[1L]]) # Test out-of-support
  expect_diff_density(genpareto, x2, xparams[[1L]])
  expect_diff_density(genpareto, x3, xparams[[1L]])
  expect_diff_probability(genpareto, x1, xparams[[1L]]) # Test out-of-support
  expect_diff_probability(genpareto, x2, xparams[[1L]])
  expect_diff_probability(genpareto, x3, xparams[[1L]])
})

test_that("constrained generalized pareto distribution works", {
  const_dist <- dist_genpareto1()
  expect_identical(const_dist$get_param_bounds()$xi, I_UNIT_INTERVAL)
})
