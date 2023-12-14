test_that("dirac distribution works", {
  set.seed(1337L)
  d0 <- dist_dirac(0)
  x <- d0$sample(100)

  expect_identical(d0$get_type(), "discrete")
  expect_identical(x, rep_len(0, length(x)))
  expect_identical(d0$density(c(0, 1)), c(1, 0))
  expect_identical(d0$density(c(0, 1), log = TRUE), c(0, -Inf))
  expect_identical(d0$probability(c(-1, 0, 1)), c(0, 1, 1))
  expect_identical(d0$probability(c(-1, 0, 1), lower.tail = FALSE), c(1, 0, 0))
  expect_identical(d0$probability(c(-1, 0, 1), log.p = TRUE), c(-Inf, 0, 0))
  expect_identical(
    d0$probability(c(-1, 0, 1), lower.tail = FALSE, log.p = TRUE),
    c(0, -Inf, -Inf)
  )
  expect_identical(d0$quantile(c(0, 0.5, 1)), rep(0, 3))
  expect_identical(d0$quantile(c(0, 0.5, 1), lower.tail = FALSE), rep(0, 3))
  expect_identical(d0$quantile(c(-Inf, -log(2), 0), log.p = TRUE), rep(0, 3))
  expect_identical(
    d0$quantile(c(-Inf, -log(2), 0), lower.tail = FALSE, log.p = TRUE),
    rep(0, 3)
  )
  expect_identical(d0$is_in_support(c(0, 1)), c(TRUE, FALSE))
  expect_tf_logdensity(d0, list(point = 0.0), c(-1.0, 0.0, 1.0))
  expect_tf_logprobability(
    d0, list(point = 0.0),
    c(-Inf, -1.0, -1.0, -1.0, 0.0, 0.0, 0.0, -0.5, 0.0, 0.5, 0.0, -Inf),
    c(0.0, -0.5, 0.0, 0.5, -Inf, 0.0, Inf, 1.0, 1.0, 1.0, Inf, Inf)
  )

  skip_if_no_tensorflow()
  tf_is_d <- d0$tf_is_discrete_at()
  expect_equal(
    as.logical(tf_is_d(
      x = keras::k_constant(c(0.0, 1.0)),
      args = list(point = keras::k_constant(0.0))
    )),
    c(TRUE, FALSE)
  )
})
