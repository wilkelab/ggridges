test_that("test weighted_moments", {
  x <- rexp(100)
  w <- runif(100)

  expect_length(weighted_moments(rexp(100)), 2L)

  expect_equal(weighted_moments(x, w, n = 1L), weighted.mean(x, w))
  expect_equal(
    weighted_moments(x, w, n = 1L, center = FALSE),
    weighted.mean(x, w)
  )
  expect_gt(
    weighted_moments(x, w, center = FALSE)[2L],
    weighted_moments(x, w)[2L]
  )
})

test_that("test weighted_quantile", {
  expect_equal(weighted_median(1:6), median(1:6))
  expect_equal(weighted_median(1:3, c(1, 4, 9)), 3L)
  expect_equal(weighted_median(1:3, c(9, 4, 1)), 1L)

  expect_equal(
    weighted_quantile(1:3, c(1, 4, 9), seq(0.0, 1.0, by = 0.25)),
    unname(quantile(rep(1:3, c(1, 4, 9)), seq(0.0, 1.0, by = 0.25)))
  )
})

test_that("test weighted_tabulate", {
  expect_equal(
    weighted_tabulate(c(1, 1, 2)),
    c(2, 1)
  )

  expect_equal(
    weighted_tabulate(c(1, 1, 2), nbins = 3L),
    c(2, 1, 0)
  )

  expect_equal(
    weighted_tabulate(c(1, 1, 2), w = c(0.5, 0.5, 1), nbins = 3L),
    c(1, 1, 0)
  )
})
