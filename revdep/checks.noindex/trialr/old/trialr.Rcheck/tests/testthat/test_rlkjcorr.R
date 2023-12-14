
test_that('rlkjcorr correlations with eta=1 are approximately uniform', {
  breaks <- seq(-1, 1, length = 21)
  set.seed(123)
  samp <- rlkjcorr(100000, 2, eta = 1)
  dens <- hist(samp[, 2, 1], breaks = breaks, plot = FALSE)$density
  expect_lt(max(dens) - min(dens), 0.05)
})

test_that('rlkjcorr correlations with eta=10 are distributed as expected', {
  breaks <- seq(-1, 1, length = 21)
  set.seed(123)
  samp <- rlkjcorr(100000, 2, eta = 10)
  dens <- hist(samp[, 2, 1], breaks = breaks, plot = FALSE)$density
  # We expect that:
  # ...Distribution is symmetric
  expect_lt(max(dens[1:10] - dens[20:11]), 0.05)
  # ...The centre is way more common than the outer
  expect_gt(dens[11] - dens[1], 1.5)
  # ...An event further into the tail is less common
  expect_gt(dens[5] - dens[1], 0)
})

test_that('rlkjcorr correlations with eta=0.1 are distributed as expected', {
  breaks <- seq(-1, 1, length = 21)
  set.seed(123)
  samp <- rlkjcorr(100000, 2, eta = 1 / 10)
  dens <- hist(samp[, 2, 1], breaks = breaks, plot = FALSE)$density
  # We expect that:
  # ...Distribution is symmetric
  expect_lt(max(dens[1:10] - dens[20:11]), 0.05)
  # ...The centre is way less common than the outer
  expect_gt(dens[1] - dens[11], 3)
  # ...An event further into the tail is more common
  expect_gt(dens[5] - dens[11], 0)
})
