test_that("empirical distribution works", {
  set.seed(1337L)
  sample <- runif(10)
  dist <- dist_empirical(sample = sample)
  dist2 <- dist_empirical(sample = sample, positive = TRUE)

  x <- dist$sample(100L)
  x2 <- dist2$sample(100L)

  expect_true(dist$is_discrete())
  expect_true(dist2$is_discrete())
  expect_setequal(sample, x)
  expect_setequal(sample, x2)
  expect_true(all(dist$is_in_support(x)))
  expect_true(all(dist2$is_in_support(x2)))

  expect_length(dist$density(sample), length(sample))
  expect_true(all(dist$density(sample) > 0.0))
  expect_true(all(dist2$density(sample, log = TRUE) > -Inf))
  expect_true(all(dist$density(sample) > 0.0))
  expect_true(all(dist2$density(sample, log = TRUE) > -Inf))

  expect_equal(dist$probability(max(sample)), 1.0)
  expect_equal(dist2$probability(max(sample)), 1.0)
  expect_equal(dist$probability(min(sample)), 0.1)
  expect_equal(dist2$probability(min(sample)), 0.1)
})
