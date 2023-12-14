
test_that('ranBin2 works', {
  p1 <- 0.8; p2 <- 0.5
  abc <- ranBin2(10000, c(p1, p2), 2)
  expect_equal(nrow(abc), 10000)
  expect_equal(ncol(abc), 2)
  probs <- colMeans(abc)
  epsilon <- 0.01
  expect_true(abs(probs[1] - p1) < epsilon)
  expect_true(abs(probs[2] - p2) < epsilon)
  cor_mat <- cor(abc)
  expect_true(cor_mat[1, 2] > 0.1)
})
