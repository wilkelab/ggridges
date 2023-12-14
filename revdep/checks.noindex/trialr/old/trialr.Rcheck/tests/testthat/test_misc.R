
test_that('closest_to_target works', {
  expect_equal(closest_to_target(c(0.1, 0.2, 0.3), 0.05), 1)
  expect_equal(closest_to_target(c(0.1, 0.2, 0.3), 0.22), 2)
  expect_equal(closest_to_target(c(0.1, 0.2, 0.3), -0.05), 1)
  expect_equal(closest_to_target(c(0.1, 0.2, 0.3), 8), 3)
})
