test_that("returns weights as expected", {
  DAT_min <- c(-100, 10, 100, 200)
  DAT_max <- c(-100, 11, 110, 300)
  results <- c(1, 1, 0.1, 0.01)

  expect_equal(get.weights(DAT_min = DAT_min,
                           DAT_max = DAT_max,
                           verbose = FALSE),
               results)
})

test_that("returns message for same values", {
  DAT_min <- c(1)
  DAT_max <- c(1)
  results <- c(1)

  expect_message(get.weights(DAT_min = DAT_min,
                             DAT_max = DAT_max,
                             verbose = TRUE),
                 "same value")
})

test_that("fails for nun-numeric values", {
  DAT_min <- c("1")
  DAT_max <- c("1")

  expect_error(get.weights(DAT_min = DAT_min,
                           DAT_max = DAT_max),
                 "numeric")
})
