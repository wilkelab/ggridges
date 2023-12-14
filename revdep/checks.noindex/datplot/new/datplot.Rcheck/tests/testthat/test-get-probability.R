test_that("returns probabilities as expected", {
  DAT_min <- c(-100, 10, 101, 201)
  DAT_max <- c(-100, 11, 110, 300)
  results <- c(1, 0.5, 0.1, 0.01)

  expect_equal(get.probability(DAT_min = DAT_min,
                               DAT_max = DAT_max),
               results)
})

test_that("fails for nun-numeric values", {
  DAT_min <- c("1")
  DAT_max <- c("1")

  expect_error(get.probability(DAT_min = DAT_min,
                               DAT_max = DAT_max),
               "numeric")
})
