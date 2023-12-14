test_that("generate.stepsize returns one when smallest difference is 1", {
  testmat <- matrix(c(1, 0, 50,
                      2, 10, 12,
                      3, -100, -99),
                    byrow = TRUE, ncol = 3)
  colnames(testmat) <- c("index", "datmin", "datmax")

  expect_equal(generate.stepsize(testmat, verbose = FALSE), 1)
})

test_that("generate.stepsize returns one when smallest difference is 0", {
  testmat <- matrix(c(1, 0, 50,
                      2, 10, 10,
                      3, -100, -50),
                    byrow = TRUE, ncol = 3)
  colnames(testmat) <- c("index", "datmin", "datmax")

  expect_equal(generate.stepsize(testmat, verbose = FALSE), 1)
})

test_that("generate.stepsize notifies of result when verbose", {
  testmat <- matrix(c(1, -100, 50,
                      2, 1, 11,
                      3, -100, -99),
                    byrow = TRUE, ncol = 3)
  colnames(testmat) <- c("index", "datmin", "datmax")

  expect_message(generate.stepsize(testmat, verbose = TRUE), "auto")
  expect_message(generate.stepsize(testmat, verbose = TRUE), "1")
})

test_that("generate.stepsize throws error for nun-numeric values", {
  testmat <- matrix(c(1, "-100", "50", 2, "1", "11"),
                    byrow = TRUE, ncol = 3)
  colnames(testmat) <- c("index", "datmin", "datmax")

  expect_error(generate.stepsize(testmat, verbose = FALSE),
               "numeric")
})
