test_that("test softmax", {
  expect_equal(softmax(c(1, 1)), c(0.5, 0.5))
  expect_equal(softmax(c(2, 1)), exp(c(2, 1)) / sum(exp(c(2, 1))))

  expect_equal(
    softmax(matrix(c(1, 1, 2, 1), nrow = 2L, byrow = TRUE)),
    matrix(
      c(0.5, 0.5, exp(c(2, 1)) / sum(exp(c(2, 1)))),
      nrow = 2L,
      byrow = TRUE
    )
  )

  expect_equal(dsoftmax(c(1, 1)),
               matrix(c(0.25, -0.25, -0.25, 0.25), nrow = 2L))
  expect_equal(dsoftmax(c(1, 1)), dsoftmax(c(2, 2)))
  expect_equal(dsoftmax(c(5, 1)), dsoftmax(c(6, 2)))
})
