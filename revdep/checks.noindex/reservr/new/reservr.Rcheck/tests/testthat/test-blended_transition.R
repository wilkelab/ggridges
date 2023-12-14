test_that("test blended_transition", {
  x <- seq(from = 0, to = 20, length.out = 101)

  blend_mat <- blended_transition(x, 10, 3, .gradient = TRUE)
  blend_mat_grad <- attr(blend_mat, "gradient", exact = TRUE)
  expect_equal(dim(blend_mat), c(length(x), 2L))
  expect_equal(dim(blend_mat_grad), c(length(x), 2L))

  # smooth transition
  expect_equal(rowSums(attr(blend_mat, "gradient")), rep(1, length(x)))
  expect_equal(blend_mat[x > 13, 1], rep(10, sum(x > 13)))
  expect_equal(blend_mat[x < 7, 2], rep(10, sum(x < 7)))

  # extend_na works properly
  blend_mat_na <- blended_transition(x, 10, 3, .gradient = TRUE, .extend_na = TRUE)
  blend_mat_na_grad <- attr(blend_mat_na, "gradient")
  expect_equal(is.na(blend_mat_na), cbind(x > 13, x < 7))
  expect_equal(is.na(blend_mat_na_grad), cbind(x > 13, x < 7))

  expect_equal(blend_mat_na[!is.na(blend_mat_na)], blend_mat[!is.na(blend_mat_na)])
  expect_equal(blend_mat_na[!is.na(blend_mat_na)], blend_mat[!is.na(blend_mat_na)])
  expect_equal(blend_mat_na_grad[!is.na(blend_mat_na)], blend_mat_grad[!is.na(blend_mat_na)])
  expect_equal(blend_mat_na_grad[!is.na(blend_mat_na)], blend_mat_grad[!is.na(blend_mat_na)])

  # Inverse works properly
  expect_equal(blended_transition_inv(blend_mat[x <= 13, 1], 10, 3, 1), x[x <= 13], tolerance = 1.0e-6)
  expect_equal(blended_transition_inv(blend_mat[x > 13, 1], 10, 3, 1), rep(13, sum(x > 13)))
  expect_equal(blended_transition_inv(blend_mat[x >= 7, 2], 10, 3, 2), x[x >= 7], tolerance = 1.0e-6)
  expect_equal(blended_transition_inv(blend_mat[x < 7, 2], 10, 3, 2), rep(7, sum(x < 7)))

  # error condition is helpful
  expect_error(blended_transition(1, c(1, 3), c(2, 2)), "\\(1 \u00b1 2\\) overlaps \\(3 \u00b1 2\\)")
  expect_error(blended_transition_inv(1, c(1, 3), c(2, 2), .component = 1), "\\(1 \u00b1 2\\) overlaps \\(3 \u00b1 2\\)")
})
