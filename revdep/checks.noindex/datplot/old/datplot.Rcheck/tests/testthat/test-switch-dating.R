correct_df <- as.data.frame(matrix(c("id1", "A", 10, 20,
                                     "id2", "B", -10, 10,
                                     "id3", "C", 8, 8),
                                   byrow = TRUE, ncol = 4))
wrong_df <- as.data.frame(matrix(c("id1", "A", 20, 10,
                                   "id2", "B", -10, 10,
                                   "id3", "C", 8, 8),
                                 byrow = TRUE, ncol = 4))


test_that("switch.dating returns values in correct order", {
  expect_equal(suppressWarnings(switch.dating(wrong_df)), correct_df)
})

test_that("switch.dating issues a warning", {
  expect_warning(switch.dating(wrong_df), "wrong order at ID id1")
})

test_that("switch.dating issues no warning", {
  expect_failure(expect_warning(switch.dating(correct_df)))
})
