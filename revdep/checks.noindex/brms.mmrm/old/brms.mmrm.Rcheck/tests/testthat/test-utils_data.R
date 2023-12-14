test_that("unname_df()", {
  x <- unname_df(tibble::tibble(x = c(a = 1, b = 2), y = c(c = 3, d = 4)))
  expect_null(names(x$x))
  expect_null(names(x$y))
})
