test_that("`description` is sliced along with expressions", {
  x <- as.list(expression(x + y, z + b))
  x <- new_bench_expr(x, c("a", "b"))

  expect_identical(attr(x[2], "description"), "b")
  expect_identical(attr(x[c(2, 2, 1)], "description"), c("b", "b", "a"))
})

test_that("`vec_slice()` slices `description` attribute", {
  skip_if_not_installed("vctrs")

  x <- as.list(expression(x + y, z + b))
  x <- new_bench_expr(x, c("a", "b"))

  expect_identical(attr(vctrs::vec_slice(x, 2), "description"), "b")
  expect_identical(attr(vctrs::vec_slice(x, c(2, 2, 1)), "description"), c("b", "b", "a"))
})
