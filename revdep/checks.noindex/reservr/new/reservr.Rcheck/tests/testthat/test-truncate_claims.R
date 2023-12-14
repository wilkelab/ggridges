test_that("test truncate_claims", {
  x <- data.frame(
    t_acc = c(0, 0, 0, 1, 1, 2),
    repdel = c(0, 1, 2, 0, 1, 0)
  )

  expect_equal(
    truncate_claims(x, t_acc, repdel, Inf)$report,
    c(0, 1, 2, 1, 2, 2)
  )

  expect_equal(
    truncate_claims(x, t_acc, repdel, 1),
    data.frame(
      t_acc = c(0, 0, 1),
      repdel = c(0, 1, 0),
      report = c(0, 1, 1),
      row.names = c(1L, 2L, 4L)
    )
  )

  expect_warning(
    truncate_claims(x, t_acc, repdel, time = Inf, .report_col = NULL),
    fixed = "No work will be done."
  )

  expect_equal(
    truncate_claims(x, t_acc, repdel, time = 2, .report_col = NULL),
    x
  )

  expect_equal(
    truncate_claims(x, t_acc, repdel, time = 0, .report_col = NULL),
    data.frame(t_acc = 0, repdel = 0)
  )

  expect_s3_class(
    truncate_claims(x, t_acc, repdel, time = 2),
    class(x),
    exact = TRUE
  )

  expect_s3_class(
    truncate_claims(tibble::as_tibble(x), t_acc, repdel, time = 2),
    class(tibble::as_tibble(x)),
    exact = TRUE
  )

  expect_s3_class(
    truncate_claims(data.table::as.data.table(x), t_acc, repdel, time = 2),
    class(data.table::as.data.table(x)),
    exact = TRUE
  )
})
