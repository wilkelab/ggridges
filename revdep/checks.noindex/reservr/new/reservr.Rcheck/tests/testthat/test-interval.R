# covr doesn't register tested code for constant I_*
I_REALS <- interval(read_only = TRUE)
I_POSITIVE_REALS <- interval(0, Inf, read_only = TRUE)
I_UNIT_INTERVAL <- interval(0, 1, closed = TRUE, read_only = TRUE)
I_NATURALS <- interval(0, Inf,
                       include_lowest = TRUE, integer = TRUE, read_only = TRUE)

test_that("test interval", {
  I_EMPTY <- interval(0, 0)
  I_OPEN_UNIT_INTERVAL <- interval(0, 1)
  I_READONLY <- interval(0, 1, read_only = TRUE)

  expect_equal(I_UNIT_INTERVAL$format(), "[0, 1]")
  expect_output(print(I_UNIT_INTERVAL), fixed = "[0, 1]")
  expect_equal(I_OPEN_UNIT_INTERVAL$format(), "(0, 1)")
  expect_equal(I_EMPTY$format(), "{}")
  expect_equal(interval(0, 0, closed = TRUE)$format(), "{0}")

  expect_true(I_REALS$contains(I_POSITIVE_REALS))
  expect_identical(I_REALS$contains(c(-1, 1, 0)), rep_len(TRUE, 3L))
  expect_identical(I_UNIT_INTERVAL$contains(c(-1, 1, 0)), c(FALSE, TRUE, TRUE))
  expect_error(I_REALS$contains("a"), fixed = "Interval or a numeric")

  expect_true(I_EMPTY$is_empty())
  expect_false(I_REALS$is_empty())

  expect_false(I_UNIT_INTERVAL$equals("a"))
  expect_false(I_OPEN_UNIT_INTERVAL$equals(I_UNIT_INTERVAL))
  expect_true(I_OPEN_UNIT_INTERVAL$equals(
    interval_intersection(I_OPEN_UNIT_INTERVAL, I_REALS)
  ))

  expect_equal(
    local({
      my_i <- interval(-0.5, 0.5)
      my_i$integer <- TRUE
      my_i
    })$format(),
    "{0}"
  )

  expect_warning(interval(0, 1, 2), fixed = "only using the first")
  expect_warning(interval(c(0, 1), 2), fixed = "range is length 2")
  expect_error(interval(c(0, 1, 2)), fixed = "Invalid arguments")
})

test_that("test interval_union", {
  I_OPEN_UNIT_INTERVAL <- interval(0, 1)

  expect_equal(interval_union()$format(), "{}")

  expect_equal(
    interval_union(
      I_UNIT_INTERVAL, interval(1, 2)
    )$format(),
    "[0, 2)"
  )

  expect_equal(
    interval_union(
      interval(c(0, 5)),
      interval(c(1, 4), closed = TRUE)
    )$format(),
    "(0, 5)"
  )

  expect_equal(
    interval_union(
      interval(c(0, 1)),
      interval(c(2, 3))
    )$format(),
    "(0, 3)"
  )

  expect_equal(
    interval_union(
      I_UNIT_INTERVAL,
      I_POSITIVE_REALS,
      I_OPEN_UNIT_INTERVAL
    )$format(),
    "[0, Inf)"
  )

  expect_equal(
    interval_union(intervals = list(
      I_UNIT_INTERVAL,
      I_POSITIVE_REALS,
      I_OPEN_UNIT_INTERVAL
    ))$format(),
    "[0, Inf)"
  )

  expect_error(interval_union(I_UNIT_INTERVAL, "a"),
               fixed = "must be a list of intervals")

  expect_error(interval_union(list(I_UNIT_INTERVAL, "a")),
               fixed = "must be a list of intervals")
})

test_that("test interval_intersection", {
  expect_equal(interval_intersection()$format(), "(-Inf, Inf)")

  expect_equal(
    interval_intersection(
      interval(c(0, 1)),
      interval(c(0.5, 2))
    )$format(),
    "(0.5, 1)"
  )

  expect_equal(
    interval_intersection(
      interval(c(0, Inf)),
      interval(c(-Inf, 0))
    )$format(),
    "{}"
  )

  expect_equal(
    interval_intersection(
      interval(c(2, 3)),
      I_UNIT_INTERVAL
    )$format(),
    "{}"
  )

  expect_equal(
    interval_intersection(
      interval(c(0, Inf), include_lowest = TRUE),
      interval(c(-Inf, 0), include_highest = TRUE)
    )$format(),
    "{0}"
  )

  expect_equal(
    interval_intersection(
      interval(c(0, 5)),
      interval(c(1, 6), closed = TRUE)
    )$format(),
    "[1, 5)"
  )

  expect_equal(
    interval_intersection(intervals = list(
      interval(c(0, 5)),
      interval(c(1, 6), closed = TRUE)
    ))$format(),
    "[1, 5)"
  )

  expect_error(interval_intersection(I_UNIT_INTERVAL, "a"),
               fixed = "must be a list of intervals")

  expect_error(interval_intersection(list(I_UNIT_INTERVAL, "a")),
               fixed = "must be a list of intervals")
})

test_that("test tf_make_layer", {
  skip_if_no_tensorflow()

  test_intervals <- list(
    I_UNIT_INTERVAL,
    I_REALS,
    I_POSITIVE_REALS,
    interval(3, 5),
    interval(3, Inf),
    interval(-Inf, 3)
  )

  set.seed(1307L)
  tensorflow::tf$random$set_seed(1341L)
  test_inputs <- keras::k_constant(
    value = c(-10, 10, runif(98, -10, 10)),
    shape = c(100, 1)
  )

  tf_in <- keras::layer_input(1L)

  tf_layers <- lapply(
    test_intervals,
    function(int) {
      tf_out <- int$tf_make_layer(input = tf_in)
      keras::keras_model(tf_in, tf_out)
    }
  )

  tf_outputs <- lapply(
    tf_layers,
    function(ll) {
      as.numeric(ll(test_inputs))
    }
  )


  for (i in seq_along(test_intervals)) {
    expect_identical(
      test_intervals[[!!i]]$contains(tf_outputs[[!!i]]),
      rep_len(TRUE, 100L)
    )
  }

  # Rough range checks
  rounded_ranges <- lapply(tf_outputs, function(x) round(range(x)))
  expect_identical(rounded_ranges[[1L]], test_intervals[[1L]]$range)
  expect_identical(rounded_ranges[[3L]][1], test_intervals[[3L]]$range[1])
  expect_identical(rounded_ranges[[4L]], test_intervals[[4L]]$range)
  expect_identical(rounded_ranges[[5L]][1], test_intervals[[5L]]$range[1])
  expect_identical(rounded_ranges[[6L]][2], test_intervals[[6L]]$range[2])

  expect_error(I_NATURALS$tf_make_layer(),
               fixed = "Integer ranges are not supported.")
})
