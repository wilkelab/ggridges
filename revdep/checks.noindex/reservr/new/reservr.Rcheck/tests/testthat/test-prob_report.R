test_that("test prob_report", {
  expect_equal(
    prob_report(
      dist_dirac(0),
      data.frame(
        xmin = 0.0,
        xmax = 1.0,
        tmin = c(0.0, 1.0),
        tmax = c(1.0, 2.0)
      )
    ),
    c(1, 0)
  )

  expect_equal(
    prob_report(
      dist_dirac(0),
      data.frame(
        xmin = 0.0,
        xmax = 1.0,
        tmin = c(0.0, 1.0),
        tmax = c(1.0, 2.0)
      ),
      expo = identity
    ),
    c(1, 0)
  )

  expect_equal(
    prob_report(
      dist_uniform(0, 1),
      data.frame(
        xmin = 0,
        xmax = 1.0,
        tmin = c(0.0, 1.0, 0.0),
        tmax = c(1.0, 2.0, 2.0)
      )
    ),
    c(0.5, 0.5, 1.0)
  )

  dist <- dist_exponential()
  ints <- data.frame(
    xmin = 0,
    xmax = 1,
    tmin = seq_len(10) - 1.0,
    tmax = seq_len(10)
  )
  params <- list(rate = rep(c(1, 0.5), each = 5))

  ref <- prob_report(dist, ints, with_params = params, .try_compile = FALSE)
  expect_equal(
    prob_report(dist, ints, with_params = params),
    ref
  )
})
