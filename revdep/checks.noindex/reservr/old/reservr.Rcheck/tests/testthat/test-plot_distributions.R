test_that("test plot_distributions", {
  set.seed(1337L)
  expect_s3_class(
    local({
      rate <- 1
      x <- rexp(20, rate)
      d_emp <- dist_empirical(x, positive = TRUE)
      d_exp <- dist_exponential()
      plot_distributions(
        empirical = d_emp,
        theoretical = d_exp,
        estimated = d_exp,
        with_params = list(
          theoretical = list(rate = rate),
          estimated = list(rate = 1 / mean(x))
        ),
        .x = seq(1e-4, 5, length.out = 100)
      )
    }),
    "ggplot"
  )

  expect_s3_class(
    local({
      rate <- 1
      x <- rexp(20, rate)
      d_emp <- dist_empirical(x, positive = TRUE)
      d_exp <- dist_exponential()
      plot_distributions(
        empirical = d_emp,
        theoretical = d_exp,
        estimated = d_exp,
        with_params = list(
          list(),
          list(rate = rate),
          list(rate = 1 / mean(x))
        ),
        .x = seq(1e-4, 5, length.out = 100)
      )
    }),
    "ggplot"
  )

  expect_s3_class(
    local({
      rate <- 1
      x <- rexp(20, rate)
      d_emp <- dist_empirical(x, positive = TRUE)
      d_exp <- dist_exponential(rate)
      plot_distributions(
        empirical = d_emp,
        theoretical = d_exp,
        .x = seq(1e-4, 5, length.out = 100)
      )
    }),
    "ggplot"
  )

  expect_error(
    local({
      rate <- 1
      x <- rexp(20, rate)
      d_emp <- dist_empirical(x, positive = TRUE)
      d_exp <- dist_exponential(rate)
      plot_distributions(
        distributions = list(
          d_emp,
          d_exp
        ),
        .x = seq(1e-4, 5, length.out = 100)
      )
    }),
    fixed = "must be named"
  )
})
