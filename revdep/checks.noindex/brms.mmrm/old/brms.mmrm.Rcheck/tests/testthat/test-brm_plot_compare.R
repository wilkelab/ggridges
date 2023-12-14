test_that("brm_plot_compare()", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate()$data),
    outcome = "response",
    role = "response",
    group = "group",
    time = "time",
    patient = "patient"
  )
  formula <- brm_formula(
    data = data,
    effect_base = FALSE,
    interaction_base = FALSE
  )
  tmp <- utils::capture.output(
    suppressMessages(
      suppressWarnings(
        model <- brm_model(
          data = data,
          formula = formula,
          chains = 1,
          iter = 100,
          refresh = 0
        )
      )
    )
  )
  draws <- brm_marginal_draws(
    model = model,
    data = data,
    control = "group.1",
    baseline = "time.1"
  )
  suppressWarnings(summaries_draws <- brm_marginal_summaries(draws))
  summaries_data <- brm_marginal_data(data)
  out <- brm_plot_compare(
    summaries_draws = summaries_draws,
    summaries_data = summaries_data
  )
  expect_s3_class(out, "ggplot")
})
