test_that("brm_marginal_draws_average()", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = brm_simulate()$data,
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
  out <- brm_marginal_draws(
    model = model,
    data = data,
    control = "group.1",
    baseline = "time.1"
  )
  averages_all <- brm_marginal_draws_average(
    draws = out,
    data = data,
    label = "average"
  )
  averages_sub <- brm_marginal_draws_average(
    draws = out,
    data = data,
    times = c("time 2", "time 3"),
    label = "mean"
  )
  for (group in setdiff(unique(data$group), "group.1")) {
    cols_all <- grep(
      pattern = group,
      x = colnames(out$response),
      fixed = TRUE,
      value = TRUE
    )
    cols_sub <- grep(
      pattern = "time\\.2|time\\.3",
      x = cols_all,
      fixed = FALSE,
      value = TRUE
    )
    expect_equal(
      averages_all$response[[name_marginal(group, "average")]],
      apply(tibble::as_tibble(out$response)[, cols_all], 1, mean)
    )
    expect_equal(
      averages_sub$response[[name_marginal(group, "mean")]],
      apply(tibble::as_tibble(out$response)[, cols_sub], 1, mean)
    )
    cols_all <- intersect(cols_all, colnames(out$change))
    cols_sub <- intersect(cols_sub, colnames(out$change))
    expect_equal(
      averages_all$change[[name_marginal(group, "average")]],
      apply(tibble::as_tibble(out$change)[, cols_all], 1, mean)
    )
    expect_equal(
      averages_sub$change[[name_marginal(group, "mean")]],
      apply(tibble::as_tibble(out$change)[, cols_sub], 1, mean)
    )
    cols_all <- intersect(cols_all, colnames(out$difference))
    cols_sub <- intersect(cols_sub, colnames(out$difference))
    for (field in c("difference", "effect")) {
      expect_equal(
        averages_all[[field]][[name_marginal(group, "average")]],
        apply(tibble::as_tibble(out[[field]])[, cols_all], 1, mean)
      )
      expect_equal(
        averages_sub[[field]][[name_marginal(group, "mean")]],
        apply(tibble::as_tibble(out[[field]])[, cols_sub], 1, mean)
      )
    }
  }
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_all))))
  expect_true(is.list(suppressWarnings(brm_marginal_summaries(averages_sub))))
  expect_true(is.list(brm_marginal_probabilities(averages_all)))
  expect_true(is.list(brm_marginal_probabilities(averages_sub)))
})
