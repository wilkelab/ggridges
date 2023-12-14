test_that("brm_marginal_summaries() on response", {
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
  suppressWarnings(
    x <- brm_marginal_summaries(
      draws,
      level = 0.95
    )
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("marginal", "group", "time", "statistic", "value", "mcse"))
  )
  expect_equal(
    sort(unique(x$marginal)),
    sort(c("response", "change", "difference", "effect"))
  )
  for (marginal in c("response", "change", "difference", "effect")) {
    groups <- unique(data$group)
    times <- unique(data$time)
    if (marginal %in% c("change", "difference")) {
      times <- setdiff(times, "time.1")
    }
    if (identical(marginal, "difference")) {
      groups <- setdiff(groups, "difference")
    }
    for (group in groups) {
      for (time in times) {
        name <- paste(group, time, sep = brm_sep())
        index <- x$marginal == "response" & x$group == group & x$time == time
        subset <- x[index, ]
        expect_equal(
          unname(subset$value[subset$statistic == "mean"]),
          mean(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "median"]),
          median(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "sd"]),
          sd(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "lower"]),
          unname(quantile(draws$response[[name]], probs = 0.025))
        )
        expect_equal(
          unname(subset$value[subset$statistic == "upper"]),
          unname(quantile(draws$response[[name]], probs = 0.975))
        )
        suppressWarnings({
          expect_equal(
            unname(subset$mcse[subset$statistic == "mean"]),
            posterior::mcse_mean(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "median"]),
            posterior::mcse_median(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "sd"]),
            posterior::mcse_sd(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "lower"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.025
              )
            )
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "upper"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.975
              )
            )
          )
        })
      }
    }
  }
})

test_that("brm_marginal_summaries() on change", {
  skip_on_cran()
  set.seed(0L)
  data <- brm_data(
    data = tibble::as_tibble(brm_simulate()$data),
    outcome = "response",
    role = "change",
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
  suppressWarnings(
    x <- brm_marginal_summaries(
      draws,
      level = 0.9
    )
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("marginal", "group", "time", "statistic", "value", "mcse"))
  )
  expect_equal(
    sort(unique(x$marginal)),
    sort(c("response", "difference", "effect"))
  )
  for (marginal in c("response", "difference", "effect")) {
    groups <- unique(data$group)
    times <- unique(data$time)
    if (identical(marginal, "difference")) {
      groups <- setdiff(groups, "difference")
    }
    for (group in groups) {
      for (time in times) {
        name <- paste(group, time, sep = brm_sep())
        index <- x$marginal == "response" & x$group == group & x$time == time
        subset <- x[index, ]
        expect_equal(
          unname(subset$value[subset$statistic == "mean"]),
          mean(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "median"]),
          median(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "sd"]),
          sd(draws$response[[name]])
        )
        expect_equal(
          unname(subset$value[subset$statistic == "lower"]),
          unname(quantile(draws$response[[name]], probs = 0.05))
        )
        expect_equal(
          unname(subset$value[subset$statistic == "upper"]),
          unname(quantile(draws$response[[name]], probs = 0.95))
        )
        suppressWarnings({
          expect_equal(
            unname(subset$mcse[subset$statistic == "mean"]),
            posterior::mcse_mean(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "median"]),
            posterior::mcse_median(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "sd"]),
            posterior::mcse_sd(draws$response[[name]])
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "lower"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.05
              )
            )
          )
          expect_equal(
            unname(subset$mcse[subset$statistic == "upper"]),
            unname(
              posterior::mcse_quantile(
                draws$response[[name]],
                probs = 0.95
              )
            )
          )
        })
      }
    }
  }
})
