test_that("brm_marginal_probabilities() on response", {
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
  x <- brm_marginal_probabilities(
    draws,
    threshold = 0,
    direction = "greater"
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("group.2", 3))
  expect_equal(x$time, paste0("time.", seq(2, 4)))
  expect_equal(x$direction, rep("greater", 3))
  expect_equal(x$threshold, rep(0, 3))
  column <- function(group, time) {
    sprintf("group.%s%stime.%s", group, brm_sep(), time)
  }
  expect_equal(
    x$value[1L],
    mean(draws$difference[[column(2L, 2L)]] > 0)
  )
  expect_equal(
    x$value[2L],
    mean(draws$difference[[column(2L, 3L)]] > 0)
  )
  expect_equal(
    x$value[3L],
    mean(draws$difference[[column(2L, 4L)]] > 0)
  )
})

test_that("brm_marginal_probabilities() on change and multiple probs", {
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
  for (index in seq_along(draws$difference)) {
    draws$difference[[index]] <- seq_len(nrow(draws$difference))
  }
  x <- brm_marginal_probabilities(
    draws,
    direction = c("less", "greater"),
    threshold = c(15, 30)
  )
  expect_equal(
    sort(colnames(x)),
    sort(c("group", "time", "direction", "threshold", "value"))
  )
  expect_equal(x$group, rep("group.2", 8))
  expect_equal(x$time, rep(paste0("time.", seq(1, 4)), times = 2))
  expect_equal(x$direction, rep(c("greater", "less"), each = 4))
  expect_equal(x$threshold, c(rep(30, 4), rep(15, 4)))
  expect_equal(x$value, rep(c(0.4, 0.28), each = 4L))
})
