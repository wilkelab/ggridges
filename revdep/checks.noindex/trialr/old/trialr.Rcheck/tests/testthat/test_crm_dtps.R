

test_that('crm_dtps fails when cohort_sizes is not vector of +ve integers', {

  target <- 0.4
  skeleton <- seq(0.1, 0.6, 0.1)

  expect_error(
    crm_dtps(skeleton,
             target,
             model = 'empiric',
             cohort_sizes = c(3, 3, 0),
             previous_outcomes = '',
             beta_sd = 1)
  )

  expect_error(
    crm_dtps(skeleton,
             target,
             model = 'empiric',
             cohort_sizes = c(3, 3, -1),
             previous_outcomes = '',
             beta_sd = 1)
  )

  expect_error(
    crm_dtps(skeleton,
             target,
             model = 'empiric',
             cohort_sizes = c(3, 3, 2.3),
             previous_outcomes = '',
             beta_sd = 1)
  )

  expect_error(
    crm_dtps(skeleton,
             target,
             model = 'empiric',
             cohort_sizes = c(3, 3, NA),
             previous_outcomes = '',
             beta_sd = 1,
             refresh = 0)
  )
})

test_that('crm_dtps and derived tibbles perform as expected.', {

  skeleton <- c(0.05, 0.1, 0.15, 0.33, 0.5)
  target <- 0.33

  set.seed(123)
  paths <- crm_dtps(skeleton = skeleton,
                    target = target,
                    model = 'empiric',
                    cohort_sizes = c(1, 1),
                    next_dose = 2,
                    beta_sd = 1,
                    refresh = 0)

  # Expected number of nodes Here we expect 1 + 2 + 4  = 7 nodes
  expect_equal(length(paths), 7)

  # Expected names
  expect_true("" %in% names(paths))
  expect_true("2N" %in% names(paths))
  expect_true("2N 4N" %in% names(paths))
  expect_true("2T" %in% names(paths))
  expect_true("2T 1N" %in% names(paths))
  expect_true("2N 4T" %in% names(paths))
  expect_true("2T 1T" %in% names(paths))

  # Expected conversion to tibble
  library(tibble)
  df <- as_tibble(paths)
  expect_equal(nrow(df), 7)

  # Expected conversion to wide tibble, version 1
  expect_equal(nrow(spread_paths(df)), 4)

  # Expected conversion to wide tibble, version 2
  expect_equal(nrow(spread_paths(dose_finding_paths = paths)), 4)
})
