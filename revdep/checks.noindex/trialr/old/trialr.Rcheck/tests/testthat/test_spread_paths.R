
test_that('spread_paths returns data.frames with the expected number of rows', {

  library(tibble)

  target <- 0.25
  skeleton <- c(0.05, 0.15, 0.25, 0.4, 0.6)
  paths <- crm_dtps(skeleton = skeleton, target = target, model = 'empiric',
                    cohort_sizes = c(1, 1), next_dose = 3, beta_sd = 1,
                    refresh = 0)
  expect_equal(length(paths), 7)

  df1 <- spread_paths(dose_finding_paths = paths)
  expect_equal(nrow(df1), 4)

  df <- as_tibble(paths)
  df2 <- spread_paths(df)
  expect_equal(nrow(df2), 4)

  df3 <- spread_paths(df, max_depth = 1)
  expect_equal(nrow(df3), 2)
})
