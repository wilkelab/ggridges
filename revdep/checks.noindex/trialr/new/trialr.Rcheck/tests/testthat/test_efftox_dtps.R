
test_that('efftox_dtps fails when cohort_sizes is not vector of +ve integers', {

  expect_error(
    efftox_dtps(cohort_sizes = c(3, 0),
                next_dose = 1,
                real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
                efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                p_e = 0.1, p_t = 0.1,
                eff0 = 0.5, tox1 = 0.65,
                eff_star = 0.7, tox_star = 0.25,
                alpha_mean = -7.9593, alpha_sd = 3.5487,
                beta_mean = 1.5482, beta_sd = 3.5018,
                gamma_mean = 0.7367, gamma_sd = 2.5423,
                zeta_mean = 3.4181, zeta_sd = 2.4406,
                eta_mean = 0, eta_sd = 0.2,
                psi_mean = 0, psi_sd = 1,
                seed = 123, refresh = 0)
  )

  expect_error(
    efftox_dtps(cohort_sizes = c(3, -1),
                next_dose = 1,
                real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
                efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                p_e = 0.1, p_t = 0.1,
                eff0 = 0.5, tox1 = 0.65,
                eff_star = 0.7, tox_star = 0.25,
                alpha_mean = -7.9593, alpha_sd = 3.5487,
                beta_mean = 1.5482, beta_sd = 3.5018,
                gamma_mean = 0.7367, gamma_sd = 2.5423,
                zeta_mean = 3.4181, zeta_sd = 2.4406,
                eta_mean = 0, eta_sd = 0.2,
                psi_mean = 0, psi_sd = 1,
                seed = 123, refresh = 0)
  )

  expect_error(
    efftox_dtps(cohort_sizes = c(3, 2.3),
                next_dose = 1,
                real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
                efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                p_e = 0.1, p_t = 0.1,
                eff0 = 0.5, tox1 = 0.65,
                eff_star = 0.7, tox_star = 0.25,
                alpha_mean = -7.9593, alpha_sd = 3.5487,
                beta_mean = 1.5482, beta_sd = 3.5018,
                gamma_mean = 0.7367, gamma_sd = 2.5423,
                zeta_mean = 3.4181, zeta_sd = 2.4406,
                eta_mean = 0, eta_sd = 0.2,
                psi_mean = 0, psi_sd = 1,
                seed = 123, refresh = 0)
  )

  expect_error(
    efftox_dtps(cohort_sizes = c(3, NA),
                next_dose = 1,
                real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
                efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                p_e = 0.1, p_t = 0.1,
                eff0 = 0.5, tox1 = 0.65,
                eff_star = 0.7, tox_star = 0.25,
                alpha_mean = -7.9593, alpha_sd = 3.5487,
                beta_mean = 1.5482, beta_sd = 3.5018,
                gamma_mean = 0.7367, gamma_sd = 2.5423,
                zeta_mean = 3.4181, zeta_sd = 2.4406,
                eta_mean = 0, eta_sd = 0.2,
                psi_mean = 0, psi_sd = 1,
                seed = 123, refresh = 0)
  )
})

test_that('efftox_dtps and derived tibbles perform as expected.', {

  paths <- efftox_dtps(cohort_sizes = c(1, 1),
                       previous_outcomes = '',
                       next_dose = 1,
                       real_doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
                       efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                       p_e = 0.1, p_t = 0.1,
                       eff0 = 0.5, tox1 = 0.65,
                       eff_star = 0.7, tox_star = 0.25,
                       alpha_mean = -7.9593, alpha_sd = 3.5487,
                       beta_mean = 1.5482, beta_sd = 3.5018,
                       gamma_mean = 0.7367, gamma_sd = 2.5423,
                       zeta_mean = 3.4181, zeta_sd = 2.4406,
                       eta_mean = 0, eta_sd = 0.2,
                       psi_mean = 0, psi_sd = 1,
                       seed = 123, refresh = 0)

  # Expected number of nodes Here we expect 1 + 4 + 16 = 21 nodes
  expect_equal(length(paths), 21)

  # Expected names
  expect_true("" %in% names(paths))
  expect_true("1B" %in% names(paths))
  expect_true("1B 2B" %in% names(paths))
  expect_true("1E" %in% names(paths))
  expect_true("1E 2B" %in% names(paths))
  expect_true("1N" %in% names(paths))
  expect_true("1N 2B" %in% names(paths))
  expect_true("1T" %in% names(paths))
  expect_true("1T 2B" %in% names(paths))
  expect_true("1B 2E" %in% names(paths))
  expect_true("1E 2E" %in% names(paths))
  expect_true("1N 2E" %in% names(paths))
  expect_true("1T 2E" %in% names(paths))
  expect_true("1B 2N" %in% names(paths))
  expect_true("1E 2N" %in% names(paths))
  expect_true("1N 2N" %in% names(paths))
  expect_true("1T 2N" %in% names(paths))
  expect_true("1B 2T" %in% names(paths))
  expect_true("1E 2T" %in% names(paths))
  expect_true("1N 2T" %in% names(paths))
  expect_true("1T 2T" %in% names(paths))

  # Expected conversion to tibble
  library(tibble)
  df <- as_tibble(paths)
  expect_equal(nrow(df), 21)

  # Expected conversion to wide tibble, version 1
  expect_equal(nrow(spread_paths(df)), 16)

  # Expected conversion to wide tibble, version 2
  expect_equal(nrow(spread_paths(dose_finding_paths = paths)), 16)
})
