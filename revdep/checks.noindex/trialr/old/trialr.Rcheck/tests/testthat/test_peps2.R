
# Test functions in peps2.R

test_that('peps2_get_data works', {
  dat <- peps2_get_data(num_patients = 60,
                        prob_eff = c(0.167, 0.192, 0.5, 0.091, 0.156, 0.439),
                        prob_tox = rep(0.1, 6),
                        eff_tox_or = rep(1, 6))
  expect_type(object = dat, type = "list")
  expect_equal(dat$J, 60)
  expect_equal(length(dat$eff), 60)
  expect_equal(length(dat$tox), 60)
  expect_equal(length(dat$cohorts), 60)
  expect_equal(length(dat$x1), 60)
  expect_equal(length(dat$x2), 60)
  expect_equal(length(dat$x3), 60)
  expect_equal(sum(dat$cohort_eff), sum(dat$eff))
  expect_equal(sum(dat$cohort_tox), sum(dat$tox))
  expect_equal(sum(dat$cohort_n), 60)
  expect_true('alpha_mean' %in% names(dat))
  expect_true('alpha_sd' %in% names(dat))
  expect_true('beta_mean' %in% names(dat))
  expect_true('beta_sd' %in% names(dat))
  expect_true('gamma_mean' %in% names(dat))
  expect_true('gamma_sd' %in% names(dat))
  expect_true('zeta_mean' %in% names(dat))
  expect_true('zeta_sd' %in% names(dat))
  expect_true('lambda_mean' %in% names(dat))
  expect_true('lambda_sd' %in% names(dat))
  expect_true('psi_mean' %in% names(dat))
  expect_true('psi_sd' %in% names(dat))
})

test_that('peps2_process works', {

  set.seed(123)
  fit <- stan_peps2(
    eff = c(0, 1, 0, 1, 0, 0),
    tox = c(0, 0, 1, 1, 0, 0),
    cohorts = c(3, 1, 1, 4, 5, 6),
    refresh = 0
  )
  decision <- peps2_process(fit)
  expect_type(object = decision, type = "list")
  expect_equal(length(decision$Accept), 6)
  expect_equal(length(decision$ProbAccEff), 6)
  expect_equal(length(decision$ProbAccTox), 6)
  expect_equal(length(decision$ProbEff), 6)
  expect_equal(length(decision$ProbTox), 6)
  expect_true('alpha' %in% names(decision))
  expect_true('beta' %in% names(decision))
  expect_true('gamma' %in% names(decision))
  expect_true('zeta' %in% names(decision))
  expect_true('lambda' %in% names(decision))
  expect_true('psi' %in% names(decision))
})

test_that('stan_peps2 works', {
  fit <- stan_peps2(
    eff = c(0, 1, 0, 1, 0, 0),
    tox = c(0, 0, 1, 1, 0, 0),
    cohorts = c(3, 1, 1, 4, 5, 6),
    refresh = 0
  )
  decision <- peps2_process(fit = fit, min_eff = 0.2, max_tox = 0.5,
                            eff_cert = 0.7, tox_cert = 0.8)

  epsilon <- 0.05
  expect_true(all(abs(decision$ProbEff -
                    c(0.47, 0.13, 0.17, 0.50, 0.14, 0.21)) < epsilon))
  expect_true(all(abs(decision$ProbTox - rep(0.29, 6)) < epsilon))
  expect_true(all(abs(decision$ProbAccEff -
                        c(0.84, 0.20, 0.31, 0.83, 0.24, 0.39)) < epsilon))
  expect_true(all(abs(decision$ProbAccTox - rep(0.89, 6)) < epsilon))

  expect_equal(decision$Accept, c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE))
})
