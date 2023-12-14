
test_that('efftox_utility_density_plot works', {
  # Thall et al example ----
  fit <- stan_efftox_demo('1N 2E 3B', refresh = 0)

  # All doses
  p <- efftox_utility_density_plot(fit)
  # Subset of doses
  p <- efftox_utility_density_plot(fit, doses = 1:3)

  # Three-dose example ----
  fit2 <- stan_efftox('1N 2E 3B',
                      real_doses = c(1.0, 2.0, 4.0),
                      efficacy_hurdle = 0.5, toxicity_hurdle = 0.3,
                      p_e = 0.1, p_t = 0.1,
                      eff0 = 0.5, tox1 = 0.65,
                      eff_star = 0.7, tox_star = 0.25,
                      alpha_mean = -7.9593, alpha_sd = 3.5487,
                      beta_mean = 1.5482, beta_sd = 3.5018,
                      gamma_mean = 0.7367, gamma_sd = 2.5423,
                      zeta_mean = 3.4181, zeta_sd = 2.4406,
                      eta_mean = 0, eta_sd = 0.2,
                      psi_mean = 0, psi_sd = 1, seed = 123, refresh = 0)
  # All doses
  p <- efftox_utility_density_plot(fit2)
  # Subset of doses
  p <- efftox_utility_density_plot(fit2, doses = 1:3)

  # Test passes if no error thrown.
  expect_true(TRUE)
})

