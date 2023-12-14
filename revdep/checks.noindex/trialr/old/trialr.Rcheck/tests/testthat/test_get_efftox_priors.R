
test_that('get_efftox_priors matches published example.', {

  # Reproduce the example in Thall et al. (2014)

  # To measure the distance between two normal distributions we need a measure:
  bhattacharyya_distance <- function(mu1, sigma1, mu2, sigma2) {
    # See https://en.wikipedia.org/wiki/Bhattacharyya_distance
    a <- sigma1^2 / sigma2^2 + sigma2^2 / sigma1^2 + 2
    b <- (mu1 - mu2)^2 / (sigma1^2 + sigma2^2)
    0.25 * (log(0.25 * a) + b)
  }

  # Thall et al. use:
  p1 <- get_efftox_priors(
    doses = c(1.0, 2.0, 4.0, 6.6, 10.0),
    pi_T = c(0.02, 0.04, 0.06, 0.08, 0.10), ess_T = 0.9,
    pi_E = c(0.2, 0.4, 0.6, 0.8, 0.9), ess_E = 0.9
  )

  # Thall et al. derive:
  p2 <- efftox_priors(alpha_mean = -7.9593, alpha_sd = 3.5487,
                      beta_mean = 1.5482, beta_sd = 3.5018,
                      gamma_mean = 0.7367, gamma_sd = 2.5423,
                      zeta_mean = 3.4181, zeta_sd = 2.4406,
                      eta_mean = 0, eta_sd = 0.2,
                      psi_mean = 0, psi_sd = 1)

  # For similar distributions, these distances should be "small".
  # What is small? Let us use 0.05
  epsilon <- 0.05

  d_alpha <- bhattacharyya_distance(p1$alpha_mean, p1$alpha_sd,
                                    p2$alpha_mean, p2$alpha_sd)
  expect_lt(d_alpha, epsilon)

  d_beta <- bhattacharyya_distance(p1$beta_mean, p1$beta_sd,
                                    p2$beta_mean, p2$beta_sd)
  expect_lt(d_beta, epsilon)

  d_gamma <- bhattacharyya_distance(p1$gamma_mean, p1$gamma_sd,
                                    p2$gamma_mean, p2$gamma_sd)
  expect_lt(d_gamma, epsilon)

  d_zeta <- bhattacharyya_distance(p1$zeta_mean, p1$zeta_sd,
                                    p2$zeta_mean, p2$zeta_sd)
  expect_lt(d_zeta, epsilon)

  d_eta <- bhattacharyya_distance(p1$eta_mean, p1$eta_sd,
                                    p2$eta_mean, p2$eta_sd)
  expect_lt(d_eta, epsilon)

  d_psi <- bhattacharyya_distance(p1$psi_mean, p1$psi_sd,
                                    p2$psi_mean, p2$psi_sd)
  expect_lt(d_psi, epsilon)
})
