

# test_that('stan_augbin fits sensible data and works as expected.', {
#
#   # Quite tight priors lead to fast sampling
#   prior_params <- list(alpha_mean = 0, alpha_sd = 0.5,
#                        beta_mean = 0, beta_sd = 0.5,
#                        gamma_mean = 0, gamma_sd = 0.2,
#                        sigma_mean = 0, sigma_sd = 1,
#                        omega_lkj_eta = 1,
#                        alpha_d1_mean = 0, alpha_d1_sd = 0.5,
#                        gamma_d1_mean = 0, gamma_d1_sd = 0.2,
#                        alpha_d2_mean = 0, alpha_d2_sd = 0.5,
#                        gamma_d2_mean = 0, gamma_d2_sd = 0.2)
#   N <- 40
#   sigma <- 1
#   delta1 <- -0.356
#   mu <- c(0.5 * delta1, delta1)
#   Sigma = matrix(c(0.5 * sigma^2, 0.5 * sigma^2, 0.5 * sigma^2, sigma^2),
#                  ncol = 2)
#   alphaD <- -1.5
#   gammaD <- 0.02
#   set.seed(123)
#   y <- MASS::mvrnorm(n = N, mu, Sigma)
#   z0 <- runif(N, min = 5, max = 10)
#   z1 <- exp(y[, 1]) * z0
#   z2 <- exp(y[, 2]) * z0
#   d1 <- rbinom(N, size = 1, prob = gtools::inv.logit(alphaD + gammaD * z0))
#   d2 <- rbinom(N, size = 1, prob = gtools::inv.logit(alphaD + gammaD * z1))
#   tumour_size = data.frame(z0, z1, z2) # cm
#   non_shrinkage_failure <- data.frame(d1, d2)
#
#   iter <- 2436
#   chains <- 3
#   fit <- stan_augbin(tumour_size, non_shrinkage_failure,
#                      prior_params = prior_params,
#                      model = '2t-1a',
#                      chains = chains, iter = iter)
#
#   df <- as.data.frame(fit$fit, pars = c('alpha', 'beta', 'gamma', 'Omega',
#                                         'sigma', 'alphaD1', 'gammaD1',
#                                         'alphaD2', 'gammaD2'))
#   # Check dimensions of returned data.frame of samples
#   expect_equal(ncol(df), 13)
#   expect_equal(nrow(df), iter * chains / 2)
#
#   # Get predictions
#   pred1 <- predict(fit)
#   expect_equal(nrow(pred1), N)
#   # Look for expected columns
#   expect_true(any(colnames(pred1) == 'prob_success'))
#   expect_true(any(colnames(pred1) == 'ci_width'))
#
#   # Get predictions a different way
#   pred2 <- prob_success(fit)
#   expect_equal(nrow(pred2), N)
#   # Look for expected columns
#   expect_true(any(colnames(pred2) == 'prob_success'))
#   expect_true(any(colnames(pred2) == 'ci_width'))
#
#   # Get binary predictions
#   pred3 <- binary_prob_success(fit)
#   # Look for expected columns
#   expect_true(any(colnames(pred3) == 'mean'))
#   expect_true(any(colnames(pred3) == 'ci_width'))
# })

test_that('stan_augbin errors when it should.', {

  ts1 <- data.frame(baseline = c(1, 2, 3),
                    assessment1 = c(2, 3, 5),
                    assessment2 = c(2, 2, 6))

  ts2 <- data.frame(baseline = c(1, 2, 3),
                    assessment1 = c(2, 3, 5))

  ts3 <- data.frame(baseline = c(1, 2),
                    assessment1 = c(2, 3),
                    assessment2 = c(2, 2))

  nsf1 <- data.frame(assessment1 = c(F, F, T), assessment2 = c(F, F, T))
  nsf2 <- data.frame(assessment1 = c(F, F, T))
  nsf3 <- data.frame(assessment1 = c(F, F), assessment2 = c(F, F))

  # Fail for no prior parameters
  expect_error(stan_augbin(tumour_size = ts1,
                           non_shrinkage_failure = nsf1))
  # Fail for missing prior parameters
  expect_error(stan_augbin(tumour_size = ts1,
                           non_shrinkage_failure = nsf1,
                           prior_params = list(alpha_mean = 0)))

  # Use sensible priors from now
  prior_params <- list(alpha_mean = 0, alpha_sd = 0.5,
                       beta_mean = 0, beta_sd = 0.5,
                       gamma_mean = 0, gamma_sd = 0.2,
                       sigma_mean = 0, sigma_sd = 1,
                       omega_lkj_eta = 1,
                       alpha_d1_mean = 0, alpha_d1_sd = 0.5,
                       gamma_d1_mean = 0, gamma_d1_sd = 0.2,
                       alpha_d2_mean = 0, alpha_d2_sd = 0.5,
                       gamma_d2_mean = 0, gamma_d2_sd = 0.2)
  # Fail because tumour_size does not have enough columns
  expect_error(stan_augbin(tumour_size = ts2,
                           non_shrinkage_failure = nsf1,
                           prior_params = prior_params,
                           model = '2t-1a'))
  # Fail because non_shrinkage_failure does not have enough columns
  expect_error(stan_augbin(tumour_size = ts1,
                           non_shrinkage_failure = nsf2,
                           prior_params = prior_params,
                           model = '2t-1a'))
  # Fail because tumour_size & non_shrinkage_failure are not compatible, v1
  expect_error(stan_augbin(tumour_size = ts1,
                           non_shrinkage_failure = nsf3,
                           prior_params = prior_params,
                           model = '2t-1a'))
  # Fail because tumour_size & non_shrinkage_failure are not compatible, v2
  expect_error(stan_augbin(tumour_size = ts3,
                           non_shrinkage_failure = nsf1,
                           prior_params = prior_params,
                           model = '2t-1a'))
  # Fail because the model type is not supported
  expect_error(stan_augbin(tumour_size = ts1,
                           non_shrinkage_failure = nsf1,
                           prior_params = prior_params,
                           model = 'not supported model'))
})

# TODO:

# Once there is an available independent implementation of this model, check
# the two methods agree.

# Check convergence by running two fits to the same data.
