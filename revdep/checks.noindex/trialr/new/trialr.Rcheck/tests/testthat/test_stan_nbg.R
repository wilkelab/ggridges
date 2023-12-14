
# Operational checks ----
test_that('stan_nbg passes ellipsis variables to rstan::sampling', {

  dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
  d_star = 250
  target <- 0.30
  x <- stan_nbg(outcome_str = '1NNN 2TNT',
                real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8,
                iter = 1000, chains = 2, seed = 123, refresh = 0)
  df <- as.data.frame(x$fit)
  # Expect 2 * 500 / 2 post-warmup samples
  expect_equal(nrow(df), 1000)
})

test_that('stan_nbg fits to zero patients', {

  dose <- c(1, 2.5, 5, 10)
  d_star = 10
  target <- 0.30
  x <- stan_nbg(outcome_str = '',
                real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8, refresh = 0)
  expect_equal(x$num_patients, 0)
  expect_equal(length(x$doses), 0)
  expect_equal(length(x$tox), 0)
  expect_equal(length(x$weights), 0)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), rep(0, 4))
  expect_equal(tox_at_dose(x), rep(0, 4))
  expect_error(eff_at_dose(x))
  l <- weights_at_dose(x)
  expect_equal(length(l), 4)
  expect_equal(l[[1]], numeric(0))
  expect_equal(l[[2]], numeric(0))
  expect_equal(l[[3]], numeric(0))
  expect_equal(l[[4]], numeric(0))
  expect_equal(weights_at_dose(x, dose = 1), numeric(0))
  expect_equal(weights_at_dose(x, dose = 2), numeric(0))
  expect_equal(weights_at_dose(x, dose = 3), numeric(0))
  expect_equal(weights_at_dose(x, dose = 4), numeric(0))
  expect_equal(total_weight_at_dose(x), c(0, 0, 0, 0))
})

test_that('stan_nbg fits to one patient', {
  dose <- c(1, 2.5, 5, 10)
  d_star = 10
  target <- 0.30
  x <- stan_nbg(outcome_str = '1N',
                real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8, refresh = 0)
  expect_equal(x$num_patients, 1)
  expect_equal(length(x$doses), 1)
  expect_equal(length(x$tox), 1)
  expect_equal(length(x$weights), 1)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), c(1, 0, 0, 0))
  expect_equal(tox_at_dose(x), c(0, 0, 0, 0))
  expect_error(eff_at_dose(x))
  l <- weights_at_dose(x)
  expect_equal(length(l), 4)
  expect_equal(l[[1]], c(1))
  expect_equal(l[[2]], numeric(0))
  expect_equal(l[[3]], numeric(0))
  expect_equal(l[[4]], numeric(0))
  expect_equal(weights_at_dose(x, dose = 1), c(1))
  expect_equal(weights_at_dose(x, dose = 2), numeric(0))
  expect_equal(weights_at_dose(x, dose = 3), numeric(0))
  expect_equal(weights_at_dose(x, dose = 4), numeric(0))
  expect_equal(total_weight_at_dose(x), c(1, 0, 0, 0))
})

test_that('stan_nbg fits to two patients in one cohort', {
  dose <- c(1, 2.5, 5, 10)
  d_star = 10
  target <- 0.30
  x <- stan_nbg(outcome_str = '2TT',
                real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8, refresh = 0)
  expect_equal(x$num_patients, 2)
  expect_equal(length(x$doses), 2)
  expect_equal(length(x$tox), 2)
  expect_equal(length(x$weights), 2)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), c(0, 2, 0, 0))
  expect_equal(tox_at_dose(x), c(0, 2, 0, 0))
  expect_error(eff_at_dose(x))
  l <- weights_at_dose(x)
  expect_equal(length(l), 4)
  expect_equal(l[[1]], numeric(0))
  expect_equal(l[[2]], array(c(1, 1)))
  expect_equal(l[[3]], numeric(0))
  expect_equal(l[[4]], numeric(0))
  expect_equal(weights_at_dose(x, dose = 1), numeric(0))
  expect_equal(weights_at_dose(x, dose = 2), array(c(1, 1)))
  expect_equal(weights_at_dose(x, dose = 3), numeric(0))
  expect_equal(weights_at_dose(x, dose = 4), numeric(0))
  expect_equal(total_weight_at_dose(x), c(0, 2, 0, 0))
})

test_that('stan_nbg fits to two patients in two cohorts', {
  dose <- c(1, 2.5, 5, 10)
  d_star = 10
  target <- 0.30
  x <- stan_nbg(outcome_str = '1N 2N',
                real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8, refresh = 0)
  expect_equal(x$num_patients, 2)
  expect_equal(length(x$doses), 2)
  expect_equal(length(x$tox), 2)
  expect_equal(length(x$weights), 2)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), c(1, 1, 0, 0))
  expect_equal(tox_at_dose(x), c(0, 0, 0, 0))
  expect_error(eff_at_dose(x))
  l <- weights_at_dose(x)
  expect_equal(length(l), 4)
  expect_equal(l[[1]], c(1))
  expect_equal(l[[2]], c(1))
  expect_equal(l[[3]], numeric(0))
  expect_equal(l[[4]], numeric(0))
  expect_equal(weights_at_dose(x, dose = 1), c(1))
  expect_equal(weights_at_dose(x, dose = 2), c(1))
  expect_equal(weights_at_dose(x, dose = 3), numeric(0))
  expect_equal(weights_at_dose(x, dose = 4), numeric(0))
  expect_equal(total_weight_at_dose(x), c(1, 1, 0, 0))
})


test_that('stan_nbg fits when weights are provided', {
  dose <- c(1, 2.5, 5, 10)
  d_star = 10
  target <- 0.30
  x <- stan_nbg(real_doses = dose, d_star = d_star,
                target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                beta_mean = 0.52, beta_sd = 0.8,
                doses = c(1, 1, 2, 2, 2),
                tox   = c(0, 0, 0, 0, 0),
                weights = c(1, 1, 0.9, 0.1, 0.1),
                refresh = 0)
  expect_equal(x$num_patients, 5)
  expect_equal(length(x$doses), 5)
  expect_equal(length(x$tox), 5)
  expect_equal(length(x$weights), 5)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), c(2, 3, 0, 0))
  expect_equal(tox_at_dose(x), c(0, 0, 0, 0))
  expect_error(eff_at_dose(x))
  l <- weights_at_dose(x)
  expect_equal(length(l), 4)
  expect_equal(l[[1]], array(c(1, 1)))
  expect_equal(l[[2]], array(c(0.9, 0.1, 0.1)))
  expect_equal(l[[3]], numeric(0))
  expect_equal(l[[4]], numeric(0))
  expect_equal(weights_at_dose(x, dose = 1), array(c(1, 1)))
  expect_equal(weights_at_dose(x, dose = 2), array(c(0.9, 0.1, 0.1)))
  expect_equal(weights_at_dose(x, dose = 3), numeric(0))
  expect_equal(weights_at_dose(x, dose = 4), numeric(0))
  expect_equal(total_weight_at_dose(x), c(2, 1.1, 0, 0))
})

# Accuracy checks ----
testthat::test_that('stan_nbg output matches published example.', {

  # This example is taken from the manual for the bcrm package.
  # It fits the NBG model to data in Neuenschwander et al. (2008).

  # Reading values off the plot in lower right panel of Figure 1 in
  # Neuenschwander et al. (2008), these are the posterior means of prob(tox)
  # that we seek by fitting the model to data:
  nbg_post_mean = c(0.01, 0.02, 0.05, 0.13, 0.19, 0.25, 0.30, 0.35, 0.47, 0.53,
                    0.68, 0.74, 0.85, 0.89, 0.92)
  # We seek to reproduce those

  # Data
  dose <- c(1, 2.5, 5, 10, 15, 20, 25, 30, 40, 50, 75, 100, 150, 200, 250)
  d_star <- 250
  target <- 0.30
  df <- data.frame(
    patient=1:18,
    dose = rep(c(1:4, 7), c(3, 4, 5, 4, 2)),
    tox = rep(0:1, c(16, 2)))

  # bcrm version, commented out to not create dependency on JAGS and BUGS.
  # sdose <- log(dose / 250)
  # ## Bivariate lognormal prior
  # mu <- c(2.15, 0.52)
  # Sigma <- rbind(c(0.84^2, 0.134), c(0.134, 0.80^2))
  # fit1 <- bcrm::bcrm(stop = list(nmax=18),
  #                    data = df,
  #                    sdose = sdose,
  #                    dose = dose,
  #                    ff = "logit2",
  #                    prior.alpha = list(4, mu, Sigma),
  #                    target.tox = target,
  #                    constrain = FALSE,
  #                    pointest = "mean",
  #                    method = "rjags")
  # fit1$ndose[[1]]$ndose # Returns 7
  mtd1 <- 7
  prob_tox1 <- c(0.01272546, 0.03191676, 0.06520755, 0.13256280, 0.19729063,
                 0.25756595, 0.31282832, 0.36307054, 0.44959181, 0.52003487,
                 0.64499703, 0.72359061, 0.81275763, 0.86026158, 0.88919066)


  # trialr version
  outcomes <- '1NNN 2NNNN 3NNNN 4NNNN 7TT'
  fit2 <- stan_nbg(outcome_str = outcomes, real_doses = dose, d_star = d_star,
                   target = target, alpha_mean = 2.15, alpha_sd = 0.84,
                   beta_mean = 0.52, beta_sd = 0.8, seed = 2020, refresh = 0)

  # MTD matches?
  expect_equal(mtd1, fit2$recommended_dose)

  # mean_prob_tox matches?
  epsilon <- 0.04
  expect_true(all(abs(prob_tox1 - fit2$prob_tox) < epsilon))

  # mean_prob_tox matches NBG publication?
  epsilon <- 0.04
  expect_true(all(abs(nbg_post_mean - fit2$prob_tox) < epsilon))
})
