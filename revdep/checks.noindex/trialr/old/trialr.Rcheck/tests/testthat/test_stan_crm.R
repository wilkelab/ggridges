# Test stan_crm

# Operational checks ----
test_that('stan_crm passes ellipsis variables to rstan::sampling', {
  x <- stan_crm(outcome_str = '1NNN 2TNT',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = sqrt(1.34),
                iter = 1000, chains = 2, seed = 123,
                refresh = 0)
  df <- as.data.frame(x$fit)
  # Expect 2 * 500 / 2 post-warmup samples
  expect_equal(nrow(df), 1000)
})

test_that('stan_crm fits to zero patients', {
  x <- stan_crm(outcome_str = '',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = 1,
                refresh = 0)
  expect_equal(x$num_patients, 0)
  expect_equal(length(x$doses), 0)
  expect_equal(length(x$tox), 0)
  expect_equal(length(x$weights), 0)
  expect_equal(length(x$dose_indices), 4)
  expect_equal(n_at_dose(x), c(0, 0, 0, 0))
  expect_equal(tox_at_dose(x), c(0, 0, 0, 0))
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

test_that('stan_crm fits to one patient', {
  x <- stan_crm(outcome_str = '1N',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = 1,
                refresh = 0)
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

test_that('stan_crm fits to two patients in one cohort', {
  x <- stan_crm(outcome_str = '2TT',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = 1,
                refresh = 0)
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

test_that('stan_crm fits to two patients in two cohorts', {
  x <- stan_crm(outcome_str = '1N 2N',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = 1,
                refresh = 0)
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


test_that('stan_crm fits when weights are provided', {
  x <- stan_crm(skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                beta_sd = 1,
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

# Published example of logistic model:
# p.21 of Cheung's "Dose Finding by the Continual Reassessment Method",
# ISBN 9781420091519
test_that('stan_crm output for logistic model matches DFCRM by Cheung', {

  # The target is the result of this:
  # fooB <- dfcrm::crm(prior = c(0.05, 0.12, 0.25, 0.40, 0.55), target = 0.25,
  #                    tox = c(0, 0, 1, 0, 0), lev = c(3, 5, 5, 3, 4),
  #                    model = "logistic", intcpt = 3)

  epsilon <- 0.03

  skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)
  target <- 0.25
  a0 <- 3
  beta_mean <- 0
  beta_sd <- sqrt(1.34)

  x <- stan_crm(outcome_str = '3N 5N 5T 3N 4N',
                skeleton = skeleton, target = target,
                model = 'logistic', a0 = a0,
                beta_mean = beta_mean, beta_sd = beta_sd,
                seed = 123, refresh = 0)
  expect_equal(x$recommended_dose, 4)
  beta_samp <- as.data.frame(x, pars = 'beta')
  # fooB$estimate
  expect_lt(abs(mean(beta_samp$beta) - 0.28), epsilon)

  # fooB$doses
  coded_doses <- (gtools::logit(skeleton) - a0) / exp(beta_mean)
  expect_true(all(abs(coded_doses - c(-5.94, -4.99, -4.10, -3.41, -2.80)) <
                    epsilon))

  # Not given in the book but continuing the checking via the dfcrm package with
  # fooB$post.var
  expect_lt(abs(var(beta_samp$beta) - 0.11), epsilon)

  # fooB$ptox
  beta_mean_post <- mean(beta_samp$beta)
  prob_tox_post <- gtools::inv.logit(a0 + exp(beta_mean_post) * coded_doses)
  expect_true(all(abs(prob_tox_post - c(0.01, 0.03, 0.08, 0.18, 0.33)) <
                    epsilon))
  # Note that the estimated posterior probability of toxicity from dfcrm
  # is calculated by plugging the posterior estimate of beta into the assumed
  # dose-tox function. This differs from that calculated by trialr because
  # trialr draws samples from the posterior distribution of prob_tox and
  # calculates the mean from there.
})

# Published example of TITE-CRM empiric model:
# p.124 of Cheung's "Dose Finding by the Continual Reassessment Method",
# ISBN 9781420091519
test_that('stan_crm output for TITE-CRM empiric model matches DFCRM by Cheung', {

  epsilon <- 0.08

  x <-stan_crm(skeleton = c(0.05, 0.12, 0.25, 0.40, 0.55), target = 0.25,
               doses_given = c(3, 3, 3, 3),
               tox = c(0, 0, 0, 0),
               weights = c(73, 66, 35, 28) / 126,
               model = 'empiric', beta_sd = sqrt(1.34),
               seed = 123, iter = 5000, refresh = 0)
  expect_equal(x$recommended_dose, 4)
  beta_samp <- as.data.frame(x, pars = 'beta')
  expect_lt(abs(mean(beta_samp$beta) - 0.49), epsilon)
})



# Invoke some expected errors ----
test_that("stan_crm demands monotonically increasing skeleton", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.4),
                        target = 0.25,
                        model = 'empiric',
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_sd for 'empiric' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                skeleton = c(0.1, 0.25, 0.4, 0.6),
                target = 0.25,
                model = 'empiric',
                seed = 123, refresh = 0))
})

test_that("stan_crm requires a0 for 'logistic' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic',
                        beta_mean = 0,
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_mean for 'logistic' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic',
                        a0 = 1,
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_sd for 'logistic' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic',
                        a0 = 1,
                        beta_mean = 0,
                        seed = 123, refresh = 0))
})


test_that("stan_crm requires a0 for 'logistic_gamma' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic_gamma',
                        beta_shape = 1,
                        beta_inverse_scale = 1,
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_shape for 'logistic_gamma' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic_gamma',
                        a0 = 1,
                        beta_inverse_scale = 1,
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_inverse_scale for 'logistic_gamma' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic_gamma',
                        a0 = 1,
                        beta_shape = 1,
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires alpha_mean for 'logistic2' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic2',
                        alpha_sd = 1,
                        beta_mean = 0,
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires alpha_sd for 'logistic2' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic2',
                        alpha_mean = 0,
                        beta_mean = 0,
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_mean for 'logistic2' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic2',
                        alpha_mean = 0,
                        alpha_sd = 1,
                        beta_sd = sqrt(1.34),
                        seed = 123, refresh = 0))
})

test_that("stan_crm requires beta_sd for 'logistic2' model", {
  expect_error(stan_crm(outcome_str = '1NNN 2TNT',
                        skeleton = c(0.1, 0.25, 0.4, 0.6),
                        target = 0.25,
                        model = 'logistic2',
                        alpha_mean = 0,
                        alpha_sd = 1,
                        beta_mean = 0,
                        seed = 123, refresh = 0))
})





# Test careful_escalation
test_that('careful_escalation advocates start_dose when n = 0', {

  fit <- stan_crm('', skeleton = c(0.1, 0.2, 0.33, 0.6),
                  target = 0.33, model = 'empiric', beta_sd = 1,
                  seed = 123, refresh = 0)
  dose <- careful_escalation(fit, tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             start_dose = 1)
  expect_equal(dose, 1)

  dose <- careful_escalation(fit, tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             start_dose = 2)
  expect_equal(dose, 2)

  dose <- careful_escalation(fit, tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             start_dose = 3)
  expect_equal(dose, 3)

  dose <- careful_escalation(fit, tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             start_dose = 4)
  expect_equal(dose, 4)
})

test_that('careful_escalation does not skip doses', {
  fit1 <- stan_crm('1NNN', skeleton = c(0.1, 0.2, 0.33, 0.6),
                   target = 0.33, model = 'empiric', beta_sd = 1,
                   seed = 123, refresh = 0)
  dose <- careful_escalation(fit1, tox_threshold = 0.33,
                             certainty_threshold = 0.8)
  expect_lte(dose, max(fit1$doses) + 1)
  expect_gte(fit1$recommended_dose, dose)

  fit2 <- stan_crm('1NNN 2NNN', skeleton = c(0.1, 0.2, 0.33, 0.6),
                   target = 0.33, model = 'empiric', beta_sd = 1,
                   seed = 123, refresh = 0)
  dose <- careful_escalation(fit2, tox_threshold = 0.33,
                             certainty_threshold = 0.8)
  expect_lte(dose, max(fit2$doses) + 1)
  expect_gte(fit2$recommended_dose, dose)
})


test_that('careful_escalation stops appropriately', {
  fit1 <- stan_crm('1NNN 2TTT', skeleton = c(0.1, 0.2, 0.33, 0.6),
                   target = 0.33, model = 'empiric', beta_sd = 1,
                   seed = 123, refresh = 0)
  dose <- careful_escalation(fit1,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 1)
  expect_equal(dose, 1)

  dose <- careful_escalation(fit1,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 2)
  expect_equal(dose, NA)



  fit2 <- stan_crm('1NNN 2TTT 1T', skeleton = c(0.1, 0.2, 0.33, 0.6),
                   target = 0.33, model = 'empiric', beta_sd = 1,
                   seed = 123, refresh = 0)
  dose <- careful_escalation(fit2,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 1)
  expect_equal(dose, 1)

  dose <- careful_escalation(fit2,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 2)
  expect_equal(dose, NA)



  fit3 <- stan_crm('1NNN 2TTT 1TT', skeleton = c(0.1, 0.2, 0.33, 0.6),
                   target = 0.33, model = 'empiric', beta_sd = 1,
                   seed = 123, refresh = 0)
  dose <- careful_escalation(fit3,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 1)
  expect_equal(dose, NA)

  dose <- careful_escalation(fit3,
                             tox_threshold = 0.33,
                             certainty_threshold = 0.8,
                             reference_dose = 2)
  expect_equal(dose, NA)
})

