
# For some reason, tests in this file fail. testthat is a mystery to me.
# The tests are in test_stan_crm.R where they work.

# test_that('careful_escalation advocates start_dose when n = 0', {
#
#   fit <- stan_crm('', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                   target = 0.33, model = 'empiric', beta_sd = 1,
#                   seed = 123)
#   dose <- careful_escalation(fit, tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              start_dose = 1)
#   expect_equal(dose, 1)
#
#   dose <- careful_escalation(fit, tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              start_dose = 2)
#   expect_equal(dose, 2)
#
#   dose <- careful_escalation(fit, tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              start_dose = 3)
#   expect_equal(dose, 3)
#
#   dose <- careful_escalation(fit, tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              start_dose = 4)
#   expect_equal(dose, 4)
# })
#
# test_that('careful_escalation does not skip doses', {
#   fit1 <- stan_crm('1NNN', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                    target = 0.33, model = 'empiric', beta_sd = 1,
#                    seed = 123)
#   dose <- careful_escalation(fit1, tox_threshold = 0.33,
#                              certainty_threshold = 0.8)
#   expect_lte(dose, max(fit1$doses) + 1)
#   expect_gte(fit1$recommended_dose, dose)
#
#   fit2 <- stan_crm('1NNN 2NNN', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                    target = 0.33, model = 'empiric', beta_sd = 1,
#                    seed = 123)
#   dose <- careful_escalation(fit2, tox_threshold = 0.33,
#                              certainty_threshold = 0.8)
#   expect_lte(dose, max(fit2$doses) + 1)
#   expect_gte(fit2$recommended_dose, dose)
# })
#
#
# test_that('careful_escalation stops appropriately', {
#   fit1 <- stan_crm('1NNN 2TTT', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                    target = 0.33, model = 'empiric', beta_sd = 1,
#                    seed = 123)
#   dose <- careful_escalation(fit1,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 1)
#   expect_equal(dose, 1)
#
#   dose <- careful_escalation(fit1,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 2)
#   expect_equal(dose, NA)
#
#
#
#   fit2 <- stan_crm('1NNN 2TTT 1T', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                    target = 0.33, model = 'empiric', beta_sd = 1,
#                    seed = 123)
#   dose <- careful_escalation(fit2,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 1)
#   expect_equal(dose, 1)
#
#   dose <- careful_escalation(fit2,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 2)
#   expect_equal(dose, NA)
#
#
#
#   fit3 <- stan_crm('1NNN 2TTT 1TT', skeleton = c(0.1, 0.2, 0.33, 0.6),
#                    target = 0.33, model = 'empiric', beta_sd = 1,
#                    seed = 123)
#   dose <- careful_escalation(fit3,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 1)
#   expect_equal(dose, NA)
#
#   dose <- careful_escalation(fit3,
#                              tox_threshold = 0.33,
#                              certainty_threshold = 0.8,
#                              reference_dose = 2)
#   expect_equal(dose, NA)
# })
