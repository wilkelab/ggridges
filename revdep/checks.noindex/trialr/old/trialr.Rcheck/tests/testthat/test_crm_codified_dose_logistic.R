# Test CRM.R
# Note: stan_crm is tested in its own file, test_stan_crm.R

# Published examples of CRM dose codification
# Table 3.1, p.19 of "Dose Finding by the Continual Reassessment Method",
# ISBN 9781420091519 by Cheung.

test_that('crm_codified_dose_logistic matches Cheung, example 1', {

  epsilon <- 0.01
  skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)
  a0 = 0
  beta_mean = exp(0)
  coded_doses <- crm_codified_dose_logistic(skeleton, a0, beta_mean)
  cheung <- c(-2.94, -1.99, -1.10, -0.41, 0.20)
  abs_diffs <- abs(coded_doses - cheung)
  expect_true(all(abs_diffs < epsilon))
})

test_that('crm_codified_dose_logistic matches Cheung, example 2', {

  epsilon <- 0.01
  skeleton <- c(0.05, 0.12, 0.25, 0.40, 0.55)
  a0 = 3
  beta_mean = exp(0)
  coded_doses <- crm_codified_dose_logistic(skeleton, a0, beta_mean)
  cheung <- c(-5.94, -4.99, -4.10, -3.41, -2.80)
  abs_diffs <- abs(coded_doses - cheung)
  expect_true(all(abs_diffs < epsilon))
})
