test_that("Simple model runs with mcmcObsProb", {
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")
  datjags <- as.list(sim_data)
  
  ### observed value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  obs_prob <- mcmcObsProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim)
  
  value <- obs_prob[1, 1]
  check_against <- c(-0.998)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value <- obs_prob[7, 4]
  check_against <- c(0.617)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  
  ## Compare to Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/MCMC_observed_probs.R")
  # prot_obs_prob <- MCMC_observed_probs(model_matrix = xmat,
  #                                      mcmc_out = mcmc_mat,
  #                                      x_col = 2,
  #                                      x_range_vec = X1_sim,
  #                                      upper = 0.975,
  #                                      lower = 0.025)
  # 
  
})

test_that("Simple model runs with mcmcObsProb probit", {
  
  data("jags_probit")
  fit <- jags_probit
  
  data("sim_data")
  datjags <- as.list(sim_data)
  
  ### observed value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  obs_prob <- mcmcObsProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim,
                          link = "probit")
  
  value <- obs_prob[1, 1]
  check_against <- c(-0.998)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value <- obs_prob[7, 4]
  check_against <- c(0.618)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  
  ## Compare to Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/MCMC_observed_probs.R")
  # prot_obs_prob <- MCMC_observed_probs(model_matrix = xmat,
  #                                      mcmc_out = mcmc_mat,
  #                                      x_col = 2,
  #                                      x_range_vec = X1_sim,
  #                                      upper = 0.975,
  #                                      lower = 0.025)
  # 
  
})


test_that("full_sims argument works", {
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")
  datjags <- as.list(sim_data)
  
  ### observed value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  obs_prob <- mcmcObsProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim,
                          fullsims = TRUE)
  
  # not sure what to check here in terms of properness, this basically just 
  # established that the argument works
  expect_equal(
    colnames(obs_prob),
    c("Iteration", "x", "pp")
  )
  
})
