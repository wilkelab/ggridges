
test_that("Simple model runs with mcmcAveProb", {
  
  data("jags_logit")
  fit <- jags_logit
  
  data("sim_data")
  datjags <- as.list(sim_data)
  
  ### average value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  ave_prob <- mcmcAveProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim)
  
  value <- ave_prob[1, 1]
  check_against <- c(-0.998)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value <- ave_prob[7, 4]
  check_against <- c(0.629)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  
  ## Compare to Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/MCMC_simcase_probs.R")  
  # prob <- MCMC_simcase_probs(model_matrix = xmat,
  #                                      mcmc_out = mcmc_mat,
  #                                      x_col = 2,
  #                                      x_range_vec = X1_sim,
  #                            upper = 0.975,
  #                            lower = 0.025
  #                            )
  
  
})

test_that("Simple model runs with mcmcAveProb probit", {
  
  data("jags_probit")
  fit <- jags_probit
  
  data("sim_data")
  datjags <- as.list(sim_data)
  
  ### average value approach
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  X1_sim <- seq(from = min(datjags$X1),
                to = max(datjags$X1), 
                length.out = 10)
  ave_prob <- mcmcAveProb(modelmatrix = xmat,
                          mcmcout = mcmc_mat,
                          xcol = 2,
                          xrange = X1_sim,
                          link = "probit")
  
  value <- ave_prob[1, 1]
  check_against <- c(-0.998)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  value <- ave_prob[7, 4]
  check_against <- c(0.629)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  
  ## Compare to Johannes' previous function
  # devtools::source_url("https://raw.githubusercontent.com/jkarreth/JKmisc/master/MCMC_simcase_probs.R")  
  # prob <- MCMC_simcase_probs(model_matrix = xmat,
  #                                      mcmc_out = mcmc_mat,
  #                                      x_col = 2,
  #                                      x_range_vec = X1_sim,
  #                            upper = 0.975,
  #                            lower = 0.025
  #                            )
  
  
})
