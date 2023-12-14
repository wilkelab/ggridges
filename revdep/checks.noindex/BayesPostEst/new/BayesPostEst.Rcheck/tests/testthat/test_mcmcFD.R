data("sim_data")
data("jags_probit")
data("jags_logit")

test_that("Simple model runs with mcmcFD", {
  
  fit <- jags_logit
  
  ## running function with logit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat)
  
  value <- object[1, 2]
  check_against <- c(0.048)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
})

test_that("Simple probit model runs with mcmcFD", {
  
  fit <- jags_probit
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat,
                   link = "probit") ## check to see if these are correct
  
  value <- object[1, 2]
  check_against <- c(0.050)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  ## checking FD plot
  full <- mcmcFD(modelmatrix = xmat,
                 mcmcout = mcmc_mat,
                 fullsims = TRUE) # first running mcmcFD with full output
  expect_silent(plot(full))
  
})


test_that("ROPE version works", {
  
  fit <- jags_probit
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat,
                   link = "probit") ## check to see if these are correct
  
  value <- object[1, 2]
  check_against <- c(0.050)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
  
  ## checking FD plot
  full <- mcmcFD(modelmatrix = xmat,
                 mcmcout = mcmc_mat,
                 fullsims = TRUE) # first running mcmcFD with full output
  expect_silent(plot(full, ROPE = c(0.1, 0.15)))
  
})

test_that("Errors work", {
  
  fit <- jags_probit
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  
  expect_error(mcmcFD(modelmatrix = xmat,
                      mcmcout = mcmc_mat,
                      link = 'legit'))
  expect_error(mcmcFD())
  
})

test_that("Print method works", {
  
  fit <- jags_probit
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  
  expect_output(print(mcmcFD(modelmatrix = xmat, mcmcout = mcmc_mat,
                             link = 'logit')),
                '0.08001370')
  expect_output(print(mcmcFD(modelmatrix = xmat, mcmcout = mcmc_mat, link = 'logit', fullsims = T)))
  
})

test_that("Deprecated plotting function works", {
  
  fit <- jags_probit
  
  ## running function with probit
  xmat <- model.matrix(Y ~ X1 + X2, data = sim_data)
  mcmc <- coda::as.mcmc(fit)
  mcmc_mat <- as.matrix(mcmc)[, 1:ncol(xmat)]
  object <- mcmcFD(modelmatrix = xmat,
                   mcmcout = mcmc_mat,
                   link = "probit") ## check to see if these are correct

  ## checking FD plot
  full <- mcmcFD(modelmatrix = xmat,
                 mcmcout = mcmc_mat,
                 fullsims = TRUE) # first running mcmcFD with full output
  expect_warning(mcmcFDplot(full))
  expect_warning(mcmcFDplot(full, ROPE = c(0, 10)))
  expect_error(plot(object))
  
})