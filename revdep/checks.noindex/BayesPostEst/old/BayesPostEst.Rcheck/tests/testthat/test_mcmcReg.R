data("jags_logit")

test_that("Simple model runs with mcmcReg", {
  ## running some expect silent code
  expect_silent(mcmcReg(jags_logit))
  expect_silent(mcmcReg(jags_logit, pars = c("b[1]", "b[3]")))
  expect_silent(mcmcReg(jags_logit, pars = c("b[1]", "b[3]"),
                        coefnames = c("beta 1", "beta 3")))
  expect_silent(mcmcReg(jags_logit, pars = c("b"), coefnames = paste("beta", 1:3),
                        regex = T))
  expect_silent(mcmcReg(jags_logit, pars = c("b"), regex = T))
  expect_silent(mcmcReg(jags_logit, pars = c("b\\[1\\]", "b\\[3\\]"), regex = T))
  expect_silent(object3 <- mcmcReg(jags_logit, pars = c("b"),
     custom.coef.names = c("Variable 1",
                           "Variable 2",
                           "Variable 3"), regex = T))
  expect_silent(mcmcReg(jags_logit, coefnames = c("Variable 1",
                                           "Variable 2",
                                           "Variable 3",
                                           "deviance")))
  expect_silent(mcmcReg(jags_logit, gof = 1234,
                        gofnames = "Fake jags_logit Statistic"))
  expect_silent(mcmcReg(jags_logit, format = "html", file = "table"))
  
  ## removing table.html to avoid non-standard file note on check
  if (file.exists("table.html")) file.remove("table.html")
})

test_that("mcmcReg works with hpdi", {
  ## running some general value matches
  expect_match(mcmcReg(jags_logit, pointest = "median", hpdi = T),
               "667.30; 675.03")
  expect_match(mcmcReg(jags_logit, pointest = "median", hpdi = T,
                       format = "html"), "667.30; 675.03")
})

test_that("mcmcReg works with custom coef names", {
  expect_match(mcmcReg(jags_logit, coefnames = c("beta 1","beta 2", "beta 3",
                                                 "deviance")), "beta 1")
  expect_error(mcmcReg(list(jags_logit, jags_logit),
                       coefnames = c("beta 1","beta 2",
                                     "beta 3", "deviance")),
               regexp = "number of models does not match number of custom coefficient vectors")
})

test_that("mcmcReg works with standard deviation", {
  expect_match(mcmcReg(jags_logit, sd = T), "\\$\\(0.16\\)\\$")
  expect_match(mcmcReg(jags_logit, sd = T, format = "html"), ">\\(0.16\\)<")
})

test_that("mcmcReg works with pr direction", {
  expect_match(mcmcReg(jags_logit, pr = T), "\\$0.89\\$")
  expect_match(mcmcReg(jags_logit, pr = T, format = "html"), ">0.89<")
})

test_that("mcmcReg works with filenames", {
  expect_silent(mcmcReg(jags_logit, sd = T, file = "tab.tex"))
  expect_silent(mcmcReg(jags_logit, sd = T, format = "html", file = "tab.html"))
})

pkgs <- c("runjags", "MCMCpack")
if (!all(sapply(pkgs, require, quietly = TRUE, character.only = TRUE))) {
  
  ## Generate an example runjags interactive fitted model
  ## formatting the data for jags
  datalist <- list(X = model.matrix(~ X1 * X2, sim_data_interactive),
                   Y = sim_data_interactive[, 3],
                   N = nrow(sim_data_interactive))
  
  ## creating jags model
  model <- "model { 
  for(i in 1 : N){ 
  Y[i] ~ dnorm(beta %*% X[i, ], tau);
  } 
  for(i in 1:4) {
  beta[i] ~ dnorm(0, 0.001)
  }
  tau ~ dexp(1)
  }"

  ## fitting the model with runjags
  runjags_interactive <- runjags::run.jags(model = model, monitor = c("beta", "tau"),
                                           data = datalist, n.chains = 2, method = "rjags")
  
  ## Do the pass test
  test_that("mcmcReg works with runjags", {
    expect_match(mcmcReg(runjags_interactive), "-0.55; -0.24")
  })
  
  ## fitting the model with MCMCpack
  mcmcpack_linear <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
                                           sigma.mu = 5, sigma.var = 10,
                                           data = list(X = rnorm(100),
                                                       Y = rnorm(100, 5, 5)),
                                           seed = 1)
  
  ## Do fail test with multiple object types
  test_that("mcmcReg fails with multiple object types", {
    
    expect_error(mcmcReg(list(jags_logit, runjags_interactive)))
    expect_error(mcmcReg(list(runjags_interactive, mcmcpack_linear)))
    expect_error(mcmcReg(list(jags_logit, mcmcpack_linear)))
    
    
  })
}

#### Commenting this out Jan 2021 since even with seed set too much randomness here
#### Will work on it later

# if (require("MCMCpack", quietly = TRUE)) {
#   ## fitting the model with MCMCpack
#   mcmcpack_linear2 <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
#                                            sigma.mu = 5, sigma.var = 10,
#                                            data = list(X = rnorm(100),
#                                                        Y = rnorm(100, 5, 5)),
#                                            seed = 1)
#   ## testing
#   testthat::test_that("mcmcReg works with mcmc", {
#     testthat::expect_match(mcmcReg(mcmcpack_linear2), "3.99;  6.00")
#   })
# }

pkgs_win <- c("rjags", "R2WinBUGS")
if (!all(sapply(pkgs_win, require, quietly = TRUE, character.only = TRUE))) {
  ## Generate an example BUGS fitted model object
  data(LINE, package = "rjags")
  LINE$recompile()
  
  ## fitting the model with jags
  bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                    n.iter = 1000)
  bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
  
  ## testing
  test_that("mcmcReg works with bugs", {
    expect_match(mcmcReg(bugs_model), "0.33; 3.64")
  })
}

# clean up output artifacts
unlink("tab.html")
unlink("tab.tex")