test_that("Simple model runs with mcmcMargEff", {
  
  data("jags_interactive")
  fit <- jags_interactive
  
  data("sim_data_interactive")
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive$X2,
                        plot = F)
  
  ## testing
  value <- fit_me[25, 3]
  check_against <- 0.56214
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcMargEff with arguments", {
  
  data("jags_interactive")
  fit <- jags_interactive
  
  data("sim_data_interactive")
  
  ## processing the data
  mcmc <- coda::as.mcmc(fit)
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive$X2,
                        pointest = 'median',
                        seq = 50,
                        ci = .9,
                        hpdi = T,
                        plot = F)
  
  ## testing
  value <- fit_me[37, 4]
  check_against <- 0.5267605
  expect_equal(value, check_against, tolerance = 1e-2)
  
})

test_that("Simple model runs with mcmcMargEff with plotting arguments", {
  
  data("jags_interactive")
  fit <- jags_interactive
  
  data("sim_data_interactive")
  
  ## using mcmcMargEff
  plot_me <- mcmcMargEff(mod = fit,
                         main = 'b[2]',
                         int = 'b[4]',
                         moderator = sim_data_interactive$X2,
                         plot = T,
                         xlab = 'Moderating Variable',
                         ylab = 'Marginal Effect of X1')
  
  ## testing
  expect_equal(plot_me$labels$y, 'Marginal Effect of X1')
  expect_equal(plot_me$labels$x, 'Moderating Variable')
  
})

test_that("Simple model runs with mcmcMargEff with categorical moderator", {
  
  data("jags_interactive_cat")
  fit <- jags_interactive_cat
  
  data("sim_data_interactive_cat")
  
  ## using mcmcMargEff
  fit_me <- mcmcMargEff(mod = fit,
                        main = 'b[2]',
                        int = 'b[4]',
                        moderator = sim_data_interactive_cat$X3,
                        plot = F,
                        xlab = 'Moderating Variable',
                        ylab = 'Marginal Effect of X1')
  
  ## testing
  value <- fit_me[4, 3]
  check_against <- -0.91814
  expect_equal(value, check_against, tolerance = 1e-2)
  
})
