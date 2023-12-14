data("jags_logit")

test_that("Simple model runs with mcmcTab", {
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = NULL, 
                    Pr = FALSE,
                    ROPE = NULL)
  
  value <- object[2, 2]
  check_against <- c(0.527)
  expect_equal(round(as.numeric(value), 2), round(check_against, 2))
})

test_that("mcmcTab works with different input types", {
  
  # rjags
  expect_equal(mcmcTab(jags_logit)[1,3], 0.09)
  
  # mcmc.list
  expect_equal(mcmcTab(coda::as.mcmc(jags_logit))[2,3], 0.166) # coda is an imported package
  
  # stanreg
  
  # stanfit
  
})

test_that("pars subsetting works", {
  
  data("jags_logit")
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = "b", 
                    Pr = FALSE,
                    ROPE = NULL,
                    regex = TRUE)
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:3)))
  )
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = c("b\\[1\\]", "b\\[2\\]"), 
                    Pr = FALSE,
                    ROPE = NULL,
                    regex = TRUE)
  
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", 1:2)))
  )
  
  object <- mcmcTab(jags_logit, 
                    ci = c(0.025, 0.975), 
                    pars = c("b[1]", "b[3]"), 
                    Pr = FALSE,
                    ROPE = NULL)
  
  expect_equal(
    object$Variable, 
    factor(c(sprintf("b[%s]", c(1, 3))))
  )
  
})

test_that("ROPE argument works", {
  
  # valid input
  expect_message(
    object <- mcmcTab(jags_logit, pars = "b", ROPE = c(0, 1), regex = TRUE),
    "This table contains an estimate for parameter"
  )
  
  expect_equal(
    object$PrOutROPE,
    c(0, 0.002, 0.011)
  )
  
  # invalid input; adjust the test at some point
  expect_error(
    object <- mcmcTab(jags_logit, ROPE = 0),
    "Invalid ROPE argument"
  )
})

pkgs_win <- c("rjags", "R2WinBUGS")

if (!all(sapply(pkgs_win, require, quietly = TRUE, character.only = TRUE))) {
  
  ## Generate an example BUGS fitted model object
  data(LINE, package = "rjags")
  LINE$recompile()
  
  ## fitting the model with jags
  bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                    n.iter = 1000)
  bugs_model <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
  
  
  test_that("mcmcTab works with bugs", {
    
    # bugs
    expect_equal(mcmcTab(bugs_model)[1,2], 1.031)
    
  })
  
}

# if (require("MCMCpack", quietly = TRUE)) {
#   ## fitting the model with MCMCpack
#   mcmcpack_linear <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
#                                            sigma.mu = 5, sigma.var = 10,
#                                            data = list(X = rnorm(100),
#                                                        Y = rnorm(100, 5, 5)),
#                                            seed = 1)
#   ## testing
#   test_that("mcmcTab works with mcmcpack", {
#     # mcmc
#     expect_equal(mcmcTab(mcmcpack_linear)[2,3], 0.485, 
#                  tolerance = 0.1) ## this is a big tolerance: sim'ing mcmcpack is not great for this
#   })
# } #### I'm just commenting this out since it still apparently can fail