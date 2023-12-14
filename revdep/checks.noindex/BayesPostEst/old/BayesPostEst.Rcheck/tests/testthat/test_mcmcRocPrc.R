

# Test mcmcRocPrc constructor and default method --------------------------

# pull out a predicted probability matrix and yvec, for simple testing
data("jags_logit")
object   <- jags_logit
mdl_data <- object$model$data()
xdata <- as.matrix(cbind(X0 = 1L, as.data.frame(mdl_data[c("X1", "X2")])))
yvec  <- mdl_data[["Y"]]
pardraws <- as.matrix(coda::as.mcmc(object))
betadraws <- pardraws[, c(sprintf("b[%s]", 1:ncol(xdata - 1)))]
pred_prob <- plogis(xdata %*% t(betadraws))  

test_that("constructor and default method work", {
  
  expect_error(new_mcmcRocPrc(pred_prob, yvec, curves = FALSE, fullsims = FALSE),
               NA)
  # since pred_prob is a matrix, it should dispatch to the .default method,
  # which is setup to use a matrix of predicted probabilities as input
  expect_error(mcmcRocPrc(pred_prob, FALSE, FALSE, yvec = yvec), NA)

})

test_that("constructor rejects invalid input", {
  
  # matrix rows and yvec length differ
  expect_error(
    new_mcmcRocPrc(pred_prob[-1, ], yvec, FALSE, FALSE),
    "number of predictions and observed outcomes do not match"
  )
  # yvec is not all 0 or 1
  tmp <- yvec
  tmp[1] <- 0.1
  expect_error(
    new_mcmcRocPrc(pred_prob, tmp, FALSE, FALSE),
    "yvec must be 0 or 1"
  )
  # pred_prob is not all withint [0, 1]
  tmp <- pred_prob
  tmp[1, 1] <- 1.1
  expect_error(
    new_mcmcRocPrc(tmp, yvec, FALSE, FALSE),
    "must be in the interval"
  )
})

test_that(".default method rejects invalid input", {
  
  # object is not a matrix
  expect_error(
    mcmcRocPrc(pred_prob[, 1], FALSE, FALSE, yvec),
    "requires 'matrix'"
  )
  
})

test_that("Simple model runs with mcmcRocPrc Full", {
  
  data("jags_logit")
  
  ## using mcmcRocPrc with full draws
  full_with_curves <- mcmcRocPrc(jags_logit,
                                 yname = "Y",
                                 xnames = c("X1", "X2"),
                                 curves = TRUE,
                                 fullsims = TRUE)
  
  ## testing
  value_area_under_roc <- as.numeric(full_with_curves$area_under_roc[986])
  check_against_full_roc <- c(0.626)
  expect_equal(value_area_under_roc, check_against_full_roc, tolerance = 1e-2)
  
  value_area_under_prc <- as.numeric(full_with_curves$area_under_prc[965])
  check_against_full_prc <- c(0.61994)
  expect_equal(value_area_under_prc, check_against_full_prc, tolerance = 1e-2)
  
})


# JAGS-like input (rjags, R2jags, runjags) --------------------------------

test_that("class 'jags' input works", {
  # both the "runjags" and "rjags" classes produced by jags(), run.jags(),
  # from packages R2jags and runjags include "jags" classes that are model 
  # definitions without posterior samples (see rjags::jags.model()). Both 
  # include underlying "jags" classes in them
  data("jags_logit")
  jags_object       <- jags_logit$model
  posterior_samples <- as.mcmc(jags_logit)
  
  expect_error(
    out <- mcmcRocPrc(jags_object, curves = FALSE, fullsims = FALSE, 
                      yname = "Y", xnames = c("X1", "X2"), 
                      posterior_samples = posterior_samples),
    NA
  )
  
  
})

test_that("JAGS logit input works", {
  
  data("jags_logit")
  
  ## using mcmcRocPrc
  expect_error(
    with_curves <- mcmcRocPrc(jags_logit,
                              yname = "Y",
                              xnames = c("X1", "X2"),
                              curves = TRUE,
                              fullsims = FALSE),
    NA
  )
  
  ## testing
  value <- with_curves$prc_dat[[1]][156, 2]
  check_against <- c(0.658)
  expect_equal(value, check_against, tolerance = 1e-2)
  
  value_roc <- as.numeric(with_curves$area_under_roc)
  check_against_roc <- c(0.627)
  expect_equal(value_roc, check_against_roc, tolerance = 1e-2)
  
  value_prc <- as.numeric(with_curves$area_under_prc)
  check_against_prc <- c(0.621)
  expect_equal(value_prc, check_against_prc, tolerance = 1e-2)
  
})

test_that("JAGS probit input works", {
  
  data("jags_probit")
  
  expect_error(
    with_curves <- mcmcRocPrc(jags_probit,
                              yname = "Y",
                              xnames = c("X1", "X2"),
                              curves = TRUE,
                              fullsims = FALSE),
    NA
  )
  
})

test_that("Non-logit/probit JAGS does not work", {
  data(jags_logit)
  fake_jags <- structure(
    list(
      model = structure(
        list(model = function() "incompatible model"), 
        class = "jags"
      ),
      BUGSoutput = jags_logit$BUGSoutput),
    class = "rjags"
  )
  
  expect_error(mcmcRocPrc(fake_jags, 
                          yname = "Y",
                          xnames = c("X1", "X2")),
               "Could not identify model link function")
  
})



test_that("runjags input works", {
  runjags_logit <- readRDS("../testdata/runjags-logit.rds")
  expect_error(
    out <- mcmcRocPrc(runjags_logit, FALSE, FALSE, yname = "Y", 
                      xnames = c("X1", "X2")),
    NA
  )
})


# STAN-like input (rstan, rstanarm, brms) ---------------------------------

#
#   rstan input
#
#   The next couple of tests build up to checking if rstan input works
#

rstan_logit <- readRDS("../testdata/rstan-logit.rds")

# based on the ?stan_model example and code at 
# https://github.com/stan-dev/rstan/blob/develop/rstan/rstan/R/AllClass.R
# !!!
# stan_model requires compiling; figure out a way to do this for tests
# !!!
# stancode <- 'data {real y_mean;} parameters {real y;} model {y ~ normal(y_mean,1);}'
# mod      <- stan_model(model_code = stancode, verbose = TRUE)
# nonbinary_model <- new("stanfit", 
#                        model_name = "foo", model_pars = character(0), 
#                        par_dims = list(NULL), mode = 1L, sim = list(0), 
#                        inits = list(0), stan_args = list(0), 
#                        stanmodel = mod,
#                        date = "2020-06-17", .MISC = emptyenv())

test_that("binary/non-binary stanfit models are correctly IDd", {
  
  # It would be good to also have a negative test for a non-binary model. 
  # Unfortunately stanfit is a S4 class, so it's not super straightforward
  # to create dummy examples for testing, like I do with the S3 classes 
  # elsewhere here. See the commented out code above. 
  
  expect_true(is_binary_model(rstan_logit))
  expect_equal(identify_link_function(rstan_logit), "logit")
  
})

test_that("RStan input works", {
  df <- carData::Cowles
  df$female <- (as.numeric(df$sex) - 2) * (-1)
  df$volunteer <- as.numeric(df$volunteer) - 1
  df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
  df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))
  
  dl <- as.list(df[, c("volunteer", "female", "neuroticism", "extraversion")])
  dl$N <- nrow(df)
  
  expect_error(
    out <- mcmcRocPrc(rstan_logit, FALSE, FALSE, data = dl, yname = "volunteer",
                      xnames = c("female", "neuroticism", "extraversion")),
    NA
  )
})

#
#   rstanarm input
#

test_that("rstanarm input works", {
  rstanarm_logit <- readRDS("../testdata/rstanarm-logit.rds")
  expect_error(
    out <- mcmcRocPrc(rstanarm_logit, FALSE, FALSE),
    NA
  )
  # non-binomial models are rejected
  dummy_model <- structure(
    list(family = gaussian()),
    class = "stanreg"
  )
  expect_error(
    mcmcRocPrc(dummy_model),
    "does not seem to be a binary choice model"
  )
  
})

#
#   brms input
#

test_that("brms input works", {
  
  brms_logit <- readRDS("../testdata/brms-logit.rds")
  
  expect_error(
    out <- mcmcRocPrc(brms_logit, FALSE, FALSE),
    NA
  )
  
  # non-binomial models are rejected
  dummy_model <- brms_logit
  dummy_model$formula$family <- gaussian()
  expect_error(
    mcmcRocPrc(dummy_model),
    "does not seem to be a binary choice model"
  )
  
})


# Other types of input (MCMpack, BUGS) ------------------------------------

pkgs_win <- c("rjags", "R2WinBUGS")
if (!all(sapply(pkgs_win, require, quietly = TRUE, character.only = TRUE))) {
  ## Generate an example BUGS fitted model object
  data(LINE, package = "rjags")
  LINE$recompile()
  
  ## fitting the model with jags
  bugs_model <- rjags::coda.samples(LINE, c("alpha", "beta", "sigma"),
                                    n.iter = 1000)
  bugs_logit <- R2WinBUGS::as.bugs.array(sims.array = as.array(bugs_model))
  
  ## loading sim data
  sim_data <- BayesPostEst::sim_data
  
  ## testing
  test_that("BUGS input works", {
  expect_error(
    out <- mcmcRocPrc(bugs_logit, FALSE, FALSE, data = sim_data, yname = "Y",
                      xnames = c("X1", "X2"), type = "logit"),
    NA
  )
  })
}

test_that("MCMCpack input works", {
  mcmcpack_logit <- readRDS("../testdata/mcmcpack-logit.rds")
  
  df <- carData::Cowles
  df$female <- (as.numeric(df$sex) - 2) * (-1)
  df$volunteer <- as.numeric(df$volunteer) - 1
  df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
  df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))
  
  expect_error(
    out <- mcmcRocPrc(mcmcpack_logit, FALSE, FALSE, data = df, yname = "volunteer",
                      xnames = c("female", "neuroticism", "extraversion"),
                      type = "logit"),
    NA
  )
  
  # a mcmc object without call attribute should cause an error
  mock <- structure(
    list(NULL),
    class = "mcmc"
  )
  expect_error(
    mcmcRocPrc(mock, FALSE, FALSE, data = df, yname = "volunteer",
               xnames = c("female", "neuroticism", "extraversion"),
               type = "logit"),
    "object does not have a 'call' attribute"
  )
  
  # a mcmc object whose call attribute does not indicate MCMClogit or MCMCprobit
  # was used should also cause an error
  if (require("MCMCpack", quietly = TRUE)) {
    ## fitting the model with MCMCpack
    mcmcpack_linear <- MCMCpack::MCMCregress(Y ~ X, b0 = 0, B0 = 0.001,
                                             sigma.mu = 5, sigma.var = 10,
                                             data = list(X = rnorm(100),
                                                         Y = rnorm(100, 5, 5)),
                                             seed = 1)
    ## testing
    expect_error(
      mcmcRocPrc(mcmcpack_linear, FALSE, FALSE, data = df, yname = "volunteer",
                 xnames = c("female", "neuroticism", "extraversion"),
                 type = "logit"),
      "object does not appear to have been fitted using"
    )
    
    # but we can force computation anyways using force;
    # simulate an instance in which this would be desirable by stripping the call
    # attribute from mcmcpack_logit
    attr(mcmcpack_logit, "call") <- NULL
    expect_error(
      mcmcRocPrc(mcmcpack_logit, FALSE, FALSE, data = df, yname = "volunteer",
                 xnames = c("female", "neuroticism", "extraversion"),
                 type = "logit"),
      "object does not have a 'call' attribute"
    )
    expect_error(
      mcmcRocPrc(mcmcpack_logit, FALSE, FALSE, data = df, yname = "volunteer",
                 xnames = c("female", "neuroticism", "extraversion"),
                 type = "logit", 
                 force = TRUE),
      NA
    )
  }
})

# Test methods for print, plot, etc. --------------------------------------

# Now that it's established that basic creation works, set up examples of all
# four possible return value types that can be used for the rest of the tests
# below

with_curves <- mcmcRocPrc(jags_logit,
                          yname = "Y",
                          xnames = c("X1", "X2"),
                          curves = TRUE,
                          fullsims = FALSE)

no_curves <- mcmcRocPrc(jags_logit,
                        yname = "Y",
                        xnames = c("X1", "X2"),
                        curves = FALSE,
                        fullsims = FALSE)

full_no_curves <- mcmcRocPrc(jags_logit,
                             yname = "Y",
                             xnames = c("X1", "X2"),
                             curves = FALSE,
                             fullsims = TRUE)

full_with_curves <- mcmcRocPrc(jags_logit,
                               yname = "Y",
                               xnames = c("X1", "X2"),
                               curves = TRUE,
                               fullsims = TRUE)

test_that("mcmcRocPrc returns the same output structure, sans NULL elements", {
  
  expect_true(is.list(with_curves$prc_dat))
  expect_true(is.list(full_with_curves$prc_dat))
  
})


test_that("print method works", {
  
  expect_equal(
    capture_output(print(with_curves)), 
    "mcmcRocPrc object\ncurves: TRUE; fullsims: FALSE\nAUC-ROC: 0.627\nAUC-PR:  0.621"
  )
  expect_equal(
    capture_output(print(no_curves)), 
    "mcmcRocPrc object\ncurves: FALSE; fullsims: FALSE\nAUC-ROC: 0.627\nAUC-PR:  0.621"
  )
  expect_equal(
    capture_output(print(full_with_curves)), 
    "mcmcRocPrc object\ncurves: TRUE; fullsims: TRUE\nAUC-ROC: 0.624 [80%: 0.619 - 0.627]\nAUC-PR:  0.618 [80%: 0.613 - 0.620]"
  )
  expect_equal(
    capture_output(print(full_no_curves)), 
    "mcmcRocPrc object\ncurves: FALSE; fullsims: TRUE\nAUC-ROC: 0.624 [80%: 0.619 - 0.627]\nAUC-PR:  0.618 [80%: 0.613 - 0.620]"
  )
  
})

test_that("plot method gives informative errors", {
  
  expect_error(plot(no_curves), "to generate data for plots")
  expect_error(plot(with_curves, n = 0), "n must be")
  expect_error(plot(with_curves, alpha = 5), "alpha must be")
  
})

test_that("plot method works", {
  
  expect_error(plot(with_curves), NA)
  expect_error(plot(full_with_curves), NA)
  
  expect_error(plot(no_curves), "to generate data for plots")
  expect_error(plot(full_no_curves), "to generate data for plots")

})

# the plots from above will be sent to a Rplots.pdf file; clean that up
unlink("Rplots.pdf")

test_that("data frame conversion works with all 4 output sets", {
  
  # all 4 output types have AUC, so this should work across the board
  # (auc is the default what argument)
  expect_equal(nrow(as.data.frame(no_curves)), 1L)
  expect_equal(nrow(as.data.frame(with_curves)), 1L)
  expect_equal(nrow(as.data.frame(full_no_curves)), 2000L)
  expect_equal(nrow(as.data.frame(full_with_curves)), 2000L)
  
  # when called without curves, there will be no data for what = "roc"/"prc"
  expect_error(as.data.frame(no_curves, what = "roc"), "No curve data")
  expect_error(as.data.frame(no_curves, what = "prc"), "No curve data")
  expect_error(as.data.frame(full_no_curves, what = "roc"), "No curve data")
  expect_error(as.data.frame(full_no_curves, what = "roc"), "No curve data")
  
  # Otherwise, roc/prc data will either be average across sims, or a curve
  # for each sim. To ensure consistency in output, always return coordinate
  # data with an identifying "sim" column
  
  expect_error(out <- as.data.frame(with_curves, what = "roc"), NA)
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), c("sim", "x", "y"))
  expect_equal(unique(out$sim), 1L)
  expect_equal(nrow(out), 501L)
  
  # fullsims
  expect_error(out <- as.data.frame(full_with_curves, what = "roc"), NA)
  expect_s3_class(out, "data.frame")
  expect_equal(colnames(out), c("sim", "x", "y"))
  expect_equal(length(unique(out$sim)), 2000L)
  expect_equal(nrow(out), 501L*2000L)
  
  
})

test_that("auc_roc and pr work", {
  
  expect_equal(auc_roc(c(0, 0, 1, 1), c(0, 0, 1, 1)), 1)
  expect_equal(auc_pr(c(0, 0, 1, 1), c(0, 0, 1, 1)), NaN)
  
})


