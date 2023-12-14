#
#   Generate an example brms logit model for testing
#

library("brms")

data("sim_data")

brms_logit <- brm(Y ~ X1 + X2, data = sim_data, family = bernoulli("logit"),
                  chains = 2, iter = 2000, warmup = 1000)

saveRDS(brms_logit, "tests/testdata/brms-logit.rds")
