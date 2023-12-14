#
#   Generate an example runjags model for testing
#

data("sim_data")

# formatting the data for jags
datjags <- as.list(sim_data)
datjags$N <- length(datjags$Y)

model_string <- "
model{
  for(i in 1:N){
    Y[i] ~ dbern(p[i])  ## Bernoulli distribution of y_i
    logit(p[i]) <- mu[i]    ## Logit link function
    mu[i] <- b[1] +
      b[2] * X1[i] +
      b[3] * X2[i]
  }
  
  for(j in 1:3){
    b[j] ~ dnorm(0, 0.001) ## Use a coefficient vector for simplicity
  }
}
"

inits1 <- list("b" = rnorm(3))
inits2 <- list("b" = rnorm(3))
inits <- list(inits1, inits2)

set.seed(123)
runjags_logit <- run.jags(model = model_string, monitor = "b", data = datjags, 
                          n.chains = 2, sample = 2000, burnin = 1000,
                          inits = inits)

saveRDS(runjags_logit, "tests/testdata/runjags-logit.rds")

