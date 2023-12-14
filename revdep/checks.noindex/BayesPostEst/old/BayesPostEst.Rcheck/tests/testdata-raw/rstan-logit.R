#
#   Generate am example RStan logit fitted model object
#

library(rstan)

df <- carData::Cowles
df$female <- (as.numeric(df$sex) - 2) * (-1)
df$volunteer <- as.numeric(df$volunteer) - 1
df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))

dl <- as.list(df[, c("volunteer", "female", "neuroticism", "extraversion")])
dl$N <- nrow(df)


mod.stan <- paste("	
data {
  int<lower=0> N;
  int<lower=0,upper=1> volunteer[N];
  vector[N] female;
  vector[N] neuroticism;
  vector[N] extraversion;
}
parameters {
  vector[4] b;
}
model {
  volunteer ~ bernoulli_logit(b[1] + b[2] * female + b[3] * neuroticism + b[4] * extraversion);
  for(i in 1:4){
    b[i] ~ normal(0, 3); 
  }
}

")

writeLines(mod.stan, "mod.stan")

rstan_logit <- stan(file = "mod.stan",  
                   data = dl,         
                   pars = c("b"),     
                   chains = 4,        
                   iter = 2000,       
                   seed = 123)     

unlink("mod.stan")

format(object.size(rstan_logit), "Kb")
saveRDS(rstan_logit, "tests/testdata/rstan-logit.rds")
