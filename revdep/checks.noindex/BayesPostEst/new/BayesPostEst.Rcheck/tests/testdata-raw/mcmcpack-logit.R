#
#   Generate am example MCMCpack logit fitted model object
#
if (require("MCMCpack", quietly = TRUE)) {
  
library("MCMCpack")

df <- carData::Cowles

df$female <- (as.numeric(df$sex) - 2) * (-1)
df$volunteer <- as.numeric(df$volunteer) - 1
df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))

set.seed(123)
fit.MCMCpack <- MCMClogit(volunteer ~ female + neuroticism + extraversion, 
                          data = df, burning = 1000, mcmc = 2000, seed = 123,
                          b0 = 0, B0 = 0.1)

mcmcpack_logit <- fit.MCMCpack

format(object.size(mcmcpack_logit), "Kb")

saveRDS(mcmcpack_logit, "tests/testdata/mcmcpack-logit.rds")

}
