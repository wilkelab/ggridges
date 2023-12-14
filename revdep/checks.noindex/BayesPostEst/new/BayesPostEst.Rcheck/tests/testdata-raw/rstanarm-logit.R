#
#   Generate am example rstanarm logit fitted model object
#

library(rstanarm)

df <- carData::Cowles
df$female <- (as.numeric(df$sex) - 2) * (-1)
df$volunteer <- as.numeric(df$volunteer) - 1
df$extraversion <- (df$extraversion - mean(df$extraversion)) / (2 * sd(df$extraversion))
df$neuroticism <- (df$neuroticism - mean(df$neuroticism)) / (2 * sd(df$neuroticism))

rstanarm_logit <- stan_glm(volunteer ~ female + neuroticism + extraversion, 
                           data = df, family = binomial(link = "logit"),
                           prior = normal(0, 3),
                           prior_intercept = normal(0, 3),
                           chains = 4, 
                           iter = 2000,
                           seed = 123)

format(object.size(rstanarm_logit), "Kb")
saveRDS(rstanarm_logit, "tests/testdata/rstanarm-logit.rds")
