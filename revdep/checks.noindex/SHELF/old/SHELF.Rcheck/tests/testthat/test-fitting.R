
test_that("normal distribution fitting and feedback works",{
  skip_on_cran()
  m <- 10
  s <- 20
  vals <- c(m - s, m , m + 2 * s)
  myfit <- fitdist(vals, pnorm(vals, m, s ))
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(m -0.5*s, m+s))
  norm.parameters <- unlist(myfit$Normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(norm.parameters) <- NULL
  expect_equal(norm.parameters, c(m, s))
  expect_equal(best.name, "normal")
  expect_equal(fb$fitted.quantiles[, 1], 
               signif(qnorm(c(0.05, 0.95), m, s),3))
  expect_equal(fb$fitted.probabilities[, 1],
               signif(pnorm(c(m -0.5*s, m+s), m, s),3))
})

test_that("student-t distribution fitting and feedback works",{
  skip_on_cran()
  m <- 10
  s <- 20
  tdftest <- 4
  vals <- c(m - s, m , m + 2 * s)
  myfit <- fitdist(vals, pt((vals-m)/s, tdftest ), tdf = tdftest)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(m -0.5*s, m+s))
  t.parameters <- unlist(myfit$Student.t)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(t.parameters) <- NULL
  expect_equal(t.parameters, c(m, s, tdftest), tolerance = 0.001)
  expect_equal(best.name, "t")
  expect_equal(fb$fitted.quantiles[, "t"], 
               signif(m + s * qt(c(0.05, 0.95), tdftest),3))
  expect_equal(fb$fitted.probabilities[, "t"],
               signif(pt(c( -0.5, 1), tdftest),3))
})


test_that("log-t distribution fitting and feedback works",{
  skip_on_cran()
  m <- log(30)
  s <- 0.5
  tdftest <- 5
  vals <- c(22, 30, 42)
  myfit <- fitdist(vals, pt((log(vals) - m) / s, tdftest ), lower = 0, tdf = tdftest)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(25, 55))
  lt.parameters <- unlist(myfit$Log.Student.t)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lt.parameters) <- NULL
  expect_equal(lt.parameters, c(m, s, tdftest), tolerance = 0.001)
  expect_equal(best.name, "logt")
  expect_equal(fb$fitted.quantiles[, "logt"], 
               signif(exp(m + s * qt(c(0.05, 0.95), tdftest)), 3))
  expect_equal(fb$fitted.probabilities[, "logt"],
               signif(pt((log(c(25, 55)) - m )/s, tdftest), 3))
})

test_that("mirror log-t distribution fitting and feedback works",{
  skip_on_cran()
  m <- log(30)
  s <- 0.5
  tdftest <- 5
  vals <- c(22, 30, 42)
  u <- 60
  myfit <- fitdist(vals, 1 - pt((log(u - vals) - m) / s, tdftest ), lower = 0, tdf = tdftest,
                   upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(25, 55))
  mirrorlogtparameters <- unlist(myfit$mirrorlogt)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(mirrorlogtparameters) <- NULL
  expect_equal(mirrorlogtparameters, c(m, s, tdftest), tolerance = 0.001)
  expect_equal(best.name, "mirrorlogt")
  expect_equal(fb$fitted.quantiles[, "mirrorlogt"], 
               signif(u - exp(m + s * qt(1 - c(0.05, 0.95), tdftest)), 3))
  expect_equal(fb$fitted.probabilities[, "mirrorlogt"],
               signif(1 - pt((log(u - c(25, 55)) - m )/s, tdftest), 3))
})



test_that("scaled beta distribution fitting and feedback works",{
  skip_on_cran()
  a <- 5
  b <- 20
  l <- 10
  u <- 60
  vals <- c(18, 20, 24)
  myfit <- fitdist(vals, pbeta((vals-l)/(u-l), a, b ), lower = l, upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(19, 29))
  beta.parameters <- unlist(myfit$Beta)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(beta.parameters) <- NULL
  expect_equal(beta.parameters, c(a, b), tolerance = 0.001)
  expect_equal(best.name, "beta")
  expect_equal(fb$fitted.quantiles[, "beta"], 
               signif(l + (u-l) * qbeta(c(0.05, 0.95), a, b),3))
  expect_equal(fb$fitted.probabilities[, "beta"],
               signif(pbeta((c(19, 29)-l)/(u-l), a, b),3))
})

test_that("shifted lognormal distribution fitting and feedback works",{
  skip_on_cran()
  l <- -100
  m <- log(30)
  s <- 0.5
  vals <- c(22, 30, 42) + l
  myfit <- fitdist(vals, plnorm(vals - l, m, s ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(25, 55) + l)
  lnorm.parameters <- unlist(myfit$Log.normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lnorm.parameters) <- NULL
  expect_equal(lnorm.parameters, c(m, s), tolerance = 0.001)
  expect_equal(best.name, "lognormal")
  expect_equal(fb$fitted.quantiles[, "lognormal"], 
               signif(l + qlnorm(c(0.05, 0.95), m, s),3))
  expect_equal(fb$fitted.probabilities[, "lognormal"],
               signif(plnorm(c(25, 55), m, s),3))
})

test_that("shifted lognormal distribution fitting and feedback works",{
  skip_on_cran()
  m <- 2
  s <- 0.5
  l <- -10
  vals <- c(-6, -2, 2, 6)
  myfit <- fitdist(vals, plnorm(vals - l, m, s ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(-4, 4))
  lnorm.parameters <- unlist(myfit$Log.normal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(lnorm.parameters) <- NULL
  expect_equal(lnorm.parameters, c(m, s), tolerance = 0.001)
  expect_equal(best.name, "lognormal")
  expect_equal(fb$fitted.quantiles[, "lognormal"], 
               signif(qlnorm(c(0.05, 0.95), m, s)+l, 3) )
  expect_equal(fb$fitted.probabilities[, "lognormal"],
               signif(plnorm(c(-4, 4) - l, m, s),3))
})

test_that("mirror lognormal distribution fitting and feedback works",{
  skip_on_cran()
  m <- 2
  s <- 0.5
  u <- 10
  vals <- c(-6, -2, 2, 6)
  myfit <- fitdist(vals, 1 - plnorm(u - vals, m, s ), lower = -20,
                   upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(-4, 4))
  mirrorlognormalparameters <- unlist(myfit$mirrorlognormal)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(mirrorlognormalparameters) <- NULL
  expect_equal(mirrorlognormalparameters, c(m, s),
               tolerance = 0.001)
  expect_equal(best.name, "mirrorlognormal")
  expect_equal(fb$fitted.quantiles[, "mirrorlognormal"], 
               signif(u - qlnorm(1 - c(0.05, 0.95), m, s), 3) )
  expect_equal(fb$fitted.probabilities[, "mirrorlognormal"],
               signif(1 - plnorm(u - c(-4, 4), m, s),3))
})


test_that("shifted gamma distribution fitting and feedback works",{
  skip_on_cran()
  a <- 50
  b <- 2
  l <- 10
  vals <- c(32, 35, 37)
  myfit <- fitdist(vals, pgamma(vals-l, a, b ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(33, 40))
  gamma.parameters <- unlist(myfit$Gamma)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(gamma.parameters) <- NULL
  expect_equal(gamma.parameters, c(a, b), tolerance = 0.001)
  expect_equal(best.name, "gamma")
  expect_equal(fb$fitted.quantiles[, "gamma"], 
               signif(l + qgamma(c(0.05, 0.95), a, b),3))
  expect_equal(fb$fitted.probabilities[, "gamma"],
               signif(pgamma(c(33, 40)-l, a, b),3))
})

test_that("mirror gamma distribution fitting and feedback works",{
  skip_on_cran()
  a <- 50
  b <- 2
  u <- 25
  p <- c(0.25, 0.5 , 0.75)
  v <- u - qgamma(1 - p, a, b)
  myfit <- fitdist(vals = v, probs = p, lower = -10,
                   upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(-3, 3))
  mirrorgammaparameters <- unlist(myfit$mirrorgamma)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(mirrorgammaparameters) <- NULL
  expect_equal(mirrorgammaparameters, c(a, b), tolerance = 0.001)
  expect_equal(best.name, "mirrorgamma")
  expect_equal(fb$fitted.quantiles[, "mirrorgamma"], 
               signif(u - qgamma(1 - c(0.05, 0.95), a, b),3))
  expect_equal(fb$fitted.probabilities[, "mirrorgamma"],
               signif(1 - pgamma(u - c(-3, 3), a, b),3))
})

test_that("shifted exponential distribution fitting and feedback works",{
  skip_on_cran()
  lambda <- 0.5
  l <- 10
  vals <- 12
  myfit <- fitdist(vals, pexp(vals-l, rate = lambda ), lower = l)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(11, 14))
  gamma.parameters <- unlist(myfit$Gamma)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(gamma.parameters) <- NULL
  expect_equal(gamma.parameters, c(1, lambda), tolerance = 0.001)
  expect_equal(best.name, "gamma")
  expect_equal(fb$fitted.quantiles[, "gamma"], 
               signif(l + qgamma(c(0.05, 0.95), 1, lambda),3))
  expect_equal(fb$fitted.probabilities[, "gamma"],
               signif(pgamma(c(11, 14)-l, 1, lambda),3))
})

test_that("mirror exponential distribution fitting and feedback works",{
  skip_on_cran()
  lambda <- 0.1
  u <- 25
  p <- 0.33
  v <- u - qgamma(1 - p, 1, lambda)
  myfit <- fitdist(vals = v, probs = p, lower = -10,
                   upper = u)
  fb <- feedback(myfit, quantiles=c(0.05, 0.95), values = c(-3, 3))
  mirrorgammaparameters <- unlist(myfit$mirrorgamma)
  best.name <- as.character(unlist(myfit$best.fitting))
  attributes(mirrorgammaparameters) <- NULL
  expect_equal(mirrorgammaparameters, c(1, lambda), tolerance = 0.001)
  expect_equal(best.name, "mirrorgamma")
  expect_equal(fb$fitted.quantiles[, "mirrorgamma"], 
               signif(u - qgamma(1 - c(0.05, 0.95), 1, lambda),3),
               tolerance = 0.01)
  expect_equal(fb$fitted.probabilities[, "mirrorgamma"],
               signif(1 - pgamma(u - c(-3, 3), 1, lambda),3))
})



test_that("precision fitting works - normal",{
  skip_on_cran()
  med <- 10
  k <- 1
  # sigma^-2 ~ gamma(a, b)
  a <- 3
  b <- 4
  sigmasq <- 1 / qgamma(c(0.05, 0.95), a, b)
  probs1 <- pnorm(rep(med + k, 2), med, sigmasq^0.5) - 0.5
  pfit1 <- fitprecision(c(med, med + k), probs1, pplot = F)
  gamma.parameters1 <- unlist(pfit1$Gamma)
  attributes(gamma.parameters1) <- NULL
  expect_equal(gamma.parameters1, c(a, b), tolerance = 1e-4)
  
  probs2 <- pnorm(rep(med - k, 2), med, sigmasq^0.5) 
  pfit2 <- fitprecision(c(-Inf, med - k), probs2, med = med,
                        pplot = F)
  gamma.parameters2 <- unlist(pfit2$Gamma)
  attributes(gamma.parameters2) <- NULL
  expect_equal(gamma.parameters2, c(a, b), tolerance = 1e-4)
  
  probs3 <- 1 - pnorm(rep(med + k, 2), med, sigmasq^0.5) 
  pfit3 <- fitprecision(c(med + k, Inf), probs3, med = med,
                        pplot = F)
  gamma.parameters3 <- unlist(pfit3$Gamma)
  attributes(gamma.parameters3) <- NULL
  expect_equal(gamma.parameters3, c(a, b), tolerance = 1e-4)
  
})

test_that("precision fitting works - lognormal",{
  skip_on_cran()
  med <- 10
  k <- 5
  # sigma^-2 ~ gamma(a, b)
  a <- 3
  b <- 4
  sigmasq <- 1 / qgamma(c(0.05, 0.95), a, b)
  
  probs1 <- plnorm(rep(med + k, 2), log(med), sigmasq^0.5) - 0.5
  pfit1 <- fitprecision(interval = c(med, med + k), propvals = probs1,
                       trans = "log", pplot = F)
  gamma.parameters1 <- unlist(pfit1$Gamma)
  attributes(gamma.parameters1) <- NULL
  expect_equal(gamma.parameters1, c(a, b), tolerance = 1e-4)
  
  probs2 <- plnorm(rep(med - k, 2), log(med), sigmasq^0.5) 
  pfit2 <- fitprecision(interval = c(-Inf, med - k), propvals = probs2,
                        med = med, 
                        trans = "log", pplot = F)
  gamma.parameters2 <- unlist(pfit2$Gamma)
  attributes(gamma.parameters2) <- NULL
  expect_equal(gamma.parameters2, c(a, b), tolerance = 1e-4)
  
  probs3 <- 1 - plnorm(rep(med + k, 2), log(med), sigmasq^0.5) 
  pfit3 <- fitprecision(interval = c(med + k, Inf), propvals = probs3,
                        med = med, 
                        trans = "log", pplot = F)
  gamma.parameters3 <- unlist(pfit3$Gamma)
  attributes(gamma.parameters3) <- NULL
  expect_equal(gamma.parameters3, c(a, b), tolerance = 1e-4)
})


test_that("linear pooling works",{
  skip_on_cran()
  #p1 <- c(runif(1, 0.1, 0.4), 0.5, runif(1, 0.6, 0.9))
  p1 <- c(0.25, 0.5, 0.75)
  a <- 10; b <- 4
  v1 <- qgamma(p1, a, b)
  mu <- 3 ; sigma <- 2
  v2 <- qnorm(p1, mu, sigma)
  v3 <- qlnorm(p1, log(mu), sigma)
  V <- matrix(c(v1, v2, v3), 3, 3)
  myfit <- fitdist(vals = V, probs = p1, lower = 0)
  
  w1 <- 1/6; w2 <- 2/6; w3 <- 3/6
  xtest <- 1.5
  qu <- 0.95
  
  qlp <- qlinearpool(myfit, qu, w = c(w1, w2, w3))
  qcheck <- w1 * pgamma(qlp, a, b) + 
    w2 * pnorm(qlp, mu, sigma) +
    w3 * plnorm(qlp, log(mu), sigma)
  expect_equal(qcheck, qu , tolerance = 1e-4)
  
  expect_equal(plinearpool(myfit, qlp, w = c(w1, w2, w3)),
               qu , tolerance = 1e-4)
  
  plp <- plinearpool(myfit, x = xtest, w = c(w1, w2, w3))
  pcheck <- w1 * pgamma(xtest, a, b) + 
    w2 * pnorm(xtest, mu, sigma) +
    w3 * plnorm(xtest, log(mu), sigma)
  expect_equal(plp, pcheck , tolerance = 1e-4)
})

test_that("linear pooling works - different lower limits",{
  skip_on_cran()
  llimits <- c(-2, 1, -4)
  p1 <- c(0.25, 0.5, 0.6, 0.75)
  a <- 10; b <- 4
  v1 <- llimits[1] + qgamma(p1, a, b)
  mu <- 3 ; sigma <- 2
  v2 <- llimits[2] + qlnorm(p1, log(mu), sigma)
  v3 <- llimits[3] + exp(1 + 2 * qt(p1, 3))
  V <- matrix(c(v1, v2, v3), length(p1), 3)
  myfit <- fitdist(vals = V, probs = p1, lower = llimits)
  
  w1 <- 1/6; w2 <- 2/6; w3 <- 3/6
  xtest <- 3
  qu <- 0.03
  
  qlp <- qlinearpool(myfit, qu, w = c(w1, w2, w3))
  qcheck <- w1 * pgamma(qlp - llimits[1], a, b) + 
    w2 * plnorm(qlp - llimits[2], log(mu), sigma) +
    w3 * pt((log(qlp - llimits[3]) - 1) / 2 , 3)
  expect_equal(qcheck, qu , tolerance = 1e-4)
  
  expect_equal(plinearpool(myfit, qlp, w = c(w1, w2, w3)),
               qu , tolerance = 1e-4)
  
  plp <- plinearpool(myfit, x = xtest, w = c(w1, w2, w3))
  pcheck <- w1 * pgamma(xtest - llimits[1], a, b) + 
    w2 * plnorm(xtest - llimits[2], log(mu), sigma) +
    w3 * pt((log(xtest - llimits[3]) - 1) / 2, 3)
  expect_equal(plp, pcheck , tolerance = 1e-4)
})
