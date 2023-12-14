test_that("extension: normal Y, normal X | Y ",{
  skip_on_cran()
  mY <- 5
  sY <- 2
  # c-distribution X| Y = 5 ~ N(3, 3^2)
  a <- 1
  b <- 2
  mX <- a + b * mY
  sX <- 3
  myfit <- fitdist(qnorm(c(0.25, 0.5, 0.75), mX, sX),
                   c(0.25, 0.5, 0.75))
  N <- 100000
  rY <- rnorm(N, mY, sY)
  yHyp <- c(0, 5, 10)
  x <- sampleMarginalFit(fitX = myfit,
                         sampleY = rY,
                         medianY = mY,
                         yCP = yHyp,
                         xMed = a + b * yHyp)
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), a+ b * mY, tolerance = 4 * se)
  expect_equal(var(x)^0.5, sqrt(b^2 *sY^2 + sX^2), tolerance = 0.1)
  
})

test_that("extension: normal Y, lognormal X | Y ",{
  skip_on_cran()
  mY <- 2
  sY <- 1
  # c-distribution log X | Y = 5 ~ N(3, 3^2)
  a <- 1
  b <- -2
  mX <- a + b * mY
  sX <- 0.5
  myfit <- fitdist(qlnorm(c(0.25, 0.5, 0.75), mX, sX),
                   c(0.25, 0.5, 0.75),
                   lower = 0)
  N <- 100000
  rY <- rnorm(N, mY, sY)
  yHyp <- c(0, 2, 4)
  x <- sampleMarginalFit(fitX = myfit,
                         sampleY = rY,
                         medianY = mY,
                         yCP = yHyp,
                         xMed = exp(a + b * yHyp),
                         link = "log")
  se <- sqrt(var(log(x)) / N)
  expect_equal(mean(log(x)), a+ b * mY, tolerance = 4 * se)
  expect_equal(var(log(x))^0.5, sqrt(b^2 *sY^2 + sX^2), tolerance = 0.1)
  
})

test_that("extension: sum of Gamma random variables ",{
  skip_on_cran()
  a <- 2
  b <- 3
  theta <- 4
  # c-distribution log X | Y = 5 ~ N(3, 3^2)
  mY <- qgamma(0.5, b, theta)
  myfit <- fitdist(mY + qgamma(c(0.25, 0.5, 0.75), a, theta),
                   c(0.25, 0.5, 0.75),
                   lower = mY)
  N <- 100000
  rY <- rgamma(N, b, theta)
  yHyp <- qgamma(c(0.05, 0.5, 0.95), b, theta)
  x <- sampleMarginalFit(fitX = myfit,
                         sampleY = rY,
                         medianY = mY,
                         yCP = yHyp,
                         xMed = yHyp + qgamma(0.5, a, theta),
                         link = "identity")
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), a/theta + b/theta, tolerance = 4 * se)
  expect_equal(var(x)^0.5, (a/theta^2 + b/theta^2)^0.5, tolerance = 0.1)
  
  
})