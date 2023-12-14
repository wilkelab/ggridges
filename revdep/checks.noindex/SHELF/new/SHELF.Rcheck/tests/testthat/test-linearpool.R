test_that("linear pool sampling ",{
  skip_on_cran()
  set.seed(123)
  m1 <- 10; m2 <- 20; m3 <- 40
  s1 <- 1; s2<- 2; s3 <- 3
  p <- c(0.25, 0.5, 0.75)
  N <- 1000
  v <- matrix(c(qnorm(p, m1, s1),
                qnorm(p, m2, s2),
                qnorm(p, m3, s3)),
              nrow = 3, ncol = 3)
  myfit <- fitdist(vals = v, probs = p, lower = 0, upper = 100)
  
  x <- rlinearpool(myfit, n = N)
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), mean(c(m1, m2, m3)), tolerance = 4 * se)
  
  weights <- c(0.1, 0.1, 0.8)
  x <- rlinearpool(myfit, n = N, w = weights)
  se <- sqrt(var(x) / N)
  expect_equal(mean(x), 
               sum(weights * c(m1, m2, m3)),
               tolerance = 4 * se)
})
