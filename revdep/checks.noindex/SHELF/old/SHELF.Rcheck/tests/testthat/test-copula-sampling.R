test_that("copula sampling ",{
  skip_on_cran()
  set.seed(123)
  p <- c(0.25, 0.5, 0.75)
  m1 <- 0.3
  m2 <- 20
 
  myfit1 <- fitdist(vals = c(0.2, m1, 0.45), probs = p, lower = 0, upper = 1)
  myfit2 <- fitdist(vals = c(10, m2, 35), probs = p, lower = 0, upper = 100)
  
  cp <- matrix(0, 2, 2)
  cp[1, 2] <- 0.8
  
  x <- copulaSample(myfit1, myfit2, cp = cp, n = 100000) 
  
  
  expect_equal(mean((x[, 1] > m1) & (x[, 2] > m2) | (x[, 1] < m1) & (x[, 2] < m2)), 
               0.8,
               tolerance = 3 * sqrt(0.8 * 0.2 / 100000))
})
