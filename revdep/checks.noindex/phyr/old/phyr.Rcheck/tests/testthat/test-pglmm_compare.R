context("test phylogenetic GLMMs for comparative data")

test_that("ignore these tests when on CRAN since they are time consuming", {
  library(ape)
  n <- 100
  phy <- compute.brlen(rtree(n = n), method = "Grafen", power = 1)
  # Generate random data and standardize to have mean 0 and variance 1
  X1 <- rTraitCont(phy, model = "BM", sigma = 1)
  X1 <- (X1 - mean(X1)) / var(X1)
  # Simulate binary Y
  sim.dat <- data.frame(Y = array(0, dim = n),  X1 = X1, row.names = phy$tip.label)
  sim.dat$Y <- ape::binaryPGLMM.sim(Y ~ X1, phy = phy, data = sim.dat, s2 = 1,
      B = matrix(c(0, .25), nrow = 2, ncol = 1), nrep = 1)$Y
  # Fit model success
  expect_error(pglmm_compare(Y ~ X1, family = "binomial", phy = phy, data = sim.dat), NA)
})