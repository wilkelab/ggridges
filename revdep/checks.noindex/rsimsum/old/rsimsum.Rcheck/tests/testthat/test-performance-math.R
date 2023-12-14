testthat::context("performance-math")

testthat::test_that("bias", {
  set.seed(20190107)
  n <- 1000

  # No bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.50, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_equal(object = s$summ$est[s$summ$stat == "bias"], expected = 0, tolerance = 1e-4)

  # Positive bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.5001, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_true(object = (s$summ$est[s$summ$stat == "bias"] > 0))

  # Negative bias
  df <- data.frame(
    i = seq(n),
    b = rnorm(n = n, mean = 0.4999, sd = 0.001),
    se = exp(rnorm(n = n, mean = 0.1, sd = 0.1))
  )
  s <- rsimsum::simsum(data = df, estvarname = "b", se = "se", true = 0.50)
  testthat::expect_true(object = (s$summ$est[s$summ$stat == "bias"] < 0))
})

### Compare MIsim results with Stata's
testthat::test_that("MIsim, vs Stata", {
  data("MIsim", package = "rsimsum")
  r <- rsimsum::simsum(data = MIsim, estvarname = "b", se = "se", true = 0.5, methodvar = "method")$summ
  r <- r[!(r$stat %in% c("thetamean", "thetamedian", "se2mean", "se2median", "nsim", "becover")), ]
  r$method <- as.character(r$method)
  r <- r[order(r$stat, r$method), ]
  s <- MIsim_res_stata
  s <- s[order(s$stat, s$method), ]
  # Test equivalence of estimates
  testthat::expect_equivalent(object = r$est, expected = s$est, tolerance = 1e-6)
  # Test equivalence of MCSEs
  testthat::expect_equivalent(object = r$mcse, expected = s$mcse, tolerance = 1e-3)
})

### Compare relhaz results with Stata's
testthat::test_that("relhaz, vs Stata", {
  data("relhaz", package = "rsimsum")
  relhaz$model <- as.numeric(factor(relhaz$model))
  r <- rsimsum::simsum(data = relhaz, estvarname = "theta", se = "se", true = -0.5, methodvar = "model", by = c("n", "baseline"))$summ
  r <- r[!(r$stat %in% c("thetamean", "thetamedian", "se2mean", "se2median", "nsim", "becover")), ]
  r$model <- as.character(r$model)
  r$n <- as.character(r$n)
  r$baseline <- as.character(r$baseline)
  r <- r[order(r$stat, r$model, r$n, r$baseline), ]
  s <- relhaz_res_stata
  s <- s[order(s$stat, s$model, s$n, s$baseline), ]
  # Test equivalence of estimates
  testthat::expect_equivalent(object = r$est, expected = s$est, tolerance = 1e-6)
  # Test equivalence of MCSEs
  testthat::expect_equivalent(object = r$mcse, expected = s$mcse, tolerance = 0.005)
})
