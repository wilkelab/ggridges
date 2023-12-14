testthat::context("dropbig")

testthat::test_that("dropbig detects values above 'max'", {
  set.seed(238746)
  n <- 1000
  df <- data.frame(
    theta = rnorm(n),
    se = exp(rnorm(n, sd = 0.1))
  )
  df$theta[1] <- rnorm(1, mean = 1000)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = FALSE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  # Using robust standardisation is better at detecting outliers:
  df$theta[1] <- rnorm(1, mean = 500)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 100)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 50)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 25)
  out <- dropbig(data = df, estvarname = "theta", se = "se", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  # Without passing 'se':
  df$theta[1] <- rnorm(1, mean = 500)
  out <- dropbig(data = df, estvarname = "theta", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 100)
  out <- dropbig(data = df, estvarname = "theta", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 50)
  out <- dropbig(data = df, estvarname = "theta", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
  df$theta[1] <- rnorm(1, mean = 25)
  out <- dropbig(data = df, estvarname = "theta", methodvar = NULL, by = NULL, robust = TRUE)
  testthat::expect_equal(object = out$.dropbig[1], expected = TRUE)
})

testthat::test_that("dropbig throws errors when appropriate", {
  set.seed(238746)
  n <- 1000
  df <- data.frame(
    theta = rnorm(n),
    se = exp(rnorm(n, sd = 0.1))
  )
  df$theta[1] <- rnorm(1, mean = 1000)
  testthat::expect_error(rsimsum::dropbig(data = df))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = 1, se = "se"))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = 1))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = TRUE, se = "se"))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = TRUE))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = "se", methodvar = 1))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = "se", methodvar = TRUE))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = "se", by = 1))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = "se", by = TRUE))
  testthat::expect_error(object = rsimsum::dropbig(data = df, estvarname = "theta", se = "se", robust = "yeah!"))
})
