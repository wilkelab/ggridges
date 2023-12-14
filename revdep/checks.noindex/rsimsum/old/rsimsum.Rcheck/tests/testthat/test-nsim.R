testthat::context("nsim")

testthat::test_that("nsim returns a single, double value", {
  testthat::expect_length(
    nsim(alpha = 0.05, sigma = 1, delta = 1, power = 0.5),
    n = 1
  )
  testthat::expect_type(
    nsim(alpha = 0.05, sigma = 1, delta = 1, power = 0.5),
    type = "double"
  )
})

testthat::test_that("alpha is in the appropriate range", {
  testthat::expect_error(
    nsim(alpha = 1.5, sigma = 1, delta = 1, power = 0.5),
    "Variable 'alpha': .+ <= 1",
    fixed = FALSE
  )
  testthat::expect_error(
    nsim(alpha = -0.1, sigma = 1, delta = 1, power = 0.5),
    "Variable 'alpha': .+ >= 0",
    fixed = FALSE
  )
})

testthat::test_that("sigma is greater than zero", {
  testthat::expect_error(
    nsim(alpha = 0.05, sigma = -0.1, delta = 1, power = -0.1),
    "Variable 'sigma': .+ >= 0",
    fixed = FALSE
  )
})

testthat::test_that("delta is greater than zero", {
  testthat::expect_error(
    nsim(alpha = 0.05, sigma = 1, delta = -0.1, power = -0.1),
    "Variable 'delta': .+ >= 0",
    fixed = FALSE
  )
})

testthat::test_that("power is in the appropriate range", {
  testthat::expect_error(
    nsim(alpha = 0.05, sigma = 1, delta = 1, power = 1.5),
    "Variable 'power': .+ <= 1",
    fixed = FALSE
  )
  testthat::expect_error(
    nsim(alpha = 0.05, sigma = 1, delta = 1, power = -0.1),
    "Variable 'power': .+ >= 0",
    fixed = FALSE
  )
})
