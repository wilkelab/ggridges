test_that("test integration of univariate function", {
  expect_equal(integrate_gk(sin, 0, c(1, 2) * pi), c(2, 0))
})

test_that("test integration of function with parameters", {
  dist <- dist_exponential()
  intfun <- function(x, p) dist$density(x, with_params = p)
  params <- list(rate = 1 / 1:10)
  lower <- 0
  upper <- 1:10
  expect_equal(
    integrate_gk(intfun, lower, upper, params),
    dist$probability(upper, with_params = params)
  )

  intfun_cmp <- dist$compile_density()
  expect_equal(
    integrate_gk(function(x, p) intfun_cmp(x, p), lower, upper, flatten_params_matrix(params)),
    dist$probability(upper, with_params = params)
  )
})

test_that("slow convergence still yields acceptable results", {
  integrand <- function(x) 1 / ((x + 1) * sqrt(x))
  expect_equal(
    integrate_gk(
      fun = integrand,
      lower = 0,
      upper = pi * c(1, 2),
      .max_iter = 100L,
      .tolerance = 1e-08
    ),
    vapply(pi * c(1, 2), function(u) integrate(
      f = integrand,
      lower = 0,
      upper = u
    )$value, numeric(1L))
  )

  expect_warning(
    integrate_gk(
      fun = integrand,
      lower = 0,
      upper = pi * c(1, 2),
      .max_iter = 10L
    ),
    fixed = "`.max_iter` reached"
  )

  expect_warning(
    integrate_gk(
      fun = integrand,
      lower = 0,
      upper = pi * c(1, 2),
      params = list(a = 42L)
    ),
    fixed = "Ignoring `params`."
  )
})
