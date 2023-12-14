test_that("test tf_initialize", {
  skip_if_no_tensorflow()
  set.seed(1337L)
  tensorflow::tf$keras$backend$set_floatx("float32")
  on.exit({ tensorflow::tf$keras$backend$set_floatx("float64") })

  N <- 100L
  dist <- dist_exponential()
  group <- sample(c(0, 1), size = N, replace = TRUE)
  x <- dist$sample(N, with_params = list(rate = group + 1))
  global_fit <- fit(dist, x)
  expect_equal(global_fit$params$rate, 1.36503094795327, tolerance = 1.0e-7)

  l_in <- keras::layer_input(shape = 1L)
  mod <- tf_compile_model(
    inputs = list(l_in),
    intermediate_output = l_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = FALSE
  )

  bias <- log(exp(global_fit$params$rate) - 1)

  tensorflow::set_random_seed(1337L)
  random_weight <- as.numeric(
    tensorflow::tf$random$uniform(list(1L), minval = -0.1, maxval = 0.1, dtype = keras::k_floatx()) * bias
  )
  tensorflow::set_random_seed(1337L)

  old_weights <- mod$model$get_weights()

  tf_initialise_model(mod, global_fit$params, mode = "none")
  expect_identical(mod$model$get_weights(), old_weights)
  tf_initialise_model(mod, global_fit$params, mode = "perturb")
  expect_equal(mod$model$get_weights(), list(old_weights[[1L]], array(bias)), tolerance = 1.0e-7)
  tf_initialise_model(mod, global_fit$params, mode = "scale")
  expect_equal(mod$model$get_weights(), list(matrix(random_weight), array(bias)), tolerance = 1.0e-7)
  tf_initialise_model(mod, global_fit$params, mode = "zero")
  expect_equal(mod$model$get_weights(), list(matrix(0.0), array(bias)), tolerance = 1.0e-7)
})
