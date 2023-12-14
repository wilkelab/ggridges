test_that("test tf_fit for full data", {
  skip_if_no_tensorflow()
  set.seed(1337L)

  dist <- dist_exponential()
  params <- list(rate = 1.0)
  N <- 100L
  rand_input <- runif(N)
  x <- dist$sample(N, with_params = params)

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = FALSE
  )

  expect_s3_class(mod, "reservr_keras_model")
  expect_identical(mod$dist, dist)
  expect_false(mod$loss_cens)
  expect_false(mod$loss_trunc)
  expect_s3_class(mod$model, "keras.engine.training.Model")

  tf_fit <- fit(
    object = mod,
    x = k_matrix(rand_input),
    y = x,
    epochs = 10L,
    callbacks = list(
      callback_adaptive_lr("loss", factor = 0.5, patience = 2L),
      keras::callback_reduce_lr_on_plateau("loss", min_lr = 1.0)
    )
  )

  expect_named(tf_fit, c("params", "metrics"), ignore.order = TRUE)
  expect_identical(unname(lengths(tf_fit$metrics)), rep(10L, 2L))
  # Loss was reduced in every step
  expect_lt(max(diff(tf_fit$metrics$loss)), 0)

  tf_preds <- predict(mod, data = k_matrix(rand_input))

  expect_named(tf_preds, "rate")
  expect_vector(tf_preds$rate, numeric(), size = N)
  expect_gt(min(tf_preds$rate), 0)

  tf_preds_mat <- predict(mod, data = k_matrix(rand_input), as_matrix = TRUE)
  expect_equal(colnames(tf_preds_mat), "rate")
  expect_equal(drop(tf_preds_mat), tf_preds$rate)

  expect_error(
    fit(
      mod,
      x = k_matrix(rand_input),
      y = trunc_obs(rep_len(NA_real_, N), x, x + 1.0)
    ),
    fixed = "contains censored observations"
  )

  expect_warning(
    fit(
      mod,
      x = k_matrix(rand_input),
      y = trunc_obs(x, tmin = 0.0),
      epochs = 10L
    ),
    fixed = "seems to contain truncated observations"
  )
})

test_that("test tf_fit for censored data", {
  skip_if_no_tensorflow()
  set.seed(1337L)

  dist <- dist_exponential()
  params <- list(rate = 1.0)
  N <- 100L
  rand_input <- runif(N)
  x <- dist$sample(N, with_params = params)

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = TRUE,
    truncation = FALSE
  )

  expect_s3_class(mod, "reservr_keras_model")
  expect_identical(mod$dist, dist)
  expect_true(mod$loss_cens)
  expect_false(mod$loss_trunc)
  expect_s3_class(mod$model, "keras.engine.training.Model")

  tf_fit <- fit(
    object = mod,
    x = k_matrix(rand_input),
    y = trunc_obs(
      x = rep_len(NA_real_, N),
      xmin = floor(x),
      xmax = ceiling(x)
    ),
    epochs = 10L,
    callbacks = list(
      callback_adaptive_lr("loss", factor = 0.5, patience = 2L),
      keras::callback_reduce_lr_on_plateau("loss", min_lr = 1.0)
    )
  )

  expect_named(tf_fit, c("params", "metrics"), ignore.order = TRUE)
  expect_identical(unname(lengths(tf_fit$metrics)), rep(10L, 2L))
  # Loss was reduced in every step
  expect_lt(max(diff(tf_fit$metrics$loss)), 0)

  tf_preds <- predict(mod, data = k_matrix(rand_input))

  expect_named(tf_preds, "rate")
  expect_vector(tf_preds$rate, numeric(), size = N)
  expect_gt(min(tf_preds$rate), 0)
})

test_that("test tf_fit for truncated data", {
  skip_if_no_tensorflow()
  set.seed(1747L)

  dist <- dist_exponential()
  params <- list(rate = 1.0)
  N <- 100L
  x <- dist$sample(N, with_params = params)

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = TRUE
  )

  expect_s3_class(mod, "reservr_keras_model")
  expect_identical(mod$dist, dist)
  expect_false(mod$loss_cens)
  expect_true(mod$loss_trunc)
  expect_s3_class(mod$model, "keras.engine.training.Model")

  obs <- truncate_obs(x, tmin_new = 0.5, tmax_new = 1.5)
  N <- nrow(obs)
  rand_input <- runif(N)

  tf_fit <- fit(
    object = mod,
    x = k_matrix(rand_input),
    y = obs,
    epochs = 10L,
    callbacks = list(
      callback_adaptive_lr("loss", factor = 0.5, patience = 2L),
      keras::callback_reduce_lr_on_plateau("loss", min_lr = 1.0)
    )
  )

  expect_named(tf_fit, c("params", "metrics"), ignore.order = TRUE)
  expect_identical(unname(lengths(tf_fit$metrics)), rep(10L, 2L))
  # Loss was reduced in every step
  expect_lt(max(diff(tf_fit$metrics$loss)), 0)

  tf_preds <- predict(mod, data = k_matrix(rand_input))

  expect_named(tf_preds, "rate")
  expect_vector(tf_preds$rate, numeric(), size = N)
  expect_gt(min(tf_preds$rate), 0)
})

test_that("test tf_fit for truncated and censored data", {
  skip_if_no_tensorflow()
  set.seed(1337L)

  dist <- dist_translate(dist_exponential(), offset = 1.0)
  params <- list(dist = list(rate = 1.0))
  N <- 100L
  x <- dist$sample(N, with_params = params)

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = TRUE,
    truncation = TRUE
  )

  expect_s3_class(mod, "reservr_keras_model")
  expect_identical(mod$dist, dist)
  expect_true(mod$loss_cens)
  expect_true(mod$loss_trunc)
  expect_s3_class(mod$model, "keras.engine.training.Model")

  obs <- truncate_obs(
    trunc_obs(
      x = rep_len(NA_real_, N),
      xmin = floor(x),
      xmax = ceiling(x)
    ), tmin_new = 1.5, tmax_new = 2.5,
    .partial = TRUE
  )
  N <- nrow(obs)
  rand_input <- runif(N)

  tf_fit <- fit(
    object = mod,
    x = k_matrix(rand_input),
    y = obs,
    epochs = 10L,
    callbacks = list(
      callback_adaptive_lr("loss", factor = 0.5, patience = 2L),
      keras::callback_reduce_lr_on_plateau("loss", min_lr = 1.0)
    )
  )

  expect_named(tf_fit, c("params", "metrics"), ignore.order = TRUE)
  expect_identical(unname(lengths(tf_fit$metrics)), rep(10L, 2L))
  # Loss was reduced in every step
  expect_lt(max(diff(tf_fit$metrics$loss)), 0)

  tf_preds <- predict(mod, data = k_matrix(rand_input))

  expect_named(tf_preds, "dist")
  expect_named(tf_preds$dist, "rate")
  expect_vector(tf_preds$dist$rate, numeric(), size = N)
  expect_gt(min(tf_preds$dist$rate), 0)
})

test_that("keras interop fitting works", {
  skip_if_no_tensorflow()
  set.seed(1337L)

  dist <- dist_exponential()
  params <- list(rate = 1.0)
  N <- 100L
  rand_input <- runif(N)
  x <- dist$sample(N, with_params = params)

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = FALSE
  )

  expect_s3_class(mod, "reservr_keras_model")
  expect_identical(mod$dist, dist)
  expect_false(mod$loss_cens)
  expect_false(mod$loss_trunc)
  expect_s3_class(mod$model, "keras.engine.training.Model")

  keras::fit(
    mod$model,
    x = k_matrix(rand_input),
    y = k_matrix(trunc_obs(x)),
    epochs = 10,
    batch_size = N,
    verbose = FALSE
  )

  tf_preds <- predict(mod, data = k_matrix(rand_input))

  expect_named(tf_preds, "rate")
  expect_vector(tf_preds$rate, numeric(), size = N)
  expect_gt(min(tf_preds$rate), 0)
})

test_that("tf_fit works with multiple nested parameters", {
  skip_if_no_tensorflow()
  set.seed(1337L)

  dist <- dist_blended(
    list(
      dist_translate(dist_exponential(), offset = 0, multiplier = -1),
      dist_exponential()
    )
  )

  params <- list(
    probs = list(0.5, 0.5),
    dists = list(
      list(dist = list(rate = 1)),
      list(rate = 1)
    ),
    breaks = list(0),
    bandwidths = list(0.3)
  )

  N <- 100L
  rand_input <- runif(N)
  x <- dist$sample(N, with_params = params)

  dist$default_params$breaks <- params$breaks
  dist$default_params$bandwidths <- params$bandwidths

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = TRUE
  )

  keras::fit(
    mod$model,
    x = k_matrix(rand_input),
    y = k_matrix(trunc_obs(x)),
    epochs = 10,
    batch_size = N,
    verbose = FALSE
  )

  tf_preds <- predict(mod, data = k_matrix(rand_input))
  expect_length(tf_preds, 2L)
})

test_that("complicated mixtures work with tensorflow", {
  skip_if_no_tensorflow()
  set.seed(1023L)
  tensorflow::set_random_seed(1023L)
  keras::k_set_floatx("float64")

  create_family <- function(n, m, u, eps) {
    diracs <- seq_len(n) - 1L
    diracs <- lapply(diracs, dist_dirac)
    offset <- n - 0.5
    erlang_mix <- dist_translate(dist_erlangmix(shapes = vector("list", m)), offset = offset)
    dist_mixture(
      dists = c(
        diracs,
        list(dist_blended(dists = list(
          erlang_mix,
          dist_genpareto1(u = u)
        ), breaks = list(u), bandwidths = list(eps)))
      )
    )
  }

  dist <- create_family(1L, 2L, 160, 32)
  params <- list(
    dists = list(
      list(),
      list(
        dists = list(
          list(dist = list(
            probs = list(
              0.5, 0.5
            ),
            shapes = list(1, 18),
            scale = 7
          )),
          list(sigmau = 160, xi = 0.8)
        ),
        probs = list(0.8, 0.2)
      )
    ),
    probs = list(
      0.2, 0.8
    )
  )
  dist$default_params$dists[[2]]$default_params$dists[[1]]$default_params$dist$default_params$shapes <-
    params$dists[[2]]$dists[[1]]$dist$shapes
  dist$default_params$dists[[2]]$default_params$dists[[2]]$default_params$xi <- params$dists[[2]]$dists[[2]]$xi

  N <- 1000
  x <- dist$sample(N, with_params = params)
  tau <- runif(length(x), max = 1000)
  obs <- truncate_obs(x, tmin_new = 0, tmax_new = tau)

  global_fit <- fit(dist, obs = obs, init = "kmeans")

  input <- runif(nrow(obs))

  tf_in <- keras::layer_input(1L)
  mod <- tf_compile_model(
    inputs = list(tf_in),
    intermediate_output = tf_in,
    dist = dist,
    optimizer = keras::optimizer_adam(),
    censoring = FALSE,
    truncation = TRUE
  )

  tf_initialise_model(mod, global_fit$params)

  cb <- callback_debug_dist_gradients(mod, k_matrix(input), obs, keep_grads = TRUE)

  expect_silent({
    history <- fit(
      mod, x = input, y = obs,
      batch_size = nrow(obs),
      callbacks = list(cb)
    )
  })

  expect_false(anyNA(history$metrics$loss))
})
