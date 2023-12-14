skip_if_no_tensorflow <- function() {
  if (
    !reticulate::py_available(initialize = TRUE) ||
    !reticulate::py_module_available("tensorflow") ||
    !reticulate::py_module_available("tensorflow.keras")
  ) {
    skip("TensorFlow not available for testing")
  }
}

# Perform tests with high-precision floats
test_that("set floatx to 64-bit", {
  skip_if_no_tensorflow()
  expect_true({
    tensorflow::tf$keras$backend$set_floatx("float64")
    keras::k_set_floatx("float64")
    TRUE
  })
})

expect_density <- function(dist, dfun, args, x) {
  expect_equal(
    dist$density(x, with_params = args),
    do.call(dfun, c(list(x = x), args))
  )
  expect_equal(
    dist$density(x, log = TRUE, with_params = args),
    do.call(dfun, c(list(x = x, log = TRUE), args))
  )
  if (utils::hasName(dist, "compile_density")) {
    cmp <- dist$compile_density()
    acmp <- flatten_params_matrix(args)
    acmp <- acmp[rep(1L, length(x)), , drop = FALSE]
    expect_equal(
      cmp(x, acmp),
      do.call(dfun, c(list(x = x), args))
    )
    expect_equal(
      cmp(x, acmp, TRUE),
      do.call(dfun, c(list(x = x, log = TRUE), args))
    )
  }
}

expect_probability <- function(dist, pfun, args, q) {
  expect_equal(
    dist$probability(q, with_params = args),
    do.call(pfun, c(list(q = q), args))
  )
  expect_equal(
    dist$probability(q, lower.tail = FALSE, with_params = args),
    do.call(pfun, c(list(q = q, lower.tail = FALSE), args))
  )
  expect_equal(
    dist$probability(q, log.p = TRUE, with_params = args),
    do.call(pfun, c(list(q = q, log.p = TRUE), args))
  )
  expect_equal(
    dist$probability(q, lower.tail = FALSE, log.p = TRUE, with_params = args),
    do.call(pfun, c(list(q = q, lower.tail = FALSE, log.p = TRUE), args))
  )
  if (utils::hasName(dist, "compile_probability")) {
    cmp <- dist$compile_probability()
    acmp <- flatten_params_matrix(args)
    acmp <- acmp[rep(1L, length(q)), , drop = FALSE]
    expect_equal(
      cmp(q, acmp),
      do.call(pfun, c(list(q = q), args))
    )
    expect_equal(
      cmp(q, acmp, lower.tail = FALSE),
      do.call(pfun, c(list(q = q, lower.tail = FALSE), args))
    )
    expect_equal(
      cmp(q, acmp, log.p = TRUE),
      do.call(pfun, c(list(q = q, log.p = TRUE), args))
    )
    expect_equal(
      cmp(q, acmp, lower.tail = FALSE, log.p = TRUE),
      do.call(pfun, c(list(q = q, lower.tail = FALSE, log.p = TRUE), args))
    )
  }
}

expect_iprobability <- function(dist, args, xmin, xmax) {
  n <- check_lengths(xmin, xmax)
  xmin <- rep_len(xmin, n)
  xmax <- rep_len(xmax, n)

  interval_prob <- dist$probability(xmax, with_params = args) -
    dist$probability(xmin, with_params = args)

  disc <- dist$is_discrete_at(xmin, with_params = args)
  if (any(disc)) {
    interval_prob[disc] <- interval_prob[disc] +
      dist$density(xmin[disc], with_params = args)
  }

  cmp <- dist$compile_probability_interval()
  acmp <- flatten_params_matrix(args)
  acmp <- acmp[rep(1L, n), , drop = FALSE]

  expect_equal(
    cmp(xmin, xmax, acmp),
    interval_prob
  )
  expect_equal(
    cmp(xmin, xmax, acmp, log.p = TRUE),
    log(interval_prob)
  )
}

expect_quantile <- function(dist, qfun, args,
                            p = seq(0.0, 1.0, length.out = 100)) {
  expect_equal(
    dist$quantile(p, with_params = args),
    do.call(qfun, c(list(p = p), args))
  )
  expect_equal(
    dist$quantile(p, lower.tail = FALSE, with_params = args),
    do.call(qfun, c(list(p = p, lower.tail = FALSE), args))
  )
  expect_equal(
    dist$quantile(log(p), log.p = TRUE, with_params = args),
    do.call(qfun, c(list(p = log(p), log.p = TRUE), args))
  )
  expect_equal(
    dist$quantile(log(p), lower.tail = FALSE, log.p = TRUE, with_params = args),
    do.call(qfun, c(list(p = log(p), lower.tail = FALSE, log.p = TRUE), args))
  )
  if (utils::hasName(dist, "compile_quantile")) {
    cmp <- dist$compile_quantile()
    acmp <- flatten_params_matrix(args)
    acmp <- acmp[rep(1L, length(p)), , drop = FALSE]
    expect_equal(
      cmp(p, acmp),
      do.call(qfun, c(list(p = p), args))
    )
    expect_equal(
      cmp(p, acmp, lower.tail = FALSE),
      do.call(qfun, c(list(p = p, lower.tail = FALSE), args))
    )
    expect_equal(
      cmp(log(p), acmp, log.p = TRUE),
      do.call(qfun, c(list(p = log(p), log.p = TRUE), args))
    )
    expect_equal(
      cmp(log(p), acmp, lower.tail = FALSE, log.p = TRUE),
      do.call(qfun, c(list(p = log(p), lower.tail = FALSE, log.p = TRUE), args))
    )
  }
}

expect_diff_density <- function(dist, x, args, vars = NULL, lvars = NULL,
                                tolerance = .Machine$double.eps^(1 / 3)) {
  diff_dens <- dist$diff_density(x, with_params = args)
  diff_log_dens <- dist$diff_density(x, log = TRUE, with_params = args)

  if (is.null(vars)) {
    vars <- names(args[vapply(args, Negate(is.list), logical(1L))])
  }

  if (is.null(lvars)) {
    lvars <- names(args[vapply(args, is.list, logical(1L))])
  }

  expect_named(diff_dens, names(args), ignore.order = TRUE)
  expect_named(diff_log_dens, names(args), ignore.order = TRUE)

  for (arg in vars) {
    expect_length(diff_dens[[!!arg]], length(x))
    expect_length(diff_log_dens[[!!arg]], length(x))

    dfun <- function(val) {
      args[[arg]] <- val
      dist$density(x, with_params = args)
    }

    dlogfun <- function(val) {
      args[[arg]] <- val
      dist$density(x, log = TRUE, with_params = args)
    }

    expect_equal(
      diff_dens[[!!arg]],
      jacobian(func = dfun, x = !!args[[arg]]),
      tolerance = tolerance
    )
    expect_equal(
      diff_log_dens[[!!arg]],
      jacobian(func = dlogfun, x = !!args[[arg]]),
      tolerance = tolerance
    )
  }

  for (arg in lvars) {
    num_components <- length(diff_dens[[arg]])
    for (k in seq_len(num_components)) {
      expect_length(diff_dens[[!!arg]][[!!k]], length(x))
      expect_length(diff_log_dens[[!!arg]][[!!k]], length(x))

      dfun <- function(val) {
        args[[arg]][[k]] <- val
        dist$density(x, with_params = args)
      }

      dlogfun <- function(val) {
        args[[arg]][[k]] <- val
        dist$density(x, log = TRUE, with_params = args)
      }

      expect_equal(
        diff_dens[[!!arg]][[!!k]],
        jacobian(func = dfun, x = !!args[[arg]][[k]]),
        tolerance = tolerance
      )
      expect_equal(
        diff_log_dens[[!!arg]][[!!k]],
        jacobian(func = dlogfun, x = !!args[[arg]][[k]]),
        tolerance = tolerance
      )
    }
  }
}

expect_diff_probability <- function(dist, q, args, vars = NULL, lvars = NULL,
                                    tolerance = .Machine$double.eps^(1 / 3)) {
  diff_prob <- dist$diff_probability(q, with_params = args)
  diff_log_prob <- dist$diff_probability(q, log.p = TRUE, with_params = args)
  diff_prob_ut <- dist$diff_probability(
    q, lower.tail = FALSE, with_params = args
  )
  diff_log_prob_ut <- dist$diff_probability(
    q, lower.tail = FALSE, log.p = TRUE, with_params = args
  )

  if (is.null(vars)) {
    vars <- names(args[vapply(args, Negate(is.list), logical(1L))])
  }

  if (is.null(lvars)) {
    lvars <- names(args[vapply(args, function(x) {
      is.list(x) && all(vapply(x, Negate(is.list), logical(1L)))
    }, logical(1L))])
  }

  expect_named(diff_prob, names(args), ignore.order = TRUE)
  expect_named(diff_log_prob, names(args), ignore.order = TRUE)
  expect_named(diff_prob_ut, names(args), ignore.order = TRUE)
  expect_named(diff_log_prob_ut, names(args), ignore.order = TRUE)

  for (arg in vars) {
    expect_length(diff_prob[[!!arg]], length(q))
    expect_length(diff_log_prob[[!!arg]], length(q))
    expect_length(diff_prob_ut[[!!arg]], length(q))
    expect_length(diff_log_prob_ut[[!!arg]], length(q))

    pfun <- function(val) {
      args[[arg]] <- val
      dist$probability(q, with_params = args)
    }

    plogfun <- function(val) {
      args[[arg]] <- val
      dist$probability(q, log.p = TRUE, with_params = args)
    }

    pfun_ut <- function(val) {
      args[[arg]] <- val
      dist$probability(q, lower.tail = FALSE, with_params = args)
    }

    plogfun_ut <- function(val) {
      args[[arg]] <- val
      dist$probability(q, lower.tail = FALSE, log.p = TRUE, with_params = args)
    }

    expect_equal(
      diff_prob[[!!arg]],
      jacobian(func = pfun, x = !!args[[arg]]),
      tolerance = tolerance
    )
    expect_equal(
      diff_log_prob[[!!arg]],
      jacobian(func = plogfun, x = !!args[[arg]]),
      tolerance = tolerance
    )
    expect_equal(
      diff_prob_ut[[!!arg]],
      jacobian(func = pfun_ut, x = !!args[[arg]]),
      tolerance = tolerance
    )
    expect_equal(
      diff_log_prob_ut[[!!arg]],
      jacobian(func = plogfun_ut, x = !!args[[arg]]),
      tolerance = tolerance
    )
  }

  for (arg in lvars) {
    num_components <- length(diff_prob[[arg]])
    for (k in seq_len(num_components)) {
      expect_length(diff_prob[[!!arg]][[!!k]], length(q))
      expect_length(diff_log_prob[[!!arg]][[!!k]], length(q))
      expect_length(diff_prob_ut[[!!arg]][[!!k]], length(q))
      expect_length(diff_log_prob_ut[[!!arg]][[!!k]], length(q))

      pfun <- function(val) {
        args[[arg]][[k]] <- val
        dist$probability(q, with_params = args)
      }

      plogfun <- function(val) {
        args[[arg]][[k]] <- val
        dist$probability(q, log.p = TRUE, with_params = args)
      }

      pfun_ut <- function(val) {
        args[[arg]][[k]] <- val
        dist$probability(q, lower.tail = FALSE, with_params = args)
      }

      plogfun_ut <- function(val) {
        args[[arg]][[k]] <- val
        dist$probability(q, lower.tail = FALSE, log.p = TRUE,
                         with_params = args)
      }

      expect_equal(
        diff_prob[[!!arg]][[!!k]],
        jacobian(func = pfun, x = !!args[[arg]][[k]]),
        tolerance = tolerance
      )
      expect_equal(
        diff_log_prob[[!!arg]][[!!k]],
        jacobian(func = plogfun, x = !!args[[arg]][[k]]),
        tolerance = tolerance
      )
      expect_equal(
        diff_prob_ut[[!!arg]][[!!k]],
        jacobian(func = pfun_ut, x = !!args[[arg]][[k]]),
        tolerance = tolerance
      )
      expect_equal(
        diff_log_prob_ut[[!!arg]][[!!k]],
        jacobian(
          func = plogfun_ut, x = !!args[[arg]][[k]]
        ),
        tolerance = tolerance
      )
    }
  }
}

expect_tf_logdensity <- function(dist, args, x, tolerance = 1.0e-7) {
  skip_if_no_tensorflow()

  tf_logdens <- dist$tf_logdensity()
  x_tf <- keras::k_constant(x)
  args_tf <- dist$tf_make_constants(args)
  tf_logdens_result <- tf_logdens(x_tf, args_tf)
  expect_equal(tf_logdens_result$shape$rank, 1L)
  expect_equal(
    as.numeric(tf_logdens_result),
    dist$density(x, with_params = args, log = TRUE),
    tolerance = tolerance
  )
}

expect_tf_logprobability <- function(dist, args, xmin, xmax,
                                     tolerance = 1.0e-5) {
  skip_if_no_tensorflow()

  n <- check_lengths(xmin, xmax)
  xmin <- rep_len(xmin, n)
  xmax <- rep_len(xmax, n)

  tf_logprob <- dist$tf_logprobability()
  interval_prob <- dist$probability(xmax, with_params = args) -
    dist$probability(xmin, with_params = args)

  xmin_tf <- keras::k_constant(xmin)
  xmax_tf <- keras::k_constant(xmax)
  args_tf <- dist$tf_make_constants(args)

  disc <- dist$is_discrete_at(xmin, with_params = args)
  if (any(disc)) {
    interval_prob[disc] <- interval_prob[disc] +
      dist$density(xmin[disc], with_params = args)
  }

  log_interval_prob <- rep_len(NA_real_, length(interval_prob))
  log_interval_prob[interval_prob > 0.0] <-
    log(interval_prob[interval_prob > 0.0])
  log_interval_prob[interval_prob == 0.0] <- -Inf

  tf_logprobs <- tf_logprob(xmin_tf, xmax_tf, args_tf)

  expect_equal(tf_logprobs$shape$rank, 1L)

  expect_equal(
    as.numeric(tf_logprobs),
    log_interval_prob,
    tolerance = tolerance
  )
}

expect_tf_fit <- function(dist, args, support, global_fit_args = NULL) {
  skip_if_no_tensorflow()
  x <- dist$sample(10L, with_params = args)
  c0 <- support$range[1L]
  c1 <- pmax(x - 1.0, 0.5 * (x + support$range[1L]))
  c2 <- pmin(x + 1.0, 0.5 * (x + support$range[2L]))
  if (support$integer) {
    c1 <- floor(c1)
    c2 <- ceiling(c2)
  }
  c3 <- support$range[2L]

  #                   1     2   3   4  5   6   7
  sample_mat <- rbind(-Inf, c0, c1, x, c2, c3, Inf)
  #           obs, obs|supp, lcens, lcens|supp, icens, rcens|supp, rcens, ltrunc, rtrunc, itrunc
  r_xmin <- c(4L,  4L,       1L,    2L,         3L,    3L,         3L,    4L,     4L,     4L)
  r_xmax <- c(4L,  4L,       5L,    5L,         5L,    6L,         7L,    4L,     4L,     4L)
  r_tmin <- c(1L,  2L,       1L,    1L,         1L,    1L,         1L,    1L,     3L,     3L)
  r_tmax <- c(7L,  6L,       7L,    7L,         7L,    7L,         7L,    5L,     7L,     5L)
  c_offsets <- 7L * (seq_len(10L) - 1L)

  obs <- trunc_obs(
    xmin = sample_mat[r_xmin + c_offsets],
    xmax = sample_mat[r_xmax + c_offsets],
    tmin = sample_mat[r_tmin + c_offsets],
    tmax = sample_mat[r_tmax + c_offsets]
  )
  if (is.null(global_fit_args)) {
    p0 <- fit(dist, obs)$params
  } else {
    p0 <- do.call(fit, c(list(dist, obs = obs), global_fit_args))$params
  }
  x_in <- keras::k_constant(rep(1.0, nrow(obs)), shape = list(nrow(obs)))
  x_in1 <- keras::k_constant(1.0, shape = 1L)
  l_in <- keras::layer_input(shape = 1L)
  tf_mod <- tf_compile_model(
    inputs = list(l_in),
    intermediate_output = l_in,
    dist = dist,
    optimizer = keras::optimizer_sgd()
  )
  expect_s3_class(tf_mod, "reservr_keras_model")
  out <- character(nrow(obs))
  for (i in seq_len(nrow(obs))) {
    tf_initialise_model(tf_mod, p0, mode = "zero")
    gr_debugger <- callback_debug_dist_gradients(tf_mod, x_in1, obs[i, ], keep_grads = TRUE)
    out[i] <- paste(capture.output(
      fit(tf_mod, x_in1, obs[i, ], callbacks = list(gr_debugger)),
      type = "message"
    ), collapse = "")
  }
  expect_identical(out, character(nrow(obs)))
  tf_initialise_model(tf_mod, p0, mode = "zero")
  gr_debugger <- callback_debug_dist_gradients(tf_mod, x_in, obs, keep_grads = TRUE)
  train_out <- paste(capture.output(
    tf_hist <- fit(tf_mod, x_in, obs, callbacks = list(gr_debugger)),
    type = "message"
  ), collapse = "")
  expect_equal(train_out, "")
  expect_s3_class(tf_hist, "keras_training_history")
}
