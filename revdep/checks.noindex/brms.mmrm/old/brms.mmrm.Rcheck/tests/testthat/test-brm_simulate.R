test_that("brm_simulate() data", {
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  data <- out$data
  expect_equal(dim(data), c(12L, 4L))
  expect_true(is.numeric(data$response))
  expect_false(anyNA(data$response))
  expect_equal(data$group, paste("group", rep(seq_len(2L), each = 6L)))
  expect_equal(data$patient, paste("patient", rep(seq_len(4L), each = 3L)))
  levels_time <- paste("time", seq_len(3L))
  expect_equal(data$time, rep(levels_time, times = 4L))
})

test_that("brm_simulate() model_matrix", {
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  matrix <- out$model_matrix
  expect_equal(dim(matrix), c(12L, 4L))
  colnames(matrix) <- gsub("^group", "", colnames(matrix))
  colnames(matrix) <- gsub("^time", "", colnames(matrix))
  expect_equal(as.integer(matrix[, "group 1"]), rep(c(1L, 0L), each = 6L))
  expect_equal(as.integer(matrix[, "group 2"]), rep(c(0L, 1L), each = 6L))
  expect_equal(
    as.integer(matrix[, "time 2"]),
    as.integer(out$data$time == "time 2")
  )
  expect_equal(
    as.integer(matrix[, "time 3"]),
    as.integer(out$data$time == "time 3")
  )
})

test_that("brm_simulate() parameters", {
  set.seed(0L)
  set.seed(0L)
  out <- brm_simulate(
    n_group = 2L,
    n_patient = 2L,
    n_time = 3L,
    hyper_beta = 1,
    hyper_sigma = 1,
    hyper_correlation = 1
  )
  params <- out$parameters
  expect_equal(names(params), c("beta", "sigma", "covariance"))
  expect_equal(length(params$beta), 4L)
  expect_null(dim(params$beta))
  expect_equal(length(params$sigma), 3L)
  expect_null(dim(params$sigma))
  expect_equal(dim(params$covariance), c(3L, 3L))
  for (value in params) {
    expect_true(all(is.finite(value)))
  }
})
