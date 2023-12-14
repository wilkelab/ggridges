test_that("brm_data() good", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$group <- as.factor(data$group)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  out <- brm_data(
    data = data,
    outcome = "col_response",
    role = "response",
    group = "col_group",
    time = "col_time",
    patient = "col_patient",
    covariates = c("col_factor2", "col_factor3")
  )
  expect_s3_class(out, "brm_data")
  expect_true(tibble::is_tibble(out))
  expect_silent(brm_data_validate(out))
  expect_true(all(is.na(out$col_response[c(2L, 3L)])))
  expect_false(anyNA(out$col_response[- c(2L, 3L)]))
  expect_equal(nrow(out), 800L)
  expect_equal(
    sort(colnames(out)),
    sort(
      c(
        "col_patient",
        "col_time",
        "col_response",
        "col_group",
        "col_factor2",
        "col_factor3"
      )
    )
  )
  expect_equal(
    out$col_group,
    rep(paste0("group.", c(1L, 2L)), each = 400L)
  )
  expect_equal(
    out$col_time,
    rep(paste0("time.", seq_len(4L)), times = 200L)
  )
  expect_equal(
    sort(out$col_response[- c(2L, 3L)]),
    sort(c(data$col_response))
  )
  expect_equal(out$col_patient, out$col_factor2)
  expect_equal(out$col_patient, out$col_factor3)
  expect_equal(attr(out, "brm_outcome"), "col_response")
  expect_equal(attr(out, "brm_role"), "response")
  expect_equal(attr(out, "brm_group"), "col_group")
  expect_equal(attr(out, "brm_time"), "col_time")
  expect_equal(attr(out, "brm_patient"), "col_patient")
  expect_equal(attr(out, "brm_covariates"), c("col_factor2", "col_factor3"))
  expect_equal(
    sort(attr(out, "brm_levels_group")), c("group.1", "group.2")
  )
  expect_equal(
    sort(attr(out, "brm_levels_time")), paste0("time.", seq_len(4L))
  )
  expect_equal(
    sort(attr(out, "brm_labels_group")), c("group 1", "group 2")
  )
  expect_equal(
    sort(attr(out, "brm_labels_time")), paste("time", seq_len(4L))
  )
})

test_that("brm_data() bad role", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "nope",
      role = "response",
      group = "col_group",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})

test_that("brm_data() bad group", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "nope",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})

test_that("brm_data() levels ", {
  set.seed(0)
  sim <- brm_simulate()
  data <- tibble::as_tibble(sim$data)
  data$factor1 <- data$patient
  data$factor2 <- data$patient
  data$factor3 <- data$patient
  colnames(data) <- paste0("col_", colnames(data))
  data <- data[- c(2L, 3L), ]
  data <- data[sample.int(n = nrow(data)), ]
  expect_error(
    brm_data(
      data = data,
      outcome = "col_response",
      role = "response",
      group = "nope",
      time = "col_time",
      patient = "col_patient",
      covariates = c("col_factor2", "col_factor3")
    ),
    class = "brm_error"
  )
})

test_that("brm_levels()", {
  expect_equal(brm_levels(c("a 1", "a 1", "b 2")), c("a.1", "a.1", "b.2"))
  expect_error(brm_levels(c("a 1", "a.1")), class = "brm_error")
})
