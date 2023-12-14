context("create_populations")

test_that("save_population", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 10
  run_time <- 10
  morgan <- 1

  vx <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                      number_of_founders,
                                                    morgan = morgan),
                           pop_size = pop_size,
                           total_runtime = run_time)

  testthat::expect_true(verify_population(vx))

  save_population(vx, file_name = "test.pop")

  vy <- load_population(file_name = "test.pop")

  testthat::expect_true(verify_population(vy$population))

  testthat::expect_equal(length(vx$population), length(vy$population))

  for (i in seq_along(vx)) {
    testthat::expect_true(all.equal(vx[[i]], vy[[i]]))
  }
  testthat::expect_true(file.remove("test.pop"))
})

test_that("data", {
  data("dgrp2.3R.5k.data")
  testthat::expect_equal(length(dgrp2.3R.5k.data$markers),
                         4603)

  testthat::expect_equal(length(dgrp2.3R.5k.data$genomes[, 1]),
                         410)
})
