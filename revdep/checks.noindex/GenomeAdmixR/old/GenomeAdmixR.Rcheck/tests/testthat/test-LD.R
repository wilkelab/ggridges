context("LD stats")

test_that("calculate_average_LD", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 2
  run_time <- 1000
  morgan <- 1

  pop1 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)$population

  markers <- c(0.01, 0.99)

  outcome <- calculate_ld(pop1,
                          markers = markers)

  testthat::expect_message(
    calculate_ld(pop1,
                 markers = markers,
                 verbose = TRUE)
  )

  testthat::expect_equal(mean(outcome$ld_matrix, na.rm = T), 0.0)


  pop_size <- 1000
  number_of_founders <- 2
  run_time <- 1
  morgan <- 1

  pop1 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)$population

  markers <- c(0.5, 0.5 + 1e-4)

  outcome <- calculate_ld(pop1,
                          markers = markers)

  testthat::expect_equal(mean(outcome$ld_matrix, na.rm = T), 1.0)
})

test_that("calculate_LD_matrix", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 2
  sampled_individuals <- pop_size
  run_time <- 1
  morgan <- 1

  pop1 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

  testthat::expect_true(verify_population(pop1))

  vv <- calculate_ld(pop1, sampled_individuals,
                     markers = 10)

  vv1 <- as.vector(vv$ld_matrix[!is.na(vv$ld_matrix)])
  vv2 <- as.vector(vv$dist_matrix[!is.na(vv$dist_matrix)])

  linear_model <- lm(vv1 ~ vv2)
  # it should at least be negative
  testthat::expect_lt(linear_model$coefficients[[2]], 0.0)
})
