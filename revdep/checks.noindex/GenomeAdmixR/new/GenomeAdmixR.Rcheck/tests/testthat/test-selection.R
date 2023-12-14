context("selection two alleles")

test_that("select population two_alleles", {
  testthat::skip_on_os("solaris")
  select_matrix <- matrix(ncol = 5, nrow = 1)
  s <- 0.1
  select_matrix[1, ] <- c(0.05, 1.0, 1 + 0.5 * s, 1 + s, 0)

  selected_pop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                                10,
                                                              morgan = 1),
                                     pop_size = 100,
                                     total_runtime = 100,
                                     select_matrix = select_matrix)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  vv <- calculate_marker_frequency(selected_pop$population, 0.05)


  number_of_founders <- 10
  run_time <- 100

  selected_pop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                                number_of_founders,
                                                              morgan = 1),
                                     pop_size = 100,
                                     total_runtime = run_time,
                                     select_matrix = select_matrix)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))
})

test_that("select on population", {
  testthat::skip_on_os("solaris")
  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                             10,
                                                           morgan = 1),
                                  pop_size = 100,
                                 total_runtime = 1000)

  testthat::expect_true(verify_population(sourcepop))

  select_matrix <- matrix(ncol = 5, nrow = 1)
  s <- 0.1
  select_matrix[1, ] <- c(0.05, 1.0, 1 + 0.5 * s, 1 + s, 0)

  selected_pop <- simulate_admixture(module =
                                       ancestry_module(input_population = sourcepop,
                                                       morgan = 1),
                                     select_matrix = select_matrix,
                                    pop_size = 100,
                                    total_runtime = 100)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  selected_pop <- simulate_admixture(module =
                                      ancestry_module(input_population = sourcepop,
                                                       morgan = 1),
                                     select_matrix = select_matrix,
                                               pop_size = 100,
                                               total_runtime = 100)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))
})


test_that("select population two_alleles multiple markers", {
  testthat::skip_on_os("solaris")
  select_matrix <- matrix(ncol = 5, nrow = 2)
  s <- 0.1
  select_matrix[1, ] <- c(0.25, 1.0, 1 + 0.5 * s, 1 + s, 0)
  select_matrix[2, ] <- c(0.75, 1.0, 1, 1 + s,  1)

  selected_pop <- simulate_admixture(module = ancestry_module(number_of_founders = 10),
                                     pop_size = 100,
                                     total_runtime = 100,
                                     select_matrix = select_matrix)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders = 10),
                                  pop_size = 100,
                                  total_runtime = 1000)

  testthat::expect_true(verify_population(sourcepop))

  selected_pop <- simulate_admixture(module =
                                       ancestry_module(input_population = sourcepop),
                                     select_matrix = select_matrix,
                                     pop_size = 100,
                                     total_runtime = 100)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  selected_pop <- simulate_admixture(module =
                                       ancestry_module(input_population = sourcepop,
                                                       morgan = 1),
                                     select_matrix = select_matrix,
                                     pop_size = 100,
                                     total_runtime = 100)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))
})

test_that("select population two_alleles regions", {
  testthat::skip_on_os("solaris")
  select_matrix <- matrix(ncol = 5, nrow = 2)
  s <- 0.1
  select_matrix[1, ] <- c(0.25, 1.0, 1 + 0.5 * s, 1 + s, 0)
  select_matrix[2, ] <- c(0.75, 1.0, 1, 1 + s,  1)

  markers <- seq(from = 0.2, to = 0.3, length.out = 21)

  selected_pop <- simulate_admixture(module = ancestry_module(number_of_founders = 10,
                                                              markers = markers),
    pop_size = 100,
                                     total_runtime = 100,
                                     select_matrix = select_matrix)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  sourcepop <- simulate_admixture(module = ancestry_module(number_of_founders = 10),
                                  pop_size = 100,
                                  total_runtime = 1000)

  testthat::expect_true(verify_population(sourcepop))

  selected_pop <- simulate_admixture(module = ancestry_module(input_population = sourcepop,
                                                              markers = markers),
                                     select_matrix = select_matrix,
                                     pop_size = 100,
                                     total_runtime = 100)

  testthat::expect_equal(length(selected_pop$population), 100)
  testthat::expect_true(verify_population(selected_pop$population))

  plot_start_end(selected_pop)
  plot_start_end(selected_pop,
                 picked_ancestor = 0)

  plot_difference_frequencies(selected_pop)
  plot_difference_frequencies(selected_pop,
                              picked_ancestor = 0)
})

test_that("selection abuse", {
  testthat::skip_on_os("solaris")
  sourcepop <- simulate_admixture(pop_size = 100,
                                  total_runtime = 100)

  testthat::expect_true(verify_population(sourcepop))

  select_matrix <- matrix(ncol = 5, nrow = 3)
  s <- 0.1
  select_matrix[1, ] <- c(0.05, 1.0, 1 + 0.5 * s, 1 + s, 0)
  select_matrix[2, ] <- c(0.15, 1.0, 1 + 0.5 * s, 1 + s, 0)

  testthat::expect_error(
      simulate_admixture(module = ancestry_module(input_population = sourcepop),
                         select_matrix = select_matrix,
                         pop_size = 1000,
                         total_runtime = 1000),
    "Can't start, there are NA values in the selection matrix!"
  )

  testthat::expect_error(
    simulate_admixture(select_matrix = select_matrix,
                       pop_size = 1000,
                       total_runtime = 1000),
    "Can't start, there are NA values in the selection matrix!"
  )

  select_matrix <- matrix(ncol = 3, nrow = 3)
  select_matrix[1, ] <- c(0.0, NA, 0)
  select_matrix[2, ] <- c(NA, 1.0, 1)


  testthat::expect_error(
    simulate_admixture(module = ancestry_module(input_population = sourcepop),
                       select_matrix = select_matrix,
                       pop_size = 1000,
                       total_runtime = 1000),
    "Can't start, there are NA values in the selection matrix!"
  )

  testthat::expect_error(
    simulate_admixture(select_matrix = select_matrix,
                       pop_size = 100,
                       total_runtime = 10),
    "Can't start, there are NA values in the selection matrix!"
  )


  select_matrix <- matrix(ncol = 5, nrow = 2)
  s <- 0.1
  select_matrix[1, ] <- c(0.05, 1.0, 1 + 0.5 * s, 1 + s, 0)
  select_matrix[2, ] <- c(0.15, 1.0, 1 + 0.5 * s, 1 + s, 0)

  select_matrix <- matrix(ncol = 3, nrow = 1)
  s <- 0.1
  select_matrix[1, ] <- c(0.5, 0.1, 0.2)

  testthat::expect_error(
    simulate_admixture(module = ancestry_module(input_population = sourcepop),
                       select_matrix = select_matrix,
                       pop_size = 1000,
                       total_runtime = 10)
  )

  testthat::expect_error(
    simulate_admixture(module = ancestry_module(input_population = sourcepop,
                                                number_of_founders = 10),
                       select_matrix = select_matrix,
                       pop_size = 100,
                       total_runtime = 10)
  )
})
