context("simulate_admixture")

test_that("simulate_admixture", {
  testthat::skip_on_os("solaris")
  select_matrix <- matrix(NA, nrow = 2, ncol = 5)

  s <- 0.1
  select_matrix[1, ] <- c(0.5, 0.5, 0.5 + 0.5 * s, 0.5 + s, 0)
  select_matrix[2, ] <- c(0.6, 0.5, 0.5 + 0.5 * s, 0.5 + s, 0)

  testthat::expect_message(
    vx <- simulate_admixture(pop_size = 100,
                             module = ancestry_module(number_of_founders = 2,
                                                      morgan = 1),
                             total_runtime = 1000,
                             select_matrix = select_matrix,
                             multiplicative_selection = FALSE)
  )
})


test_that("simulate admixture use", {
  testthat::skip_on_os("solaris")
  testthat::expect_output(
    vx <- simulate_admixture(pop_size = 100,
                             module = ancestry_module(number_of_founders = 2,
                                                      morgan = 1,
                                                      track_junctions = FALSE),
                             total_runtime = 100,
                             select_matrix = NA,
                             verbose = TRUE,
                             multiplicative_selection = TRUE)
  )

  select_matrix <- matrix(NA, nrow = 1, ncol = 5)
  testthat::expect_message(
    testthat::expect_error(simulate_admixture(pop_size = 100,
                                              total_runtime = 100,
                                              select_matrix = select_matrix,
                                              multiplicative_selection = TRUE))
  )
  select_matrix <- matrix(NA, nrow = 1, ncol = 3)
  testthat::expect_error(simulate_admixture(pop_size = 100,
                                            total_runtime = 100,
                                            select_matrix = select_matrix,
                                            multiplicative_selection = TRUE))

  markers <- seq(from = 0.4, to = 0.6, length.out = 100)
  testthat::expect_silent(
    vx <- simulate_admixture(module = ancestry_module(markers = markers),
                             pop_size = 100,
                             total_runtime = 100,
                             select_matrix = NA,
                             multiplicative_selection = TRUE)
  )

  testthat::expect_silent(
    vx <- simulate_admixture(module = ancestry_module(track_junctions = TRUE),
                             pop_size = 100,
                             total_runtime = 100,
                             select_matrix = NA,
                             multiplicative_selection = TRUE)
  )

  testthat::expect_silent(
    vx <- simulate_admixture(module = ancestry_module(markers = markers,
                                                      track_junctions = TRUE),
                             pop_size = 100,
                             total_runtime = 100,
                             multiplicative_selection = TRUE)
  )

  testthat::expect_silent(
  vx <- simulate_admixture(module = ancestry_module(
                                        initial_frequencies = c(0.5, 0.5)),
                           pop_size = 100,
                           total_runtime = 100)
  )

  testthat::expect_silent(
    vx <- simulate_admixture(pop_size = 100,
                             total_runtime = 100)
  )
  markers <- 0.5
  testthat::expect_silent(
  vx <- simulate_admixture(module = ancestry_module(markers = markers),
                           pop_size = 100,
                           total_runtime = 100)
  )

  # code coverage for displaying functions:
  testthat::expect_silent(plot(vx$population[[1]]))

  testthat::expect_output(
    testthat::expect_equal(print(vx$population),
                           "Population with 100 individuals")
  )
  testthat::expect_output(print(vx$population[[1]]))
})


test_that("simulate admixture use, junctions", {
  testthat::skip_on_os("solaris")
  vx <- simulate_admixture(module = ancestry_module(track_junctions = TRUE),
                           pop_size = 1000,
                           total_runtime = 100)

  num_j <- length(vx$junctions)
  testthat::expect_gt(num_j, 0)
  testthat::expect_equal(num_j, 100)
})

test_that("simulate admixture use, markers", {
  testthat::skip_on_os("solaris")
  pop <- simulate_admixture(module = ancestry_module(markers =
                                                       seq(0,
                                                           1,
                                                           length.out = 1000),
                                                     morgan = 1),
                            pop_size = 1000,
                            total_runtime = 3)

  a <- subset(pop$final_frequency, pop$final_frequency$location < 0.5)
  b <-  subset(pop$final_frequency, pop$final_frequency$location > 0.5)
  testthat::expect_equal(dim(a), dim(b))

  testthat::expect_silent(
    GenomeAdmixR::plot_difference_frequencies(pop)
  )


  pop <- simulate_admixture(module = ancestry_module(markers =
                                                       seq(0, 3,
                                                           length.out = 1000),
                                                     morgan = 3),
    pop_size = 1000,
                            total_runtime = 3)

  a <- subset(pop$final_frequency, pop$final_frequency$location < 1.5)
  b <-  subset(pop$final_frequency, pop$final_frequency$location > 1.5)
  testthat::expect_equal(dim(a), dim(b))

  GenomeAdmixR::plot_difference_frequencies(pop)

  pop <- simulate_admixture(module = ancestry_module(markers =
                                                       seq(0, 5,
                                                           length.out = 1000),
                                                     morgan = 5),
              pop_size = 1000,
                            total_runtime = 3)

  a <- subset(pop$final_frequency, pop$final_frequency$location < 2.5)
  b <-  subset(pop$final_frequency, pop$final_frequency$location > 2.5)
  testthat::expect_equal(dim(a), dim(b))
  GenomeAdmixR::plot_difference_frequencies(pop)
})

test_that("simulate admixture use, pop size", {
  testthat::skip_on_os("solaris")
  pop <- simulate_admixture(pop_size = 100,
                            total_runtime = 3)

  testthat::expect_equal(length(pop$population), 100)

  in_pop <- list(pop$population[[1]],
                 pop$population[[2]])

  pop2 <- simulate_admixture(module =
                               ancestry_module(input_population = in_pop),
                             pop_size = 2,
                             total_runtime = 3)

  testthat::expect_equal(length(pop2$population),
                         length(in_pop))

  pop3 <- simulate_admixture(module =
                               ancestry_module(input_population = in_pop),
                             pop_size = 100,
                             total_runtime = 3)

  testthat::expect_equal(length(pop3$population), 100)
})
