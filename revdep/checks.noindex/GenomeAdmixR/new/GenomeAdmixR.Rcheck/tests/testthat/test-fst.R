context("fst")

testthat::test_that("fst", {
  testthat::skip_on_os("solaris")
  pop_size <- 100
  number_of_founders <- 20
  run_time <- 1
  morgan <- 1

  pop1 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

  pop2 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

  pop2 <- increase_ancestor(pop2, number_of_founders)

  testthat::expect_true(verify_population(pop1))
  testthat::expect_true(verify_population(pop2))

  number_of_markers <- 100
  sampled_individuals <- 10
  v1 <- calculate_fst(pop1, pop2,
                      sampled_individuals,
                      number_of_markers, random_markers = TRUE)

  v2 <- calculate_fst(pop1, pop2,
                      sampled_individuals,
                      number_of_markers, random_markers = FALSE)

  testthat::expect_equal(v1, v2, tolerance = 0.2)

  pop_size <- 100
  number_of_founders <- 10
  run_time <- 1000
  morgan <- 1

  pop1 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)

  pop2 <- simulate_admixture(module = ancestry_module(number_of_founders =
                                                        number_of_founders,
                                                      morgan = morgan),
                             pop_size = pop_size,
                             total_runtime = run_time)
  pop2 <- increase_ancestor(pop2, number_of_founders)

  testthat::expect_true(verify_population(pop1))
  testthat::expect_true(verify_population(pop2))

  number_of_markers <- 100
  v1 <- calculate_fst(pop1, pop2,
                      number_of_markers, random_markers = TRUE)

  testthat::expect_equal(1.0, v1, tolerance = 0.2)
})
