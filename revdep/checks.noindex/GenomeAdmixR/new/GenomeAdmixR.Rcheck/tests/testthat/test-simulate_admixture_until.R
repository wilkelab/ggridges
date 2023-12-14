context("simulate_admixture_until")

test_that("simulate_admixture_until", {
  testthat::skip_on_os("solaris")

  vx <- simulate_admixture(total_runtime = 100,
                           migration = migration_settings(
                             population_size = c(100, 100),
                             initial_frequencies = list(c(0.5, 0.5),
                                                        c(0.5, 0.5)),
                             stop_at_critical_fst = TRUE,
                             generations_between_update = 2,
                             critical_fst = 0.2,
                             migration_rate = 0.001),
                           verbose = FALSE)

  fst_2 <- calculate_fst(vx$population_1,
                         vx$population_2,
                         sampled_individuals = 100,
                         number_of_markers = 100,
                         random_markers = TRUE)

  testthat::expect_true(vx$FST >= 0.01)
  testthat::expect_true(fst_2 >= 0.01)


  testthat::expect_equal(length(vx$population_1), 100)
  testthat::expect_equal(length(vx$population_2), 100)
  testthat::expect_true(verify_population(vx$population_1))
  testthat::expect_true(verify_population(vx$population_2))
  testthat::expect_true(length(all.equal(vx$population_1,
                                         vx$population_2)) > 10)
})

test_that("simulate_admixture_until_data", {
  testthat::skip_on_os("solaris")

  num_indiv <- 100
  chosen_markers <- 1:100

  fake_input_data1 <-
    create_artificial_genomeadmixr_data(number_of_individuals = num_indiv,
                                        marker_locations = chosen_markers,
                                        used_nucleotides = 1)

  fake_input_data2 <-
    create_artificial_genomeadmixr_data(number_of_individuals = num_indiv,
                                        marker_locations = chosen_markers,
                                        used_nucleotides = 2)


  vx <- simulate_admixture(
    module = sequence_module(molecular_data =
                               list(fake_input_data1,
                                    fake_input_data2),
                             markers = chosen_markers,
                             morgan = 1),
    migration = migration_settings(
      population_size = c(100, 100),
      migration_rate = 0.001,
      critical_fst = 0.2,
      generations_between_update = 10),
    total_runtime = 100)



  fst_2 <- calculate_fst(vx$population_1,
                         vx$population_2,
                         sampled_individuals = 10,
                         number_of_markers = 100,
                         random_markers = TRUE)

  testthat::expect_true(vx$FST >= 0.05)
  testthat::expect_true(fst_2 >= 0.05)


  testthat::expect_equal(length(vx$population_1), 100)
  testthat::expect_equal(length(vx$population_2), 100)
  testthat::expect_true(length(all.equal(vx$population_1,
                                         vx$population_2)) > 10)
})
