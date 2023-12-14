context("test plink data")

test_that("plink data", {
  testthat::skip_on_os("solaris")

  chosen_markers <- 1:100

  fake_input_data1 <- create_artificial_genomeadmixr_data(
    number_of_individuals = 100,
    marker_locations = chosen_markers,
    used_nucleotides = 1
  )


  recombination_rate <- 2

  simulation_result <- simulate_admixture(module =
                                            sequence_module(molecular_data =
                                                              fake_input_data1,
                                                            recombination_rate =
                                                              recombination_rate),
                                               pop_size = 1000,
                                               total_runtime = 10)

  write_as_plink(input_pop = simulation_result$population,
                 marker_locations = chosen_markers,
                 file_name_prefix = "plink_test",
                 recombination_rate = recombination_rate)

  read_result <- read_input_data(file_names = c("plink_test.ped",
                                                "plink_test.map"),
                                   chosen_chromosome = 1,
                                   type = "ped",
                                   verbose = TRUE)

  genomeadmixr_data <-
    simulation_data_to_genomeadmixr_data(simulation_result$population,
                                         markers = chosen_markers)

  testthat::expect_true(all.equal(genomeadmixr_data$markers,
                                  read_result$markers))

  testthat::expect_true(all.equal(genomeadmixr_data$genomes,
                                  read_result$genomes))

  testthat::expect_true(file.remove("plink_test.ped"))
  testthat::expect_true(file.remove("plink_test.map"))
  testthat::expect_false(file.exists("plink_test.ped"))
  testthat::expect_false(file.exists("plink_test.map"))
})
